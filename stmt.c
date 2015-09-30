#include "cc.h"

static node_t * statement(void);

static node_t *loop_context;
static node_t *switch_context;
static struct vector *cases;
static node_t *default_context;

struct vector *gotos;
struct map *labels;

#define SET_LOOP_CONTEXT(loop)			\
    node_t *saved_loop = loop_context;		\
    loop_context = loop

#define RESTORE_LOOP_CONTEXT()			\
    loop_context = saved_loop

#define SET_SWITCH_CONTEXT(sw)			\
    node_t *saved_sw = switch_context;		\
    struct vector *saved_cases = cases;		\
    node_t *saved_default = default_context;	\
    switch_context = sw;			\
    cases = vec_new();				\
    default_context = NULL

#define RESTORE_SWITCH_CONTEXT()		\
    switch_context = saved_sw;			\
    cases = saved_cases;			\
    default_context = saved_default

#define IN_SWITCH         (switch_context)
#define IN_LOOP           (loop_context)
#define CASES             (cases)
#define DEFAULT_CONTEXT   (default_context)

static bool check_case_duplicates(node_t *node)
{
    for (int i = 0; i < vec_len(CASES); i++) {
	node_t *n = vec_at(CASES, i);
	if (STMT_CASE_INDEX(n) == STMT_CASE_INDEX(node)) {
	    errorf(AST_SOURCE(node), "duplicate case value '%d'", STMT_CASE_INDEX(node));
	    return false;
	}
    }

    return true;
}

static node_t * expr_stmt(void)
{
    node_t *ret = NULL;
    
    if (token->id == ';')
        ret = ast_null_stmt();
    else if (firstexpr(token))
        ret = expression();
    else
        error("missing statement before '%s'", token->name);
    
    expect(';');
    return ret;
}

static node_t * if_stmt(void)
{
    node_t *ret = NULL;
    node_t *expr;
    node_t *thenpart;
    node_t *elsepart = NULL;

    SAVE_ERRORS;
    expect(IF);
    expect('(');
    expr = bool_expr();
    expect(')');
    thenpart = statement();

    if (token->id == ELSE) {
        expect(ELSE);
	elsepart = statement();
    }

    if (NO_ERROR) {
	ret = ast_stmt(IF_STMT, NULL, NULL);
	STMT_COND(ret) = expr;
	STMT_THEN(ret) = thenpart;
	STMT_ELSE(ret) = elsepart;
    }
    
    return ret;
}

static node_t * while_stmt(void)
{
    node_t *ret = ast_stmt(WHILE_STMT, NULL, NULL);
    node_t *expr;
    node_t *stmt;

    SAVE_ERRORS;
    expect(WHILE);
    expect('(');
    expr = bool_expr();
    expect(')');
    SET_LOOP_CONTEXT(ret);
    stmt = statement();
    RESTORE_LOOP_CONTEXT();

    if (NO_ERROR) {
	AST_KID(ret, 0) = expr;
	AST_KID(ret, 1) = stmt;
    } else {
        ret = NULL;
    }
    
    return ret;
}

static node_t * do_while_stmt(void)
{
    node_t *ret = ast_stmt(DO_WHILE_STMT, NULL, NULL);
    node_t *stmt;
    node_t *expr;

    SAVE_ERRORS;
    expect(DO);
    SET_LOOP_CONTEXT(ret);
    stmt = statement();
    RESTORE_LOOP_CONTEXT();
    expect(WHILE);
    expect('(');
    expr = bool_expr();
    expect(')');
    expect(';');

    if (NO_ERROR) {
	AST_KID(ret, 0) = expr;
	AST_KID(ret, 1) = stmt;
    } else {
	ret = NULL;
    }
    
    return ret;
}

static node_t * for_stmt(void)
{
    node_t *ret = ast_stmt(FOR_STMT, NULL, NULL);
    node_t *stmt;

    SAVE_ERRORS;
    expect(FOR);
    expect('(');

    enter_scope();
    
    if (token->id == ';') {
        expect(';');
    } else {
        if (firstdecl(token)) {
            // declaration
            STMT_DECL(ret) = declaration();
        } else {
            // expression
            STMT_INIT(ret) = expression();
            expect(';');
        }
    }
    
    if (token->id != ';')
        STMT_COND(ret) = expression();
    
    expect(';');
    
    if (token->id != ')')
        STMT_CTRL(ret) = expression();
    
    expect(')');

    SET_LOOP_CONTEXT(ret);
    stmt = statement();
    RESTORE_LOOP_CONTEXT();
    
    exit_scope();

    if (NO_ERROR)
	AST_KID(ret, 0) = stmt;
    else
	ret = NULL;
    
    return ret;
}

static node_t * switch_stmt(void)
{
    node_t *ret = ast_stmt(SWITCH_STMT, NULL, NULL);
    node_t *expr;
    node_t *stmt;

    SAVE_ERRORS;
    expect(SWITCH);
    expect('(');
    expr = switch_expr();
    expect(')');
    SET_SWITCH_CONTEXT(ret);
    stmt = statement();
    RESTORE_SWITCH_CONTEXT();
    
    if (NO_ERROR) {
	AST_KID(ret, 0) = expr;
	AST_KID(ret, 1) = stmt;
    } else {
	ret = NULL;
    }
    
    return ret;
}

static node_t * case_stmt(void)
{
    node_t *ret = NULL;
    node_t *stmt;
    int index;
    struct source src = source;

    SAVE_ERRORS;
    expect(CASE);
    index = intexpr();
    expect(':');

    if (!IN_SWITCH)
	error("'case' statement not in switch statement");
    
    // always parse even if not in a switch statement
    stmt = statement();

    if (NO_ERROR) {
	ret = ast_stmt(CASE_STMT, stmt, NULL);
	STMT_CASE_INDEX(ret) = index;
	AST_SOURCE(ret) = src;
	if (check_case_duplicates(ret))
	    vec_push(CASES, ret);
	else
	    ret = NULL;
    }
    
    return ret;
}

static node_t * default_stmt(void)
{
    node_t *ret = NULL;
    node_t *stmt;
    struct source src = source;

    SAVE_ERRORS;
    expect(DEFAULT);
    expect(':');

    // print before parsing statement
    if (!IN_SWITCH)
	error("'default' statement not in switch statement");
    
    stmt = statement();

    if (DEFAULT_CONTEXT)
	errorf(src, "multiple default labels in one switch");

    if (NO_ERROR) {
	ret = ast_stmt(DEFAULT_STMT, stmt, NULL);
	DEFAULT_CONTEXT = ret;
    }

    return ret;
}

static node_t * label_stmt(void)
{
    node_t *ret = NULL;
    node_t *stmt;
    const char *name = NULL;
    struct source src = source;

    SAVE_ERRORS;
    if (token->id == ID)
	name = token->name;
    expect(ID);
    expect(':');
    stmt = statement();

    if (name && map_get(labels, name))
	errorf(src, "redefinition of label '%s'", name);
    
    if (NO_ERROR) {
	ret = ast_stmt(LABEL_STMT, stmt, NULL);
	STMT_LABEL_NAME(ret) = name;
	AST_SOURCE(ret) = src;
	map_put(labels, name, ret);
    }

    return ret;
}

static node_t * goto_stmt(void)
{
    node_t *ret = NULL;
    const char *name = NULL;
    struct source src = source;

    SAVE_ERRORS;
    expect(GOTO);
    if (token->id == ID)
        name = token->name;
    expect(ID);
    expect(';');

    if (NO_ERROR) {
	ret = ast_stmt(GOTO_STMT, NULL, NULL);
	STMT_LABEL_NAME(ret) = name;
	AST_SOURCE(ret) = src;
	vec_push(gotos, ret);
    }

    return ret;
}

static node_t * break_stmt(void)
{
    node_t *ret = NULL;

    SAVE_ERRORS;
    expect(BREAK);
    expect(';');

    if (!IN_LOOP && !IN_SWITCH)
	error("'break' statement not in loop or switch statement");

    if (NO_ERROR)
	ret = ast_stmt(BREAK_STMT, NULL, NULL);
    
    return ret;
}

static node_t * continue_stmt(void)
{
    node_t *ret = NULL;

    SAVE_ERRORS;
    expect(CONTINUE);
    expect(';');

    if (!IN_LOOP)
	error("'continue' statement not in loop statement");

    if (NO_ERROR)
	ret = ast_stmt(CONTINUE_STMT, NULL, NULL);
    
    return ret;
}

static node_t * return_stmt(void)
{
    node_t *ret = NULL;
    node_t *expr;

    SAVE_ERRORS;
    expect(RETURN);
    expr = expr_stmt();

    if (NO_ERROR)
	ret = ast_stmt(RETURN_STMT, expr, NULL);
    
    return ret;
}

static node_t * statement(void)
{
    switch (token->id) {
        case '{':       return compound_stmt();
        case IF:        return if_stmt();
        case SWITCH:    return switch_stmt();
        case WHILE:     return while_stmt();
        case DO:        return do_while_stmt();
        case FOR:       return for_stmt();
        case GOTO:      return goto_stmt();
        case CONTINUE:  return continue_stmt();
        case BREAK:     return break_stmt();
        case RETURN:    return return_stmt();
        case CASE:      return case_stmt();
        case DEFAULT:   return default_stmt();
        case ID:
            if (lookahead()->id == ':')
                return label_stmt();
            // go through
        default:
            return expr_stmt();
    }
}

node_t * compound_stmt(void)
{
    node_t *ret = ast_stmt(COMPOUND_STMT, NULL, NULL);
    struct vector *v = vec_new();
    
    expect('{');
    enter_scope();
    
    while (firstdecl(token) || firstexpr(token) || firststmt(token)) {
        if (firstdecl(token))
            // declaration
            vec_add_array(v, (void **)declaration());
        else
            // statement
            vec_push_safe(v, statement());
    }
    
    STMT_BLKS(ret) = (node_t **)vtoa(v);
    expect('}');
    exit_scope();
    
    return ret;
}

void backfill_labels(void)
{
    for (int i = 0; i < vec_len(gotos); i++) {
	node_t *goto_stmt = vec_at(gotos, i);
	const char *label = STMT_LABEL_NAME(goto_stmt);
	node_t *label_stmt = map_get(labels, label);
	if (!label_stmt)
	    errorf(AST_SOURCE(goto_stmt), "use of undeclared label '%s'", label);
    }

}
