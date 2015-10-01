#include "cc.h"

static node_t * statement(void);

static node_t *__loop;
static node_t *__switch;
static struct vector *__cases;
static node_t *__default;

#define SET_LOOP_CONTEXT(loop)			\
    node_t *__saved_loop = __loop;		\
    __loop = loop

#define RESTORE_LOOP_CONTEXT()			\
    __loop = __saved_loop

#define SET_SWITCH_CONTEXT(sw)			\
    node_t *__saved_sw = __switch;		\
    struct vector *__saved_cases = __cases;	\
    node_t *__saved_default = __default;	\
    __switch = sw;				\
    __cases = vec_new();			\
    __default = NULL

#define RESTORE_SWITCH_CONTEXT()		\
    vec_free(__cases);				\
    __switch = __saved_sw;			\
    __cases = __saved_cases;			\
    __default = __saved_default

#define IN_SWITCH         (__switch)
#define IN_LOOP           (__loop)
#define CASES             (__cases)
#define DEFLT             (__default)
#define FTYPE             (current_ftype)
#define FNAME             (current_fname)

static void check_case_duplicates(node_t *node)
{
    for (int i = vec_len(CASES)-1; i >= 0; i--) {
	node_t *n = vec_at(CASES, i);
	if (STMT_CASE_INDEX(n) == STMT_CASE_INDEX(node)) {
	    errorf(AST_SRC(node),
		   "duplicate case value '%d', previous case defined here: %s:%u",
		   STMT_CASE_INDEX(node),
		   AST_SRC(n).file,
		   AST_SRC(n).line);
	    break;
	}
    }
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
    struct source src = source;

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
	ret = ast_stmt(IF_STMT, src);
	STMT_COND(ret) = expr;
	STMT_THEN(ret) = thenpart;
	STMT_ELSE(ret) = elsepart;
    }
    
    return ret;
}

static node_t * while_stmt(void)
{
    node_t *ret = ast_stmt(WHILE_STMT, source);
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
	STMT_WHILE_COND(ret) = expr;
	STMT_WHILE_BODY(ret) = stmt;
    } else {
        ret = NULL;
    }
    
    return ret;
}

static node_t * do_while_stmt(void)
{
    node_t *ret = ast_stmt(DO_WHILE_STMT, source);
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
	STMT_WHILE_COND(ret) = expr;
	STMT_WHILE_BODY(ret) = stmt;
    } else {
	ret = NULL;
    }
    
    return ret;
}

static node_t * for_stmt(void)
{
    node_t *ret = ast_stmt(FOR_STMT, source);
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
            STMT_FOR_DECL(ret) = declaration();
        } else {
            // expression
            STMT_FOR_INIT(ret) = expression();
            expect(';');
        }
    }
    
    if (token->id != ';')
        STMT_FOR_COND(ret) = bool_expr();
    
    expect(';');
    
    if (token->id != ')')
        STMT_FOR_CTRL(ret) = expression();
    
    expect(')');

    SET_LOOP_CONTEXT(ret);
    stmt = statement();
    RESTORE_LOOP_CONTEXT();
    
    exit_scope();

    if (NO_ERROR)
	STMT_FOR_BODY(ret) = stmt;
    else
	ret = NULL;
    
    return ret;
}

static node_t * switch_stmt(void)
{
    node_t *ret = ast_stmt(SWITCH_STMT, source);
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
	STMT_SWITCH_EXPR(ret) = expr;
	STMT_SWITCH_BODY(ret) = stmt;
    } else {
	ret = NULL;
    }
    
    return ret;
}

static node_t * case_stmt(void)
{
    node_t *ret = ast_stmt(CASE_STMT, source);
    node_t *stmt;

    SAVE_ERRORS;
    expect(CASE);
    STMT_CASE_INDEX(ret) = intexpr();
    expect(':');

    if (!IN_SWITCH)
	error("'case' statement not in switch statement");

    // only check when intexpr is okay.
    if (NO_ERROR) {
	check_case_duplicates(ret);
	vec_push(CASES, ret);
    }
    
    // always parse even if not in a switch statement
    stmt = statement();

    if (NO_ERROR)
	STMT_CASE_BODY(ret) = stmt;
    else
	ret = NULL;
    
    return ret;
}

static node_t * default_stmt(void)
{
    node_t *ret = ast_stmt(DEFAULT_STMT, source);
    node_t *stmt;

    SAVE_ERRORS;
    expect(DEFAULT);
    expect(':');

    // print before parsing statement
    if (!IN_SWITCH)
	error("'default' statement not in switch statement");

    if (DEFLT)
	errorf(AST_SRC(ret), "multiple default labels in one switch, previous case defined here:%s:%u",
	       AST_SRC(DEFLT).file,
	       AST_SRC(DEFLT).line);
    
    DEFLT = ret;
    
    stmt = statement();

    if (NO_ERROR)
	STMT_CASE_BODY(ret) = stmt;
    else
	ret = NULL;

    return ret;
}

static node_t * label_stmt(void)
{
    node_t *ret = ast_stmt(LABEL_STMT, source);
    node_t *stmt;
    const char *name = NULL;

    SAVE_ERRORS;
    name = token->name;
    expect(ID);
    expect(':');

    if (NO_ERROR) {
	node_t *n = map_get(labels, name);
	if (map_get(labels, name))
	    errorf(AST_SRC(ret), "redefinition of label '%s', previous label defined here:%s:%u",
		   name,
		   AST_SRC(n).file,
		   AST_SRC(n).line);
	STMT_LABEL_NAME(ret) = name;
	map_put(labels, name, ret);
    }
    
    stmt = statement();
    
    if (NO_ERROR)
	STMT_LABEL_BODY(ret) = stmt;
    else
	ret = NULL;

    return ret;
}

static node_t * goto_stmt(void)
{
    node_t *ret = ast_stmt(GOTO_STMT, source);

    SAVE_ERRORS;
    expect(GOTO);
    STMT_LABEL_NAME(ret) = token->name;
    expect(ID);
    expect(';');

    if (NO_ERROR)
	vec_push(gotos, ret);
    else
	ret = NULL;

    return ret;
}

static node_t * break_stmt(void)
{
    node_t *ret = ast_stmt(BREAK_STMT, source);

    SAVE_ERRORS;
    expect(BREAK);
    expect(';');

    if (!IN_LOOP && !IN_SWITCH)
	error("'break' statement not in loop or switch statement");

    if (!NO_ERROR)
	ret = NULL;
    
    return ret;
}

static node_t * continue_stmt(void)
{
    node_t *ret = ast_stmt(CONTINUE_STMT, source);

    SAVE_ERRORS;
    expect(CONTINUE);
    expect(';');

    if (!IN_LOOP)
	error("'continue' statement not in loop statement");

    if (!NO_ERROR)
	ret = NULL;
    
    return ret;
}

static node_t * return_stmt(void)
{
    node_t *ret = ast_stmt(RETURN_STMT, source);
    node_t *expr;

    SAVE_ERRORS;
    expect(RETURN);
    expr = expr_stmt();

    if (isvoid(rtype(FTYPE))) {
	if (expr && !is_null_stmt(expr))
	    errorf(AST_SRC(ret), "void function '%s' should not return a value", FNAME);
    } else {
	if (expr && !is_null_stmt(expr)) {
	    node_t *ty1 = AST_TYPE(expr);
	    node_t *ty2 = rtype(FTYPE);
	    if (!(expr = ret_conv(ty2, expr)))
		errorf(AST_SRC(ret),
		       "returning '%s' from function '%s' with incompatible result type '%s'",
		       type2s(ty1), FNAME, type2s(ty2));
	} else {
	    errorf(AST_SRC(ret), "non-void function '%s' should return a value", FNAME);
	}
    }
    
    if (NO_ERROR)
	STMT_RETURN_EXPR(ret) = expr;
    else
	ret = NULL;
    
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
    node_t *ret = ast_stmt(COMPOUND_STMT, source);
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
	    errorf(AST_SRC(goto_stmt), "use of undeclared label '%s'", label);
    }

}
