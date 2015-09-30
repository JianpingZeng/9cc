#include "cc.h"

static node_t * statement(void);

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
    node_t *ret = NULL;
    node_t *expr;
    node_t *stmt;

    SAVE_ERRORS;
    expect(WHILE);
    expect('(');
    expr = bool_expr();
    expect(')');
    stmt = statement();

    if (NO_ERROR)
	ret = ast_stmt(WHILE_STMT, expr, stmt);
    
    return ret;
}

static node_t * do_while_stmt(void)
{
    node_t *ret = NULL;
    node_t *stmt;
    node_t *expr;

    SAVE_ERRORS;
    expect(DO);
    stmt = statement();
    expect(WHILE);
    expect('(');
    expr = bool_expr();
    expect(')');
    expect(';');

    if (NO_ERROR)
	ret = ast_stmt(DO_WHILE_STMT, expr, stmt);
    
    return ret;
}

static node_t * for_stmt(void)
{
    node_t *ret = NULL;
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
    
    stmt = statement();
    exit_scope();

    if (NO_ERROR)
	ret = ast_stmt(FOR_STMT, stmt, NULL);
    
    return ret;
}

static node_t * switch_stmt(void)
{
    node_t *ret = NULL;
    node_t *expr;
    node_t *stmt;

    SAVE_ERRORS;
    expect(SWITCH);
    expect('(');
    expr = switch_expr();
    expect(')');
    stmt = statement();

    if (NO_ERROR)
	ret = ast_stmt(SWITCH_STMT, expr, stmt);
    
    return ret;
}

static node_t * case_stmt(void)
{
    node_t *ret = NULL;
    node_t *stmt;
    int index;

    SAVE_ERRORS;
    expect(CASE);
    index = intexpr();
    expect(':');
    // always parse even if not in a switch statement
    stmt = statement();

    if (NO_ERROR) {
	ret = ast_stmt(CASE_STMT, stmt, NULL);
	STMT_CASE_INDEX(ret) = index;
    }
    
    return ret;
}

static node_t * default_stmt(void)
{
    node_t *ret = NULL;
    node_t *stmt;

    SAVE_ERRORS;
    expect(DEFAULT);
    expect(':');
    stmt = statement();

    if (NO_ERROR)
	ret = ast_stmt(DEFAULT_STMT, stmt, NULL);

    return ret;
}

static node_t * label_stmt(void)
{
    node_t *ret = NULL;
    node_t *stmt;
    const char *name;
    
    SAVE_ERRORS;
    if (token->id == ID)
	name = token->name;
    expect(ID);
    expect(':');
    stmt = statement();

    if (NO_ERROR) {
	ret = ast_stmt(LABEL_STMT, stmt, NULL);
	STMT_LABEL_NAME(ret) = name;
    }

    return ret;
}

static node_t * goto_stmt(void)
{
    node_t *ret = NULL;
    const char *name;

    SAVE_ERRORS;
    expect(GOTO);
    if (token->id == ID)
        name = token->name;
    expect(ID);
    expect(';');

    if (NO_ERROR) {
	ret = ast_stmt(GOTO_STMT, NULL, NULL);
	STMT_LABEL_NAME(ret) = name;
    }

    return ret;
}

static node_t * break_stmt(void)
{
    node_t *ret = NULL;

    SAVE_ERRORS;
    expect(BREAK);
    expect(';');

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
