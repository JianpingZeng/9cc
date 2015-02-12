#include "cc.h"

static struct stmt * expr_stmt()
{
    struct stmt *ret;
    
    if (token->id == ';')
	return NULL;

    ret = stmt_node(EXPR_STMT, expression(), NULL);
    match(';');

    return ret;
}

static struct stmt * if_stmt()
{
    struct stmt *ret;
    struct expr *expr;
    struct stmt *stmt1;
    
    match(IF);
    match('(');
    expr = expression();
    match(')');

    stmt1 = statement();
    ret = stmt_node(IF_STMT, expr, stmt1);
    
    if (token->id == ELSE) {
	match(ELSE);
	ret = stmt_node(ELSE_STMT, ret, statement());
    }

    return ret;
}

static struct stmt * while_stmt()
{
    struct expr *expr;

    match(WHILE);
    match('(');
    expr = expression();
    match(')');

    return stmt_node(WHILE_STMT, expr, statement());
}

static struct stmt * do_while_stmt()
{
    struct stmt *stmt;
    struct expr *expr;
    
    match(DO);
    stmt = statement();
    match(WHILE);
    match('(');
    expr = expression();
    match(')');
    match(';');

    return stmt_node(DO_WHILE_STMT, stmt, expr);
}

struct stmt * statement()
{
    switch (token->id) {
	// compound
    case '{':
	return compound_statement();
	// selection
    case IF:
	return if_stmt();
    case SWITCH:
	break;
	// iteration
    case WHILE:
	return while_stmt();
    case DO:
	return do_while_stmt();
    case FOR:
	break;
	// jump
    case GOTO:
	break;
    case CONTINUE:
	break;
    case BREAK:
	break;
    case RETURN:
	break;
	// labeled
    case ID:
	{
	    struct token *ahead = lookahead();
	    if (ahead->id == ':') {

	    } else {
		goto expr;
	    }
	}
	break;
    case CASE:
	break;
    case DEFAULT:
	break;
	// expression
    default:
    expr:
        return expr_stmt();
    }
}

struct stmt * compound_statement()
{
    struct stmt *ret;

    match('{');


    match('}');

    return ret;
}
