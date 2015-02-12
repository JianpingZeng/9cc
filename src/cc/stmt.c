#include "cc.h"

static struct stmt * expr_stmt()
{

}

static struct stmt * if_stmt()
{
    struct stmt *ret;
    struct expr *expr1;
    struct stmt *stmt1;
    
    match(IF);
    match('(');
    expr1 = expression();
    match(')');

    stmt1 = statement();

    

    if (token->id == ELSE) {
	struct stmt *stmt2;
	match(ELSE);
	stmt2 = statement();
    }
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
	break;
    case DO:
	break;
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
	break;
    case CASE:
	break;
    case DEFAULT:
	break;
	// expression
    default:
        return expr_stmt();
    }
}

struct stmt * compound_statement()
{

}
