#include "cc.h"

struct stmt * stmt()
{
    switch (token->id) {
	// compound
    case '{':
	return compound_stmt();
	// selection
    case IF:
	break;
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
	break;
    }
}

struct stmt * compound_stmt()
{

}
