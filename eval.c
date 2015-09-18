#include "cc.h"

/* Constant expressions in C:
 *
 * 1. interger
 * 2. floating
 * 3. address (address of a static extent)
 * 4. initializer (combination of the aboves)
 */

// TODO: 
static bool eval_bool(union node *cond)
{
    return false;
}

union node * eval(union node *expr)
{
    CCAssert(isexpr(expr));
    switch (AST_ID(expr)) {
    case BINARY_OPERATOR:
        {
	    switch (EXPR_OP(expr)) {
	    case '=':
		return NULL;
	    case ',':
	
	    case '+':
	    case '-':
	    case '*':
	    case '/':
	    case '%':

	    case LSHIFT:
	    case RSHIFT:

	    case '>':
	    case '<':
	    case GEQ:
	    case LEQ:
	    case EQ:
	    case NEQ:

	    case '|':
	    case '&':
	    case '^':
	    
	    case AND:
	    case OR:
		if (eval(EXPR_OPERAND(expr, 0)) && eval(EXPR_OPERAND(expr, 1)))    
		    return expr;
		else
		    return NULL;
		break;
	    default:
		CCAssert(0);
	    }
	}
    case UNARY_OPERATOR:
        {
	    switch (EXPR_OP(expr)) {
	    case INCR:
	    case DECR:
	    case '*':
		return NULL;
	    case '&':
	    case '+':
	    case '-':
	    case '~':
	    case '!':
		if (eval(EXPR_OPERAND(expr, 0)))
		    return expr;
		else
		    return NULL;
	    case SIZEOF:
		return expr;
	    default:
		CCAssert(0);
	    }
	}
    case PAREN_EXPR:
    case CONV_EXPR:
    case COMPOUND_LITERAL:
    case CAST_EXPR:
	return eval(EXPR_OPERAND(expr, 0));
    case MEMBER_EXPR:
	if (eval(EXPR_OPERAND(expr, 0)))
	    return expr;
	else
	    return NULL;
    case COND_EXPR:
	{
	    union node *cond = eval(EXPR_COND(expr));
	    if (cond) {
		if (eval_bool(cond))
		    return eval(EXPR_THEN(expr));
		else
		    return eval(EXPR_ELSE(expr));
	    }
	}
	return NULL;
    case REF_EXPR:
        if (EXPR_OP(expr) == ENUM ||
	    EXPR_SYM(expr)->sclass == EXTERN ||
	    EXPR_SYM(expr)->sclass == STATIC ||
	    EXPR_SYM(expr)->scope == GLOBAL)
	    return expr;
	else
	    return NULL;
    case INITS_EXPR:
        for (int i = 0; i < array_len((void **)EXPR_INITS(expr)); i++) {
	    union node *n = EXPR_INITS(expr)[i];
	    if (AST_ID(n) != VINIT_EXPR && eval(n) == NULL)
		return NULL;
	}
	return expr;
    case INTEGER_LITERAL:
    case FLOAT_LITERAL:
    case STRING_LITERAL:
	return expr;
    case CALL_EXPR:
	return NULL;
    default:
	CCAssert(0);
    }
}
