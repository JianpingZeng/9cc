#include "cc.h"

/* Constant expressions in C:
 *
 * 1. interger
 * 2. floating
 * 3. address (address of a static extent)
 * 4. initializer (combination of the aboves)
 */

// TODO: 
static bool eval_bool(node_t *cond)
{
    return false;
}

static node_t * eval_arith(node_t *expr)
{
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
		if (eval_arith(EXPR_OPERAND(expr, 0)) &&
		    eval_arith(EXPR_OPERAND(expr, 1)))    
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
		if (eval_arith(EXPR_OPERAND(expr, 0)))
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
	return eval_arith(EXPR_OPERAND(expr, 0));
    case MEMBER_EXPR:
	if (eval_arith(EXPR_OPERAND(expr, 0)))
	    return expr;
	else
	    return NULL;
    case COND_EXPR:
	{
	    node_t *cond = eval_arith(EXPR_COND(expr));
	    if (cond) {
		if (eval_bool(cond))
		    return eval_arith(EXPR_THEN(expr));
		else
		    return eval_arith(EXPR_ELSE(expr));
	    }
	}
	return NULL;
    case REF_EXPR:
        if (EXPR_OP(expr) == ENUM ||
	    SYM_SCLASS(EXPR_SYM(expr)) == EXTERN ||
	    SYM_SCLASS(EXPR_SYM(expr)) == STATIC ||
	    SYM_SCOPE(EXPR_SYM(expr)) == GLOBAL)
	    return expr;
	else
	    return NULL;
    case INITS_EXPR:
        for (int i = 0; i < array_len((void **)EXPR_INITS(expr)); i++) {
	    node_t *n = EXPR_INITS(expr)[i];
	    if (AST_ID(n) != VINIT_EXPR && eval_arith(n) == NULL)
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

static node_t * eval_address(node_t *expr)
{
    return NULL;
}

static node_t * eval_initializer(node_t *expr)
{
    return NULL;
}

node_t * eval(node_t *expr, node_t *ty)
{
    if (expr == NULL)
	return NULL;

    CCAssert(isexpr(expr));

    if (isarith(ty)) {
	return eval_arith(expr);
    } else if (isptr(ty)) {
	return eval_address(expr);
    } else if (isrecord(ty) || isarray(ty)) {
	return eval_initializer(expr);
    } else {
	CCAssertf(0, "try to eval for type '%s'", type2s(ty));
    }
}
