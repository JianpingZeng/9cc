#include "cc.h"

/* Constant expressions in C:
 *
 * 1. interger
 * 2. floating
 * 3. address (address of a static extent)
 * 4. initializer (combination of the aboves)
 */

static node_t * eval_arith(node_t *expr);
static node_t * eval_address(node_t *expr);
static node_t * eval_initializer(node_t *expr);

static node_t * literal_node(int id)
{
    node_t *n = alloc_node();
    AST_ID(n) = id;
    EXPR_SYM(n) = anonymous(&constants, CONSTANT);
    return n;
}

static node_t * int_literal_node(node_t *ty, union value v)
{
    node_t *n = literal_node(INTEGER_LITERAL);
    AST_TYPE(n) = ty;
    SYM_TYPE(EXPR_SYM(n)) = ty;
    SYM_VALUE(EXPR_SYM(n)) = v;
    return n;
}

static node_t * float_literal_node(node_t *ty, union value v)
{
    node_t *n = literal_node(FLOAT_LITERAL);
    AST_TYPE(n) = ty;
    SYM_TYPE(EXPR_SYM(n)) = ty;
    SYM_VALUE(EXPR_SYM(n)) = v;
    return n;
}

// TODO: 
static bool eval_bool(node_t *cond)
{
    return false;
}

static node_t * arith2arith(node_t *dty, node_t *l)
{
    node_t *sty = AST_TYPE(l);
    if (isint(dty) && isint(sty)) {
	bool src_sign = op(sty) == INT ? true : false;
	bool dst_sign = op(dty) == INT ? true : false;
	union value src_val = SYM_VALUE(EXPR_SYM(l));
	union value dst_val;
	int src_size = typesize(sty);
	int dst_size = typesize(dty);
	println("%d => %d", src_size, dst_size);
	if (src_size > dst_size) {
	    // narrow
	    if (dst_sign) {
		if (src_sign)
		    dst_val.i = src_val.i & TYPE_LIMITS_MAX(dty).i;
		else
		    dst_val.i = src_val.u & TYPE_LIMITS_MAX(dty).u;

		if (kind(dty) == _BOOL)
		    dst_val.i = dst_val.i == 0 ? 0 : 1;
	    } else {
		if (src_sign)
		    dst_val.u = src_val.i & TYPE_LIMITS_MAX(dty).i;
		else
		    dst_val.u = src_val.u & TYPE_LIMITS_MAX(dty).u;

		if (kind(dty) == _BOOL)
		    dst_val.u = dst_val.u == 0 ? 0 : 1;
	    }
	} else {
	    // widden or equal
	    if (dst_sign) {
		if (src_sign)
		    dst_val.i = src_val.i;
		else
		    dst_val.i = src_val.u;
	    } else {
		if (src_sign)
		    dst_val.u = src_val.i;
		else
		    dst_val.u = src_val.u;
	    }
	}
	return int_literal_node(dty, dst_val);
    } else if (isint(dty) && isfloat(sty)) {
	// float => int
	bool src_ld = kind(sty) == LONG+DOUBLE ? true : false;
	bool dst_sign = op(dty) == INT ? true : false;
	union value src_val = SYM_VALUE(EXPR_SYM(l));
	union value dst_val;
	if (src_ld) {
	    if (dst_sign)
		dst_val.i = src_val.ld;
	    else
		dst_val.u = src_val.ld;
	} else {
	    if (dst_sign)
		dst_val.i = src_val.d;
	    else
		dst_val.u = src_val.d;
	}
	return int_literal_node(dty, dst_val);
    } else if (isfloat(dty) && isint(sty)) {
	// int => float
	bool src_sign = op(sty) == INT ? true : false;
	bool dst_ld = kind(dty) == LONG+DOUBLE ? true : false;
	union value src_val = SYM_VALUE(EXPR_SYM(l));
	union value dst_val;
	if (dst_ld) {
	    if (src_sign)
		dst_val.ld = src_val.i;
	    else
		dst_val.ld = src_val.u;
	} else {
	    if (src_sign)
		dst_val.d = src_val.i;
	    else
		dst_val.d = src_val.u;
	}
	return float_literal_node(dty, dst_val);
    } else if (isfloat(dty) && isfloat(sty)) {
	// float => float
	bool src_ld = kind(sty) == LONG+DOUBLE ? true : false;
	bool dst_ld = kind(dty) == LONG+DOUBLE ? true : false;
	union value src_val = SYM_VALUE(EXPR_SYM(l));
	union value dst_val;
	if (dst_ld) {
	    if (src_ld)
		dst_val.ld = src_val.ld;
	    else
		dst_val.ld = src_val.d;
	} else {
	    if (src_ld)
		dst_val.d = src_val.ld;
	    else
		dst_val.d = src_val.d;
	}
	return float_literal_node(dty, dst_val);
    }
    CCAssert(0);
}

static node_t * arith2ptr(node_t *dty, node_t *l)
{

}

static node_t * ptr2arith(node_t *dty, node_t *l)
{

}

static node_t * ptr2ptr(node_t *dty, node_t *l)
{

}

static node_t * cast(node_t *dty, node_t *l)
{
    if (l == NULL)
	return NULL;

    node_t *sty = AST_TYPE(l);
    if (isarith(sty) && isarith(dty))
	return arith2arith(dty, l);
    else if (isarith(sty) && isptr(dty))
	return arith2ptr(dty, l);
    else if (isptr(sty) && isarith(dty))
	return ptr2arith(dty, l);
    else if (isptr(sty) && isptr(dty))
	return ptr2ptr(dty, l);

    CCAssertf(0, "expect arith or ptr");
}

static node_t * bop_int(int oper, node_t *l, node_t *r)
{
    if (l == NULL || r == NULL)
	return NULL;
    CCAssert(isiliteral(l));
    CCAssert(isiliteral(r));
    CCAssert(AST_TYPE(l) == AST_TYPE(r));
    
    union value lval = SYM_VALUE(EXPR_SYM(l));
    union value rval = SYM_VALUE(EXPR_SYM(r));
    union value retval;

    if (op(AST_TYPE(l)) == INT) {
        long long ret = 0;
	switch (oper) {
	case '%':    ret = lval.i % rval.i; break;
	case LSHIFT: ret = lval.i << rval.i; break;
	case RSHIFT: ret = lval.i >> rval.i; break;
	case '|':    ret = lval.i | rval.i; break;
	case '&':    ret = lval.i & rval.i; break;
	case '^':    ret = lval.i ^ rval.i; break;
	default:     CCAssert(0);
	}
	retval.i = ret;
    } else {
	unsigned long long ret = 0;
	switch (oper) {
	case '%':    ret = lval.u % rval.u; break;
	case LSHIFT: ret = lval.u << rval.u; break;
	case RSHIFT: ret = lval.u >> rval.u; break;
	case '|':    ret = lval.u | rval.u; break;
	case '&':    ret = lval.u & rval.u; break;
	case '^':    ret = lval.u ^ rval.u; break;
	default:     CCAssert(0);
	}
	retval.u = ret;
    }

    return int_literal_node(AST_TYPE(l), retval);
}

static node_t * eval_arith(node_t *expr)
{
    node_t *l = EXPR_OPERAND(expr, 0);
    node_t *r = EXPR_OPERAND(expr, 1);
    switch (AST_ID(expr)) {
    case BINARY_OPERATOR:
        {
	    int op = EXPR_OP(expr);
	    switch (op) {
	    case '=':
		return NULL;
	    case ',':
		return r;
	    // int
	    case '%':
	    case LSHIFT: case RSHIFT:
	    case '|': case '&': case '^':
	        return bop_int(op, eval_arith(l), eval_arith(r));
	    case '+':
	    case '-':
	    case '*':
	    case '/':
	    
	    case '>':
	    case '<':
	    case GEQ:
	    case LEQ:
	    case EQ:
	    case NEQ:
	    
	    case AND:
	    case OR:
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
		if (eval_arith(l))
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
    case COMPOUND_LITERAL:
	return eval_arith(l);
    case CONV_EXPR:
    case CAST_EXPR:
	return cast(AST_TYPE(expr), eval_arith(l));
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
        if (EXPR_OP(expr) == ENUM)
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
	return expr;
    case MEMBER_EXPR:
    case STRING_LITERAL:
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
