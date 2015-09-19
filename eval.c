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
	union value dst_val = SYM_VALUE(EXPR_SYM(l));
	int src_size = typesize(sty);
	int dst_size = typesize(dty);
	if (src_size > dst_size) {
	    // narrow
	    dst_val.u &= TYPE_LIMITS_MAX(dty).u;
	    if (kind(dty) == _BOOL)
		dst_val.u = dst_val.u == 0 ? 0 : 1;
	}
	return int_literal_node(dty, dst_val);
    } else if (isint(dty) && isfloat(sty)) {
	// float => int
	union value src_val = SYM_VALUE(EXPR_SYM(l));
	union value dst_val;
	dst_val.u = src_val.d;
	return int_literal_node(dty, dst_val);
    } else if (isfloat(dty) && isint(sty)) {
	// int => float
	union value src_val = SYM_VALUE(EXPR_SYM(l));
	union value dst_val;
        dst_val.d = src_val.u;
	return float_literal_node(dty, dst_val);
    } else if (isfloat(dty) && isfloat(sty)) {
	// float => float
	int dst_kind = kind(dty);
	union value src_val = SYM_VALUE(EXPR_SYM(l));
	union value dst_val;
	if (dst_kind == FLOAT)
	    dst_val.d = (float)src_val.d;
        else if (dst_kind == DOUBLE)
	    dst_val.d = (double)src_val.d;
	else
	    dst_val.d = src_val.d;
	return float_literal_node(dty, dst_val);
    }
    return NULL;
}

static node_t * arith2ptr(node_t *dty, node_t *l)
{
    CCAssert(isint(AST_TYPE(l)));
    // int => ptr
    return int_literal_node(dty, SYM_VALUE(EXPR_SYM(l)));
}

static node_t * ptr2arith(node_t *dty, node_t *l)
{
    CCAssert(isint(dty));
    // ptr => int
    return int_literal_node(dty, SYM_VALUE(EXPR_SYM(l)));
}

static node_t * ptr2ptr(node_t *dty, node_t *l)
{
    // ptr => ptr
    return int_literal_node(dty, SYM_VALUE(EXPR_SYM(l)));
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
    union value ret;

    switch (oper) {
    case '%':    ret.u = lval.u % rval.u; break;
    case LSHIFT: ret.u = lval.u << rval.u; break;
    case RSHIFT: ret.u = lval.u >> rval.u; break;
    case '|':    ret.u = lval.u | rval.u; break;
    case '&':    ret.u = lval.u & rval.u; break;
    case '^':    ret.u = lval.u ^ rval.u; break;
    default:     CCAssert(0);
    }

    return int_literal_node(AST_TYPE(l), ret);
}

static node_t * bop_scalar(int oper, node_t *dty, node_t *l, node_t *r)
{
    if (l == NULL || r == NULL)
	return NULL;

    CCAssert(op(dty) == INT || op(dty) == UNSIGNED || op(dty) == FLOAT);
    CCAssert(isiliteral(l) || isfliteral(l));
    CCAssert(isiliteral(r) || isfliteral(r));
    
    union value lval = SYM_VALUE(EXPR_SYM(l));
    union value rval = SYM_VALUE(EXPR_SYM(r));
    union value ret;
    bool l_is_u = isiliteral(l);
    bool r_is_u = isiliteral(r);
    bool ret_is_u = op(dty) == FLOAT ? false : true;

#define LOR(oo) \
    do { \
        if (ret_is_u) { \
	    if (l_is_u) { \
		if (r_is_u) \
		    ret.u = lval.u oo rval.u; \
		else \
		    ret.u = lval.u oo rval.d; \
	    } else { \
		if (r_is_u) \
		    ret.u = lval.d oo rval.u; \
		else \
		    ret.u = lval.d oo rval.d; \
	    } \
	} else { \
	    if (l_is_u) { \
		if (r_is_u) \
		    ret.d = lval.u oo rval.u; \
		else \
		    ret.d = lval.u oo rval.d; \
	    } else { \
		if (r_is_u) \
		    ret.d = lval.d oo rval.u; \
		else \
		    ret.d = lval.d oo rval.d; \
	    } \
	} \
    } while (0)
    
    switch (oper) {
    case '*': LOR(*); break;
    case '/': LOR(/); break;
    case '+': LOR(+); break;
    case '-': LOR(-); break;
    case '>': LOR(>); break;
    case '<': LOR(<); break;
    case GEQ: LOR(>=); break;
    case LEQ: LOR(<=); break;
    case EQ: LOR(==); break;
    case NEQ: LOR(!=); break;
    default: CCAssert(0);
    }

    if (ret_is_u)
	return int_literal_node(dty, ret);
    else
	return float_literal_node(dty, ret);
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
		return eval_arith(r);
	    case '%':
	    case LSHIFT: case RSHIFT:
	    case '|': case '&': case '^':
	        return bop_int(op, eval_arith(l), eval_arith(r));
	    case '+': case '-': case '*': case '/':
	    case '>': case '<': case GEQ:
	    case LEQ: case EQ: case NEQ:
		return bop_scalar(op, AST_TYPE(expr), eval_arith(l), eval_arith(r));
	    case AND:
		{

		}
	    case OR:
		{

		}
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
