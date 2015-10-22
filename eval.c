#include "cc.h"

/* Constant expressions in C:
 *
 * 1. interger
 * 2. floating
 * 3. address (address of a static extent)
 * 4. initializer (combination of the aboves)
 */

static node_t * scalar_uop(int op, node_t *ty, node_t *l);
static node_t * arith_uop(int op, node_t *ty, node_t *l);
static node_t * int_uop(int op, node_t *ty, node_t *l);

static node_t * scalar_bop(int op, node_t *ty, node_t *l, node_t *r);
static node_t * arith_bop(int op, node_t *ty, node_t *l, node_t *r);
static node_t * int_bop(int op, node_t *ty, node_t *l, node_t *r);

static struct bop {
    int op;
    bool (*is) (node_t *ty);
    node_t * (*eval) (int op, node_t *ty, node_t *l, node_t *r);
} bops[] = {
    {'%',    isint,    int_bop},
    {LSHIFT, isint,    int_bop},
    {RSHIFT, isint,    int_bop},
    {'|',    isint,    int_bop},
    {'&',    isint,    int_bop},
    {'^',    isint,    int_bop},
    {'*',    isarith,  arith_bop},
    {'/',    isarith,  arith_bop},
    {'+',    isarith,  arith_bop},
    {'-',    isarith,  arith_bop},
    {'>',    isscalar, scalar_bop},
    {'<',    isscalar, scalar_bop},
    {LEQ,    isscalar, scalar_bop},
    {GEQ,    isscalar, scalar_bop},
    {EQ,     isscalar, scalar_bop},
    {NEQ,    isscalar, scalar_bop},
};

static struct uop {
    int op;
    bool (*is) (node_t *ty);
    node_t * (*eval) (int op, node_t *ty, node_t *l);
} uops[] = {
    {'-',  isarith,  arith_uop},
    {'~',  isint,    int_uop},
    {'!',  isscalar, scalar_uop},
};

static struct bop * dispatch_bop(int op)
{
    for (int i = 0; i < ARRAY_SIZE(bops); i++)
	if (bops[i].op == op)
	    return &bops[i];
    return NULL;
}

static struct uop * dispatch_uop(int op)
{
    for (int i = 0; i < ARRAY_SIZE(uops); i++)
	if (uops[i].op == op)
	    return &uops[i];
    return NULL;
}

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

static node_t * one_literal(void)
{
    static node_t * _one_literal;
    if (!_one_literal)
	_one_literal = new_integer_literal(1);
    return _one_literal;
}

static node_t * zero_literal(void)
{
    static node_t * _zero_literal;
    if (!_zero_literal)
	_zero_literal = new_integer_literal(0);
    return _zero_literal;
}

static node_t * arith2arith(node_t *dty, node_t *l)
{
    node_t *sty = AST_TYPE(l);
    if (isint(dty) && isint(sty)) {
	union value dst_val = SYM_VALUE(EXPR_SYM(l));
	int src_size = TYPE_SIZE(sty);
	int dst_size = TYPE_SIZE(dty);
	if (src_size > dst_size) {
	    // narrow
	    VALUE_U(dst_val) &= VALUE_U(TYPE_LIMITS_MAX(dty));
	    if (TYPE_KIND(dty) == _BOOL)
		VALUE_U(dst_val) = VALUE_U(dst_val) == 0 ? 0 : 1;
	}
	return int_literal_node(dty, dst_val);
    } else if (isint(dty) && isfloat(sty)) {
	// float => int
	union value src_val = SYM_VALUE(EXPR_SYM(l));
	union value dst_val;
	VALUE_U(dst_val) = VALUE_D(src_val);
	return int_literal_node(dty, dst_val);
    } else if (isfloat(dty) && isint(sty)) {
	// int => float
	union value src_val = SYM_VALUE(EXPR_SYM(l));
	union value dst_val;
	if (TYPE_OP(sty) == INT)
	    VALUE_D(dst_val) = (long long)VALUE_U(src_val);
	else
	    VALUE_D(dst_val) = VALUE_U(src_val);
	return float_literal_node(dty, dst_val);
    } else if (isfloat(dty) && isfloat(sty)) {
	// float => float
	int dst_kind = TYPE_KIND(dty);
	union value src_val = SYM_VALUE(EXPR_SYM(l));
	union value dst_val;
	if (dst_kind == FLOAT)
	    VALUE_D(dst_val) = (float)VALUE_D(src_val);
        else if (dst_kind == DOUBLE)
	    VALUE_D(dst_val) = (double)VALUE_D(src_val);
	else
	    VALUE_D(dst_val) = VALUE_D(src_val);
	return float_literal_node(dty, dst_val);
    }
    return NULL;
}

static node_t * arith2ptr(node_t *dty, node_t *l)
{
    cc_assert(isint(AST_TYPE(l)));
    // int => ptr
    return int_literal_node(dty, SYM_VALUE(EXPR_SYM(l)));
}

static node_t * ptr2arith(node_t *dty, node_t *l)
{
    cc_assert(isint(dty));
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

    cc_assert(0);
}

static bool scalar_bool(node_t *expr)
{

}

static node_t * address_uop(node_t *expr)
{

}

static node_t * sizeof_uop(node_t *expr)
{
    node_t *l = EXPR_OPERAND(expr, 0);
    node_t *ty = istype(l) ? l : AST_TYPE(l);
    union value val;
    VALUE_U(val) = TYPE_SIZE(ty);
    return int_literal_node(AST_TYPE(expr), val);
}

static node_t * scalar_uop(int op, node_t *ty, node_t *l)
{

}

static node_t * arith_uop(int op, node_t *ty, node_t *l)
{

}

static node_t * int_uop(int op, node_t *ty, node_t *l)
{
    if (!isiliteral(l))
	return NULL;
    
    switch (op) {
    case '~':
    default:  cc_assert(0);
    }
}

static node_t * scalar_bop(int op, node_t *ty, node_t *l, node_t *r)
{
    return NULL;
}

static node_t * ptr_int_bop(int op, node_t *ty, node_t *ptr, node_t *i)
{
    return NULL;
}

static node_t * arith_bop(int op, node_t *ty, node_t *l, node_t *r)
{
    if (!isiliteral(l) && !isfliteral(r))
	return NULL;
    else if (!isiliteral(r) && !isfliteral(r))
	return NULL;

    union value lval = SYM_VALUE(EXPR_SYM(l));
    union value rval = SYM_VALUE(EXPR_SYM(r));
    bool l_is_int = isiliteral(l);
    bool r_is_int = isiliteral(r);
    bool ret_is_int = isint(ty);
    union value val;

#define ARITH_BOP(op)  \
    do { \
        if (ret_is_int) { \
	    if (l_is_int) { \
		if (r_is_int) \
		    VALUE_U(val) = VALUE_U(lval) op VALUE_U(rval); \
		else \
		    VALUE_U(val) = VALUE_U(lval) op VALUE_D(rval); \
	    } else { \
		if (r_is_int) \
		    VALUE_U(val) = VALUE_D(lval) op VALUE_U(rval); \
		else \
		    VALUE_U(val) = VALUE_D(lval) op VALUE_D(rval); \
	    } \
	} else { \
	    if (l_is_int) { \
		if (r_is_int) \
		    VALUE_D(val) = VALUE_U(lval) op VALUE_U(rval); \
		else \
		    VALUE_D(val) = VALUE_U(lval) op VALUE_D(rval); \
	    } else { \
		if (r_is_int) \
		    VALUE_D(val) = VALUE_D(lval) op VALUE_U(rval); \
		else \
		    VALUE_D(val) = VALUE_D(lval) op VALUE_D(rval); \
	    } \
	} \
    } while (0)
    
    switch (op) {
    case '+': ARITH_BOP(+); break;
    case '-': ARITH_BOP(-); break;
    case '*': ARITH_BOP(*); break;
    case '/': ARITH_BOP(/); break;
    default:  cc_assert(0);
    }

    node_t *ret;
    if (ret_is_int)
	ret = int_literal_node(ty, val);
    else
	ret = float_literal_node(ty, val);
    return cast(ty, ret);
}

static node_t * int_bop(int op, node_t *ty, node_t *l, node_t *r)
{
    if (!isiliteral(l) || !isiliteral(r))
	return NULL;

    union value lval = SYM_VALUE(EXPR_SYM(l));
    union value rval = SYM_VALUE(EXPR_SYM(r));
    union value val;

#define INT_BOP(op)  VALUE_U(val) = VALUE_U(lval) op VALUE_U(rval)
    
    switch (op) {
    case '%':    INT_BOP(%);  break;
    case LSHIFT: INT_BOP(<<); break;
    case RSHIFT: INT_BOP(>>); break;
    case '|':    INT_BOP(|);  break;
    case '&':    INT_BOP(&);  break;
    case '^':    INT_BOP(^);  break;
    default:	 cc_assert(0);
    }

    node_t *ret = int_literal_node(ty, val);
    return cast(ty, ret);
}

static node_t * doeval(node_t *expr)
{
    cc_assert(isexpr(expr));
    switch (AST_ID(expr)) {
    case BINARY_OPERATOR:
	{
	    node_t *l = EXPR_OPERAND(expr, 0);
	    node_t *r = EXPR_OPERAND(expr, 1);
	    int op = EXPR_OP(expr);
	    struct bop *bop;
	    switch (op) {
	    case '=':
		return NULL;
	    case ',':
		if (doeval(l))
		    return doeval(r);
		else
		    return NULL;
		// int
	    case '%':
	    case '|':
	    case '&':
	    case '^':
	    case LSHIFT:
	    case RSHIFT:
		// arith
	    case '*':
	    case '/':
		// scalar
	    case '<':
	    case '>':
	    case GEQ:
	    case LEQ:
	    case EQ:
	    case NEQ:
	    dispatch:
		bop = dispatch_bop(op);
		cc_assert(bop->is(AST_TYPE(l)));
		cc_assert(bop->is(AST_TYPE(r)));
		l = doeval(l);
		if (!l)
		    return NULL;
		r = doeval(r);
		if (!r)
		    return NULL;
		return bop->eval(op, AST_TYPE(expr), l, r);
	    case '+':
		{
		    if (isarith(AST_TYPE(l)) && isarith(AST_TYPE(r)))
			goto dispatch;
		    // ptr + int or int + ptr
		    node_t *ptr = isptr(AST_TYPE(l)) ? l : r;
		    node_t *i = ptr == l ? r : l;
		    cc_assert(isptr(ptr));
		    cc_assert(isint(i));
		    return ptr_int_bop(op, AST_TYPE(expr), ptr, i);
		}
	    case '-':
		if (!isptr(AST_TYPE(l)))
		    goto dispatch;
		// ptr - int
		cc_assert(isint(AST_TYPE(r)));
		return ptr_int_bop(op, AST_TYPE(expr), l, r);
	    case AND:
	    case OR:
		l = doeval(l);
		if (!l)
		    return NULL;
		if (op == AND && !scalar_bool(l))
		    return zero_literal();
		else if (op == OR && scalar_bool(l))
		    return one_literal();
		return doeval(r);
	    default:
		cc_assert(0);
	    }
	}
	break;
    case UNARY_OPERATOR:
	{
	    node_t *l = EXPR_OPERAND(expr, 0);
	    int op = EXPR_OP(expr);
	    struct uop *uop;
	    switch (op) {
	    case INCR:
	    case DECR:
	    case '*':
		return NULL;
	    case '&':
		return address_uop(expr);
	    case '+':
		return doeval(l);
	    case '-':
	    case '~':
	    case '!':
		uop = dispatch_uop(op);
		cc_assert(uop->is(AST_TYPE(l)));
		l = doeval(l);
		if (!l)
		    return NULL;
		return uop->eval(op, AST_TYPE(expr), l);
	    case SIZEOF:
		return sizeof_uop(expr);
	    default:
		cc_assert(0);
	    }
	}
	break;
    case PAREN_EXPR:
    case COMPOUND_LITERAL:
	return doeval(EXPR_OPERAND(expr, 0));
    case CAST_EXPR:
    case CONV_EXPR:
	return cast(AST_TYPE(expr), EXPR_OPERAND(expr, 0));
    case COND_EXPR:
	{
	    node_t *cond = doeval(EXPR_COND(expr));
	    if (!cond)
		return NULL;
	    if (scalar_bool(cond))
		return doeval(EXPR_THEN(expr));
	    else
		return doeval(EXPR_ELSE(expr));
	}
    case REF_EXPR:
	if (EXPR_OP(expr) == ENUM)
	    return cast(AST_TYPE(expr), int_literal_node(AST_TYPE(expr), SYM_VALUE(EXPR_SYM(expr))));
	else
	    return expr;
    case INITS_EXPR:
    case SUBSCRIPT_EXPR:
	break;
    case INTEGER_LITERAL:
    case FLOAT_LITERAL:
    case STRING_LITERAL:
	return expr;
    case MEMBER_EXPR:
    case CALL_EXPR:
	return NULL;
    case VINIT_EXPR:
    default:
        cc_assert(0);
    }
}

node_t * eval(node_t *expr, node_t *ty)
{
    if (!expr)
    	return NULL;

    return cast(ty, doeval(expr));
}

bool eval_cpp_cond(void)
{
    gettok();
    return intexpr();
}
