
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

static node_t * new_int_literal(int i)
{
    return int_literal_node(inttype, (union value){.u = i});
}

static bool bool_arith(node_t *n)
{
    if (!n)
	return false;
    if (isiliteral(n))
	return ILITERAL_VALUE(n) != 0;
    else if (isfliteral(n))
	return FLITERAL_VALUE(n) != 0;
    else
	return false;
}

// TODO: 
static bool do_eval_bool(node_t *cond)
{
    return false;
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

static node_t * bop_scalar(int op, node_t *dty, node_t *l, node_t *r)
{
    if (l == NULL || r == NULL)
	return NULL;
    
    union value lval = SYM_VALUE(EXPR_SYM(l));
    union value rval = SYM_VALUE(EXPR_SYM(r));
    union value ret;
    bool l_is_u = !isfliteral(l);
    bool r_is_u = !isfliteral(r);
    bool ret_is_u = TYPE_OP(dty) == FLOAT ? false : true;
    
#define LOR(oo) \
    do { \
        if (ret_is_u) { \
	    if (l_is_u) { \
		if (r_is_u) \
		    VALUE_U(ret) = VALUE_U(lval) oo VALUE_U(rval); \
		else \
		    VALUE_U(ret) = VALUE_U(lval) oo VALUE_D(rval); \
	    } else { \
		if (r_is_u) \
		    VALUE_U(ret) = VALUE_D(lval) oo VALUE_U(rval); \
		else \
		    VALUE_U(ret) = VALUE_D(lval) oo VALUE_D(rval); \
	    } \
	} else { \
	    if (l_is_u) { \
		if (r_is_u) \
		    VALUE_D(ret) = VALUE_U(lval) oo VALUE_U(rval); \
		else \
		    VALUE_D(ret) = VALUE_U(lval) oo VALUE_D(rval); \
	    } else { \
		if (r_is_u) \
		    VALUE_D(ret) = VALUE_D(lval) oo VALUE_U(rval); \
		else \
		    VALUE_D(ret) = VALUE_D(lval) oo VALUE_D(rval); \
	    } \
	} \
    } while (0)

#define LORI(oo) \
    do { \
        VALUE_U(ret) = VALUE_U(lval) oo VALUE_U(rval);	\
    } while (0)
    
    switch (op) {
    case '%':    LORI(%); break;
    case LSHIFT: LORI(<<); break;
    case RSHIFT: LORI(>>); break;
    case '|':    LORI(|); break;
    case '&':    LORI(&); break;
    case '^':    LORI(^); break;
	
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

static node_t * uop_scalar(int op, node_t *dty, node_t *l)
{
    if (l == NULL)
	return NULL;

    union value lval = SYM_VALUE(EXPR_SYM(l));
    bool l_is_u = !isfliteral(l);
    bool ret_is_u = TYPE_OP(dty) == FLOAT ? false : true;
    union value ret;

#define LOO(oo) \
    do { \
	if (ret_is_u) { \
	    if (l_is_u) \
		VALUE_U(ret) = oo VALUE_U(lval); \
	    else \
		VALUE_U(ret) = oo VALUE_D(lval); \
	} else { \
	    if (l_is_u) \
		VALUE_D(ret) = oo VALUE_U(lval); \
	    else \
		VALUE_D(ret) = oo VALUE_D(lval); \
	} \
    } while (0)
    
    switch (op) {
	// arith
    case '-': LOO(-); break;
	// int
    case '~':
	CCAssert(ret_is_u);
	VALUE_U(ret) = ~ VALUE_U(lval);
	break;
	// scalar
    case '!': LOO(!); break;
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

	    case '+': case '-': case '*': case '/':
	    case '>': case '<': case GEQ:
	    case LEQ: case EQ: case NEQ:
		return bop_scalar(op, AST_TYPE(expr), eval_arith(l), eval_arith(r));
	    case AND:
		{
		    node_t *l1 = eval_arith(l);
		    if (!bool_arith(l1))
			return new_int_literal(0);
		    else
			return eval_arith(r);
		}
	    case OR:
		{
		    node_t *l1 = eval_arith(l);
		    if (bool_arith(l1))
		        return new_int_literal(1);
		    else
			return eval_arith(r);
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
	    case '&':
		return NULL;
	    case '+':
		return eval_arith(EXPR_OPERAND(expr, 0));
	    case '-':
	    case '~':
	    case '!':
		return uop_scalar(EXPR_OP(expr), AST_TYPE(expr), eval_arith(EXPR_OPERAND(expr, 0)));
	    case SIZEOF:
		// TODO: 
		if (istype(EXPR_OPERAND(expr, 0))) {
		    union value ret;
		    VALUE_I(ret) = TYPE_SIZE(EXPR_OPERAND(expr, 0));
		    return int_literal_node(AST_TYPE(expr), ret);
		} else {
		    return NULL;
		}
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
		if (do_eval_bool(cond))
		    return eval_arith(EXPR_THEN(expr));
		else
		    return eval_arith(EXPR_ELSE(expr));
	    }
	}
	return NULL;
    case REF_EXPR:
	// TODO:
        if (EXPR_OP(expr) == ENUM)
	    return int_literal_node(unpack(AST_TYPE(expr)), SYM_VALUE(EXPR_SYM(expr)));
	else
	    return NULL;
    case INITS_EXPR:
        for (int i = 0; i < LIST_LEN(EXPR_INITS(expr)); i++) {
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

// TODO: 
static node_t * eval_address(node_t *expr)
{
    return expr;
}

// TODO: 
static node_t * eval_initializer(node_t *expr)
{
    return expr;
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

// TODO: 
node_t * eval_bool(node_t *expr)
{
    if (expr == NULL)
	return NULL;

    return NULL;
}

bool eval_cpp_cond(void)
{
    for (;;) {
        int t = gettok();
	if (t == EOI)
	    break;
    }
    return true;
}
