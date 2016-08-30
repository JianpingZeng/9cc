#include "cc.h"

/* Constant expressions in C:
 *
 * 1. interger
 * 2. floating
 * 3. address (address of a static extent)
 * 4. initializer (combination of the aboves)
 */

static node_t *scalar_uop(int op, node_t * ty, node_t * l);
static node_t *arith_uop(int op, node_t * ty, node_t * l);
static node_t *int_uop(int op, node_t * ty, node_t * l);

static node_t *scalar_bop(int op, node_t * ty, node_t * l, node_t * r);
static node_t *arith_bop(int op, node_t * ty, node_t * l, node_t * r);
static node_t *int_bop(int op, node_t * ty, node_t * l, node_t * r);

static node_t *doeval(node_t * expr);

static struct bop {
    int op;
    bool(*is) (node_t * ty);
    node_t *(*eval) (int op, node_t * ty, node_t * l, node_t * r);
} bops[] = {
    {'%', isint, int_bop},
    {LSHIFT, isint, int_bop},
    {RSHIFT, isint, int_bop},
    {'|', isint, int_bop},
    {'&', isint, int_bop},
    {'^', isint, int_bop},
    {'*', isarith, arith_bop},
    {'/', isarith, arith_bop},
    {'+', isarith, arith_bop},
    {'-', isarith, arith_bop},
    {'>', isscalar, scalar_bop},
    {'<', isscalar, scalar_bop},
    {LEQ, isscalar, scalar_bop},
    {GEQ, isscalar, scalar_bop},
    {EQ, isscalar, scalar_bop},
    {NEQ, isscalar, scalar_bop},};

static struct uop {
    int op;
    bool(*is) (node_t * ty);
    node_t *(*eval) (int op, node_t * ty, node_t * l);
} uops[] = {
    {'+', isarith, arith_uop},
    {'-', isarith, arith_uop},
    {'~', isint, int_uop},
    {'!', isscalar, scalar_uop},};

static struct bop *dispatch_bop(int op)
{
    for (int i = 0; i < ARRAY_SIZE(bops); i++) {
        if (bops[i].op == op)
            return &bops[i];
    }
    return NULL;
}

static struct uop *dispatch_uop(int op)
{
    for (int i = 0; i < ARRAY_SIZE(uops); i++) {
        if (uops[i].op == op)
            return &uops[i];
    }
    return NULL;
}

static node_t *literal_node(int id)
{
    node_t *n = NEWS(node_t, PERM);
    AST_ID(n) = id;
    EXPR_SYM(n) = anonymous(&constants, CONSTANT);
    return n;
}

static node_t *int_literal_node(node_t * ty, union value v)
{
    node_t *n = literal_node(INTEGER_LITERAL);
    AST_TYPE(n) = ty;
    SYM_TYPE(EXPR_SYM(n)) = ty;
    SYM_VALUE(EXPR_SYM(n)) = v;
    return n;
}

static node_t *float_literal_node(node_t * ty, union value v)
{
    node_t *n = literal_node(FLOAT_LITERAL);
    AST_TYPE(n) = ty;
    SYM_TYPE(EXPR_SYM(n)) = ty;
    SYM_VALUE(EXPR_SYM(n)) = v;
    return n;
}

static node_t *one_literal(void)
{
    static node_t *_one_literal;
    if (!_one_literal)
        _one_literal = new_integer_literal(1);
    return _one_literal;
}

static node_t *zero_literal(void)
{
    static node_t *_zero_literal;
    if (!_zero_literal)
        _zero_literal = new_integer_literal(0);
    return _zero_literal;
}

static node_t *arith2arith(node_t * dty, node_t * l)
{
    if (!isiliteral(l) && !isfliteral(l))
        return NULL;

    node_t *sty = AST_TYPE(l);
    if (isint(dty) && isint(sty)) {
        union value dst_val = SYM_VALUE(EXPR_SYM(l));
        int src_size = TYPE_SIZE(sty);
        int dst_size = TYPE_SIZE(dty);
        if (src_size > dst_size) {
            // narrow
            VALUE_U(dst_val) &= VALUE_U(TYPE_LIMITS_MAX(dty));
            if (TYPE_KIND(dty) == _BOOL)
                VALUE_U(dst_val) =
                    VALUE_U(dst_val) == 0 ? 0 : 1;
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
    assert(0);
}

static node_t *arith2ptr(node_t * dty, node_t * l)
{
    assert(isint(AST_TYPE(l)));
    if (!isiliteral(l))
        return NULL;
    // int => ptr
    return int_literal_node(dty, SYM_VALUE(EXPR_SYM(l)));
}

static node_t *ptr2arith(node_t * dty, node_t * l)
{
    assert(isint(dty));
    if (!isiliteral(l))
        return NULL;
    // ptr => int
    return int_literal_node(dty, SYM_VALUE(EXPR_SYM(l)));
}

static node_t *ptr2ptr(node_t * dty, node_t * l)
{
    // ptr => ptr
    if (AST_ID(l) == REF_EXPR) {
        node_t *ty = SYM_TYPE(EXPR_SYM(l));
        if (!isfunc(ty) && !isarray(ty))
            return NULL;
    }
    l = copy_node(l);
    AST_TYPE(l) = dty;
    return l;
}

static node_t *func2ptr(node_t * dty, node_t * l)
{
    assert(AST_ID(l) == REF_EXPR);
    l = copy_node(l);
    AST_TYPE(l) = dty;
    return l;
}

static node_t *array2ptr(node_t * dty, node_t * l)
{
    assert(AST_ID(l) == REF_EXPR || issliteral(l));
    if (AST_ID(l) == REF_EXPR && !has_static_extent(EXPR_SYM(l)))
        return NULL;
    l = copy_node(l);
    AST_TYPE(l) = dty;
    return l;
}

static node_t *cast(node_t * dty, node_t * l)
{
    if (!l)
        return NULL;

    if (AST_ID(l) == INITS_EXPR && isscalar(dty))
        l = vec_at(EXPR_INITS(l), 0);

    node_t *sty = AST_TYPE(l);
    if (isarith(dty)) {
        if (isarith(sty))
            return arith2arith(dty, l);
        else if (isptr(sty))
            return ptr2arith(dty, l);
    } else if (isptr(dty)) {
        if (isptr(sty))
            return ptr2ptr(dty, l);
        else if (isarith(sty))
            return arith2ptr(dty, l);
        else if (isfunc(sty))
            return func2ptr(dty, l);
        else if (isarray(sty))
            return array2ptr(dty, l);
    } else {
        // record or lvalue2rvalue cast
        if (isrecord(dty) && AST_ID(l) != INITS_EXPR)
            return NULL;
        l = copy_node(l);
        AST_TYPE(l) = dty;
        return l;
    }
    return NULL;
}

// 'expr' was evaluated and _NOT_ null.
static bool scalar_bool(node_t * expr)
{
    assert(isiliteral(expr) || isfliteral(expr));

    if (isiliteral(expr))
        return ILITERAL_VALUE(expr) != 0;
    else
        return FLITERAL_VALUE(expr) != 0;
}

// '&': 'expr' was not evaluated.
static node_t *address_uop(node_t * expr)
{
    node_t *l = EXPR_OPERAND(expr, 0);
    if (!l || !(l = doeval(l)))
        return NULL;
    if (issliteral(l) || AST_ID(l) == INITS_EXPR) {
        return ast_uop(EXPR_OP(expr), AST_TYPE(expr), l);
    } else if (AST_ID(l) == UNARY_OPERATOR) {
        assert(EXPR_OP(l) == '*');
        return EXPR_OPERAND(l, 0);
    } else if (AST_ID(l) == REF_EXPR) {
        node_t *sym = EXPR_SYM(l);
        if (!has_static_extent(sym))
            return NULL;
        return ast_uop(EXPR_OP(expr), AST_TYPE(expr), l);
    } else if (isiliteral(l) &&
               AST_ID(EXPR_OPERAND(expr, 0)) == MEMBER_EXPR) {
        return l;
    }
    assert(0);
}

static node_t *sizeof_uop(node_t * expr)
{
    node_t *l = EXPR_OPERAND(expr, 0);
    node_t *ty = istype(l) ? l : AST_TYPE(l);
    union value val;
    VALUE_U(val) = TYPE_SIZE(ty);
    return int_literal_node(AST_TYPE(expr), val);
}

static node_t *scalar_uop(int op, node_t * ty, node_t * l)
{
    if (!isiliteral(l) && !isfliteral(l))
        return NULL;

    union value lval = SYM_VALUE(EXPR_SYM(l));

    switch (op) {
    case '!':
        if (isiliteral(l))
            return VALUE_U(lval) ==
                0 ? one_literal() : zero_literal();
        else
            return VALUE_D(lval) ==
                0 ? one_literal() : zero_literal();
    default:
        assert(0);
    }
}

static node_t *arith_uop(int op, node_t * ty, node_t * l)
{
    if (!isiliteral(l) && !isfliteral(l))
        return NULL;

    union value lval = SYM_VALUE(EXPR_SYM(l));
    union value rval;

    switch (op) {
    case '+':
        return l;
    case '-':
        if (isiliteral(l)) {
            VALUE_U(rval) = -VALUE_U(lval);
            return int_literal_node(ty, rval);
        } else {
            VALUE_D(rval) = -VALUE_D(lval);
            return float_literal_node(ty, rval);
        }
    default:
        assert(0);
    }
}

static node_t *int_uop(int op, node_t * ty, node_t * l)
{
    if (!isiliteral(l))
        return NULL;

    union value lval = SYM_VALUE(EXPR_SYM(l));
    union value rval;

    switch (op) {
    case '~':
        VALUE_U(rval) = ~VALUE_U(lval);
        return int_literal_node(ty, rval);
    default:
        assert(0);
    }
}

static node_t *scalar_bop(int op, node_t * ty, node_t * l, node_t * r)
{
    if (!isiliteral(l) && !isfliteral(l))
        return NULL;

    assert(TYPE_KIND(AST_TYPE(l)) == TYPE_KIND(AST_TYPE(r)));

    union value lval = SYM_VALUE(EXPR_SYM(l));
    union value rval = SYM_VALUE(EXPR_SYM(r));
    bool is_int = AST_TYPE(l);

#define SCALAR_OP(op)                                        \
    if (is_int) {                                        \
        bool b;                                                \
        int sign1 = TYPE_OP(AST_TYPE(l));                \
        int sign2 = TYPE_OP(AST_TYPE(r));                \
        if (sign1 == UNSIGNED && sign2 == UNSIGNED) {        \
            b = VALUE_U(lval) op VALUE_U(rval);                \
        }  else if (sign1 == INT && sign2 == INT) {        \
            long long l = VALUE_I(lval);                \
            long long r = VALUE_I(rval);                \
            b = l op r;                                        \
        } else if (sign1 == UNSIGNED) {                        \
            b = true;                                        \
        } else {                                        \
            b = false;                                        \
        }                                                \
        return b ? one_literal() : zero_literal();        \
    } else {                                                \
        bool b = VALUE_D(lval) op VALUE_D(rval);        \
        return b ? one_literal() : zero_literal();        \
    }

    switch (op) {
    case '<':
        SCALAR_OP(<);
        break;
    case '>':
        SCALAR_OP(>);
        break;
    case GEQ:
        SCALAR_OP(>=);
        break;
    case LEQ:
        SCALAR_OP(<=);
        break;
    case EQ:
        SCALAR_OP( ==);
        break;
    case NEQ:
        SCALAR_OP(!=);
        break;
    default:
        assert(0);
    }
}

// both 'ptr' and 'i' are _NOT_ evaluated.
static node_t *ptr_int_bop(int op, node_t * ty, node_t * ptr, node_t * i)
{
    node_t *l = doeval(ptr);
    if (!l)
        return NULL;
    node_t *r = doeval(i);
    if (!r || !isiliteral(r))
        return NULL;
    // combine
    if (AST_ID(l) == BINARY_OPERATOR) {
        int op1 = EXPR_OP(l);
        node_t *r1 = EXPR_OPERAND(l, 1);

        assert(op1 == '+' || op1 == '-');
        assert(isiliteral(r1));

        node_t *n;
        if (op == op1) {
            n = bop('+', r, r1);
        } else {
            node_t *r2 = op == '+' ? r : r1;
            node_t *r3 = r2 == r ? r1 : r;
            n = bop('-', r2, r3);
            op = '+';
        }
        r = arith_bop(EXPR_OP(n), AST_TYPE(n), EXPR_OPERAND(n, 0),
                      EXPR_OPERAND(n, 1));
        l = EXPR_OPERAND(l, 0);
    }
    return ast_bop(op, ty, l, r);
}

static node_t *arith_bop(int op, node_t * ty, node_t * l, node_t * r)
{
    if (!isiliteral(l) && !isfliteral(r))
        return NULL;
    else if (!isiliteral(r) && !isfliteral(r))
        return NULL;

    assert(TYPE_KIND(AST_TYPE(l)) == TYPE_KIND(AST_TYPE(r)));
    assert(TYPE_KIND(AST_TYPE(l)) == TYPE_KIND(ty));
    assert(TYPE_OP(AST_TYPE(l)) == TYPE_OP(AST_TYPE(r)));
    assert(TYPE_OP(AST_TYPE(l)) == TYPE_OP(ty));

    union value lval = SYM_VALUE(EXPR_SYM(l));
    union value rval = SYM_VALUE(EXPR_SYM(r));
    bool is_int = isint(ty);
    union value val;

#define ARITH_BOP(op)                                                \
    do {                                                        \
        if (is_int)                                                \
            VALUE_U(val) = VALUE_U(lval) op VALUE_U(rval);        \
        else                                                        \
            VALUE_D(val) = VALUE_D(lval) op VALUE_D(rval);        \
    } while (0)

    switch (op) {
    case '+':
        ARITH_BOP(+);
        break;
    case '-':
        ARITH_BOP(-);
        break;
    case '*':
        ARITH_BOP(*);
        break;
    case '/':
        ARITH_BOP(/);
        break;
    default:
        assert(0);
    }

    if (is_int)
        return int_literal_node(ty, val);
    else
        return float_literal_node(ty, val);
}

static node_t *int_bop(int op, node_t * ty, node_t * l, node_t * r)
{
    if (!isiliteral(l) || !isiliteral(r))
        return NULL;

    assert(TYPE_KIND(AST_TYPE(l)) == TYPE_KIND(AST_TYPE(r)));
    assert(TYPE_KIND(AST_TYPE(l)) == TYPE_KIND(ty));
    assert(TYPE_OP(AST_TYPE(l)) == TYPE_OP(AST_TYPE(r)));
    assert(TYPE_OP(AST_TYPE(l)) == TYPE_OP(ty));

    union value lval = SYM_VALUE(EXPR_SYM(l));
    union value rval = SYM_VALUE(EXPR_SYM(r));
    union value val;

#define INT_BOP(op)  VALUE_U(val) = VALUE_U(lval) op VALUE_U(rval)

    switch (op) {
    case '%':
        INT_BOP(%);
        break;
    case LSHIFT:
        INT_BOP(<<);
        break;
    case RSHIFT:
        INT_BOP(>>);
        break;
    case '|':
        INT_BOP(|);
        break;
    case '&':
        INT_BOP(&);
        break;
    case '^':
        INT_BOP(^);
        break;
    default:
        assert(0);
    }

    return int_literal_node(ty, val);
}

static node_t *doeval(node_t * expr)
{
    assert(isexpr(expr));
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
                assert(bop->is(AST_TYPE(l)));
                assert(bop->is(AST_TYPE(r)));
                l = doeval(l);
                if (!l)
                    return NULL;
                r = doeval(r);
                if (!r)
                    return NULL;
                return bop->eval(op, AST_TYPE(expr), l, r);
            case '+':
                {
                    if (isarith(AST_TYPE(l))
                        && isarith(AST_TYPE(r)))
                        goto dispatch;
                    // ptr + int or int + ptr
                    node_t *ptr =
                        isptr(AST_TYPE(l)) ? l : r;
                    node_t *i = ptr == l ? r : l;
                    assert(isptr(AST_TYPE(ptr)));
                    assert(isint(AST_TYPE(i)));
                    return ptr_int_bop(op, AST_TYPE(expr),
                                       ptr, i);
                }
            case '-':
                if (!isptr(AST_TYPE(l)))
                    goto dispatch;
                // ptr - int
                assert(isint(AST_TYPE(r)));
                return ptr_int_bop(op, AST_TYPE(expr), l, r);
            case AND:
            case OR:
                l = doeval(l);
                if (!l || (!isiliteral(l) && !isfliteral(l)))
                    return NULL;
                if (op == AND && !scalar_bool(l))
                    return zero_literal();
                else if (op == OR && scalar_bool(l))
                    return one_literal();
                return doeval(r);
            default:
                assert(0);
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
            case '-':
            case '~':
            case '!':
                uop = dispatch_uop(op);
                assert(uop->is(AST_TYPE(l)));
                l = doeval(l);
                if (!l)
                    return NULL;
                return uop->eval(op, AST_TYPE(expr), l);
            case SIZEOF:
                return sizeof_uop(expr);
            default:
                assert(0);
            }
        }
        break;
    case PAREN_EXPR:
    case COMPOUND_LITERAL:
        return doeval(EXPR_OPERAND(expr, 0));
    case CAST_EXPR:
    case CONV_EXPR:
        return cast(AST_TYPE(expr), doeval(EXPR_OPERAND(expr, 0)));
    case COND_EXPR:
        {
            node_t *cond = doeval(EXPR_COND(expr));
            if (!cond || (!isiliteral(cond) && !isfliteral(cond)))
                return NULL;
            if (scalar_bool(cond))
                return doeval(EXPR_THEN(expr));
            else
                return doeval(EXPR_ELSE(expr));
        }
    case INITS_EXPR:
        {
            struct vector *v = vec_new();
            struct vector *inits = EXPR_INITS(expr);
            for (int i = 0; i < vec_len(inits); i++) {
                node_t *n = vec_at(inits, i);
                if (AST_ID(n) == VINIT_EXPR) {
                    vec_push(v, n);
                    continue;
                }
                n = doeval(n);
                if (!n)
                    return NULL;
                vec_push(v, n);
            }
            expr = copy_node(expr);
            EXPR_INITS(expr) = v;
            return expr;
        }
    case SUBSCRIPT_EXPR:
        {
            if (isptr(AST_TYPE(expr)))
                return NULL;
            node_t *l = EXPR_OPERAND(expr, 0);
            node_t *r = EXPR_OPERAND(expr, 1);
            node_t *ptr = isptr(AST_TYPE(l)) ? l : r;
            node_t *i = isint(AST_TYPE(r)) ? r : l;
            assert(isptr(AST_TYPE(ptr)));
            assert(isint(AST_TYPE(i)));
            node_t *p = ptr_int_bop('+', AST_TYPE(ptr), ptr, i);
            return ast_uop('*', AST_TYPE(expr), p);
        }
    case REF_EXPR:
        if (EXPR_OP(expr) == ENUM)
            return int_literal_node(AST_TYPE(expr),
                                    SYM_VALUE(EXPR_SYM(expr)));
        else
            return expr;
    case INTEGER_LITERAL:
    case FLOAT_LITERAL:
    case STRING_LITERAL:
        return expr;
    case MEMBER_EXPR:
        {
            node_t *l = EXPR_OPERAND(expr, 0);
            node_t *ty = AST_TYPE(l);
            l = doeval(l);
            if (!l || !isiliteral(l))
                return NULL;
            ty = isptr(ty) ? rtype(ty) : ty;
            assert(isrecord(ty));
            node_t *field = find_field(ty, AST_NAME(expr));
            assert(field);
            long off = FIELD_OFFSET(field) + SYM_VALUE_I(EXPR_SYM(l));
            union value v = {.i = off};
            return int_literal_node(longtype, v);
        }
        break;
    case CALL_EXPR:
        return NULL;
    case VINIT_EXPR:
    default:
        assert(0);
    }
}

node_t *eval(node_t * expr, node_t * ty)
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
