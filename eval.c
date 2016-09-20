#include <assert.h>
#include "cc.h"

/* Constant expressions in C:
 *
 * 1. interger
 * 2. floating
 * 3. address (address of a static extent)
 * 4. initializer (combination of the aboves)
 */

static struct expr *scalar_uop(int op, struct type * ty, struct expr * l);
static struct expr *arith_uop(int op, struct type * ty, struct expr * l);
static struct expr *int_uop(int op, struct type * ty, struct expr * l);

static struct expr *scalar_bop(int op, struct type * ty, struct expr * l, struct expr * r);
static struct expr *arith_bop(int op, struct type * ty, struct expr * l, struct expr * r);
static struct expr *int_bop(int op, struct type * ty, struct expr * l, struct expr * r);

static struct expr *doeval(struct expr * expr);

static struct bop {
    int op;
    bool(*is) (struct type * ty);
    struct expr *(*eval) (int op, struct type * ty, struct expr * l, struct expr * r);
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
    bool(*is) (struct type * ty);
    struct expr *(*eval) (int op, struct type * ty, struct expr * l);
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

static struct expr *copy_node(struct expr * node)
{
    struct expr *copy = NEWS0(struct expr, PERM);
    memcpy(copy, node, sizeof(struct expr));
    return copy;
}

static struct expr *literal_node(int id)
{
    struct expr *n = NEWS0(struct expr, PERM);
    EXPR_ID(n) = id;
    EXPR_SYM(n) = anonymous(&constants, CONSTANT, PERM);
    return n;
}

static struct expr *int_literal_node(struct type * ty, union value v)
{
    struct expr *n = literal_node(INTEGER_LITERAL);
    EXPR_TYPE(n) = ty;
    SYM_TYPE(EXPR_SYM(n)) = ty;
    SYM_VALUE(EXPR_SYM(n)) = v;
    return n;
}

static struct expr *float_literal_node(struct type * ty, union value v)
{
    struct expr *n = literal_node(FLOAT_LITERAL);
    EXPR_TYPE(n) = ty;
    SYM_TYPE(EXPR_SYM(n)) = ty;
    SYM_VALUE(EXPR_SYM(n)) = v;
    return n;
}

static struct expr *one_literal(void)
{
    static struct expr *_one_literal;
    if (!_one_literal)
        _one_literal = new_integer_literal(1);
    return _one_literal;
}

static struct expr *zero_literal(void)
{
    static struct expr *_zero_literal;
    if (!_zero_literal)
        _zero_literal = new_integer_literal(0);
    return _zero_literal;
}

static struct expr *arith2arith(struct type * dty, struct expr * l)
{
    if (!isiliteral(l) && !isfliteral(l))
        return NULL;

    struct type *sty = EXPR_TYPE(l);
    if (isint(dty) && isint(sty)) {
        union value dst_val = SYM_VALUE(EXPR_SYM(l));
        int src_size = TYPE_SIZE(sty);
        int dst_size = TYPE_SIZE(dty);
        if (src_size > dst_size) {
            // narrow
            dst_val.u &= TYPE_LIMITS(dty).max.u;
            if (TYPE_KIND(dty) == _BOOL)
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
        if (TYPE_OP(sty) == INT)
            dst_val.d = src_val.i;
        else
            dst_val.d = src_val.u;
        return float_literal_node(dty, dst_val);
    } else if (isfloat(dty) && isfloat(sty)) {
        // float => float
        int dst_kind = TYPE_KIND(dty);
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
    assert(0);
}

static struct expr *arith2ptr(struct type * dty, struct expr * l)
{
    assert(isint(EXPR_TYPE(l)));
    if (!isiliteral(l))
        return NULL;
    // int => ptr
    return int_literal_node(dty, SYM_VALUE(EXPR_SYM(l)));
}

static struct expr *ptr2arith(struct type * dty, struct expr * l)
{
    assert(isint(dty));
    if (!isiliteral(l))
        return NULL;
    // ptr => int
    return int_literal_node(dty, SYM_VALUE(EXPR_SYM(l)));
}

static struct expr *ptr2ptr(struct type * dty, struct expr * l)
{
    // ptr => ptr
    if (EXPR_ID(l) == REF_EXPR) {
        struct type *ty = SYM_TYPE(EXPR_SYM(l));
        if (!isfunc(ty) && !isarray(ty))
            return NULL;
    }
    l = copy_node(l);
    EXPR_TYPE(l) = dty;
    return l;
}

static struct expr *func2ptr(struct type * dty, struct expr * l)
{
    assert(EXPR_ID(l) == REF_EXPR);
    l = copy_node(l);
    EXPR_TYPE(l) = dty;
    return l;
}

static struct expr *array2ptr(struct type * dty, struct expr * l)
{
    assert(EXPR_ID(l) == REF_EXPR || issliteral(l));
    if (EXPR_ID(l) == REF_EXPR && !has_static_extent(EXPR_SYM(l)))
        return NULL;
    l = copy_node(l);
    EXPR_TYPE(l) = dty;
    return l;
}

static struct expr *cast(struct type * dty, struct expr * l)
{
    if (!l)
        return NULL;

    if (EXPR_ID(l) == INITS_EXPR && isscalar(dty))
        l = EXPR_INITS(l)[0];

    struct type *sty = EXPR_TYPE(l);
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
        if (isrecord(dty) && EXPR_ID(l) != INITS_EXPR)
            return NULL;
        l = copy_node(l);
        EXPR_TYPE(l) = dty;
        return l;
    }
    return NULL;
}

// 'expr' was evaluated and _NOT_ null.
static bool scalar_bool(struct expr * expr)
{
    assert(isiliteral(expr) || isfliteral(expr));

    if (isiliteral(expr))
        return ILITERAL_VALUE(expr).u != 0;
    else
        return FLITERAL_VALUE(expr).d != 0;
}

// '&': 'expr' was not evaluated.
static struct expr *address_uop(struct expr * expr)
{
    struct expr *l = EXPR_OPERAND(expr, 0);
    if (!l || !(l = doeval(l)))
        return NULL;
    if (issliteral(l) || EXPR_ID(l) == INITS_EXPR) {
        return ast_uop(EXPR_OP(expr), EXPR_TYPE(expr), l);
    } else if (EXPR_ID(l) == UNARY_OPERATOR) {
        assert(EXPR_OP(l) == '*');
        return EXPR_OPERAND(l, 0);
    } else if (EXPR_ID(l) == REF_EXPR) {
        struct symbol *sym = EXPR_SYM(l);
        if (!has_static_extent(sym))
            return NULL;
        return ast_uop(EXPR_OP(expr), EXPR_TYPE(expr), l);
    } else if (isiliteral(l) &&
               EXPR_ID(EXPR_OPERAND(expr, 0)) == MEMBER_EXPR) {
        return l;
    }
    assert(0);
}

static struct expr *scalar_uop(int op, struct type * ty, struct expr * l)
{
    if (!isiliteral(l) && !isfliteral(l))
        return NULL;

    union value lval = SYM_VALUE(EXPR_SYM(l));

    switch (op) {
    case '!':
        if (isiliteral(l))
            return lval.u == 0 ? one_literal() : zero_literal();
        else
            return lval.d == 0 ? one_literal() : zero_literal();
    default:
        assert(0);
    }
}

static struct expr *arith_uop(int op, struct type * ty, struct expr * l)
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
            rval.u = - lval.u;
            return int_literal_node(ty, rval);
        } else {
            rval.d = - lval.d;
            return float_literal_node(ty, rval);
        }
    default:
        assert(0);
    }
}

static struct expr *int_uop(int op, struct type * ty, struct expr * l)
{
    if (!isiliteral(l))
        return NULL;

    union value lval = SYM_VALUE(EXPR_SYM(l));
    union value rval;

    switch (op) {
    case '~':
        rval.u = ~ lval.u;
        return int_literal_node(ty, rval);
    default:
        assert(0);
    }
}

static struct expr *scalar_bop(int op, struct type * ty, struct expr * l, struct expr * r)
{
    if (!isiliteral(l) && !isfliteral(l))
        return NULL;

    assert(TYPE_KIND(EXPR_TYPE(l)) == TYPE_KIND(EXPR_TYPE(r)));

    union value lval = SYM_VALUE(EXPR_SYM(l));
    union value rval = SYM_VALUE(EXPR_SYM(r));
    bool is_int = EXPR_TYPE(l);

#define SCALAR_OP(op)                                   \
    if (is_int) {                                       \
        bool b;                                         \
        int sign1 = TYPE_OP(EXPR_TYPE(l));              \
        int sign2 = TYPE_OP(EXPR_TYPE(r));              \
        if (sign1 == UNSIGNED && sign2 == UNSIGNED) {   \
            b = lval.u op rval.u;                       \
        }  else if (sign1 == INT && sign2 == INT) {     \
            long long l = lval.i;                       \
            long long r = rval.i;                       \
            b = l op r;                                 \
        } else if (sign1 == UNSIGNED) {                 \
            b = true;                                   \
        } else {                                        \
            b = false;                                  \
        }                                               \
        return b ? one_literal() : zero_literal();      \
    } else {                                            \
        bool b = lval.d op rval.d;                      \
        return b ? one_literal() : zero_literal();      \
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
static struct expr *ptr_int_bop(int op, struct type * ty, struct expr * ptr, struct expr * i)
{
    struct expr *l = doeval(ptr);
    if (!l)
        return NULL;
    struct expr *r = doeval(i);
    if (!r || !isiliteral(r))
        return NULL;
    // combine
    if (EXPR_ID(l) == BINARY_OPERATOR) {
        int op1 = EXPR_OP(l);
        struct expr *r1 = EXPR_OPERAND(l, 1);

        assert(op1 == '+' || op1 == '-');
        assert(isiliteral(r1));

        struct expr *n;
        if (op == op1) {
            n = binop('+', r, r1);
        } else {
            struct expr *r2 = op == '+' ? r : r1;
            struct expr *r3 = r2 == r ? r1 : r;
            n = binop('-', r2, r3);
            op = '+';
        }
        r = arith_bop(EXPR_OP(n), EXPR_TYPE(n), EXPR_OPERAND(n, 0),
                      EXPR_OPERAND(n, 1));
        l = EXPR_OPERAND(l, 0);
    }
    return ast_bop(op, ty, l, r);
}

static struct expr *arith_bop(int op, struct type * ty, struct expr * l, struct expr * r)
{
    if (!isiliteral(l) && !isfliteral(r))
        return NULL;
    else if (!isiliteral(r) && !isfliteral(r))
        return NULL;

    assert(TYPE_KIND(EXPR_TYPE(l)) == TYPE_KIND(EXPR_TYPE(r)));
    assert(TYPE_KIND(EXPR_TYPE(l)) == TYPE_KIND(ty));
    assert(TYPE_OP(EXPR_TYPE(l)) == TYPE_OP(EXPR_TYPE(r)));
    assert(TYPE_OP(EXPR_TYPE(l)) == TYPE_OP(ty));

    union value lval = SYM_VALUE(EXPR_SYM(l));
    union value rval = SYM_VALUE(EXPR_SYM(r));
    bool is_int = isint(ty);
    union value val;

#define ARITH_BOP(op)                           \
    do {                                        \
        if (is_int)                             \
            val.u = lval.u op rval.u;           \
        else                                    \
            val.d = lval.d op rval.d;           \
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

static struct expr *int_bop(int op, struct type * ty, struct expr * l, struct expr * r)
{
    if (!isiliteral(l) || !isiliteral(r))
        return NULL;

    assert(TYPE_KIND(EXPR_TYPE(l)) == TYPE_KIND(EXPR_TYPE(r)));
    assert(TYPE_KIND(EXPR_TYPE(l)) == TYPE_KIND(ty));
    assert(TYPE_OP(EXPR_TYPE(l)) == TYPE_OP(EXPR_TYPE(r)));
    assert(TYPE_OP(EXPR_TYPE(l)) == TYPE_OP(ty));

    union value lval = SYM_VALUE(EXPR_SYM(l));
    union value rval = SYM_VALUE(EXPR_SYM(r));
    union value val;

#define INT_BOP(op)  val.u = lval.u op rval.u

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

static struct expr *doeval(struct expr * expr)
{
    switch (EXPR_ID(expr)) {
    case BINARY_OPERATOR:
        {
            struct expr *l = EXPR_OPERAND(expr, 0);
            struct expr *r = EXPR_OPERAND(expr, 1);
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
                assert(bop->is(EXPR_TYPE(l)));
                assert(bop->is(EXPR_TYPE(r)));
                l = doeval(l);
                if (!l)
                    return NULL;
                r = doeval(r);
                if (!r)
                    return NULL;
                return bop->eval(op, EXPR_TYPE(expr), l, r);
            case '+':
                {
                    if (isarith(EXPR_TYPE(l))
                        && isarith(EXPR_TYPE(r)))
                        goto dispatch;
                    // ptr + int or int + ptr
                    struct expr *ptr =
                        isptr(EXPR_TYPE(l)) ? l : r;
                    struct expr *i = ptr == l ? r : l;
                    assert(isptr(EXPR_TYPE(ptr)));
                    assert(isint(EXPR_TYPE(i)));
                    return ptr_int_bop(op, EXPR_TYPE(expr),
                                       ptr, i);
                }
            case '-':
                if (!isptr(EXPR_TYPE(l)))
                    goto dispatch;
                // ptr - int
                assert(isint(EXPR_TYPE(r)));
                return ptr_int_bop(op, EXPR_TYPE(expr), l, r);
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
            struct expr *l = EXPR_OPERAND(expr, 0);
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
                assert(uop->is(EXPR_TYPE(l)));
                l = doeval(l);
                if (!l)
                    return NULL;
                return uop->eval(op, EXPR_TYPE(expr), l);
            case SIZEOF:
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
        return cast(EXPR_TYPE(expr), doeval(EXPR_OPERAND(expr, 0)));
    case COND_EXPR:
        {
            struct expr *cond = doeval(EXPR_COND(expr));
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
            struct expr **inits = EXPR_INITS(expr);
            for (int i = 0; inits[i]; i++) {
                struct expr *n = inits[i];
                if (EXPR_ID(n) == VINIT_EXPR) {
                    vec_push(v, n);
                    continue;
                }
                n = doeval(n);
                if (!n)
                    return NULL;
                vec_push(v, n);
            }
            expr = copy_node(expr);
            EXPR_INITS(expr) = vtoa(v, PERM);
            return expr;
        }
    case SUBSCRIPT_EXPR:
        {
            if (isptr(EXPR_TYPE(expr)))
                return NULL;
            struct expr *l = EXPR_OPERAND(expr, 0);
            struct expr *r = EXPR_OPERAND(expr, 1);
            struct expr *ptr = isptr(EXPR_TYPE(l)) ? l : r;
            struct expr *i = isint(EXPR_TYPE(r)) ? r : l;
            assert(isptr(EXPR_TYPE(ptr)));
            assert(isint(EXPR_TYPE(i)));
            struct expr *p = ptr_int_bop('+', EXPR_TYPE(ptr), ptr, i);
            return ast_uop('*', EXPR_TYPE(expr), p);
        }
    case REF_EXPR:
        if (EXPR_OP(expr) == ENUM)
            return int_literal_node(EXPR_TYPE(expr),
                                    SYM_VALUE(EXPR_SYM(expr)));
        else
            return expr;
    case INTEGER_LITERAL:
    case FLOAT_LITERAL:
    case STRING_LITERAL:
        return expr;
    case MEMBER_EXPR:
        {
            struct expr *l = EXPR_OPERAND(expr, 0);
            struct type *ty = EXPR_TYPE(l);
            l = doeval(l);
            if (!l || !isiliteral(l))
                return NULL;
            ty = isptr(ty) ? rtype(ty) : ty;
            assert(isrecord(ty));
            struct field *field = find_field(ty, EXPR_NAME(expr));
            assert(field);
            long off = FIELD_OFFSET(field) + SYM_VALUE(EXPR_SYM(l)).i;
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

struct expr *eval(struct expr * expr, struct type * ty)
{
    if (!expr)
        return NULL;

    return cast(ty, doeval(expr));
}
