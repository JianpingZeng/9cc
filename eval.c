#include <assert.h>
#include "cc.h"

/// constant expression evaluation

/*
 * Constant expressions in C:
 *
 * 1. interger
 * 2. floating
 * 3. address (address of a static extent)
 * 4. initializer (combination of the aboves)
 */

#define xxcv(func, ty, l)                       \
    if (OPKIND((l)->op) == CNST) {              \
        cv##func(ty, l);                        \
        (l)->op = mkop(CNST, ty);               \
        (l)->type = ty;                         \
        return l;                               \
    }

#define foldcnst1i(oper, vf, ty, l)             \
    if (OPKIND((l)->op) == CNST) {              \
        (l)->x.value.vf = oper (l)->x.value.vf; \
        (l)->op = mkop(CNST, ty);               \
        (l)->type = ty;                         \
        return l;                               \
    }

#define foldcnst1f(oper, vf1, vf2, vf3, ty, l)          \
    if (OPKIND((l)->op) == CNST) {                      \
        switch (TYPE_KIND(ty)) {                        \
        case FLOAT:                                     \
            (l)->x.value.vf1 = oper (l)->x.value.vf1;   \
            break;                                      \
        case DOUBLE:                                    \
            (l)->x.value.vf2 = oper (l)->x.value.vf2;   \
            break;                                      \
        case LONG+DOUBLE:                               \
            (l)->x.value.vf3 = oper (l)->x.value.vf3;   \
            break;                                      \
        default:                                        \
            CC_UNAVAILABLE();                           \
        }                                               \
        (l)->op = mkop(CNST, ty);                       \
        (l)->type = ty;                                 \
        return l;                                       \
    }

#define foldcnst2i(oper, vf, ty, l, r)                          \
    if (OPKIND((l)->op) == CNST && OPKIND((r)->op) == CNST) {   \
        (l)->x.value.vf = (l)->x.value.vf oper (r)->x.value.vf; \
        (l)->op = mkop(CNST, ty);                               \
        (l)->type = ty;                                         \
        return l;                                               \
    }

#define foldcnst2f(oper, vf1, vf2, vf3, ty, l, r)                       \
    if (OPKIND((l)->op) == CNST && OPKIND((r)->op) == CNST) {           \
        switch (TYPE_KIND(ty)) {                                        \
        case FLOAT:                                                     \
            (l)->x.value.vf1 = (l)->x.value.vf1 oper (r)->x.value.vf1;  \
            break;                                                      \
        case DOUBLE:                                                    \
            (l)->x.value.vf2 = (l)->x.value.vf2 oper (r)->x.value.vf2;  \
            break;                                                      \
        case LONG+DOUBLE:                                               \
            (l)->x.value.vf3 = (l)->x.value.vf3 oper (r)->x.value.vf3;  \
            break;                                                      \
        default:                                                        \
            CC_UNAVAILABLE();                                           \
        }                                                               \
        (l)->op = mkop(CNST, ty);                                       \
        (l)->type = ty;                                                 \
        return l;                                                       \
    }

#define foldcnst1fx(oper, ty, l)  foldcnst1f(oper, f, d, ld, ty, l)
#define foldcnst2fx(oper, ty, l, r)  foldcnst2f(oper, f, d, ld, ty, l, r)

#define exchange(l, r)                                  \
    if (OPKIND((l)->op) == CNST) {                      \
        struct expr *tmp; tmp = l; l = r; r = tmp;      \
    }

#define xfoldcnst2(func, opid, ty, l, r)                        \
    if ((OPKIND((l)->op) == ADD || OPKIND((l)->op) == SUB) &&   \
        OPKIND((l)->kids[1]->op) == CNST) {                     \
        if (OPKIND((r)->op) == CNST)                            \
            return xfold##func(opid, ty, l, r);                 \
        else                                                    \
            return xswap(opid, ty, l, r);                       \
    }

#define doxfoldadd(oper, vfi, vf1, vf2, vf3, op, ty, a, b)              \
    do {                                                                \
        switch (OPTYPE(op)) {                                           \
        case I:                                                         \
        case U:                                                         \
            a->x.value.vfi = a->x.value.vfi oper b->x.value.vfi;        \
            break;                                                      \
        case F:                                                         \
            switch (TYPE_KIND(ty)) {                                    \
            case FLOAT:                                                 \
                a->x.value.vf1 = a->x.value.vf1 oper b->x.value.vf1;    \
                break;                                                  \
            case DOUBLE:                                                \
                a->x.value.vf2 = a->x.value.vf2 oper b->x.value.vf2;    \
                break;                                                  \
            case LONG+DOUBLE:                                           \
                a->x.value.vf3 = a->x.value.vf3 oper b->x.value.vf3;    \
                break;                                                  \
            }                                                           \
            break;                                                      \
        }                                                               \
    } while (0)


static void cvii(struct type *ty, struct expr *l)
{
    if (TYPE_SIZE(l->type) > TYPE_SIZE(ty))
        // narrow
        l->x.value.u &= TYPE_LIMITS(ty).max.u;
}

static void cvif(struct type *ty, struct expr *l)
{
    switch (TYPE_KIND(ty)) {
    case FLOAT:
        l->x.value.f = l->x.value.i;
        break;
    case DOUBLE:
        l->x.value.d = l->x.value.i;
        break;
    case LONG+DOUBLE:
        l->x.value.ld = l->x.value.i;
        break;
    default:
        CC_UNAVAILABLE();
    }
}

static void cvuf(struct type *ty, struct expr *l)
{
    switch (TYPE_KIND(ty)) {
    case FLOAT:
        l->x.value.f = l->x.value.u;
        break;
    case DOUBLE:
        l->x.value.d = l->x.value.u;
        break;
    case LONG+DOUBLE:
        l->x.value.ld = l->x.value.u;
        break;
    default:
        CC_UNAVAILABLE();
    }
}

static void cvfi(struct type *ty, struct expr *l)
{
    switch (TYPE_KIND(l->type)) {
    case FLOAT:
        l->x.value.u = l->x.value.f;
        break;
    case DOUBLE:
        l->x.value.u = l->x.value.d;
        break;
    case LONG+DOUBLE:
        l->x.value.u = l->x.value.ld;
        break;
    default:
        CC_UNAVAILABLE();
    }
}

static void cvff(struct type *ty, struct expr *l)
{
    int dkind = TYPE_KIND(ty);
    int skind = TYPE_KIND(l->type);

    switch (skind) {
    case FLOAT:
        if (dkind == DOUBLE)
            l->x.value.d = l->x.value.f;
        else if (dkind == LONG+DOUBLE)
            l->x.value.ld = l->x.value.f;
        break;
    case DOUBLE:
        if (dkind == FLOAT)
            l->x.value.f = l->x.value.d;
        else if (dkind == LONG+DOUBLE)
            l->x.value.ld = l->x.value.d;
        break;
    case LONG+DOUBLE:
        if (dkind == FLOAT)
            l->x.value.f = l->x.value.ld;
        else if (dkind == DOUBLE)
            l->x.value.d = l->x.value.ld;
        break;
    default:
        CC_UNAVAILABLE();
    }
}

static void cvpi(struct type *ty, struct expr *l)
{
    assert(TYPE_SIZE(ty) == TYPE_SIZE(ptritype) &&
           "Fatal: Non-converted pointer type");
}

static void cvip(struct type *ty, struct expr *l)
{
    assert(TYPE_SIZE(l->type) == TYPE_SIZE(ptritype) &&
           "Fatal: Non-converted pointer type");
}

// fold l->kids[1] and r
static struct expr *xfoldadd(int op, struct type *ty, struct expr *l, struct expr *r)
{
    struct expr *r2 = l->kids[1]; // cnst
    int kind1 = OPKIND(op);
    int kind2 = OPKIND(l->op);

    if ((kind1 == ADD && kind2 == ADD) ||
        (kind1 == SUB && kind2 == SUB)) {
        doxfoldadd(+, u, f, d, ld, op, ty, r2, r);
        return l;
    } else if ((kind1 == ADD && kind2 == SUB) ||
               (kind1 == SUB && kind2 == ADD)) {
        doxfoldadd(-, u, f, d, ld, op, ty, r2, r);
        return l;
    }

    CC_UNAVAILABLE();
}

// swap l->kids[1] andr 
static struct expr *xswap(int op, struct type *ty, struct expr *l, struct expr *r)
{
    struct expr *r2 = l->kids[1]; // cnst
    int op2 = l->op;

    l->op = op;
    l->kids[1] = r;
    return ast_expr(op2, ty, l, r2);
}

// fold constants
struct expr *simplify(int op, struct type *ty, struct expr *l, struct expr *r)
{
    switch (op) {
        // binary
    case ADD+I:
    case ADD+U:
    case ADD+P:
        foldcnst2i(+, u, ty, l, r);
        exchange(l, r);
        xfoldcnst2(add, op, ty, l, r);
        break;
    case ADD+F:
        foldcnst2fx(+, ty, l, r);
        exchange(l, r);
        xfoldcnst2(add, op, ty, l, r);
        break;

    case SUB+I:
    case SUB+U:
    case SUB+P:
        foldcnst2i(-, u, ty, l, r);
        exchange(l, r);
        xfoldcnst2(add, op, ty, l, r);
        break;
    case SUB+F:
        foldcnst2fx(-, ty, l, r);
        exchange(l, r);
        xfoldcnst2(add, op, ty, l, r);
        break;

    case MUL+I:
        foldcnst2i(*, i, ty, l, r);
        break;
    case MUL+U:
        foldcnst2i(*, u, ty, l, r);
        break;
    case MUL+F:
        foldcnst2fx(*, ty, l, r);
        break;

    case DIV+I:
        foldcnst2i(/, i, ty, l, r);
        break;
    case DIV+U:
        foldcnst2i(/, u, ty, l, r);
        break;
    case DIV+F:
        foldcnst2fx(/, ty, l, r);
        break;

    case MOD+I:
        foldcnst2i(%, i, ty, l, r);
        break;
    case MOD+U:
        foldcnst2i(%, u, ty, l, r);
        break;

    case SHL+I:
    case SHL+U:
        foldcnst2i(<<, u, ty, l, r);
        break;

    case SHR+I:
    case SHR+U:
        foldcnst2i(>>, u, ty, l, r);
        break;

    case BAND+I:
    case BAND+U:
        foldcnst2i(&, u, ty, l, r);
        break;

    case BOR+I:
    case BOR+U:
        foldcnst2i(|, u, ty, l, r);
        break;

    case XOR+I:
    case XOR+U:
        foldcnst2i(^, u, ty, l, r);
        break;

        // rel
    case GT+I:
    case GT+U:
    case GT+P:
        foldcnst2i(>, u, ty, l, r);
        break;
    case GT+F:
        foldcnst2fx(>, ty, l, r);
        break;

    case GE+I:
    case GE+U:
    case GE+P:
        foldcnst2i(>=, u, ty, l, r);
        break;
    case GE+F:
        foldcnst2fx(>=, ty, l, r);
        break;
        
    case LT+I:
    case LT+U:
    case LT+P:
        foldcnst2i(<, u, ty, l, r);
        break;
    case LT+F:
        foldcnst2fx(<, ty, l, r);
        break;
        
    case LE+I:
    case LE+U:
    case LE+P:
        foldcnst2i(<=, u, ty, l, r);
        break;
    case LE+F:
        foldcnst2fx(<=, ty, l, r);
        break;

        // eq
    case EQ+I:
    case EQ+U:
    case EQ+P:
        foldcnst2i(==, u, ty, l, r);
        break;
    case EQ+F:
        foldcnst2fx(==, ty, l, r);
        break;

    case NE+I:
    case NE+U:
    case NE+P:
        foldcnst2i(!=, u, ty, l, r);
        break;
    case NE+F:
        foldcnst2fx(!=, ty, l, r);
        break;

        // unary
    case NEG+I:
    case NEG+U:
        foldcnst1i(-, u, ty, l);
        break;
    case NEG+F:
        foldcnst1fx(-, ty, l);
        break;
    case BNOT+I:
    case BNOT+U:
        foldcnst1i(~, u, ty, l);
        break;
        // cast
    case CVI+I:
    case CVI+U:
    case CVU+U:
    case CVU+I:
        xxcv(ii, ty, l);
        break;
    case CVI+F:
        xxcv(if, ty, l);
        break;
    case CVU+F:
        xxcv(uf, ty, l);
        break;
    case CVF+I:
    case CVF+U:
        xxcv(fi, ty, l);
        break;
    case CVF+F:
        xxcv(ff, ty, l);
        break;
    case CVP+I:
    case CVP+U:
        xxcv(pi, ty, l);
        break;
    case CVI+P:
    case CVU+P:
        xxcv(ip, ty, l);
        break;
    }
    return ast_expr(op, ty, l, r);
}

struct expr *eval(struct expr *expr, struct type * ty)
{
    // TODO:
    return cnsti(1, ty);
}
