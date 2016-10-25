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

#define foldcnst1(func, ty, l)                  \
    if (OPKIND((l)->op) == CNST) {              \
        func(ty, l);                            \
        (l)->op = mkop(CNST, ty);               \
        (l)->type = ty;                         \
        return l;                               \
    }

#define foldcnst2(func, ty, l, r)                               \
    if (OPKIND((l)->op) == CNST && OPKIND((r)->op) == CNST) {   \
        struct expr *n = func(ty, l, r);                        \
        (n)->op = mkop(CNST, ty);                               \
        (n)->type = ty;                                         \
        return n;                                               \
    }

struct expr *eval(struct expr *expr, struct type * ty)
{
    // TODO:
    return cnsti(1, ty);
}

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

static void negi(struct type *ty, struct expr *l)
{
    l->x.value.u = -l->x.value.u;
}

static void negf(struct type *ty, struct expr *l)
{
    switch (TYPE_KIND(ty)) {
    case FLOAT:
        l->x.value.f = -l->x.value.f;
        break;
    case DOUBLE:
        l->x.value.d = -l->x.value.d;
        break;
    case LONG+DOUBLE:
        l->x.value.ld = -l->x.value.ld;
        break;
    default:
        CC_UNAVAILABLE();
    }
}

static void bnot(struct type *ty, struct expr *l)
{
    l->x.value.u = ~l->x.value.u;
}

static struct expr *addi(struct type *ty, struct expr *l, struct expr *r)
{
    return NULL;
}

static struct expr *addf(struct type *ty, struct expr *l, struct expr *r)
{
    return NULL;
}

static struct expr *addp(struct type *ty, struct expr *l, struct expr *r)
{
    return NULL;
}

// fold constants
struct expr *simplify(int op, struct type *ty, struct expr *l, struct expr *r)
{
    switch (op) {
        // binary
    case ADD+I:
    case ADD+U:
        foldcnst2(addi, ty, l, r);
        break;
    case ADD+F:
        foldcnst2(addf, ty, l, r);
        break;
    case ADD+P:
        foldcnst2(addp, ty, l, r);
        break;
    case SUB+I:
    case SUB+U:
    case SUB+F:
    case SUB+P:
        break;
    case MUL+I:
    case MUL+U:
    case MUL+F:
        break;
    case DIV+I:
    case DIV+U:
    case DIV+F:
        break;
    case MOD+I:
    case MOD+U:
        break;
    case SHL+I:
    case SHL+U:
        break;
    case SHR+I:
    case SHR+U:
        break;
    case BAND+I:
    case BAND+U:
        break;
    case BOR+I:
    case BOR+U:
        break;
    case XOR+I:
    case XOR+U:
        break;
    case GT+I:
    case GT+U:
    case GT+F:
    case GT+P:
        break;
    case GE+I:
    case GE+U:
    case GE+F:
    case GE+P:
        break;
    case LT+I:
    case LT+U:
    case LT+F:
    case LT+P:
        break;
    case LE+I:
    case LE+U:
    case LE+F:
    case LE+P:
        break;
    case EQ+I:
    case EQ+U:
    case EQ+F:
    case EQ+P:
        break;
    case NE+I:
    case NE+U:
    case NE+F:
    case NE+P:
        break;
        // unary
    case NEG+I:
    case NEG+U:
        foldcnst1(negi, ty, l);
        break;
    case NEG+F:
        foldcnst1(negf, ty, l);
        break;
    case BNOT+I:
    case BNOT+U:
        foldcnst1(bnot, ty, l);
        break;
        // cast
    case CVI+I:
    case CVI+U:
    case CVU+U:
    case CVU+I:
        foldcnst1(cvii, ty, l);
        break;
    case CVI+F:
        foldcnst1(cvif, ty, l);
        break;
    case CVU+F:
        foldcnst1(cvuf, ty, l);
        break;
    case CVF+I:
    case CVF+U:
        foldcnst1(cvfi, ty, l);
        break;
    case CVF+F:
        foldcnst1(cvff, ty, l);
        break;
    case CVP+I:
    case CVP+U:
        foldcnst1(cvpi, ty, l);
        break;
    case CVI+P:
    case CVU+P:
        foldcnst1(cvip, ty, l);
        break;
    }
    return ast_expr(op, ty, l, r);
}
