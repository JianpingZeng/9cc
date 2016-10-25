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

struct expr *eval(struct expr *expr, struct type * ty)
{
    // TODO:
    return cnsti(1, ty);
}

static struct expr *cvii(int op, struct type *ty, struct expr *l)
{
    return NULL;
}

static struct expr *cvif(int op, struct type *ty, struct expr *l)
{
    return NULL;
}

static struct expr *cvfi(int op, struct type *ty, struct expr *l)
{
    return NULL;
}

static struct expr *cvff(int op, struct type *ty, struct expr *l)
{
    return NULL;
}

static struct expr *cvpi(int op, struct type *ty, struct expr *l)
{
    return NULL;
}

static struct expr *cvip(int op, struct type *ty, struct expr *l)
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
    case ADD+F:
    case ADD+P:
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
    case NEG+F:
        break;
    case NOT+I:
    case NOT+U:
        break;
        // cast
    case CVI+I:
    case CVI+U:
    case CVU+U:
    case CVU+I:
        if (OPKIND(op) == CNST)
            return cvii(op, ty, l);
        break;
    case CVI+F:
    case CVU+F:
        if (OPKIND(op) == CNST)
            return cvif(op, ty, l);
        break;
    case CVF+I:
    case CVF+U:
        if (OPKIND(op) == CNST)
            return cvfi(op, ty, l);
        break;
    case CVF+F:
        if (OPKIND(op) == CNST)
            return cvff(op, ty, l);
        break;
    case CVP+I:
    case CVP+U:
        if (OPKIND(op) == CNST)
            return cvpi(op, ty, l);
        break;
    case CVI+P:
    case CVU+P:
        if (OPKIND(op) == CNST)
            return cvip(op, ty, l);
        break;
    }
    return ast_expr(op, ty, l, r);
}
