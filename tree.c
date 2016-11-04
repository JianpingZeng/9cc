#include <assert.h>
#include "cc.h"

#define WRN_EXPR_RESULT_NOT_USED  "expression result not used"

static struct expr *reduce1(struct expr *p, int warn)
{    
    switch (OPKIND(p->op)) {
    case RIGHT:
        if (p->kids[1] == NULL)
            return reduce1(p->kids[0], warn);
        // TODO: 
        return p;
    case COND:
        {
            struct expr *r = p->kids[1];
            assert(OPKIND(r->op) == RIGHT);

            if (p->s.sym && OPKIND(r->kids[0]->op) == ASGN)
                r->kids[0] = reduce1(r->kids[0]->kids[1], warn+1);
            else
                r->kids[0] = reduce1(r->kids[0], warn+1);

            if (p->s.sym && OPKIND(r->kids[1]->op) == ASGN)
                r->kids[1] = reduce1(r->kids[1]->kids[1], warn+1);
            else
                r->kids[1] = reduce1(r->kids[1], warn+1);

            // discard the result
            if (p->s.sym)
                unuse(p->s.sym);
            p->s.sym = NULL;
            if (r->kids[0] == NULL && r->kids[1] == NULL)
                return reduce1(p->kids[0], warn);
            else
                return p;
        }
    case AND:
    case OR:
        if (reduce1(p->kids[1], warn) == NULL)
            return reduce1(p->kids[0], warn+1);
        else
            return p;
        // binary
    case ADD:
    case SUB:
    case MUL:
    case DIV:
    case MOD:
    case SHL:
    case SHR:
    case BAND:
    case BOR:
    case XOR:
    case EQ:
    case NE:
    case GT:
    case GE:
    case LT:
    case LE:
        if (warn++ == 0)
            warning(WRN_EXPR_RESULT_NOT_USED);
        p = ast_expr(RIGHT, p->type,
                     reduce1(p->kids[0], warn),
                     reduce1(p->kids[1], warn));
        return p->kids[0] || p->kids[1] ? p : NULL;
    case INDIR:
    case BFIELD:
        // unary
    case NEG:
    case BNOT:
        // conv
        if (warn++ == 0)
            warning(WRN_EXPR_RESULT_NOT_USED);
    case CVI:
    case CVU:
    case CVF:
    case CVP:
        return reduce1(p->kids[0], warn);
    case CNST:
    case ADDRG:
    case ADDRP:
    case ADDRL:
        return NULL;
    case CALL:
    case ASGN:
        return p;
    case COMPOUND:
        // TODO:
        return p;
    default:
        CC_UNAVAILABLE();
    }
}

// remove expressions with no side-effect.
// expr may be NULL.
struct expr *reduce(struct expr *expr)
{
    if (expr)
        return reduce1(expr, 0);
    else
        return NULL;
}

// get the address of an expr returning struct/union
struct expr *addrof(struct expr *expr)
{
    struct expr *p = expr;

    while (1) {
        switch (OPKIND(p->op)) {
        case RIGHT:
            assert(p->kids[1] || p->kids[0]);
            p = p->kids[1] ? p->kids[1] : p->kids[0];
            continue;
        case COND:
            p = mkref(p->s.sym);
            // fall through
        case INDIR:
        case ASGN:
            if (p == expr)
                return p->kids[0];
            p = p->kids[0];
            return ast_expr(RIGHT, p->type, reduce(expr), p);
        default:
            CC_UNAVAILABLE();
        }
    }
}
