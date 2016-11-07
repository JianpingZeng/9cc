#include <assert.h>
#include "cc.h"

#define WRN_EXPR_RESULT_NOT_USED  "expression result not used"

static struct tree *root1(struct tree *p, int warn)
{    
    switch (OPKIND(p->op)) {
    case RIGHT:
        if (p->kids[1] == NULL)
            return root1(p->kids[0], warn);
        if (p->kids[0] &&
            p->kids[0]->op == RIGHT &&
            p->kids[0]->kids[0] == p->kids[1])
            // postfix increment
            return p->kids[0]->kids[1];
        if (p->kids[0] &&
            OPKIND(p->kids[0]->op) == CALL &&
            p->kids[1] &&
            OPKIND(p->kids[1]->op) == INDIR) {
            // funcall
            deuse(p->kids[0]->s.sym);
            p->kids[0]->s.sym = NULL;
            return p->kids[0];
        }
        p = ast_expr(RIGHT, p->type,
                     root1(p->kids[0], warn),
                     root1(p->kids[1], warn));
        return p->kids[0] || p->kids[1] ? p : NULL;
    case COND:
        {
            struct tree *r = p->kids[1];
            assert(OPKIND(r->op) == RIGHT);

            if (p->s.sym && OPKIND(r->kids[0]->op) == ASGN)
                r->kids[0] = root1(r->kids[0]->kids[1], warn+1);
            else
                r->kids[0] = root1(r->kids[0], warn+1);

            if (p->s.sym && OPKIND(r->kids[1]->op) == ASGN)
                r->kids[1] = root1(r->kids[1]->kids[1], warn+1);
            else
                r->kids[1] = root1(r->kids[1], warn+1);

            // discard the result
            if (p->s.sym)
                deuse(p->s.sym);
            p->s.sym = NULL;
            if (r->kids[0] == NULL && r->kids[1] == NULL)
                return root1(p->kids[0], warn);
            else
                return p;
        }
    case AND:
    case OR:
        if (root1(p->kids[1], warn) == NULL)
            return root1(p->kids[0], warn+1);
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
                     root1(p->kids[0], warn),
                     root1(p->kids[1], warn));
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
        return root1(p->kids[0], warn);
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
struct tree *root(struct tree *expr)
{
    if (expr)
        return root1(expr, 0);
    else
        return NULL;
}

// get the address of an expr returning struct/union
struct tree *addrof(struct tree *expr)
{
    struct tree *p = expr;

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
            return ast_expr(RIGHT, p->type, root(expr), p);
        default:
            CC_UNAVAILABLE();
        }
    }
}

// get the right-most kid of a RIGHT tree
struct tree *rightkid(struct tree *expr)
{
    while (expr && expr->op == RIGHT) {
        if (expr->kids[1])
            expr = expr->kids[1];
        else if (expr->kids[0])
            expr = expr->kids[0];
        else
            assert(0 && "empty RIGHT tree");
    }
    return expr;
}
