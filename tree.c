#include <assert.h>
#include "cc.h"

// simplify a root expression
struct expr *reduce(struct expr *expr)
{
    // TODO:
    return expr;
}

// get the address of an expr returning struct/union
struct expr *addrof(struct expr *expr)
{
    struct expr *p = expr;

    while (1) {
        switch (OPINDEX(p->op)) {
        case RIGHT:
            assert(p->kids[1] || p->kids[0]);
            p = p->kids[1] ? p->kids[1] : p->kids[0];
            continue;
        case COND:
            p = mkref(p->x.sym);
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
