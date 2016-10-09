#include <assert.h>
#include "cc.h"

static const char *nnames[] = {
    "null",
#define _n(_, b) b,
#include "node.def"
};

static const char *nname(int op)
{
    assert(OPINDEX(op) > OPNONE && OPINDEX(op) < OPEND);
    return nnames[OPINDEX(op)];
}

struct expr *ast_expr(int op, struct type *ty, struct expr *l, struct expr *r)
{
    struct expr *expr = NEWS0(struct expr, FUNC);
    expr->op = op;
    expr->type = ty;
    expr->kids[0] = l;
    expr->kids[1] = r;
    return expr;
}

struct stmt *ast_stmt(int id)
{
    assert(id >= LABEL && id <= RET);
    struct stmt *stmt = NEWS0(struct stmt, FUNC);
    stmt->id = id;
    return stmt;
}

const char *gen_tmpname(void)
{
    static size_t i;
    return format(".T%llu", i++);
}

const char *gen_compound_label(void)
{
    static size_t i;
    return format("__compound_literal.%llu", i++);
}

int genlabel(int count)
{
    static int lab = 1;
    assert(count > 0);
    lab += count;
    return lab - count;
}
