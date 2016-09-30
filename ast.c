#include <assert.h>
#include "cc.h"

static const char *node_names[] = {
    "OPNONE",
#define _n(a) #a,
#include "node.def"
};

const char *nname(int op)
{
    return node_names[OPINDEX(op)];
}

struct expr *ast_expr(int op, struct type *ty, struct expr *l, struct expr *r)
{
    struct expr *expr = NEWS0(struct expr, FUNC);
    expr->op = op;
    expr->type = ty;
    EXPR_OPERAND(expr, 0) = l;
    EXPR_OPERAND(expr, 1) = r;
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
