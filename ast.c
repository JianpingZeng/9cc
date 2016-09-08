#include "cc.h"

static const char *node_names[] = {
#define _ns(a)   "",
#define _n(a, b) b,
#include "node.def"
};

const char *nname(node_t * node)
{
    if (node == NULL)
        return "<NULL>";

    assert(AST_ID(node) > BEGIN_NODE_ID && AST_ID(node) < END_NODE_ID);

    return node_names[AST_ID(node)];
}

static node_t *new_node(int id, int area)
{
    node_t *n = NEWS0(node_t, area);
    AST_ID(n) = id;
    return n;
}

void *alloc_symbol(int area)
{
    return new_node(SYMBOL_NODE, area);
}

void *alloc_type(void)
{
    return new_node(TYPE_NODE, PERM);
}

void *alloc_field(void)
{
    return new_node(FIELD_NODE, PERM);
}

node_t *ast_expr(int id, node_t * ty, node_t * l, node_t * r)
{
    assert(id > BEGIN_EXPR_ID && id < END_EXPR_ID);
    node_t *expr = new_node(id, PERM);
    EXPR_OPERAND(expr, 0) = l;
    EXPR_OPERAND(expr, 1) = r;
    AST_TYPE(expr) = ty;
    return expr;
}

node_t *ast_uop(int op, node_t * ty, node_t * l)
{
    node_t *expr = ast_expr(UNARY_OPERATOR, ty, l, NULL);
    EXPR_OP(expr) = op;
    return expr;
}

node_t *ast_bop(int op, node_t * ty, node_t * l, node_t * r)
{
    node_t *expr = ast_expr(BINARY_OPERATOR, ty, l, r);
    EXPR_OP(expr) = op;
    return expr;
}

node_t *ast_conv(node_t * ty, node_t * l, const char *name)
{
    node_t *expr = ast_expr(CONV_EXPR, ty, l, NULL);
    AST_NAME(expr) = name;
    AST_SRC(expr) = AST_SRC(l);
    return expr;
}

node_t *ast_inits(node_t * ty, struct source src)
{
    node_t *expr = ast_expr(INITS_EXPR, NULL, NULL, NULL);
    AST_SRC(expr) = src;
    AST_TYPE(expr) = ty;
    return expr;
}

node_t *ast_vinit(void)
{
    node_t *vinit = ast_expr(VINIT_EXPR, NULL, NULL, NULL);
    return vinit;
}

node_t *ast_stmt(int id, struct source src)
{
    assert(id > BEGIN_STMT_ID && id < END_STMT_ID);
    node_t *stmt = NEWS0(node_t, FUNC);
    AST_ID(stmt) = id;
    AST_SRC(stmt) = src;
    return stmt;
}

const char *gen_label(void)
{
    static size_t i;
    return format(".L%llu", i++);
}

const char *gen_tmpname(void)
{
    static size_t i;
    return format(".T%llu", i++);
}

const char *gen_tmpname_r(void)
{
    static size_t i;
    return format(".t%llu", i++);
}

const char *gen_static_label(void)
{
    static size_t i;
    return format(".S%llu", i++);
}

const char *gen_compound_label(void)
{
    static size_t i;
    return format("__compound_literal.%llu", i++);
}

const char *gen_sliteral_label(void)
{
    static size_t i;
    return format(".LC%llu", i++);
}

const char *gen_block_label(void)
{
    static size_t i;
    return format(".LBB_%llu", i++);
}
