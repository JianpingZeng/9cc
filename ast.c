#include "cc.h"

static const char * node_names[] = {
#define _ns(a)   "",
#define _n(a, b) b,
#include "node.def"
};

const char *nname(node_t * node)
{
    if (node == NULL)
        return "<NULL>";
    
    CCAssert(AST_ID(node) > BEGIN_NODE_ID && AST_ID(node) < END_NODE_ID);
    
    return node_names[AST_ID(node)];
}

static inline node_t * new_node(int id)
{
    node_t *n = alloc_node();
    AST_ID(n) = id;
    return n;
}

node_t * ast_expr(int id, int op, node_t *l, node_t *r)
{
    CCAssert(id > BEGIN_EXPR_ID && id < END_EXPR_ID);
    node_t * expr = new_node(id);
    EXPR_OP(expr) = op;
    EXPR_OPERAND(expr, 0) = l;
    EXPR_OPERAND(expr, 1) = r;
    return expr;
}

node_t * ast_stmt(int id, node_t *l, node_t *r)
{
    CCAssert(id > BEGIN_STMT_ID && id < END_STMT_ID);
    node_t * stmt = new_node(id);
    AST_KID(stmt, 0) = l;
    AST_KID(stmt, 1) = r;
    return stmt;
}

node_t * ast_decl(int id, int scope)
{
    CCAssert(id > BEGIN_DECL_ID && id < END_DECL_ID);
    node_t * decl = new_node(id);
    DECL_SCOPE(decl) = scope;
    return decl;
}

node_t * ast_uop(int op, node_t *ty, node_t *l)
{
    node_t * expr = ast_expr(UNARY_OPERATOR, op, l, NULL);
    AST_TYPE(expr) = ty;
    return expr;
}

node_t * ast_bop(int op, node_t *l, node_t *r)
{
    node_t * expr = ast_expr(BINARY_OPERATOR, op, l, r);
    return expr;
}

node_t * ast_conv(node_t *ty, node_t *l, const char *name)
{
    node_t * expr = ast_expr(CONV_EXPR, 0, l, NULL);
    AST_TYPE(expr) = ty;
    AST_NAME(expr) = name;
    return expr;
}

node_t * ast_inits(void)
{
    node_t * expr = ast_expr(INITS_EXPR, 0, NULL, NULL);
    return expr;
}

node_t * ast_vinit(void)
{
    static node_t *vinit;
    if (vinit == NULL)
        vinit = ast_expr(VINIT_EXPR, 0, NULL, NULL);
    return vinit;
}
