#include "cc.h"

static const char * node_names[] = {
#define _ns(a)   "",
#define _n(a, b) b,
#include "node.def"
};

const char *nname(union node * node)
{
    if (node == NULL)
        return "<NULL>";
    
    CCAssert(AST_ID(node) > BEGIN_NODE_ID && AST_ID(node) < END_NODE_ID);
    
    return node_names[AST_ID(node)];
}

static inline union node * new_node(int id)
{
    union node *n = alloc_node();
    AST_ID(n) = id;
    return n;
}

union node * ast_expr(int id, int op, union node *l, union node *r)
{
    CCAssert(id > BEGIN_EXPR_ID && id < END_EXPR_ID);
    union node * expr = new_node(id);
    EXPR_OP(expr) = op;
    EXPR_OPERAND(expr, 0) = l;
    EXPR_OPERAND(expr, 1) = r;
    return expr;
}

union node * ast_stmt(int id, union node *l, union node *r)
{
    CCAssert(id > BEGIN_STMT_ID && id < END_STMT_ID);
    union node * stmt = new_node(id);
    AST_KID(stmt, 0) = l;
    AST_KID(stmt, 1) = r;
    return stmt;
}

union node * ast_decl(int id, int scope)
{
    CCAssert(id > BEGIN_DECL_ID && id < END_DECL_ID);
    union node * decl = new_node(id);
    DECL_SCOPE(decl) = scope;
    return decl;
}

union node * ast_uop(int op, union node *ty, union node *l)
{
    union node * expr = ast_expr(UNARY_OPERATOR, op, l, NULL);
    AST_TYPE(expr) = ty;
    return expr;
}

union node * ast_bop(int op, union node *l, union node *r)
{
    union node * expr = ast_expr(BINARY_OPERATOR, op, l, r);
    return expr;
}

union node * ast_conv(union node *ty, union node *l, const char *name)
{
    union node * expr = ast_expr(CONV_EXPR, 0, l, NULL);
    AST_TYPE(expr) = ty;
    AST_NAME(expr) = name;
    return expr;
}

union node * ast_inits()
{
    union node * expr = ast_expr(INITS_EXPR, 0, NULL, NULL);
    return expr;
}

union node * ast_vinit()
{
    static union node *vinit;
    if (vinit == NULL)
        vinit = ast_expr(VINIT_EXPR, 0, NULL, NULL);
    return vinit;
}
