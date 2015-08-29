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
    
    assert(AST_ID(node) > BEGIN_NODE_ID && AST_ID(node) < END_NODE_ID);
    
    return node_names[AST_ID(node)];
}

union node * ast_expr(int id, int op, union node *l, union node *r)
{
    assert(id > BEGIN_EXPR_ID && id < END_EXPR_ID);
    union node * expr = NEWU(node);
    AST_ID(expr) = id;
    EXPR_OP(expr) = op;
    EXPR_OPERAND(expr, 0) = l;
    EXPR_OPERAND(expr, 1) = r;
    return expr;
}

union node * ast_stmt(int id, union node *l, union node *r)
{
    assert(id > BEGIN_STMT_ID && id < END_STMT_ID);
    union node * stmt = NEWU(node);
    AST_ID(stmt) = id;
    AST_KID(stmt, 0) = l;
    AST_KID(stmt, 1) = r;
    return stmt;
}

union node * ast_decl(int id, int scope)
{
    assert(id > BEGIN_DECL_ID && id < END_DECL_ID);
    union node * decl = NEWU(node);
    AST_ID(decl) = id;
    DECL_SCOPE(decl) = scope;
    return decl;
}

union node * ast_uop(int op, struct type *ty, union node *l)
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

union node * ast_conv(struct type *ty, union node *l)
{
    union node * expr = ast_expr(CONV_EXPR, 0, l, NULL);
    AST_TYPE(expr) = ty;
    return expr;
}

union node * ast_vinit()
{
    static union node *vinit;
    if (vinit == NULL)
        vinit = ast_expr(VINIT_EXPR, 0, NULL, NULL);
    return vinit;
}

