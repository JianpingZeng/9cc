#include "cc.h"

static const char * node_names[] = {
#define _ns(a)   "",
#define _n(a, b) b,
#include "node.def"
};

const char *nname(struct node * node)
{
    if (node == NULL)
        return "<NULL>";
    
    assert(node->id > BEGIN_NODE_ID && node->id < END_NODE_ID);
    
    return node_names[node->id];
}

struct node * ast_expr(int id, int op, struct node *l, struct node *r)
{
    assert(id > BEGIN_EXPR_ID && id < END_EXPR_ID);
    struct node * expr = NEWS(node);
    expr->id = id;
    expr->u.e.op = op;
    LEFT(expr) = l;
    RIGHT(expr) = r;
    return expr;
}

struct node * ast_stmt(int id, struct node *l, struct node *r)
{
    assert(id > BEGIN_STMT_ID && id < END_STMT_ID);
    struct node * stmt = NEWS(node);
    stmt->id = id;
    LEFT(stmt) = l;
    RIGHT(stmt) = r;
    return stmt;
}

struct node * ast_decl(int id, int scope)
{
    assert(id > BEGIN_DECL_ID && id < END_DECL_ID);
    struct node * decl = NEWS(node);
    decl->id = id;
    decl->u.d.scope = scope;
    return decl;
}

struct node * ast_uop(int op, struct type *ty, struct node *l)
{
    struct node * expr = ast_expr(UNARY_OPERATOR, op, l, NULL);
    expr->type = ty;
    return expr;
}

struct node * ast_bop(int op, struct node *l, struct node *r)
{
    struct node * expr = ast_expr(BINARY_OPERATOR, op, l, r);
    return expr;
}

struct node * ast_conv(struct type *ty, struct node *l)
{
    struct node * expr = ast_expr(CONV_EXPR, 0, l, NULL);
    expr->type = ty;
    return expr;
}

struct node * ast_vinit()
{
    static struct node *vinit;
    if (vinit == NULL)
        vinit = ast_expr(VINIT_EXPR, 0, NULL, NULL);
    return vinit;
}
