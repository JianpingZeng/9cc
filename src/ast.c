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

struct node * expr_node(int id, int op, struct node *l, struct node *r)
{
    assert(id > BEGIN_EXPR_ID && id < END_EXPR_ID);
    struct node * expr = NEWS(node);
    expr->id = id;
    expr->u.e.op = op;
    LEFT(expr) = l;
    RIGHT(expr) = r;
    return expr;
}

struct node * stmt_node(int id, struct node *l, struct node *r)
{
    assert(id > BEGIN_STMT_ID && id < END_STMT_ID);
    struct node * stmt = NEWS(node);
    stmt->id = id;
    LEFT(stmt) = l;
    RIGHT(stmt) = r;
    return stmt;
}

struct node * decl_node(int id, int scope)
{
    assert(id > BEGIN_DECL_ID && id < END_DECL_ID);
    struct node * decl = NEWS(node);
    decl->id = id;
    decl->u.d.scope = scope;
    return decl;
}

struct field * new_field(char *id)
{
    struct field *field = NEWS(field);
    field->name = id;
    return field;
}

struct node * unode(int op, struct type *ty, struct node *l)
{
    struct node * expr = expr_node(UNARY_OPERATOR, op, l, NULL);
    expr->type = ty;
    return expr;
}

struct node * bnode(int op, struct node *l, struct node *r)
{
    struct node * expr = expr_node(BINARY_OPERATOR, op, l, r);
    return expr;
}
