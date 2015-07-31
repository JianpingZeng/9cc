#include "cc.h"

static const char * node_names[] = {
#define _ns(a)   "",
#define _n(a, b) b,
#include "node.def"
};

// alloc nodes
static void * alloc_node(struct bucket_info **s, size_t size)
{
    if (!*s)
        *s = alloc_bucket(size);
    return alloc_for_size(*s, size);
}

static struct bucket_info *expr_info;
static void * alloc_expr_node()
{
    void *ret = alloc_node(&expr_info, sizeof(struct expr));
    return ret;
}

static struct bucket_info *stmt_info;
static void * alloc_stmt_node()
{
    void *ret = alloc_node(&stmt_info, sizeof(struct stmt));
    return ret;
}

static struct bucket_info *decl_info;
static void * alloc_decl_node()
{
    void *ret = alloc_node(&decl_info, sizeof(struct decl));
    return ret;
}

static struct bucket_info *type_info;
static void * alloc_type_node()
{
    void *ret = alloc_node(&type_info, sizeof(struct type));
    return ret;
}

static struct bucket_info *symbol_info;
static void * alloc_symbol_node()
{
    void *ret = alloc_node(&symbol_info, sizeof(struct symbol));
    return ret;
}

static struct bucket_info *field_info;
static void * alloc_field_node()
{
    void *ret = alloc_node(&field_info, sizeof(struct field));
    return ret;
}

void print_cc_alloc_info()
{
    print_bucket(expr_info, "expr_info");
    print_bucket(stmt_info, "stmt_info");
    print_bucket(decl_info, "decl_info");
    print_bucket(type_info, "type_info");
    print_bucket(symbol_info, "symbol_info");
}

void cc_exit()
{
    free_bucket(expr_info);
    free_bucket(stmt_info);
    free_bucket(decl_info);
    free_bucket(type_info);
    free_bucket(symbol_info);
    free_bucket(field_info);
    expr_info = NULL;
    stmt_info = NULL;
    decl_info = NULL;
    type_info = NULL;
    symbol_info = NULL;
    field_info = NULL;
    symbol_exit();
}

const char *nname(struct node * node)
{
    if (node == NULL)
        return "<NULL>";
    
    assert(node->id > BEGIN_NODE_ID && node->id < END_NODE_ID);
    
    return node_names[node->id];
}

struct expr * expr_node(int id, int op, struct expr *l, struct expr *r)
{
    assert(id > BEGIN_EXPR_ID && id < END_EXPR_ID);
    struct expr * expr = alloc_expr_node();
    expr->node.id = id;
    expr->op = op;
    expr->node.kids[0] = NODE(l);
    expr->node.kids[1] = NODE(r);
    return expr;
}

struct stmt * stmt_node(int id, struct node *l, struct node *r)
{
    assert(id > BEGIN_STMT_ID && id < END_STMT_ID);
    struct stmt * stmt = alloc_stmt_node();
    stmt->node.id = id;
    stmt->node.kids[0] = l;
    stmt->node.kids[1] = r;
    return stmt;
}

struct decl * decl_node(int id, int scope)
{
    assert(id > BEGIN_DECL_ID && id < END_DECL_ID);
    struct decl * decl = alloc_decl_node();
    decl->node.id = id;
    decl->scope = scope;
    return decl;
}

struct type * new_type()
{
    return alloc_type_node();
}

struct symbol * new_symbol()
{
    return alloc_symbol_node();
}

struct field * new_field(char *id)
{
    struct field *field = alloc_field_node();
    field->name = id;
    return field;
}
