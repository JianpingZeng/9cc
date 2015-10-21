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
    
    cc_assert(AST_ID(node) > BEGIN_NODE_ID && AST_ID(node) < END_NODE_ID);
    
    return node_names[AST_ID(node)];
}

static inline node_t * new_node(int id)
{
    node_t *n = alloc_node();
    AST_ID(n) = id;
    return n;
}

void * alloc_symbol(void)
{
    return new_node(SYMBOL_NODE);
}

void * alloc_type(void)
{
    return new_node(TYPE_NODE);
}

void * alloc_field(void)
{
    return new_node(FIELD_NODE);
}

node_t * ast_expr(int id, node_t *ty, node_t *l, node_t *r)
{
    cc_assert(id > BEGIN_EXPR_ID && id < END_EXPR_ID);
    node_t * expr = new_node(id);
    EXPR_OPERAND(expr, 0) = l;
    EXPR_OPERAND(expr, 1) = r;
    AST_TYPE(expr) = ty;
    return expr;
}

node_t * ast_stmt(int id, struct source src)
{
    cc_assert(id > BEGIN_STMT_ID && id < END_STMT_ID);
    node_t * stmt = new_node(id);
    AST_SRC(stmt) = src;
    return stmt;
}

node_t * ast_null_stmt(void)
{
    node_t *stmt = ast_stmt(NULL_STMT, source);
    return stmt;
}

node_t * ast_decl(int id, int scope)
{
    cc_assert(id > BEGIN_DECL_ID && id < END_DECL_ID);
    node_t * decl = new_node(id);
    DECL_SCOPE(decl) = scope;
    return decl;
}

node_t * ast_uop(int op, node_t *ty, node_t *l)
{
    node_t *expr = ast_expr(UNARY_OPERATOR, ty, l, NULL);
    EXPR_OP(expr) = op;
    return expr;
}

node_t * ast_bop(int op, node_t *ty, node_t *l, node_t *r)
{
    node_t *expr = ast_expr(BINARY_OPERATOR, ty, l, r);
    EXPR_OP(expr) = op;
    return expr;
}

node_t * ast_conv(node_t *ty, node_t *l, const char *name)
{
    node_t * expr = ast_expr(CONV_EXPR, ty, l, NULL);
    AST_NAME(expr) = name;
    AST_SRC(expr) = AST_SRC(l);
    return expr;
}

node_t * ast_inits(void)
{
    node_t * expr = ast_expr(INITS_EXPR, NULL, NULL, NULL);
    return expr;
}

node_t * ast_vinit(void)
{
    node_t *vinit = ast_expr(VINIT_EXPR, NULL, NULL, NULL);
    return vinit;
}

const char * gen_label(void)
{
    static long i;
    return format(".L%ld", i++);
}

const char * gen_tmpname(void)
{
    static long i;
    return format(".T%ld", i++);
}

node_t * ast_if(node_t *cond, node_t *then, node_t *els)
{
    node_t * ast = new_node(AST_IF);
    GEN_COND(ast) = cond;
    GEN_THEN(ast) = then;
    GEN_ELSE(ast) = els;
    return ast;
}

node_t * ast_jump(const char *label)
{
    node_t * ast = new_node(AST_JUMP);
    GEN_LABEL(ast) = label;
    return ast;
}

node_t * ast_goto(const char *label)
{
    node_t * ast = new_node(AST_JUMP);
    GEN_LABEL(ast) = label;
    return ast;
}

node_t * ast_label(const char *label)
{
    node_t * ast = new_node(AST_LABEL);
    GEN_LABEL(ast) = label;
    return ast;
}

node_t * ast_dest(const char *label)
{
    node_t * ast = new_node(AST_LABEL);
    GEN_LABEL(ast) = label;
    return ast;
}

node_t * ast_return(node_t *node)
{
    node_t * ast = new_node(AST_RETURN);
    GEN_OPERAND(ast) = node;
    return ast;
}

node_t * ast_compound(node_t **list)
{
    node_t * ast = new_node(AST_COMPOUND);
    GEN_LIST(ast) = list;
    return ast;
}
