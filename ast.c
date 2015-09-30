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

node_t * ast_stmt(int id, struct source src)
{
    CCAssert(id > BEGIN_STMT_ID && id < END_STMT_ID);
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

node_t * ast_bop(int op, node_t *ty, node_t *l, node_t *r)
{
    node_t * expr = ast_expr(BINARY_OPERATOR, op, l, r);
    AST_TYPE(expr) = ty;
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
    node_t *vinit = ast_expr(VINIT_EXPR, 0, NULL, NULL);
    return vinit;
}

const char * gen_label(void)
{
    static long i;
    return strs(format(".L%ld", i));
}

node_t * ast_if(node_t *cond, node_t *then, node_t *els)
{
    node_t * ast = new_node(AST_IF);
    AST_KID(ast, 0) = then;
    AST_KID(ast, 1) = els;
    GEN_OPERAND(ast) = cond;
    return ast;
}

node_t * ast_jump(const char *label)
{
    node_t * ast = new_node(AST_JUMP);
    GEN_LABEL(ast) = label;
    return ast;
}

node_t * ast_dest(const char *label)
{
    node_t * ast = new_node(AST_LABEL);
    GEN_LABEL(ast) = label;
    return ast;
}
