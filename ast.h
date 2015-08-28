#ifndef _AST_H
#define _AST_H

// node ids
enum {
#define _ns(a)   a,
#define _n(a, b) a,
#include "node.def"
};

#define AST_ID(NODE)        ((NODE)->common.id)
#define AST_NAME(NODE)      ((NODE)->common.name)
#define AST_TYPE(NODE)      ((NODE)->common.type)
#define AST_KID(NODE, I)    ((NODE)->common.kids[I])

struct ast_common {
    int id;
    const char *name;
    struct type *type;
    union node *kids[2];
};

#define TYPE_TYPE(NODE)     ((NODE)->type.type)

struct ast_type {
    struct ast_common common;
    struct type *type;
};

#define DECL_SCOPE(NODE)    ((NODE)->decl.scope)
#define DECL_SYM(NODE)      ((NODE)->decl.sym)
#define DECL_BODY(NODE)     ((NODE)->decl.body)
#define DECL_EXTS(NODE)     ((NODE)->decl.exts)

struct ast_decl {
    struct ast_common common;
    int scope;
    struct symbol *sym;
    union node *body;
    union node **exts;
};

#define STMT_UP(NODE)       ((NODE)->stmt.up)
#define STMT_BLKS(NODE)     ((NODE)->stmt.blks)

struct ast_stmt {
    struct ast_common common;
    union node *up;
    union node **blks;
};

#define STMT_COND(NODE)     ((NODE)->if_stmt.cond)
#define STMT_THEN(NODE)     ((NODE)->if_stmt.thenpart)
#define STMT_ELSE(NODE)     ((NODE)->if_stmt.elsepart)

struct ast_if_stmt {
    struct ast_common common;
    union node *up;
    union node *cond, *thenpart, *elsepart;
};

#define STMT_TEST(NODE)     ((NODE)->for_stmt.test)
#define STMT_CTRL(NODE)     ((NODE)->for_stmt.ctrl)
#define STMT_DECL(NODE)     ((NODE)->for_stmt.decl)
#define STMT_INIT(NODE)     ((NODE)->for_stmt.init)

struct ast_for_stmt {
    struct ast_common common;
    union node *up;
    union node *cond, *ctrl;
    union node **decl;
    union node *init;
};

#define STMT_CASE_INDEX(NODE)     ((NODE)->case_stmt.index)

struct ast_case_stmt {
    struct ast_common common;
    union node *up;
    int index;
};

#define EXPR_OP(NODE)           ((NODE)->expr.op)
#define EXPR_PREFIX(NODE)       ((NODE)->expr.prefix)
#define EXPR_OPERANDS(NODE)     ((NODE)->expr.operands)
#define EXPR_OPERAND(NODE, I)   *expr_operand(NODE, I)

struct ast_expr {
    struct ast_common common;
    int op;
    bool prefix;
    union node *operands[1];
};

union node {
    struct ast_common common;
    struct ast_decl decl;
    struct ast_stmt stmt;
    struct ast_if_stmt if_stmt;
    struct ast_for_stmt for_stmt;
    struct ast_case_stmt case_stmt;
    struct ast_expr expr;
    struct ast_type type;
};

// ast.c
extern const char *nname(union node *node);
extern union node * ast_expr(int id, int op, union node *l, union node *r);
extern union node * ast_decl(int id, int scope);
extern union node * ast_stmt(int id, union node *l, union node *r);
extern union node * ast_uop(int op, struct type *ty, union node *l);
extern union node * ast_bop(int op, union node *l, union node *r);
extern union node * ast_conv(struct type *ty, union node *l);
extern union node * ast_vinit();
extern union node ** expr_operand(union node *node, unsigned i);

#define isexpr(n)  (AST_ID(n) > BEGIN_EXPR_ID && AST_ID(n) < END_EXPR_ID)
#define isdecl(n)  (AST_ID(n) > BEGIN_DECL_ID && AST_ID(n) < END_DECL_ID)
#define isstmt(n)  (AST_ID(n) > BEGIN_STMT_ID && AST_ID(n) < END_STMT_ID)
#define isfuncdecl(n) (AST_ID(n) == FUNC_DECL)
#define isfuncdef(n) (isfuncdecl(n) && AST_BODY(n) && AST_ID(AST_BODY(n)) == COMPOUND_STMT)
#define isliteral(n) (AST_ID(n) > BEGIN_LITERAL_ID && AST_ID(n) < END_LITERAL_ID)
#define is_switch_stmt(n) (AST_ID(n) == SWITCH_STMT)
#define is_iteration_stmt(n) (AST_ID(n) == FOR_STMT || AST_ID(n) == WHILE_STMT || AST_ID(n) == DO_WHILE_STMT)

#endif