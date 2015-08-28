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

struct ast_common {
    int id;
    const char *name;
    struct type *type;
};

#define TYPE_TYPE(NODE)     ((NODE)->type.type)

struct ast_type {
    struct ast_common common;
    struct type *type;
};

#define DECL_SCOPE(NODE)    ((NODE)->decl.scope)
#define DECL_INIT(NODE)     ((NODE)->decl.init)
#define DECL_EXTS(NODE)     ((NODE)->decl.exts)

struct ast_decl {
    struct ast_common common;
    int scope;
    union node *init;
    union node *exts[FLEX_ARRAY];
};

#define STMT_UP(NODE)       ((NODE)->stmt.up)

struct ast_stmt {
    struct ast_common common;
    union node *up;
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
#define STMT_INIT(NODE)     ((NODE)->for_stmt.init)

struct ast_for_stmt {
    struct ast_common common;
    union node *up;
    union node *test, *ctrl;
    union node *init[FLEX_ARRAY];
};

#define STMT_CASE_INDEX(NODE)     ((NODE)->case_stmt.index)

struct ast_case_stmt {
    struct ast_common common;
    union node *up;
    int index;
};

#define STMT_BLKS(NODE)     ((NODE)->compund_stmt.blks)

struct ast_compound_stmt {
    struct ast_common common;
    union node *up;
    union node *blks[FLEX_ARRAY];
};

#define EXPR_OP(NODE)           ((NODE)->expr.op)
#define EXPR_PREFIX(NODE)       ((NODE)->expr.prefix)
#define EXPR_OPERANDS(NODE)     ((NODE)->expr.operands)
#define EXPR_OPERAND(NODE, I)   ((NODE)->expr.operands[I])

struct ast_expr {
    struct ast_common common;
    int op;
    bool prefix;
    union node *oprands[FLEX_ARRAY];
};

union node {
    struct ast_common common;
    struct ast_decl decl;
    struct ast_stmt stmt;
    struct ast_if_stmt if_stmt;
    struct ast_for_stmt for_stmt;
    struct ast_case_stmt case_stmt;
    struct ast_compound_stmt compound_stmt;
    struct ast_expr expr;
    struct ast_type type;
};

struct node {
    int id;
    struct type *type;
    struct node *kids[2];
    struct symbol *sym;
    union {
        // expr
        struct {
            int op;
            // call
            struct node **args;
            // init list
            struct node **inits;
            // cond
            struct {
                struct node *cond;
                struct node *then;
                struct node *els;
            }c;
            // member
            const char *field;
            // INCR/DECR
            bool prefix;
            // sizeof
            struct type *type;
        }e;
        // stmt
        struct {
            struct node *up;		// internal
            struct {
                struct node **decl;
                struct node *init;
                struct node *cond;
                struct node *ctrl;
            }forstmt;
            struct {
                struct node **blks;	// block items
            }compoundstmt;
            struct {
                int value;
            }casestmt;
        }s;
        // decl
        struct {
            int scope;
            struct node **exts;
            struct node *init;
        }d;
    }u;
};

// ast.c
extern const char *nname(struct node *node);
extern struct node * ast_expr(int id, int op, struct node *l, struct node *r);
extern struct node * ast_decl(int id, int scope);
extern struct node * ast_stmt(int id, struct node *l, struct node *r);
extern struct node * ast_uop(int op, struct type *ty, struct node *l);
extern struct node * ast_bop(int op, struct node *l, struct node *r);
extern struct node * ast_conv(struct type *ty, struct node *l);
extern struct node * ast_vinit();

#endif