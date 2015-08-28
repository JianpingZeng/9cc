#ifndef _AST_H
#define _AST_H

// node ids
enum {
#define _ns(a)   a,
#define _n(a, b) a,
#include "node.def"
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