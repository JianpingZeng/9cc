#ifndef cc_ast_h
#define cc_ast_h

// node ids
enum {
#define _ns(a)   a,
#define _n(a, b) a,
#include "node.h"
};

// node
struct node {
    int id;
    struct symbol *symbol;
    struct node *kids[2];
};

// expr
struct expr {
    struct node node;
    int op;
};

// stmt
struct stmt {
    struct node node;
    
};

// decl
struct decl {
    struct node node;
    int scope;
};

// ast
extern const char * node_print_function(void *data);
extern const char *nname(struct node *node);
extern void print_tree(struct node *tree);

// expr
extern struct expr * expr_node(int id, int op, struct expr *l, struct expr *r);
extern struct expr * expr();

// decl
extern struct decl * initializer_list();
extern int kind(int t);

#define NODE(n)    ((struct node*) (n))
#define isexpr(n)  ((n)->id > BEGIN_EXPR_ID && (n)->id < END_EXPR_ID)
#define isdecl(n)  ((n)->id > BEGIN_DECL_ID && (n)->id < END_DECL_ID)
#define isstmt(n)  ((n)->id > BEGIN_STMT_ID && (n)->id < END_STMT_ID)

#endif
