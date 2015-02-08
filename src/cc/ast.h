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

#define NODE(n)     ((struct node*) (n))
extern const char * node_print_function(void *data);
extern const char *nname(struct node *node);

extern struct expr * expr_node(int id, int op, struct expr *l, struct expr *r);
extern struct expr * expr();
extern struct expr * test_expr();

extern void print_tree(struct node *root);

#define isexpr(n)  ((n)->id > BEGIN_EXPR_ID && (n)->id < END_EXPR_ID)

#endif
