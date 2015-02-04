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
extern struct decl * decl_node(int id, int scope);
extern struct stmt * stmt_node(int id);
extern struct expr * expr_node(int id, int op);

#endif
