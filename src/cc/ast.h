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
    struct stmt *up;		// internal
};

// decl
struct decl {
    struct node node;
    int scope;
};

// ast
extern const char * node_print_function(void *data);
extern const char *nname(struct node *node);
extern struct node * concat_node(struct node *l, struct node *r);
extern int is_iteration_stmt(struct stmt *stmt);
extern int is_switch_stmt(struct stmt *stmt);
extern void concats(struct node **head, struct node *node);

// expr
extern struct expr * expr_node(int id, int op, struct expr *l, struct expr *r);
extern struct expr * expression();
extern struct expr * constant_expression();
extern struct expr * assign_expression();

// decl
extern struct decl * decl_node(int id, int scope);
extern struct decl * initializer_list();
extern int kind(int t);
extern int is_typename(struct token *t);
extern struct node * declaration();
extern struct decl * translation_unit();
extern struct type * typename();

// stmt
extern struct stmt * stmt_node(int id, struct node *l, struct node *r);
extern struct stmt * compound_statement(struct stmt *context);

#define NODE(n)    ((struct node*) (n))
#define isexpr(n)  ((n)->id > BEGIN_EXPR_ID && (n)->id < END_EXPR_ID)
#define isdecl(n)  ((n)->id > BEGIN_DECL_ID && (n)->id < END_DECL_ID)
#define isstmt(n)  ((n)->id > BEGIN_STMT_ID && (n)->id < END_STMT_ID)
#define isconcat(n) ((n)->id == CONCAT_NODE)
#define isfuncdecl(n) (NODE(n)->id == FUNC_DECL)
#define isfuncdef(n) (isfuncdecl(n) && NODE(n)->kids[0] && NODE(n)->kids[0]->id == COMPOUND_STMT)

#endif
