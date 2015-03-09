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

// array of nodes
struct anode {
    struct node node;
    struct node ** kids;
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

/**
 * Because vector_to_array, where 'array' is end of 'NULL'.
 * If the vector contains a NULL element, then the conversion
 * will loss all the elements after the NULL element.
 * So here we place a null-node which is actually not NULL, but
 * to indicate a null node, for example, in the for statement.
 */
extern struct node *nullnode;

// ast
extern const char * node_print_function(void *data);
extern const char *nname(struct node *node);
extern int is_iteration_stmt(struct stmt *stmt);
extern int is_switch_stmt(struct stmt *stmt);
extern struct anode * new_anode(struct node **kids);

// expr
extern struct expr * expr_node(int id, int op, struct expr *l, struct expr *r);
extern struct expr * expression();
extern struct expr * constant_expression();
extern struct expr * assign_expression();
extern int eval_constexpr(struct expr *expr, union value *value);

// decl
extern struct decl * decl_node(int id, int scope);
extern struct decl * initializer_list();
extern int kind(int t);
extern int istypename(struct token *t);
extern struct node ** declaration();
extern struct decl * translation_unit();
extern struct type * typename();

// stmt
extern struct stmt * stmt_node(int id, struct node *l, struct node *r);
extern struct stmt * compound_statement(struct stmt *context);

#define NODE(n)    ((struct node*) (n))
#define isexpr(n)  ((n)->id > BEGIN_EXPR_ID && (n)->id < END_EXPR_ID)
#define isdecl(n)  ((n)->id > BEGIN_DECL_ID && (n)->id < END_DECL_ID)
#define isstmt(n)  ((n)->id > BEGIN_STMT_ID && (n)->id < END_STMT_ID)
#define isanode(n)  ((n)->id == ARRAY_NODE)
#define isfuncdecl(n) (NODE(n)->id == FUNC_DECL)
#define isfuncdef(n) (isfuncdecl(n) && NODE(n)->kids[0] && NODE(n)->kids[0]->id == COMPOUND_STMT)
#define isliteral(n) ((n)->id > BEGIN_LITERAL_ID && (n)->id < END_LITERAL_ID)

#endif
