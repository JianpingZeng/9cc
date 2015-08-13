#ifndef _ast_h
#define _ast_h

// node ids
enum {
#define _ns(a)   a,
#define _n(a, b) a,
#include "node.def"
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
    int prefix : 1;
    union {
        // call
        struct expr **args;
        // init list
        struct expr **inits;
        // cond
        struct {
            struct expr *o;
            struct expr *e;
            struct expr *c;
        }cond;
        // member
        const char *field;
    }u;
};

// stmt
struct stmt {
    struct node node;
    struct stmt *up;		// internal
    union {
        struct {
            struct node **decl;
            struct expr *init;
            struct expr *cond;
            struct expr *ctrl;
        }forstmt;
        struct {
            struct node **blks;	// block items
        }compoundstmt;
    }u;
};

// decl
struct decl {
    struct node node;
    int scope;
    // translation unit
    struct node **exts;
};

// ast.c
extern struct type * new_type();
extern struct symbol * new_symbol();
extern struct field * new_field(char *id);

extern const char *nname(struct node *node);
extern struct expr * expr_node(int id, int op, struct expr *l, struct expr *r);
extern struct decl * decl_node(int id, int scope);
extern struct stmt * stmt_node(int id, struct node *l, struct node *r);

// expr.c
extern struct expr * expression();
extern struct expr * constant_expr();
extern struct expr * assign_expr();
extern int intexpr();

// decl.c
extern struct expr * initializer_list();
extern int istypename(struct token *t);
extern struct node ** declaration();
extern struct decl * translation_unit();
extern struct type * typename();
extern int firstdecl(struct token *t);
extern int firststmt(struct token *t);
extern int firstexpr(struct token *t);

// stmt.c
extern struct stmt * compound_statement(struct stmt *context);

#define NODE(n)    ((struct node*) (n))
#define isexpr(n)  ((n)->id > BEGIN_EXPR_ID && (n)->id < END_EXPR_ID)
#define isdecl(n)  ((n)->id > BEGIN_DECL_ID && (n)->id < END_DECL_ID)
#define isstmt(n)  ((n)->id > BEGIN_STMT_ID && (n)->id < END_STMT_ID)
#define isanode(n)  ((n)->id == ARRAY_NODE)
#define isfuncdecl(n) (NODE(n)->id == FUNC_DECL)
#define isfuncdef(n) (isfuncdecl(n) && NODE(n)->kids[0] && NODE(n)->kids[0]->id == COMPOUND_STMT)
#define isliteral(n) ((n)->id > BEGIN_LITERAL_ID && (n)->id < END_LITERAL_ID)
#define is_switch_stmt(n) ((n) && NODE(n)->id == SWITCH_STMT)
#define is_iteration_stmt(n) ((n) && (NODE(n)->id == FOR_STMT || NODE(n)->id == WHILE_STMT || NODE(n)->id == DO_WHILE_STMT))

struct field {
    const char *name;
    struct type *type;
    int offset;
    int bitsize;
};

#endif
