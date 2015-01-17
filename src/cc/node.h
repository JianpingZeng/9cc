#ifndef cc_node_h
#define cc_node_h

// node ids
enum {
#define _ns(a)   a,
#define _n(a, b) a,
#include "nodeid.h"
};

// node
typedef struct {
    int id;
    Symbol *s;
} Node;

// expr
typedef struct {
    Node n;
    int op;
} Expr;

typedef struct {
    Expr e;
    union value u;
} LiteralExpr;

typedef struct {
    Expr e;
    const char *id;
} AddrExpr;

// stmt
typedef struct {
    Node n;
} Stmt;

typedef struct {
    Stmt s;
    Node *decls;
} DeclStmt;

typedef struct {
    Stmt s;
    Expr *expr;
    Stmt *stmt1;
    Stmt *stmt2;
} IfStmt;

typedef struct {
    Stmt s;
    Expr *expr;
    Stmt *stmt;
} WhileStmt;

typedef struct {
    Stmt s;
    Expr *expr;
    Stmt *stmt;
} DoWhileStmt;

typedef struct {
    Stmt s;
    Node **stmts;       // decl/stmt nodes
} CompoundStmt;;

// decl
typedef struct {
    Node n;
    int scope;
} Decl;

typedef struct {
    Decl d;
    Node  **exts;
} TranslationUnitDecl;

typedef struct {
    Decl d;
    Node **params;
    CompoundStmt *cs;
} FuncDecl;

#define NODE(n)     ((Node*) (n))
extern const char * node_print_function(void *data);
extern const char *nname(Node *node);
extern void printnode(Node *node);
extern Decl *decl_node(int id, int scope);
extern FuncDecl *funcdecl_node(int scope);
extern TranslationUnitDecl *tudecl_node();
extern CompoundStmt *compound_stmt_node();
extern int is_funcdef_node(Node *node);
extern LiteralExpr *literal_expr_node(int id);
extern AddrExpr *addr_expr_node(const char *id);


#endif
