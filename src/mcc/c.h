#ifndef cdecl_c_h
#define cdecl_c_h

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <memory.h>
#include "lex.h"
#include "lib.h"
#include "config.h"

typedef struct type     *Type;
typedef struct symbol   *Symbol;
typedef struct table    *Table;

// node ids
enum {
#define _ns(a)  a,
#define _n(a, b)    a,
#include "node.h"
};

struct type {
    int         op;
    int         sclass;
    unsigned    qual_const : 1;
    unsigned    qual_volatile : 1;
    unsigned    qual_restrict : 1;
    unsigned    func_spec : 1;
    unsigned    reserved : 1;
    Type        type;
    int         size;
    union {
        struct {
            Symbol  *proto;
            unsigned oldstyle : 1;
        }f;
        Symbol sym;
    }u;
};

struct symbol {
    struct lex  lex;
    Type        type;
    int         scope;
    Symbol      up;
};

// scope level
enum {
    GLOBAL,
    PARAM,
    LOCAL,
};

// node
typedef struct node             *Node;
typedef struct decl             *Decl;
typedef struct tudecl           *TranslationUnitDecl;
typedef struct funcdecl         *FuncDecl;
typedef struct expr             *Expr;
typedef struct stmt             *Stmt;
typedef struct declstmt         *DeclStmt;
typedef struct compoundstmt     *CompoundStmt;
typedef struct ifstmt           *IfStmt;
typedef struct whilestmt        *WhileStmt;
typedef struct dowhilestmt      *DoWhileStmt;
typedef struct expr             *Expr;
typedef struct literalexpr      *LiteralExpr;
typedef struct addrexpr         *AddrExpr;

#define NODE(n)     ((Node) (n))
extern const char *nname(Node node);
extern void printnode(Node node);
extern Decl decl_node(int id, int scope);
extern FuncDecl funcdecl_node(int scope);
extern TranslationUnitDecl tudecl_node();
extern CompoundStmt compound_stmt_node();
extern int is_funcdef_node(Node node);
extern LiteralExpr literal_expr_node(int id);
extern AddrExpr addr_expr_node(const char *id);

// node
struct node {
    int id;
    Symbol s;
};

// decl
struct decl {
    struct node n;
    int scope;
};

struct tudecl {
    struct decl d;
    Node  *exts;
};

struct funcdecl {
    struct decl d;
    Node    *params;
    CompoundStmt    cs;
};

// stmt
struct stmt {
    struct node n;
};

struct declstmt {
    struct stmt s;
    Node    *decls;
};

struct ifstmt {
    struct stmt s;
    Expr	expr;
    Stmt	stmt1;
    Stmt	stmt2;
};

struct whilestmt {
    struct stmt s;
    Expr	expr;
    Stmt	stmt;
};

struct dowhilestmt {
    struct stmt s;
    Expr	expr;
    Stmt	stmt;
};

struct compoundstmt {
    struct stmt s;
    Node    *stmts;       // decl/stmt nodes
};

// expr
struct expr {
    struct node n;
    int op;
};

struct literalexpr {
    struct expr e;
    union value u;
};

struct addrexpr {
    struct expr e;
    const char *id;
};

// sym
extern void enterscope();
extern void exitscope();
extern Table table(Table tp, int scope);
extern Table   identifiers;
extern Symbol lookupsym(const char *name, Table tp);
extern Symbol installsym(const char *name, Table *tpp, int scope);

// decl
extern int scopelevel;
extern TranslationUnitDecl translation_unit();
extern Node * declaration();
extern unsigned char kind(int t);
extern void initializer_list();

//kind
enum {
    SCLASS_SPEC = 01, TYPE_QUAL = 02, TYPE_SPEC = 04,
    FUNC_SPEC = 010,
};

// type
extern void init_type();
extern Type pretype(int tok);
extern void printtype(Type type);
extern void prepend_type(Type *typelist, Type type);
extern void attach_type(Type *typelist, Type type);
extern Type scls(int t, Type ty);
extern Type qual(int t, Type ty);
extern Type unqual(int t, Type ty);
extern int equal_type(Type ty1, Type ty2);
extern int istypedefname(const char *id);
extern Type typename();
extern Type    chartype;               // char
extern Type    unsignedchartype;       // unsigned char
extern Type    signedchartype;         // signed char
extern Type    shorttype;              // short (int)
extern Type    unsignedshorttype;      // unsigned short (int)
extern Type    inttype;                // int
extern Type    unsignedinttype;        // unsigned (int)
extern Type    longtype;               // long
extern Type    unsignedlongtype;       // unsigned long (int)
extern Type    longlongtype;           // long long (int)
extern Type    unsignedlonglongtype;   // unsigned long long (int)
extern Type    floattype;              // float
extern Type    doubletype;             // double
extern Type    longdoubletype;         // long double
extern Type    voidtype;               // void

#define isfunction(type)    ((type) && (type)->op == FUNCTION)
#define isarray(type)       ((type) && (type)->op == ARRAY)
#define ispointer(type)     ((type) && (type)->op == POINTER)
#define isconst(type)       ((type) && (type)->qual_const)
#define isvolatile(type)    ((type) && (type)->qual_volatile)
#define isrestrict(type)    ((type) && (type)->qual_restrict)
#define isqual(type)        (isconst(type) || isvolatile(type) || isrestrict(type))
#define isinline(type)      ((type) && (type)->func_spec)
#define isvoid(type)        ((type) && (type)->op == VOID)

extern void begin_call(const char *funcname);
extern void end_call(const char *funcname);

#ifdef SHOW_CALL_TREE
#define BEGIN_CALL(funcname)    begin_call(#funcname)
#define END_CALL(funcname)      end_call(#funcname)
#else
#define BEGIN_CALL(funcname)
#define END_CALL(funcname)
#endif

// stmt
extern CompoundStmt compound_stmt();

// expr
extern Expr expr();
extern Expr assignment_expr();

#endif
