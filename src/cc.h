#ifndef _CC_H
#define _CC_H

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <memory.h>
#include <stdarg.h>
#include <errno.h>
#include <limits.h>
#include <float.h>
#include <wchar.h>
#include <stdbool.h>

#include "utils.h"

// lex.c
#define EOI  -1

enum {
#define _a(a, b, c, d)  a,
#define _x(a, b, c, d)  a=d,
#define _t(a, b, c)     a,
#include "token.def"
    TOKEND
};

struct source {
    const char *file;
    unsigned line;
    unsigned column;
};

struct token {
    int id;
    const char *name;
    int kind;
};

extern struct source source;
extern struct token  *token;

extern bool is_digit(char c);
extern bool is_hex(char c);
extern bool is_digithex(char c);

extern void lexer_init();
extern int  gettok();
extern struct token *  lookahead();
extern void expect(int t);
extern void match(int t, int follow[]);
extern const char *tname(int t);

// value

union value {
    long long i;
    unsigned long long u;
    double d;
    long double ld;
    void *p;
    void (*g) ();
};

#define is_assign_op(op)    ((op == '=') || (op >= MULEQ && op <= RSHIFTEQ))

// ast.h
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
        }d;
    }u;
};

// ast.c
extern struct field * new_field(char *id);

extern const char *nname(struct node *node);
extern struct node * expr_node(int id, int op, struct node *l, struct node *r);
extern struct node * decl_node(int id, int scope);
extern struct node * stmt_node(int id, struct node *l, struct node *r);
extern struct node * unode(int op, struct type *ty, struct node *l);
extern struct node * bnode(int op, struct node *l, struct node *r);

// expr.c
extern struct node * expression();
extern struct node * assign_expr();
extern int intexpr();

// decl.c
extern struct node * initializer_list();
extern bool istypename(struct token *t);
extern struct node ** declaration();
extern struct node * translation_unit();
extern struct type * typename();
extern int firstdecl(struct token *t);
extern int firststmt(struct token *t);
extern int firstexpr(struct token *t);

// stmt.c
extern struct node * compound_stmt(struct node *context);

#define LEFT(n)    (n->kids[0])
#define RIGHT(n)    (n->kids[1])

#define isexpr(n)  (n->id > BEGIN_EXPR_ID && n->id < END_EXPR_ID)
#define isdecl(n)  (n->id > BEGIN_DECL_ID && n->id < END_DECL_ID)
#define isstmt(n)  (n->id > BEGIN_STMT_ID && n->id < END_STMT_ID)
#define isfuncdecl(n) (n->id == FUNC_DECL)
#define isfuncdef(n) (isfuncdecl(n) && KID0(n) && KID0(n)->id == COMPOUND_STMT)
#define isliteral(n) (n->id > BEGIN_LITERAL_ID && n->id < END_LITERAL_ID)
#define is_switch_stmt(n) (n && n->id == SWITCH_STMT)
#define is_iteration_stmt(n) (n && (n->id == FOR_STMT || n->id == WHILE_STMT || n->id == DO_WHILE_STMT))

struct field {
    const char *name;
    struct type *type;
    int offset;
    int bitsize;
};

// type.c
struct type {
    int op;
    const char *name;
    size_t size;
    struct {
        unsigned is_const : 1;
        unsigned is_volatile : 1;
        unsigned is_restrict : 1;
        unsigned is_inline : 1;
    }q;
    unsigned reserved : 1;
    struct type *type;
    const char *tag;
    union {
        // function
        struct {
            struct symbol **params;
            unsigned oldstyle : 1;
        }f;
        // array
        struct {
            struct node *assign;
            unsigned is_const : 1;
            unsigned is_volatile : 1;
            unsigned is_restrict : 1;
            unsigned is_static : 1;
            unsigned wildcard : 1;
        }a;
        // enum/struct/union
        struct {
            struct symbol **ids;
            struct field **fields;
        }s;
    }u;
    struct {
        union value max;
        union value min;
    }limits;
};

extern void type_init();
extern void prepend_type(struct type **typelist, struct type *type);
extern void attach_type(struct type **typelist, struct type *type);
extern struct type * qual(int t, struct type *ty);
extern int eqtype(struct type *ty1, struct type *ty2);
extern struct type * lookup_typedef_name(const char *id);
extern bool is_typedef_name(const char *id);
extern struct type * array_type();
extern struct type * pointer_type(struct type *ty);
extern struct type * function_type();
extern struct symbol * tag_type(int op, const char *tag, struct source src);
extern struct symbol * tag_sym(struct type *ty);

extern struct type    *chartype;               // char
extern struct type    *unsignedchartype;       // unsigned char
extern struct type    *signedchartype;         // signed char
extern struct type    *wchartype;              // wchar_t
extern struct type    *shorttype;              // short (int)
extern struct type    *unsignedshorttype;      // unsigned short (int)
extern struct type    *inttype;                // int
extern struct type    *unsignedinttype;        // unsigned (int)
extern struct type    *longtype;               // long
extern struct type    *unsignedlongtype;       // unsigned long (int)
extern struct type    *longlongtype;           // long long (int)
extern struct type    *unsignedlonglongtype;   // unsigned long long (int)
extern struct type    *floattype;              // float
extern struct type    *doubletype;             // double
extern struct type    *longdoubletype;         // long double
extern struct type    *voidtype;               // void
extern struct type    *booltype;	       // bool
extern struct type    *vartype;		       // variable type

#define isfunc(ty)      ((ty)->op == FUNCTION)
#define isarray(ty)     ((ty)->op == ARRAY)
#define isptr(ty)       ((ty)->op == POINTER)
#define isconst(ty)     ((ty)->q.is_const)
#define isvolatile(ty)  ((ty)->q.is_volatile)
#define isrestrict(ty)  ((ty)->q.is_restrict)
#define isinline(ty)    ((ty)->q.is_inline)
#define isqual(ty)      (isconst(ty) || isvolatile(ty) || isrestrict(ty))
#define isvoid(ty)      ((ty)->op == VOID)
#define isenum(ty)      ((ty)->op == ENUM)
#define isstruct(ty)    ((ty)->op == STRUCT)
#define isunion(ty)     ((ty)->op == UNION)
#define unqual(ty)      (isqual(ty) ? (ty)->type : (ty))

extern bool isint(struct type *ty);
extern bool isfloat(struct type *ty);
extern bool isarith(struct type *ty);
extern bool isscalar(struct type *ty);

// sym.c
// scope level
enum {
    CONSTANT,
    GLOBAL,
    PARAM,
    LOCAL,
};

struct symbol {
    int scope;
    const char *name;
    int sclass;
    struct type  *type;
    unsigned defined : 1;
    struct source src;
    union value value;
    unsigned refs;
    struct symbol *up;
};

struct table;

// sym
extern void symbol_init();
extern int scopelevel();
extern void enter_scope();
extern void exit_scope();

// create an anonymous symbol
extern struct symbol * anonymous(struct table **tpp, int scope);

// look up a symbol from this table to previous one, and so on
extern struct symbol * lookup(const char *name, struct table *table);

// install a symbol with specified scope
extern struct symbol * install(const char *name, struct table **tpp, int scope);

extern struct table * identifiers;
extern struct table * constants;
extern struct table * tags;

#define SCOPE  scopelevel()

// error.c
extern unsigned errors;
extern unsigned warnings;
extern void warning(const char *fmt, ...);
extern void error(const char *fmt, ...);
extern void fatal(const char *fmt, ...);
extern void warningf(struct source src, const char *fmt, ...);
extern void errorf(struct source src, const char *fmt, ...);

extern void begin_call(const char *funcname);
extern void end_call(const char *funcname);

extern void redefinition_error(struct source src, struct symbol *sym);

#define SHOW_CALL_TREE
#ifdef SHOW_CALL_TREE
#define BEGIN_CALL    begin_call(__func__);
#define END_CALL      end_call(__func__);
#else
#define BEGIN_CALL
#define END_CALL
#endif

//#define SHOW_COLOR_TERM

// gen.c
void walk(struct node *tree);

// print.c
extern void print_tree(struct node *tree);
extern void print_type(struct type *type);

#endif
