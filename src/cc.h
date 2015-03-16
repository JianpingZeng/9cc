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
#include <locale.h>
#include <float.h>
#include <wchar.h>

#include "lib.h"

union value {
    long long i;
    unsigned long long u;
    double d;
    long double ld;
    void *p;
    void (*g) ();
};

// lex.c
#define EOI  -1

enum {
#define _a(a, b, c, d)  a,
#define _x(a, b, c, d)  a=d,
#define _t(a, b, c)     a,
#include "token.h"
    TOKEND
};

struct source {
    const char *file;
    unsigned line;
};

struct token {
    int id;
    const char *name;
    struct {
	union value u;
	struct type *type;
    }v;
};

extern struct source source;
extern struct token  *token;

extern void lexer_init();
extern int  gettok();
extern struct token *  lookahead();
extern void match(int t);
extern void skipto(int t);
extern const char *tname(int t);

// ast.c
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
    union {
	// arguments
	struct {
	    struct expr **args;
	};
    };
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
    };
};

// decl
struct decl {
    struct node node;
    int scope;
    union {
	struct {
	    struct node **exts;
	};
    };
};

// ast.c
extern void free_cc();
extern void * alloc_type_node();
extern void * alloc_symbol_node();

extern const char *nname(struct node *node);
extern struct expr * expr_node(int id, int op, struct expr *l, struct expr *r);
extern struct decl * decl_node(int id, int scope);
extern struct stmt * stmt_node(int id, struct node *l, struct node *r);

// expr.c
extern struct expr * expression();
extern struct expr * constant_expression();
extern struct expr * assign_expression();
extern int intexpr();

// decl.c
extern struct decl * initializer_list();
extern int kind(int t);
extern int istypename(struct token *t);
extern struct node ** declaration();
extern struct decl * translation_unit();
extern struct type * typename();

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

};

// type.c
struct type {
    int op;
    const char *name;
    int size;
    unsigned qual_const : 1;
    unsigned qual_volatile : 1;
    unsigned qual_restrict : 1;
    unsigned func_inline : 1;
    unsigned reserved : 1;
    struct type *type;
    union {
	// function
	struct {
	    struct symbol **params;
	    unsigned oldstyle : 1;
	}f;
	// array
	struct {
	    struct expr *assign;
	    unsigned qual_const : 1;
	    unsigned qual_volatile : 1;
	    unsigned qual_restrict : 1;
	    unsigned sclass_static : 1;
	    unsigned wildcard : 1;
	}a;
	// enum/struct/union
	struct {
	    struct symbol *symbol;
	    union {
		struct symbol **ids;
		struct field **fields;
	    };
	}s;
    };
    struct {
	union value max;
	union value min;
    }limits;
};

extern const char * pname(struct type *type);
extern void type_init();
extern struct type * new_type();
extern void prepend_type(struct type **typelist, struct type *type);
extern void attach_type(struct type **typelist, struct type *type);
extern struct type * qual(int t, struct type *ty);
extern struct type * unqual(int t, struct type *ty);
extern int eqtype(struct type *ty1, struct type *ty2);
extern struct type * lookup_typedef_name(const char *id);
extern int is_typedef_name(const char *id);
extern struct type * array_type();
extern struct type * pointer_type(struct type *ty);
extern struct type * function_type();
extern struct type * tag_type(int op, const char *tag, struct source src);

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

#define isfunction(type)    ((type) && (type)->op == FUNCTION)
#define isarray(type)       ((type) && (type)->op == ARRAY)
#define ispointer(type)     ((type) && (type)->op == POINTER)
#define isconst(type)       ((type) && (type)->qual_const)
#define isvolatile(type)    ((type) && (type)->qual_volatile)
#define isrestrict(type)    ((type) && (type)->qual_restrict)
#define isqual(type)        (isconst(type) || isvolatile(type) || isrestrict(type))
#define isinline(type)      ((type) && (type)->func_inline)
#define isvoid(type)        ((type) && (type)->op == VOID)
#define isenum(type)        ((type) && (type)->op == ENUM)
#define isstruct(type)      ((type) && (type)->op == STRUCT)
#define isunion(type)       ((type) && (type)->op == UNION)

// sym
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

// sym
extern void symbol_init();
extern int scopelevel();
extern void enter_scope();
extern void exit_scope();
extern struct table * new_table(struct table *up, int scope);

// create an anonymous symbol
extern struct symbol * anonymous_symbol(struct table **tpp, int scope);

// basic function
struct symbol * find_symbol(const char *name, struct table *table, int scope);

// look up a symbol only in current table
extern struct symbol * locate_symbol(const char *name , struct table *table, int scope);

// look up a symbol from this table to previous one, and so on
extern struct symbol * lookup_symbol(const char *name, struct table *table);

// install a symbol with specified scope
extern struct symbol * install_symbol(const char *name, struct table **tpp, int scope);

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
extern void cclog(const char *fmt, ...);

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

//kind
enum {
    SCLASS_SPEC = 01,
    TYPE_QUAL = 02,
    TYPE_SPEC = 04,
    FUNC_SPEC = 010,

    FIRST_EXPR = 020,		
    FIRST_STMT = 040,
    FIRST_DECL = 0100,

    FIRST_ASSIGN_EXPR = FIRST_EXPR, // equals to FIRST_EXPR
};

// gen.c
void walk(struct node *tree);

// debug.c
extern void print_tree(struct node *tree);
extern void print_type(struct type *type);

#endif
