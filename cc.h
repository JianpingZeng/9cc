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

#include "config.h"
#include "utils.h"

#include "strbuf.h"
#include "vector.h"
#include "map.h"

// alloc.c
extern void * alloc_node(void);
extern void * alloc_symbol(void);
extern void * alloc_type(void);
extern void * alloc_field(void);

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
extern bool is_letter(char c);
extern bool is_digitletter(char c);
extern bool is_blank(char c);
extern bool is_newline(char c);
extern bool is_hex(char c);
extern bool is_digithex(char c);
extern bool is_visible(char c);

extern void input_init();
extern int  gettok();
extern struct token *  lookahead();
extern void expect(int t);
extern void match(int t, int follow[]);
extern const char *tname(int t);

// value

union value {
    unsigned long long u;
    long double d;
    void *p;
    void (*g) ();
};

#define is_assign_op(op)    ((op == '=') || (op >= MULEQ && op <= RSHIFTEQ))
#define isanonymous(name)   ((name) == NULL || !is_letter((name)[0]))

// ast.h
#include "ast.h"

// eval.c
extern node_t * eval(node_t *expr, node_t *ty);

// expr.c
extern node_t * expression();
extern node_t * assign_expr();
extern int intexpr();
extern bool islvalue(node_t *node);
extern node_t * init_elem_conv(node_t *ty, node_t *node);

// decl.c
extern node_t * initializer_list(node_t *ty);
extern bool istypename(struct token *t);
extern node_t ** declaration();
extern node_t * translation_unit();
extern node_t * typename();
extern int firstdecl(struct token *t);
extern int firststmt(struct token *t);
extern int firstexpr(struct token *t);
extern bool has_static_extent(node_t *sym);

// stmt.c
extern node_t * compound_stmt();

// type.c
extern void type_init();
extern int op(node_t *type);
extern void prepend_type(node_t **typelist, node_t *type);
extern void attach_type(node_t **typelist, node_t *type);
extern node_t * qual(int t, node_t *ty);
extern bool eqtype(node_t *ty1, node_t *ty2);
extern bool eqarith(node_t *ty1, node_t * ty2);
extern node_t * lookup_typedef_name(const char *id);
extern bool is_typedef_name(const char *id);
extern node_t * new_field(char *id);
extern node_t * array_type();
extern node_t * ptr_type(node_t *ty);
extern node_t * func_type();
extern node_t * tag_type(int t, const char *tag, struct source src);
extern const char *type2s(node_t *ty);
extern unsigned typesize(node_t *ty);
extern node_t * find_field(node_t *ty, const char *name);
extern int indexof_field(node_t *ty, node_t *field);
extern node_t * compose(node_t *ty1, node_t *ty2);
extern bool qual_contains(node_t *ty1, node_t *ty2);
extern bool isincomplete(node_t *ty);

extern node_t    *chartype;               // char
extern node_t    *unsignedchartype;       // unsigned char
extern node_t    *signedchartype;         // signed char
extern node_t    *wchartype;              // wchar_t
extern node_t    *shorttype;              // short (int)
extern node_t    *unsignedshorttype;      // unsigned short (int)
extern node_t    *inttype;                // int
extern node_t    *unsignedinttype;        // unsigned (int)
extern node_t    *longtype;               // long
extern node_t    *unsignedlongtype;       // unsigned long (int)
extern node_t    *longlongtype;           // long long (int)
extern node_t    *unsignedlonglongtype;   // unsigned long long (int)
extern node_t    *floattype;              // float
extern node_t    *doubletype;             // double
extern node_t    *longdoubletype;         // long double
extern node_t    *voidtype;               // void
extern node_t    *booltype;	       // bool
extern node_t    *vartype;		       // variable type

#define BITS(type)      (CHAR_BIT * (TYPE_SIZE(type)))

extern bool isconst(node_t *ty);
extern bool isvolatile(node_t *ty);
extern bool isrestrict(node_t *ty);
#define isinline(ty)    (TYPE_INLINE(ty))
#define isqual(ty)      (isconst(ty) || isvolatile(ty) || isrestrict(ty))
#define unqual(ty)      (isqual(ty) ? (TYPE_TYPE(ty)) : (ty))

#define kind(ty)        (TYPE_KIND(unqual(ty)))
#define rtype(ty)       (TYPE_TYPE(unqual(ty)))
#define size(ty)        (TYPE_SIZE(unqual(ty)))
#define rank(ty)        (TYPE_RANK(unqual(ty)))

extern bool isfunc(node_t *type);
extern bool isarray(node_t *type);
extern bool isptr(node_t *type);
extern bool isvoid(node_t *type);
extern bool isenum(node_t *type);
extern bool isstruct(node_t *type);
extern bool isunion(node_t *type);
extern bool isrecord(node_t *type); // isstruct or isunion
extern bool istag(node_t *type);	 // isstruct or isunion or isenum

extern bool isint(node_t *ty);
extern bool isfloat(node_t *ty);
extern bool isarith(node_t *ty);
extern bool isscalar(node_t *ty);

extern bool isptrto(node_t *ty, int kind);

// sym.c
// scope level
enum {
    CONSTANT,
    GLOBAL,
    PARAM,
    LOCAL,
};

struct table {
    int scope;
    struct table *up;
    struct map *map;
};

// sym
extern int scopelevel();
extern void enter_scope();
extern void exit_scope();

// create an anonymous symbol
extern node_t * anonymous(struct table **tpp, int scope);

// look up a symbol from this table to previous one, and so on
extern node_t * lookup(const char *name, struct table *table);

// install a symbol with specified scope
extern node_t * install(const char *name, struct table **tpp, int scope);

extern struct table * identifiers;
extern struct table * constants;
extern struct table * tags;

#define SCOPE  scopelevel()

#define currentscope(sym)   (SYM_SCOPE(sym) == SCOPE || (SYM_SCOPE(sym) == PARAM && SCOPE == LOCAL))

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

extern void redefinition_error(struct source src, node_t *sym);

// gen.c
extern void gen(node_t *tree);

// print.c
extern void print_tree(node_t *tree);

// cc.c
struct cc_config {
    int pack;			// pack bytes
};
extern struct cc_config cc_config;

#endif
