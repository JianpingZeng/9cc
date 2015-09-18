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
    long long i;
    unsigned long long u;
    double d;
    long double ld;
    void *p;
    void (*g) ();
};

#define is_assign_op(op)    ((op == '=') || (op >= MULEQ && op <= RSHIFTEQ))
#define isanonymous(name)   (is_digit((name)[0]))

// ast.h
#include "ast.h"

// eval.c
extern union node * eval(union node *expr);

// expr.c
extern union node * expression();
extern union node * assign_expr();
extern int intexpr();
extern bool islvalue(union node *node);

// decl.c
extern union node * initializer_list(union node *ty);
extern bool istypename(struct token *t);
extern union node ** declaration();
extern union node * translation_unit();
extern union node * typename();
extern int firstdecl(struct token *t);
extern int firststmt(struct token *t);
extern int firstexpr(struct token *t);

// stmt.c
extern union node * compound_stmt();

#define isbitfield(field)    (FIELD_BITSIZE(field) > 0)

extern void type_init();
extern int op(union node *type);
extern void prepend_type(union node **typelist, union node *type);
extern void attach_type(union node **typelist, union node *type);
extern union node * qual(int t, union node *ty);
extern bool eqtype(union node *ty1, union node *ty2);
extern bool eqarith(union node *ty1, union node * ty2);
extern union node * lookup_typedef_name(const char *id);
extern bool is_typedef_name(const char *id);
extern union node * new_field(char *id);
extern union node * array_type();
extern union node * ptr_type(union node *ty);
extern union node * func_type();
extern struct symbol * tag_type(int t, const char *tag, struct source src);
extern const char *type2s(union node *ty);
extern unsigned typesize(union node *ty);
extern union node * find_field(union node *ty, const char *name);
extern int indexof_field(union node *ty, union node *field);
extern union node * compose(union node *ty1, union node *ty2);
extern bool contains(int qual1, int qual2);

extern union node    *chartype;               // char
extern union node    *unsignedchartype;       // unsigned char
extern union node    *signedchartype;         // signed char
extern union node    *wchartype;              // wchar_t
extern union node    *shorttype;              // short (int)
extern union node    *unsignedshorttype;      // unsigned short (int)
extern union node    *inttype;                // int
extern union node    *unsignedinttype;        // unsigned (int)
extern union node    *longtype;               // long
extern union node    *unsignedlongtype;       // unsigned long (int)
extern union node    *longlongtype;           // long long (int)
extern union node    *unsignedlonglongtype;   // unsigned long long (int)
extern union node    *floattype;              // float
extern union node    *doubletype;             // double
extern union node    *longdoubletype;         // long double
extern union node    *voidtype;               // void
extern union node    *booltype;	       // bool
extern union node    *vartype;		       // variable type

#define BITS(type)      (CHAR_BIT * (TYPE_SIZE(type)))

extern bool isconst(union node *ty);
extern bool isvolatile(union node *ty);
extern bool isrestrict(union node *ty);
#define isinline(ty)    (TYPE_INLINE(ty))
#define isqual(ty)      (isconst(ty) || isvolatile(ty) || isrestrict(ty))
#define unqual(ty)      (isqual(ty) ? (TYPE_TYPE(ty)) : (ty))

#define kind(ty)        (TYPE_KIND(unqual(ty)))
#define rtype(ty)       (TYPE_TYPE(unqual(ty)))
#define size(ty)        (TYPE_SIZE(unqual(ty)))
#define rank(ty)        (TYPE_RANK(unqual(ty)))

extern bool isfunc(union node *type);
extern bool isarray(union node *type);
extern bool isptr(union node *type);
extern bool isvoid(union node *type);
extern bool isenum(union node *type);
extern bool isstruct(union node *type);
extern bool isunion(union node *type);
extern bool isrecord(union node *type); // isstruct or isunion
extern bool istag(union node *type);	 // isstruct or isunion or isenum

extern bool isint(union node *ty);
extern bool isfloat(union node *ty);
extern bool isarith(union node *ty);
extern bool isscalar(union node *ty);

extern bool isptrto(union node *ty, int kind);

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
    union node *type;
    bool defined;
    struct source src;
    union value value;
    unsigned refs;
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
extern struct symbol * anonymous(struct table **tpp, int scope);

// look up a symbol from this table to previous one, and so on
extern struct symbol * lookup(const char *name, struct table *table);

// install a symbol with specified scope
extern struct symbol * install(const char *name, struct table **tpp, int scope);

// sym->name is NULL or anynomous
extern bool issymnamed(struct symbol *sym);

extern struct table * identifiers;
extern struct table * constants;
extern struct table * tags;

#define SCOPE  scopelevel()

#define currentscope(sym)   (sym->scope == SCOPE || (sym->scope == PARAM && SCOPE == LOCAL))

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
extern void conflicting_types_error(struct source src, struct symbol *sym);

// gen.c
void walk(union node *tree);

// print.c
extern void print_tree(union node *tree);


#endif
