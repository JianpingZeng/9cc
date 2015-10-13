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
#include <time.h>
#include <locale.h>

#include "config.h"
#include "utils.h"

// alloc.c
extern void * alloc_node(void);
extern void * alloc_symbol(void);
extern void * alloc_type(void);
extern void * alloc_field(void);
extern void * alloc_token(void);

// value
#define VALUE_U(v)    ((v).u)
#define VALUE_I(v)    ((v).u)
#define VALUE_D(v)    ((v).d)
#define VALUE_P(v)    ((v).p)
#define VALUE_G(v)    ((v).g)

union value {
    unsigned long long u;
    long double d;
    void *p;
    void (*g) ();
};

// source
struct source {
    const char *file;
    unsigned line;
    unsigned column;
};

// input.c
#define CH(c)    ((c)->ch)

struct cc_char {
    int ch;
    unsigned line;
    unsigned column;
};

enum {
    FILE_KIND_REGULAR = 1,
    FILE_KIND_STRING,
};

#define LBUFSIZE     64
#define RBUFSIZE     4096
#define MAXTOKEN     LBUFSIZE

struct file {
    int kind;
    char buf[LBUFSIZE+RBUFSIZE+1];
    char *pc;
    char *pe;
    long bread;
    struct vector *chars;
    FILE *fp;			// FILE handle
    size_t pos;			// input string position
    const char *file;		// file name or input string
    const char *name;		// buffer name
    unsigned line;
    unsigned column;
    bool bol;			// beginning of line
    struct vector *ifstubs;
};

struct ifstub {
    int id;
    struct source src;
    bool b;
};

extern void input_init(const char *file);
extern struct file * current_file(void);
extern struct cc_char * readc(void);
extern void unreadc(struct cc_char * ch);
extern struct file * with_temp_string(const char *input, const char *name);
extern struct file * with_temp_file(const char *file, const char *name);
extern void file_stub(struct file *f);
extern void file_unstub(void);
extern struct ifstub * new_ifstub(struct ifstub *i);
extern void if_stub(struct ifstub *i);
extern void if_unstub(void);
extern struct ifstub * current_ifstub(void);

// lex.c
#define EOI  -1

enum {
#define _a(a, b, c, d)  a,
#define _x(a, b, c, d)  a=d,
#define _t(a, b, c)     a,
#include "token.def"
    TOKEND
};

struct token {
    int id;
    const char *name;
    int kind;
    struct source src;
    struct set *hideset;
    unsigned bol : 1;		// beginning of line
};

extern struct source source;
extern struct token *token;
extern struct token *eoi_token;
extern struct token *newline_token;
extern struct token *space_token;

extern bool is_digit(char c);
extern bool is_letter(char c);
extern bool is_digitletter(char c);
extern bool is_blank(char c);
extern bool is_newline(char c);
extern bool is_hex(char c);
extern bool is_digithex(char c);
extern bool is_visible(char c);

#define IS_SPACE(t)    (((struct token *)(t))->id == ' ')
#define IS_NEWLINE(t)  (((struct token *)(t))->id == '\n')
#define IS_LINENO(t)   (((struct token *)(t))->id == LINENO)

extern void lex_init(void);
extern void skipline(void);
extern struct token * lex(void);
extern void unget(struct token *t);
extern void ungetv(struct vector *v);
extern void buffer_stub(struct vector *v);
extern void buffer_unstub(void);
extern struct token *header_name(void);
extern struct token * new_token(struct token *tok);
extern struct token * with_temp_lex(const char *input);
extern void skip_ifstub(void);
extern void genlineno(unsigned line, const char *file);

extern int gettok(void);
extern struct token * lookahead(void);
extern void expect(int t);
extern void match(int t, int follow[]);
extern const char *id2s(int t);
extern const char *unwrap_scon(const char *name);

// cpp.c
extern void cpp_init(struct vector *options);
extern struct token * get_pptok(void);
extern struct vector * all_pptoks(void);

// ast.h
#include "ast.h"

// eval.c
extern node_t * eval(node_t *expr, node_t *ty);
extern node_t * eval_bool(node_t *expr);

// expr.c
#define is_assign_op(op)    ((op == '=') || (op >= MULEQ && op <= RSHIFTEQ))
extern node_t * expression(void);
extern node_t * assign_expr(void);
extern int intexpr(void);
extern bool islvalue(node_t *node);
extern node_t * init_conv(node_t *ty, node_t *node);
extern node_t * ret_conv(node_t *ty, node_t *node);
// for expression in conditional statement
extern node_t * bool_expr(void);
// for expression in switch statement
extern node_t * switch_expr(void);

// decl.c
extern node_t * initializer_list(node_t *ty);
extern bool istypename(struct token *t);
extern node_t ** declaration(void);
extern node_t * translation_unit(void);
extern node_t * typename(void);
extern int firstdecl(struct token *t);
extern int firststmt(struct token *t);
extern int firstexpr(struct token *t);
extern bool has_static_extent(node_t *sym);

// stmt.c
extern node_t * compound_stmt(void);
extern void backfill_labels(void);

extern struct vector *gotos;
extern struct map *labels;
extern node_t *current_ftype;
extern const char *current_fname;

// type.c
extern void type_init(void);
extern int type_op(node_t *type);
extern void prepend_type(node_t **typelist, node_t *type);
extern void attach_type(node_t **typelist, node_t *type);
extern node_t * qual(int t, node_t *ty);
extern node_t * unqual(node_t *ty);
extern bool eqtype(node_t *ty1, node_t *ty2);
extern bool eqarith(node_t *ty1, node_t * ty2);
extern node_t * lookup_typedef(const char *id);
extern bool istypedef(const char *id);
extern node_t * new_field(char *id);
extern node_t * array_type(node_t *ty);
extern node_t * ptr_type(node_t *ty);
extern node_t * func_type(void);
extern node_t * tag_type(int t, const char *tag, struct source src);
extern const char *type2s(node_t *ty);
extern void set_typesize(node_t *ty);
extern node_t * find_field(node_t *ty, const char *name);
extern int indexof_field(node_t *ty, node_t *field);
extern node_t * compose(node_t *ty1, node_t *ty2);
extern bool qual_contains(node_t *ty1, node_t *ty2);
extern bool isincomplete(node_t *ty);
extern bool isvarray(node_t *ty);
extern node_t * unpack(node_t *ty);

extern node_t  *chartype;               // char
extern node_t  *unsignedchartype;       // unsigned char
extern node_t  *signedchartype;         // signed char
extern node_t  *wchartype;              // wchar_t
extern node_t  *shorttype;              // short (int)
extern node_t  *unsignedshorttype;      // unsigned short (int)
extern node_t  *inttype;                // int
extern node_t  *unsignedinttype;        // unsigned (int)
extern node_t  *longtype;               // long
extern node_t  *unsignedlongtype;       // unsigned long (int)
extern node_t  *longlongtype;           // long long (int)
extern node_t  *unsignedlonglongtype;   // unsigned long long (int)
extern node_t  *floattype;              // float
extern node_t  *doubletype;             // double
extern node_t  *longdoubletype;         // long double
extern node_t  *voidtype;               // void
extern node_t  *booltype;	        // bool
extern node_t  *vartype;		// variable type

#define BITS(bytes)     (CHAR_BIT * (bytes))
#define BYTES(bits)     ((ROUNDUP(bits, CHAR_BIT)) / (CHAR_BIT))

extern bool isconst(node_t *ty);
extern bool isvolatile(node_t *ty);
extern bool isrestrict(node_t *ty);

#define isinline(ty)    (_TYPE_INLINE(ty))
#define isqual(ty)      (isconst(ty) || isvolatile(ty) || isrestrict(ty))

// operations on the unqual type
#define TYPE_KIND(ty)            _TYPE_KIND(unqual(ty))
#define TYPE_NAME(ty)            _TYPE_NAME(unqual(ty))
#define TYPE_SIZE(ty)            _TYPE_SIZE(unpack(unqual(ty)))
#define TYPE_ALIGN(ty)           _TYPE_ALIGN(unpack(unqual(ty)))
#define TYPE_LEN(ty)             _TYPE_LEN(unqual(ty))
#define TYPE_RANK(ty)            _TYPE_RANK(unpack(unqual(ty)))
#define TYPE_INLINE(ty)          _TYPE_INLINE(unqual(ty))
#define TYPE_TYPE(ty)            _TYPE_TYPE(unqual(ty))
#define TYPE_TAG(ty)             _TYPE_TAG(unqual(ty))
#define TYPE_PARAMS(ty)          _TYPE_PARAMS(unqual(ty))
#define TYPE_OLDSTYLE(ty)        _TYPE_OLDSTYLE(unqual(ty))
#define TYPE_TSYM(ty)            _TYPE_TSYM(unqual(ty)) 
#define TYPE_IDS(ty)             _TYPE_IDS(unqual(ty))
#define TYPE_FIELDS(ty)          _TYPE_FIELDS(unqual(ty))
#define TYPE_LIMITS_MAX(ty)      _TYPE_LIMITS_MAX(unpack(unqual(ty)))
#define TYPE_LIMITS_MIN(ty)      _TYPE_LIMITS_MIN(unpack(unqual(ty)))     
#define TYPE_A_ASSIGN(ty)        _TYPE_A_ASSIGN(unqual(ty))
#define TYPE_A_CONST(ty)         _TYPE_A_CONST(unqual(ty))
#define TYPE_A_VOLATILE(ty)      _TYPE_A_VOLATILE(unqual(ty))
#define TYPE_A_RESTRICT(ty)      _TYPE_A_RESTRICT(unqual(ty))
#define TYPE_A_STATIC(ty)        _TYPE_A_STATIC(unqual(ty))
#define TYPE_A_WILDCARD(ty)      _TYPE_A_WILDCARD(unqual(ty))
#define TYPE_A_HASEXPR(ty)       _TYPE_A_HASEXPR(unqual(ty))

// alias
#define rtype(ty)       TYPE_TYPE(ty)
#define TYPE_OP(ty)     type_op(ty)

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
#define isanonymous(name)   ((name) == NULL || !is_letter((name)[0]))

extern void symbol_init(void);
extern int scopelevel(void);
extern void enter_scope(void);
extern void exit_scope(void);

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

// gen.c
extern void gen(node_t *tree, const char *ofile);

// simplify.c
extern void simplify(node_t *tree);

// print.c
extern void print_tree(node_t *tree);

// error.c
extern unsigned errors;
extern unsigned warnings;
extern void warningf(struct source src, const char *fmt, ...);
extern void errorf(struct source src, const char *fmt, ...);
extern void fatalf(struct source src, const char *fmt, ...);
#define warning(...)  warningf(source, __VA_ARGS__)
#define error(...)    errorf(source, __VA_ARGS__)
#define fatal(...)    fatalf(source, __VA_ARGS__)

#define SAVE_ERRORS    unsigned err = errors
#define NO_ERROR       (err == errors)
#define HAS_ERROR      (err != errors)

#define CCAssert(expr)				\
    do {					\
	if (!(expr)) {				\
	    error("assert failed");		\
	    assert(expr);			\
	}					\
    } while (0)

#define CCAssertf(expr, ...)			\
    do {					\
	if (!(expr)) {				\
	    error(__VA_ARGS__);			\
	    assert(expr);			\
	}					\
    } while (0)

#define INCOMPATIBLE_TYPES    "incompatible type conversion from '%s' to '%s'"

extern void redefinition_error(struct source src, node_t *sym);
extern void conflicting_types_error(struct source src, node_t *sym);
extern void field_not_found_error(node_t *ty, const char *name);

#endif
