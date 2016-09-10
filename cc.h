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
#include <ctype.h>
#include <stdint.h>

#include "config.h"
#include "color.h"
#include "utils/utils.h"
#include "libcpp/lex.h"

// error.c
#include "error.h"

struct cc_options {
    int E:1;
    int ast_dump:1;
    int ir_dump:1;
    int fleading_underscore:1;
    int Wall:1;
    int Werror:1;
    int ansi:1;
    int ir_dump_level:8;
};
extern struct cc_options opts;

// value
#define VALUE_U(v)    ((v).u)
#define VALUE_I(v)    ((v).i)
#define VALUE_D(v)    ((v).d)
#define VALUE_P(v)    ((v).p)
#define VALUE_G(v)    ((v).g)

union value {
    long long i;
    unsigned long long u;
    long double d;
    void *p;
    void (*g) ();
};

/**
 * The coding style tends to avoid typedefs, because
 * typedefs reduce readability, but it's not a hard rule.
 *
 * The fields of ast_node are designed to be accessed _ONLY_
 * via macros, so it's useful to use typedef to hide the
 * implementation details.
 */
typedef union ast_node node_t;

/*
 * Handle carefully for qual/unqual types.
 *
 * macros begin with '_' is for 'atom' access,
 * others use the unqual version of the type.
 */
#define _TYPE_KIND(NODE)         ((NODE)->kind)
#define _TYPE_NAME(NODE)         ((NODE)->name)
#define _TYPE_SIZE(NODE)         ((NODE)->size)
#define _TYPE_ALIGN(NODE)        ((NODE)->align)
#define _TYPE_LEN(NODE)          ((NODE)->u.a.len)
#define _TYPE_RANK(NODE)         ((NODE)->rank)
#define _TYPE_INLINE(NODE)       ((NODE)->inlined)
#define _TYPE_TYPE(NODE)         ((NODE)->type)
#define _TYPE_TAG(NODE)          ((NODE)->u.s.tag)
#define _TYPE_PROTO(NODE)        ((NODE)->u.f.proto)
#define _TYPE_PARAMS(NODE)       ((NODE)->u.f.params)
#define _TYPE_OLDSTYLE(NODE)     ((NODE)->u.f.oldstyle)
#define _TYPE_VARG(NODE)         ((NODE)->u.f.varg)
#define _TYPE_TSYM(NODE)         ((NODE)->u.s.tsym)
#define _TYPE_FIELDS(NODE)       ((NODE)->u.s.fields)
#define _TYPE_LIMITS_MAX(NODE)   ((NODE)->limits.max)
#define _TYPE_LIMITS_MIN(NODE)   ((NODE)->limits.min)
#define _TYPE_A_ASSIGN(NODE)     ((NODE)->u.a.assign)
#define _TYPE_A_CONST(NODE)      ((NODE)->u.a.is_const)
#define _TYPE_A_VOLATILE(NODE)   ((NODE)->u.a.is_volatile)
#define _TYPE_A_RESTRICT(NODE)   ((NODE)->u.a.is_restrict)
#define _TYPE_A_STATIC(NODE)     ((NODE)->u.a.is_static)
#define _TYPE_A_STAR(NODE)       ((NODE)->u.a.star)

struct type {
    const char *name;
    struct type *type;
    int kind;
    size_t size;
    unsigned align;                // align in bytes
    unsigned rank:8;
    unsigned inlined:1;
    union {
        // function
        struct {
            struct type **proto;
            node_t **params;
            unsigned oldstyle:1;
            unsigned varg:1;
        } f;
        // enum/struct/union
        struct {
            const char *tag;
            node_t *tsym;
            node_t **fields;
        } s;
        // array
        struct {
            size_t len;        // array length
            node_t *assign;
            unsigned is_const:1;
            unsigned is_volatile:1;
            unsigned is_restrict:1;
            unsigned is_static:1;
            unsigned star:1;
        } a;
    } u;
    struct {
        union value max;
        union value min;
    } limits;
};

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
#define TYPE_PROTO(ty)           _TYPE_PROTO(unqual(ty))
#define TYPE_PARAMS(ty)          _TYPE_PARAMS(unqual(ty))
#define TYPE_OLDSTYLE(ty)        _TYPE_OLDSTYLE(unqual(ty))
#define TYPE_VARG(ty)            _TYPE_VARG(unqual(ty))
#define TYPE_TSYM(ty)            _TYPE_TSYM(unqual(ty))
#define TYPE_FIELDS(ty)          _TYPE_FIELDS(unqual(ty))
#define TYPE_LIMITS_MAX(ty)      _TYPE_LIMITS_MAX(unpack(unqual(ty)))
#define TYPE_LIMITS_MIN(ty)      _TYPE_LIMITS_MIN(unpack(unqual(ty)))
#define TYPE_A_ASSIGN(ty)        _TYPE_A_ASSIGN(unqual(ty))
#define TYPE_A_CONST(ty)         _TYPE_A_CONST(unqual(ty))
#define TYPE_A_VOLATILE(ty)      _TYPE_A_VOLATILE(unqual(ty))
#define TYPE_A_RESTRICT(ty)      _TYPE_A_RESTRICT(unqual(ty))
#define TYPE_A_STATIC(ty)        _TYPE_A_STATIC(unqual(ty))
#define TYPE_A_STAR(ty)          _TYPE_A_STAR(unqual(ty))

// gen.h
#include "gen.h"

// ast.h
#include "ast.h"

// eval.c
extern node_t *eval(node_t * expr, struct type * ty);
extern bool eval_cpp_cond(void);
 
// expr.c
#define is_assign_op(op)    ((op == '=') || (op >= MULEQ && op <= RSHIFTEQ))
extern node_t *expression(void);
extern node_t *assign_expr(void);
extern long intexpr1(struct type * ty);
extern long intexpr(void);
extern bool islvalue(node_t * node);
extern node_t *assignconv(struct type * ty, node_t * node);
// for expression in conditional statement
extern node_t *bool_expr(void);
// for expression in switch statement
extern node_t *switch_expr(void);
// bop
extern node_t *bop(int op, node_t * l, node_t * r);
// literals
extern node_t *new_integer_literal(int i);
extern node_t *new_string_literal(const char *string);
extern node_t *decls2expr(node_t **decls);

// decl.c
extern node_t **declaration(void);
extern void translation_unit(void);
extern struct type *typename(void);
extern int first_decl(struct token *t);
extern int first_stmt(struct token *t);
extern int first_expr(struct token *t);
extern int first_typename(struct token *t);
extern node_t *make_localvar(const char *name, struct type * ty, int sclass);

struct funcinfo {
    const char *name;
    struct type *type;
    struct vector *gotos;
    struct map *labels;
    struct vector *staticvars;
    struct vector *localvars;
    struct vector *calls;
};
extern struct funcinfo funcinfo;

// init.c
extern bool has_static_extent(node_t * sym);
extern node_t *decl_initializer(node_t * sym, int sclass, int level);
extern node_t *initializer(struct type * ty);
extern node_t *initializer_list(struct type * ty);
extern void init_string(struct type * ty, node_t * node);

// stmt.c
extern node_t *compound_stmt(void (*) (void));

// typechk.c
extern void ensure_inline(struct type *ty, int fspec, struct source src);
extern void ensure_field(node_t * field, size_t total, bool last);
extern void ensure_decl(node_t * sym, int sclass, int kind);
extern void ensure_array(struct type * atype, struct source src, int level);
extern void ensure_func(struct type * ftype, struct source src);
extern void ensure_main(struct type *ftype, const char *name, struct source src);
extern void ensure_params(node_t *params[]);
extern void redefinition_error(struct source src, node_t * sym);
extern void conflicting_types_error(struct source src, node_t * sym);
extern void field_not_found_error(struct type * ty, const char *name);

// type.c
extern void type_init(void);
extern struct type *alloc_type(void);
extern int type_op(struct type * type);
extern void prepend_type(struct type ** typelist, struct type * type);
extern void attach_type(struct type ** typelist, struct type * type);
extern struct type *qual(int t, struct type * ty);
extern struct type *unqual(struct type * ty);
extern bool eqtype(struct type * ty1, struct type * ty2);
extern bool eqarith(struct type * ty1, struct type * ty2);
extern struct type *lookup_typedef(const char *id);
extern bool istypedef(const char *id);
extern struct type *array_type(struct type * ty);
extern struct type *ptr_type(struct type * ty);
extern struct type *func_type(void);
extern node_t *tag_type(int t, const char *tag, struct source src);
extern void set_typesize(struct type * ty);
extern node_t *find_field(struct type * ty, const char *name);
extern int indexof_field(struct type * ty, node_t * field);
extern struct type *compose(struct type * ty1, struct type * ty2);
extern bool qual_contains(struct type * ty1, struct type * ty2);
extern int qual_union(struct type * ty1, struct type * ty2);
extern bool isincomplete(struct type * ty);
extern struct type *unpack(struct type * ty);

extern struct type *chartype;
extern struct type *unsignedchartype;
extern struct type *signedchartype;
extern struct type *wchartype;
extern struct type *shorttype;
extern struct type *unsignedshorttype;
extern struct type *inttype;
extern struct type *unsignedinttype;
extern struct type *longtype;
extern struct type *unsignedlongtype;
extern struct type *longlongtype;
extern struct type *unsignedlonglongtype;
extern struct type *floattype;
extern struct type *doubletype;
extern struct type *longdoubletype;
extern struct type *voidtype;
extern struct type *booltype;

#define BITS(bytes)     (CHAR_BIT * (bytes))
#define BYTES(bits)     ((ROUNDUP(bits, CHAR_BIT)) / (CHAR_BIT))

#define isconst1(kind)     ((kind) == CONST ||                          \
                            (kind) == CONST + VOLATILE ||               \
                            (kind) == CONST + RESTRICT ||               \
                            (kind) == CONST + VOLATILE + RESTRICT)

#define isvolatile1(kind)  ((kind) == VOLATILE ||                       \
                            (kind) == VOLATILE + CONST ||               \
                            (kind) == VOLATILE + RESTRICT ||            \
                            (kind) == CONST + VOLATILE + RESTRICT)

#define isrestrict1(kind)  ((kind) == RESTRICT ||                       \
                            (kind) == RESTRICT + CONST ||               \
                            (kind) == RESTRICT + VOLATILE ||            \
                            (kind) == CONST + VOLATILE + RESTRICT)

#define isconst(ty)     isconst1(_TYPE_KIND(ty))
#define isvolatile(ty)  isvolatile1(_TYPE_KIND(ty))
#define isrestrict(ty)  isrestrict1(_TYPE_KIND(ty))

#define isinline(ty)    (_TYPE_INLINE(ty))
#define isqual(ty)      (isconst(ty) || isvolatile(ty) || isrestrict(ty))

// alias
#define rtype(ty)       TYPE_TYPE(ty)
#define TYPE_OP(ty)     type_op(ty)

extern bool isfunc(struct type * type);
extern bool isarray(struct type * type);
extern bool isptr(struct type * type);
extern bool isvoid(struct type * type);
extern bool isenum(struct type * type);
extern bool isstruct(struct type * type);
extern bool isunion(struct type * type);
extern bool isrecord(struct type * type);        // isstruct or isunion
extern bool istag(struct type * type);        // isstruct or isunion or isenum

extern bool isint(struct type * ty);
extern bool isfloat(struct type * ty);
extern bool isarith(struct type * ty);
extern bool isscalar(struct type * ty);
extern bool isptrto(struct type * ty, int kind);
extern bool isbool(struct type *ty);

// sym.c
// scope level
enum { CONSTANT, GLOBAL, PARAM, LOCAL };

struct table {
    int scope;
    struct table *up;
    struct map *map;
    node_t *all;
};

// sym
extern struct table *new_table(struct table *up, int scope);
extern void symbol_init(void);
extern void enter_scope(void);
extern void exit_scope(void);
extern void foreach(struct table *tp, int level, void (*apply) (node_t *, void *), void *context);
extern bool is_current_scope(node_t *sym);
extern bool is_anonymous(const char *name);

// create an anonymous symbol
extern node_t *anonymous(struct table **tpp, int scope, int area);

// generate a tmp symbol
extern node_t *gen_tmp_sym(int area);

// look up a symbol from this table to previous one, and so on
extern node_t *lookup(const char *name, struct table *table);

// install a symbol with specified scope
extern node_t *install(const char *name, struct table **tpp, int scope, int area);

extern struct table *identifiers;
extern struct table *constants;
extern struct table *tags;
extern int _scope;

#define SCOPE  _scope

// print.c
extern void print_tree(node_t * tree);
extern void print_tac(struct tac *tac);
extern void print_type(struct type *ty);
// extern void print_ir(struct externals * tree);
extern const char *type2s(struct type * ty);
extern const char *node2s(node_t * node);
extern void dump_operand(struct operand *operand);
extern void dump_reg(struct reg *reg);
extern void dump_tacs(struct tac *tac);
extern void print_source(struct source src);

#define INCOMPATIBLE_TYPES    "incompatible type conversion from '%s' to '%s'"
#define BUILTIN_VA_START    "__builtin_va_start"
#define BUILTIN_VA_ARG_P    "__builtin_va_arg_p"

// sema actions
struct actions {
    void (*dclvar) (node_t *);
    void (*defvar) (node_t *);
    void (*dclfun) (node_t *);
    void (*defun) (node_t *);
    void (*deftype) (struct type *);
    void (*init) (int argc, char *argv[]);
    void (*finalize) (void);
};
extern struct actions actions;

// middle-end interface
struct ir {
    void (*defvar) (node_t *);
    void (*defun) (node_t *);
    void (*init) (int argc, char *argv[]);
    void (*finalize) (void);
};
extern struct ir *IR;

// backend interface
struct metrics {
    unsigned char size, align, rank;
};

// segments
enum { TEXT, BSS, DATA };

struct im {
    void (*init) (int argc, char *argv[]);
    void (*finalize) (void);
    void (*defvar) (node_t *, int);
    void (*defun) (node_t *);
    void (*emit_compounds) (struct map *);
    void (*emit_strings) (struct map *);
    void (*emit_floats) (struct map *);
    struct metrics boolmetrics;
    struct metrics charmetrics;
    struct metrics shortmetrics;
    struct metrics wcharmetrics;
    struct metrics intmetrics;
    struct metrics longmetrics;
    struct metrics longlongmetrics;
    struct metrics floatmetrics;
    struct metrics doublemetrics;
    struct metrics longdoublemetrics;
    struct metrics ptrmetrics;
    struct metrics zerometrics;
};
extern struct im *IM;

#endif
