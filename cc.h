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

// gen.h
#include "gen.h"

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
            struct symbol **params;
            unsigned oldstyle:1;
            unsigned varg:1;
        } f;
        // enum/struct/union
        struct {
            const char *tag;
            struct symbol *tsym;
            struct field **fields;
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

#define FIELD_NAME(NODE)        ((NODE)->name)
#define FIELD_TYPE(NODE)        ((NODE)->type)
#define FIELD_SRC(NODE)         ((NODE)->src)
#define FIELD_ISBIT(NODE)       ((NODE)->isbit)
#define FIELD_OFFSET(NODE)      ((NODE)->offset)
#define FIELD_BITSIZE(NODE)     ((NODE)->bitsize)
#define FIELD_BITOFF(NODE)      ((NODE)->bitoff)

struct field {
    const char *name;
    struct type *type;
    struct source src;
    size_t offset;
    int isbit : 1;
    int bitsize : 10;
    int bitoff : 10;
};

// symtab.c

#define SYM_SRC(NODE)         ((NODE)->src)
#define SYM_SCOPE(NODE)       ((NODE)->scope)
#define SYM_NAME(NODE)        ((NODE)->name)
#define SYM_SCLASS(NODE)      ((NODE)->sclass)
#define SYM_TYPE(NODE)        ((NODE)->type)
#define SYM_DEFINED(NODE)     ((NODE)->defined)
#define SYM_PREDEFINE(NODE)   ((NODE)->predefine)
#define SYM_VALUE(NODE)       ((NODE)->value)
#define SYM_REFS(NODE)        ((NODE)->refs)
#define SYM_LINK(NODE)        ((NODE)->link)
#define SYM_INIT(NODE)        ((NODE)->init)
// convenience
#define SYM_VALUE_U(NODE)     (VALUE_U(SYM_VALUE(NODE)))
#define SYM_VALUE_I(NODE)     (VALUE_I(SYM_VALUE(NODE)))
#define SYM_VALUE_D(NODE)     (VALUE_D(SYM_VALUE(NODE)))

// sym
#define SYM_X_LABEL(NODE)     ((NODE)->x.label)
#define SYM_X_USES(NODE)      ((NODE)->x.uses)
#define SYM_X_REG(NODE)       ((NODE)->x.reg)
#define SYM_X_KIND(NODE)      ((NODE)->x.kind)
#define SYM_X_LOFF(NODE)      ((NODE)->x.loff)
#define SYM_X_INMEM(NODE)     ((NODE)->x.inmem)
#define SYM_X_FREG(NODE)      ((NODE)->x.freg)
#define SYM_X_SVARS(NODE)     ((NODE)->x.svars)
#define SYM_X_LVARS(NODE)     ((NODE)->x.lvars)
#define SYM_X_CALLS(NODE)     ((NODE)->x.calls)
#define SYM_X_HEAD(NODE)      ((NODE)->x.head)
#define SYM_X_BASIC_BLOCK(NODE)  ((NODE)->x.basic_block)
#define SYM_X_XVALUES(NODE)   ((NODE)->x.xvalues)

struct symbol {
    const char *name;
    struct type *type;
    struct source src;
    int scope;
    int sclass;
    unsigned defined : 1;
    unsigned predefine : 1;
    union value value;
    unsigned refs;
    struct symbol *link;
    node_t *init;               // the initializer expr or func body
    struct {
        const char *label;
        long loff;              // local offset (<0)
        int kind;               // kind
        struct uses uses;       // uses
        struct reg *reg;
        bool inmem;
        bool freg;              // spilled from a floating reg

        struct vector *lvars;        // function local vars
        struct vector *svars;        // function static vars
        struct vector *calls;        // function calls
        struct basic_block *basic_block;
        struct tac *head;
        struct tac *tail;
        struct vector *xvalues;
    } x;
};

// scope level
enum { CONSTANT, GLOBAL, PARAM, LOCAL };

struct table {
    int scope;
    struct table *up;
    struct map *map;
    struct symbol *all;
};

// sym
extern struct symbol *alloc_symbol(int area);
extern struct table *new_table(struct table *up, int scope);
extern void symbol_init(void);
extern void enter_scope(void);
extern void exit_scope(void);
extern void foreach(struct table *tp, int level, void (*apply) (struct symbol *, void *), void *context);
extern bool is_current_scope(struct symbol *sym);
extern bool is_anonymous(const char *name);

// create an anonymous symbol
extern struct symbol *anonymous(struct table **tpp, int scope, int area);

// generate a tmp symbol
extern struct symbol *gen_tmp_sym(int area);

// look up a symbol from this table to previous one, and so on
extern struct symbol *lookup(const char *name, struct table *table);

// install a symbol with specified scope
extern struct symbol *install(const char *name, struct table **tpp, int scope, int area);

extern struct table *identifiers;
extern struct table *constants;
extern struct table *tags;
extern int _scope;

#define SCOPE  _scope

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
extern node_t *decls2expr(struct symbol **decls);

// decl.c
extern struct symbol **declaration(void);
extern void translation_unit(void);
extern struct type *typename(void);
extern int first_decl(struct token *t);
extern int first_stmt(struct token *t);
extern int first_expr(struct token *t);
extern int first_typename(struct token *t);
extern struct symbol *make_localvar(const char *name, struct type * ty, int sclass);

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
extern bool has_static_extent(struct symbol * sym);
extern node_t *decl_initializer(struct symbol * sym, int sclass, int level);
extern node_t *initializer(struct type * ty);
extern node_t *initializer_list(struct type * ty);
extern void init_string(struct type * ty, node_t * node);

// stmt.c
extern node_t *compound_stmt(void (*) (void));

// typechk.c
extern void ensure_inline(struct type *ty, int fspec, struct source src);
extern void ensure_field(struct field * field, size_t total, bool last);
extern void ensure_decl(struct symbol * sym, int sclass, int kind);
extern void ensure_array(struct type * atype, struct source src, int level);
extern void ensure_func(struct type * ftype, struct source src);
extern void ensure_main(struct type *ftype, const char *name, struct source src);
extern void ensure_params(struct symbol *params[]);
extern void redefinition_error(struct source src, struct symbol * sym);
extern void conflicting_types_error(struct source src, struct symbol * sym);
extern void field_not_found_error(struct type * ty, const char *name);

// type.c
extern void type_init(void);
extern struct field *alloc_field(void);
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
extern struct symbol *tag_type(int t, const char *tag, struct source src);
extern void set_typesize(struct type * ty);
extern struct field *find_field(struct type * ty, const char *name);
extern int indexof_field(struct type * ty, struct field * field);
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

// print.c
extern void print_field(struct field *field);
extern void print_tree(node_t * tree);
extern void print_tac(struct tac *tac);
extern void print_type(struct symbol *sym);
extern void print_symbol(struct symbol *sym);
extern void print_ir_data(struct symbol *sym);
extern void print_ir_bss(struct symbol *sym);
extern void print_ir_text(struct symbol *sym);
extern void print_ir_compounds(struct map *compounds);
extern void print_ir_strings(struct map *strings);
extern void print_ir_floats(struct map *floats);
extern const char *type2s(struct type * ty);
extern const char *expr2s(node_t * node);
extern void dump_operand(struct operand *operand);
extern void dump_reg(struct reg *reg);
extern void dump_tacs(struct tac *tac);
extern void print_source(struct source src);

#define INCOMPATIBLE_TYPES    "incompatible type conversion from '%s' to '%s'"
#define BUILTIN_VA_START    "__builtin_va_start"
#define BUILTIN_VA_ARG_P    "__builtin_va_arg_p"

// sema actions
struct actions {
    void (*dclvar) (struct symbol *);
    void (*defvar) (struct symbol *);
    void (*dclfun) (struct symbol *);
    void (*defun) (struct symbol *);
    void (*deftype) (struct symbol *); // struct/union/enum/typedef
    void (*init) (int argc, char *argv[]);
    void (*finalize) (void);
};
extern struct actions actions;

// middle-end interface
struct ir {
    void (*defvar) (struct symbol *);
    void (*defun) (struct symbol *);
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
    void (*defvar) (struct symbol *, int);
    void (*defun) (struct symbol *);
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
