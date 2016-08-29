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

// alloc.c
extern void *alloc_node(void);
extern void *alloc_macro(void);
extern void *alloc_operand(void);
extern void *alloc_tac(void);
extern void *alloc_basic_block(void);
extern void *alloc_reladdr(void);
extern void *alloc_opcode(void);
extern void *alloc_hideset(void);
extern void *alloc_cpp_ident(void);
#define NEW(n, a)  allocate((n), (a))
#define NEW0(n, a)  memset(allocate((n), (a)), 0, (n))
#define NEWS(s, a)  NEW0(sizeof(s), a)
enum { PERM = 0, FUNC };

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
    struct vector *cpp_options;
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

// ast.h
#include "ast.h"

// eval.c
extern node_t *eval(node_t * expr, node_t * ty);
extern bool eval_cpp_cond(void);
 
// expr.c
#define is_assign_op(op)    ((op == '=') || (op >= MULEQ && op <= RSHIFTEQ))
extern node_t *expression(void);
extern node_t *assign_expr(void);
extern long intexpr1(node_t * ty);
extern long intexpr(void);
extern bool islvalue(node_t * node);
extern node_t *assignconv(node_t * ty, node_t * node);
// for expression in conditional statement
extern node_t *bool_expr(void);
// for expression in switch statement
extern node_t *switch_expr(void);
// bop
extern node_t *bop(int op, node_t * l, node_t * r);

// literals
extern node_t *new_integer_literal(int i);
extern node_t *new_string_literal(const char *string);

// decl.c
extern struct vector *declaration(void);
extern node_t *translation_unit(void);
extern void finalize(void);
extern node_t *typename(void);
extern int first_decl(struct token *t);
extern int first_stmt(struct token *t);
extern int first_expr(struct token *t);
extern bool first_typename(struct token *t);
extern node_t *make_localdecl(const char *name, node_t * ty, int sclass);
extern node_t *initializer_list(node_t * ty);
extern void init_string(node_t * ty, node_t * node);
extern bool has_static_extent(node_t * sym);

// stmt.c
extern struct vector *funcalls;
extern void func_body(node_t *decl);
extern node_t *make_localvar(const char *name, node_t * ty, int sclass);

// typechk.c
extern void check_oldstyle(node_t *ftype);
extern void ensure_inline(node_t *ty, int fspec, struct source src);
extern void ensure_field(node_t * field, size_t total, bool last);
extern void ensure_decl(node_t * decl, int sclass, int kind);
extern void ensure_array(node_t * atype, struct source src, int level);
extern void ensure_func(node_t * ftype, struct source src);
extern void ensure_main(node_t *ftype, const char *name, struct source src);
extern void ensure_params(node_t *ftype);
extern void redefinition_error(struct source src, node_t * sym);
extern void conflicting_types_error(struct source src, node_t * sym);
extern void field_not_found_error(node_t * ty, const char *name);

// type.c
extern void type_init(void);
extern int type_op(node_t * type);
extern void prepend_type(node_t ** typelist, node_t * type);
extern void attach_type(node_t ** typelist, node_t * type);
extern node_t *qual(int t, node_t * ty);
extern node_t *unqual(node_t * ty);
extern bool eqtype(node_t * ty1, node_t * ty2);
extern bool eqarith(node_t * ty1, node_t * ty2);
extern node_t *lookup_typedef(const char *id);
extern bool istypedef(const char *id);
extern node_t *new_field(void);
extern node_t *array_type(node_t * ty);
extern node_t *ptr_type(node_t * ty);
extern node_t *func_type(void);
extern node_t *tag_type(int t, const char *tag, struct source src);
extern void set_typesize(node_t * ty);
extern node_t *find_field(node_t * ty, const char *name);
extern int indexof_field(node_t * ty, node_t * field);
extern node_t *compose(node_t * ty1, node_t * ty2);
extern bool qual_contains(node_t * ty1, node_t * ty2);
extern int qual_union(node_t * ty1, node_t * ty2);
extern bool isincomplete(node_t * ty);
extern node_t *unpack(node_t * ty);

extern node_t *chartype;        // char
extern node_t *unsignedchartype;        // unsigned char
extern node_t *signedchartype;        // signed char
extern node_t *wchartype;        // wchar_t
extern node_t *shorttype;        // short (int)
extern node_t *unsignedshorttype;        // unsigned short (int)
extern node_t *inttype;                // int
extern node_t *unsignedinttype;        // unsigned (int)
extern node_t *longtype;        // long
extern node_t *unsignedlongtype;        // unsigned long (int)
extern node_t *longlongtype;        // long long (int)
extern node_t *unsignedlonglongtype;        // unsigned long long (int)
extern node_t *floattype;        // float
extern node_t *doubletype;        // double
extern node_t *longdoubletype;        // long double
extern node_t *voidtype;        // void
extern node_t *booltype;        // bool

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

// alias
#define rtype(ty)       TYPE_TYPE(ty)
#define TYPE_OP(ty)     type_op(ty)

extern bool isfunc(node_t * type);
extern bool isarray(node_t * type);
extern bool isptr(node_t * type);
extern bool isvoid(node_t * type);
extern bool isenum(node_t * type);
extern bool isstruct(node_t * type);
extern bool isunion(node_t * type);
extern bool isrecord(node_t * type);        // isstruct or isunion
extern bool istag(node_t * type);        // isstruct or isunion or isenum

extern bool isint(node_t * ty);
extern bool isfloat(node_t * ty);
extern bool isarith(node_t * ty);
extern bool isscalar(node_t * ty);
extern bool isptrto(node_t * ty, int kind);
extern bool isbool(node_t *ty);

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
extern struct table *new_table(struct table *up, int scope);
extern void symbol_init(void);
extern int scopelevel(void);
extern void enter_scope(void);
extern void exit_scope(void);
extern bool is_current_scope(node_t *sym);
extern bool is_anonymous(const char *name);

// create an anonymous symbol
extern node_t *anonymous(struct table **tpp, int scope);

// generate a tmp symbol
extern node_t *gen_tmp_sym(void);

// look up a symbol from this table to previous one, and so on
extern node_t *lookup(const char *name, struct table *table);

// install a symbol with specified scope
extern node_t *install(const char *name, struct table **tpp, int scope);

extern struct table *identifiers;
extern struct table *constants;
extern struct table *tags;

#define SCOPE  scopelevel()

// print.c
extern void print_tree(node_t * tree);
extern void print_tac(struct tac *tac);
extern void print_ir(struct externals * tree);
extern const char *type2s(node_t * ty);
extern const char *node2s(node_t * node);
extern void print_node_size(void);
extern void dump_operand(struct operand *operand);
extern void dump_reg(struct reg *reg);
extern void dump_tacs(struct tac *tac);
extern void print_source(struct source src);

#define INCOMPATIBLE_TYPES    "incompatible type conversion from '%s' to '%s'"
#define BUILTIN_VA_START    "__builtin_va_start"
#define BUILTIN_VA_ARG_P    "__builtin_va_arg_p"

struct metrics {
    size_t size;
    int align;
    unsigned rank;
};
// backend interface
struct imachine {
    void (*progbeg) (int argc, char *argv[]);
    void (*defvar) (node_t *);
    void (*defun) (node_t *);
    void (*progend) (void);
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
extern struct imachine *IM;

#endif
