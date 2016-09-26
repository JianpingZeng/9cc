#ifndef _CC_H
#define _CC_H

// for bool
#include <stdbool.h>
// for size_t
#include <stddef.h>
// for CHAR_BIT
#include <limits.h>

#include "config.h"
#include "libutils/utils.h"
#include "libcpp/lex.h"

///
/// type declarations
///

struct cc_options {
    int E:1;
    int ast_dump:1;
    int ir_dump:1;
    int fleading_underscore:1;
    int Wall:1;
    int Werror:1;
    int ansi:1;
};

/*
 * Handle carefully for qual/unqual types.
 *
 * macros begin with '_' are for 'atom' access,
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
#define _TYPE_LIMITS(NODE)       ((NODE)->limits)
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
#define TYPE_LIMITS(ty)          _TYPE_LIMITS(unpack(unqual(ty)))
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
    unsigned int align;                // align in bytes
    unsigned int rank:8;
    unsigned int inlined:1;
    union {
        // function
        struct {
            struct type **proto;
            struct symbol **params;
            unsigned int oldstyle:1;
            unsigned int varg:1;
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
            struct expr *assign;
            unsigned int is_const:1;
            unsigned int is_volatile:1;
            unsigned int is_restrict:1;
            unsigned int is_static:1;
            unsigned int star:1;
        } a;
    } u;
    struct {
        union value max;
        union value min;
    } limits;
};

struct field {
    const char *name;
    struct type *type;
    struct source src;
    size_t offset;
    int isbit : 1;
    int bitsize : 10;
    int bitoff : 10;
};

struct symbol {
    const char *name;
    struct type *type;
    struct source src;
    int scope;
    int sclass;
    bool defined;
    bool predefine;
    unsigned int refs;
    union value value;
    struct symbol *link;
    union {
        struct expr *init;               // initializer expr
        struct {
            struct expr **calls;
            struct stmt *stmt;
        } f;
    } u;
    struct {
        const char *name;
        // for goto labels
        int label;
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

// ast node ids
enum {
#define _ns(a)   a,
#define _n(a, b) a,
#include "node.def"
};

#define EXPR_OPERAND(NODE, I)   ((NODE)->operands[I])
#define EXPR_ARGS(NODE)         ((NODE)->list)
#define EXPR_INITS(NODE)        ((NODE)->list)
// conditional expr
#define EXPR_COND(NODE)         EXPR_OPERAND(NODE, 0)
#define EXPR_THEN(NODE)         EXPR_OPERAND(NODE, 1)
#define EXPR_ELSE(NODE)         EXPR_OPERAND(NODE, 2)
// va_arg
#define EXPR_VA_ARG_TYPE(NODE)  ((NODE)->vtype)
    
struct expr {
    int id;
    const char *name;
    struct type *type;
    struct source src;

    int op;
    bool prefix;
    struct symbol *sym;
    struct expr *operands[3];
    struct expr **list;
    struct type *vtype;
};

/// stmt

// stmt id
enum { LABEL = 1, GEN, JMP, CBR, RET };

struct stmt {
    int id;
    union {
        int label;              // LABEL/JMP
        struct expr *expr;      // GEN/RET
        struct {
            struct expr *expr;
            int tlab, flab;
        } cbr;
    } u;
    struct stmt *next;
};

/// switch structs

struct cse {
    struct source src;
    int label;
    long value;
    struct cse *link;
};

struct swtch {
    struct source src;
    struct type *type;
    struct cse *cases;
    struct cse *defalt;
};

/// function context

struct func {
    const char *name;
    struct type *type;
    struct vector *gotos;
    struct table *labels;
    struct vector *calls;
    struct stmt **stmt;
};

// sema actions
struct actions {
    void (*init) (int argc, char *argv[]);
    void (*finalize) (void);

    // decl
    void (*dclgvar) (struct symbol *); // declare a global variable
    void (*defgvar) (struct symbol *); // define a global variable
    void (*defsvar) (struct symbol *); // define a local static variable
    void (*deflvar) (struct symbol *); // define a local variable
    void (*dclfun) (struct symbol *);  // declare a function
    void (*defun) (struct symbol *);   // define a function
    void (*deftype) (struct symbol *); // declare/define a type: struct/union/enum/typedef
    
    // expr
    struct expr * (*commaop) (struct expr *l, struct expr *r, struct source src);
    struct expr * (*assignop) (int op, struct expr *l, struct expr *r, struct source src);
    struct expr * (*condop) (struct expr *cond, struct expr *then, struct expr *els, struct source src);
    struct expr * (*logicop) (int op, struct expr *l, struct expr *r, struct source src);
    struct expr * (*bop) (int op, struct expr *l, struct expr *r, struct source src);
    struct expr * (*castop) (struct type *ty, struct expr *cast, struct source src);
    struct expr * (*pre_increment) (int op, struct expr *unary, struct source src);
    struct expr * (*minus_plus) (int op, struct expr *operand, struct source src);
    struct expr * (*bitwise_not) (struct expr *operand, struct source src);
    struct expr * (*logical_not) (struct expr *operand, struct source src);
    struct expr * (*address) (struct expr *operand, struct source src);
    struct expr * (*indirection) (struct expr *operand, struct source src);
    struct expr * (*sizeofop) (struct type *ty, struct expr *n, struct source src);
    struct expr * (*subscript) (struct expr *node, struct expr *index, struct source src);
    struct expr * (*funcall) (struct expr *node, struct expr **args, struct source src);
    struct expr * (*direction) (struct expr *node, int op, const char *name, struct source src);
    struct expr * (*post_increment) (struct expr *node, int op, struct source src);
    struct expr * (*id) (struct token *tok);
    struct expr * (*iconst) (struct token *tok);
    struct expr * (*fconst) (struct token *tok);
    struct expr * (*sconst) (struct token *tok);
    struct expr * (*paren) (struct expr *node, struct source src);
    struct expr * (*compound_literal) (struct type *ty, struct expr *init, struct source src);

    // stmt
    void (*branch) (struct expr *expr, int tlab, int flab);
    void (*jump) (int label);
    void (*ret) (struct expr *expr);
    void (*label) (int label);
    void (*gen) (struct expr *expr);
};

// middle-end interface
struct interface {
    void (*defvar) (struct symbol *);
    void (*defun) (struct symbol *);
    void (*init) (int argc, char *argv[]);
    void (*finalize) (void);
};

// metrics
struct metrics {
    unsigned char size, align, rank;
};

// segments
enum { TEXT, BSS, DATA };

// backend interface
struct machine {
    const char *os;
    const char *arch;
    void (*init) (int argc, char *argv[]);
    void (*finalize) (void);
    void (*defvar) (struct symbol *, int seg);
    void (*defun) (struct symbol *);
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
    struct metrics voidmetrics;
};


///
/// external functions
///

// symtab.c
extern struct symbol *alloc_symbol(int area);
extern struct table *new_table(struct table *up, int scope);
extern void free_table(struct table *t);
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

/// ast.c
extern const char *nname(int id);
extern struct expr *ast_expr(int id, struct type *ty, struct expr *l, struct expr *r);
extern struct expr *ast_uop(int op, struct type *ty, struct expr *l);
extern struct expr *ast_bop(int op, struct type *ty, struct expr *l, struct expr *r);
extern struct expr *ast_conv(struct type *ty, struct expr *l, const char *name);
extern struct expr *ast_inits(struct type * ty, struct source src);
extern struct expr *ast_vinit(void);
extern struct stmt *ast_stmt(int id);
extern const char *gen_tmpname(void);
extern const char *gen_compound_label(void);
extern int genlabel(int count);

#define isfuncdef(n)   (isfunc((n)->type) && (n)->defined)
#define isvardecl(n)   ((n)->sclass != TYPEDEF && !isfunc((n)->type))
#define isiliteral(n)  ((n)->id == INTEGER_LITERAL)
#define isfliteral(n)  ((n)->id == FLOAT_LITERAL)
#define issliteral(n)  ((n)->id == STRING_LITERAL)

// eval.c
extern struct expr *eval(struct expr *expr, struct type *ty);
 
// expr.c
extern struct expr *expression(void);
extern struct expr *assign_expr(void);
extern struct expr *cond_expr(void);

// decl.c
extern void declaration(void);
extern void translation_unit(void);
extern struct type *typename(void);
extern int first_decl(struct token *t);
extern int first_stmt(struct token *t);
extern int first_expr(struct token *t);
extern int first_typename(struct token *t);
extern struct symbol *mklocalvar(const char *name, struct type *ty, int sclass);
extern struct symbol *mktmpvar(struct type *ty, int sclass);

// init.c
extern struct expr *initializer(struct type *ty);
extern struct expr *initializer_list(struct type *ty);
extern void init_string(struct type *ty, struct expr *node);
extern struct expr *ensure_init(struct expr *init, struct type *ty, struct symbol *sym);

// stmt.c
extern void compound_stmt(void (*cb) (void), int cnt, int brk, struct swtch *swtch);

// sema.c
extern bool has_static_extent(struct symbol *sym);
extern void ensure_inline(struct type *ty, int fspec, struct source src);
extern void ensure_field(struct field *field, size_t total, bool last);
extern void ensure_decl(struct symbol *sym);
extern void ensure_array(struct type *atype, struct source src, int level);
extern void ensure_func(struct type *ftype, struct source src);
extern void ensure_main(struct type *ftype, const char *name, struct source src);
extern void ensure_params(struct symbol *params[]);
extern void ensure_prototype(struct type *ftype, struct symbol *params[]);
extern void redefinition_error(struct source src, struct symbol *sym);
extern void conflicting_types_error(struct source src, struct symbol *sym);
extern void field_not_found_error(struct type *ty, const char *name);
extern bool islvalue(struct expr *node);
extern struct expr *assignconv(struct type *ty, struct expr *node);
extern struct expr *new_int_literal(long i);
extern struct expr *new_string_literal(const char *string);
extern long intexpr1(struct type *ty);
extern long intexpr(void);
// for expression in conditional statement
extern struct expr *bool_expr(void);
// for expression in switch statement
extern struct expr *switch_expr(void);
// for eval
extern struct expr *binop(int op, struct expr *l, struct expr *r);
extern struct expr *assign(struct symbol *sym, struct expr *r);
extern void ensure_return(struct expr *expr, bool isnull, struct source src);
extern void ensure_gotos(void);
extern void check_case_duplicates(struct cse *cse, struct swtch *swtch);
extern void mark_goto(const char *id, struct source src);

// type.c
extern void type_init(void);
extern struct field *alloc_field(void);
extern struct type *alloc_type(void);
extern int type_op(struct type *type);
extern void prepend_type(struct type **typelist, struct type *type);
extern void attach_type(struct type **typelist, struct type *type);
extern struct type *qual(int t, struct type *ty);
extern struct type *unqual(struct type *ty);
extern bool eqtype(struct type *ty1, struct type *ty2);
extern bool eqarith(struct type *ty1, struct type *ty2);
extern struct type *lookup_typedef(const char *id);
extern bool istypedef(const char *id);
extern struct type *array_type(struct type *ty);
extern struct type *ptr_type(struct type *ty);
extern struct type *func_type(void);
extern struct symbol *tag_type(int t, const char *tag, struct source src);
extern void set_typesize(struct type *ty);
extern struct field *find_field(struct type *ty, const char *name);
extern int indexof_field(struct type *ty, struct field *field);
extern struct type *compose(struct type *ty1, struct type *ty2);
extern bool qual_contains(struct type *ty1, struct type *ty2);
extern int qual_union(struct type *ty1, struct type *ty2);
extern bool isincomplete(struct type *ty);
extern struct type *unpack(struct type *ty);

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

extern bool isfunc(struct type *type);
extern bool isarray(struct type *type);
extern bool isptr(struct type *type);
extern bool isvoid(struct type *type);
extern bool isenum(struct type *type);
extern bool isstruct(struct type *type);
extern bool isunion(struct type *type);
extern bool isrecord(struct type *type);     // isstruct or isunion
extern bool istag(struct type *type);        // isstruct or isunion or isenum
extern bool isint(struct type *ty);
extern bool isfloat(struct type *ty);
extern bool isarith(struct type *ty);
extern bool isscalar(struct type *ty);
extern bool isptrto(struct type *ty, int kind);
extern bool isbool(struct type *ty);

// error.c
extern void warningf(struct source src, const char *fmt, ...);
extern void errorf(struct source src, const char *fmt, ...);
extern void fatalf(struct source src, const char *fmt, ...);

#define SAVE_ERRORS    unsigned int __err = errors
#define NO_ERROR       (__err == errors)
#define HAS_ERROR      (__err != errors)

#define warning_at(s, ...)   warningf(s, __VA_ARGS__)
#define error_at(s, ...)     errorf(s, __VA_ARGS__)
#define fatal_at(s, ...)     fatalf(s, __VA_ARGS__)
#define warning(...)         warning_at(source, __VA_ARGS__)
#define error(...)           error_at(source, __VA_ARGS__)
#define fatal(...)           fatal_at(source, __VA_ARGS__)

// print.c
extern void ast_dump_symbol(struct symbol *);
extern void ast_dump_type(struct symbol *);

extern void print_field(struct field *field);
extern void print_expr(struct expr *expr);
extern void print_type(struct symbol *sym);
extern void print_symbol(struct symbol *sym);
extern const char *type2s(struct type *ty);
extern const char *expr2s(struct expr *node);

#define INCOMPATIBLE_TYPES  "incompatible type conversion from '%s' to '%s'"
#define BUILTIN_VA_START    "__builtin_va_start"
#define BUILTIN_VA_ARG_P    "__builtin_va_arg_p"


///
/// external variables
///

extern struct cc_options opts;
extern struct func func;
extern struct actions actions;
extern struct interface *IR;
extern struct machine *IM;
extern struct table *identifiers;
extern struct table *constants;
extern struct table *tags;
extern int cscope;              // current scope
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
extern unsigned int errors, warnings;

#endif
