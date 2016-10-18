#ifndef CC_H
#define CC_H

// for bool
#include <stdbool.h>
// for size_t
#include <stddef.h>

#include "config.h"
#include "libutils/utils.h"
#include "libcpp/lex.h"

///
/// type declarations
///

struct options {
    int preprocess_only:1;
    int ast_dump:1;
    int fleading_underscore:1;
    int Wall:1;
    int Werror:1;
    int ansi:1;
};

/*
 * Handle carefully for qual/unqual types.
 */

// operations on the unqual type
#define TYPE_KIND(ty)            (unqual(ty)->kind)
#define TYPE_OP(ty)              (unqual(ty)->op)
#define TYPE_NAME(ty)            (unqual(ty)->name)
#define TYPE_SIZE(ty)            (unqual(ty)->size)
#define TYPE_ALIGN(ty)           (unqual(ty)->align)
#define TYPE_RANK(ty)            (unqual(ty)->rank)
#define TYPE_INLINE(ty)          (unqual(ty)->inlined)
#define TYPE_TYPE(ty)            (unqual(ty)->type)
#define TYPE_LIMITS(ty)          (unqual(ty)->limits)
// function
#define TYPE_PROTO(ty)           (unqual(ty)->u.f.proto)
#define TYPE_PARAMS(ty)          (unqual(ty)->u.f.params)
#define TYPE_OLDSTYLE(ty)        (unqual(ty)->u.f.oldstyle)
#define TYPE_VARG(ty)            (unqual(ty)->u.f.varg)
// enum/struct/union
#define TYPE_TSYM(ty)            (unqual(ty)->u.s.tsym)
#define TYPE_FIELDS(ty)          (TYPE_TSYM(ty)->u.s.flist)
// array
#define TYPE_LEN(ty)             (unqual(ty)->u.a.len)
#define TYPE_A_ASSIGN(ty)        (unqual(ty)->u.a.assign)
#define TYPE_A_CONST(ty)         (unqual(ty)->u.a.con)
#define TYPE_A_VOLATILE(ty)      (unqual(ty)->u.a.vol)
#define TYPE_A_RESTRICT(ty)      (unqual(ty)->u.a.res)
#define TYPE_A_STATIC(ty)        (unqual(ty)->u.a.stc)
#define TYPE_A_STAR(ty)          (unqual(ty)->u.a.star)

struct type {
    const char *name;
    struct type *type;
    size_t size;
    short kind;
    short op;
    unsigned char align;        // align in bytes
    unsigned char rank;
    bool inlined;
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
            struct symbol *tsym; // tag symbol
        } s;
        // array
        struct {
            size_t len;         // array length
            struct expr *assign;
            unsigned int con:1;
            unsigned int vol:1;
            unsigned int res:1;
            unsigned int stc:1;
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
    struct field *indir;        // indirect field
    struct field **of;          // indirect of
    struct field *link;         // next field
};

struct symbol {
    const char *name;
    struct type *type;
    struct source src;
    int scope;
    int sclass;
    bool defined;
    bool predefine;
    bool anonymous;
    unsigned int refs;
    union value value;
    struct symbol *link;
    union {
        struct expr *init;               // initializer expr
        // function
        struct {
            struct expr **calls;
            struct stmt *stmt;
        } f;
        // enum/struct/union
        struct {
            struct field *flist; // first field
            struct symbol **ids; // enum ids
        } s;
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
    
struct expr {
    short op;
    bool paren;
    struct type *type;
    struct symbol *sym;
    struct expr *kids[2];
    struct type *vtype;
    union {
        struct expr **args;
        struct init **inits;
        struct field *field;
    } u;
};

// designator id
enum { DESIG_NONE, DESIG_FIELD, DESIG_INDEX };

// designator
struct desig {
    int id;                     // designator id
    int braces;                 // braces count
    struct source src;          // source location
    struct type *type;          // destination type
    long offset;                // destination offset (absolute)
    union {
        struct field *field;    // struct/union field
        long index;             // array index
        const char *name;       // designator identifier
    } u;
    struct desig *prev;
    struct desig *all;
};

// initializer
struct init {
    size_t offset;
    short boff;
    short bsize;
    struct type *type;
    struct expr *body;
};

/// operator
/*
  zzzz zzyy yyxx xxxx

  zzzz-zz: op size
  yy-yy:   op type
  xx-xxxx: op kind
 */
#define OPSIZE(op)    ((op) >> 10)
#define OPTYPE(op)    ((op) & 0x3C0)
#define OPKIND(op)    ((op) & 0x3F)

#define OPINDEX(op)   ((op) & 0x3F)
#define OPID(op)      ((op) & 0x3FF)
#define MKOPSIZE(op)  ((op) << 10)
#define mkop(op, ty)  (op) + ty2op(ty)

// op size
// 1,2,4,8,16

// op type
// integer/unsigned/floating/pointer/struct
enum {
    I = 1 << 6,
    U = 2 << 6,
    F = 3 << 6,
    P = 4 << 6,
    S = 5 << 6
};

// op kind
enum {
    OPNONE,
#define _n(a, _) a,
#include "node.def"
    OPEND
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
    struct list *gotos;
    struct table *labels;
    struct list *calls;
    struct stmt **stmt;
};

// sema actions
struct actions {
    void (*init) (int argc, char *argv[]);
    void (*finalize) (void);

    // decl
    void (*enumdecl) (struct symbol *sym, struct symbol *ids[]);
    void (*recorddecl) (struct symbol *sym);
    struct symbol * (*globaldecl) (const char *, struct type *, int, int, struct source);
    struct symbol * (*localdecl) (const char *, struct type *, int, int, struct source);
    struct symbol * (*paramdecl) (const char *, struct type *, int, int, struct source);
    void (*typedefdecl) (const char *, struct type *, int, int, struct source);
    void (*funcdef) (const char *, struct type *, int, int, struct symbol *[], struct source);

    void (*array_index) (struct type *atype, struct expr *assign, struct source src);
    struct symbol ** (*prototype) (struct type *ftype, struct symbol *params[]);
    struct symbol * (*enum_id) (const char *name, int val, struct symbol *sym, struct source src);
    void (*direct_field) (struct symbol *sym, struct field *field);
    void (*indirect_field) (struct symbol *sym, struct field *field);
    
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

    long (*intexpr) (struct expr *expr, struct type *ty, struct source src);
    struct expr * (*bool_expr) (struct expr *expr, struct source src);
    struct expr * (*switch_expr) (struct expr *expr, struct source src);

    // stmt
    void (*branch) (struct expr *expr, int tlab, int flab);
    void (*jump) (int label);
    void (*ret) (struct expr *expr, bool isnull, struct source src);
    void (*label) (int label);
    void (*gen) (struct expr *expr);

    // init
    void (*element_init) (struct desig **pdesig, struct expr *expr, struct list **plist);
    struct desig * (*designator) (struct desig *desig, struct desig **ds);
    struct expr * (*initializer_list) (struct type *ty, struct init **inits);
};

// metrics
struct metrics {
    unsigned char size, align, rank;
};

// segments
enum { TEXT, BSS, DATA };

// backend interface
struct interface {
    const char *os;
    const char *arch;
    void (*init) (int argc, char *argv[]);
    void (*finalize) (void);
    void (*defvar) (struct symbol *);
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

#define use(sym)  ((sym)->refs++)
#define isindirect(field)  ((field)->indir)
#define direct(field)  (isindirect(field) ? (field)->indir : (field))
#define has_static_extent(sym)  ((sym)->sclass == EXTERN || \
                                 (sym)->sclass == STATIC || \
                                 (sym)->scope == GLOBAL)

// symtab.c
extern struct table *new_table(struct table *up, int scope);
extern void free_table(struct table *t);
extern void symbol_init(void);
extern void enter_scope(void);
extern void exit_scope(void);
extern void foreach(struct table *tp, int level, void (*apply) (struct symbol *, void *), void *context);
extern bool is_current_scope(struct symbol *sym);
// create an anonymous symbol
extern struct symbol *anonymous(struct table **tpp, int scope, int area);
// look up a symbol from this table to previous one, and so on
extern struct symbol *lookup(const char *name, struct table *table);
// install a symbol with specified scope
extern struct symbol *install(const char *name, struct table **tpp, int scope, int area);

/// ast.c
extern struct field *alloc_field(void);
extern struct field *new_indirect_field(struct field *field);
extern struct expr *ast_expr(int op, struct type *ty, struct expr *l, struct expr *r);
extern struct stmt *ast_stmt(int id);
extern const char *gen_tmpname(void);
extern const char *gen_compound_label(void);
extern int genlabel(int count);
extern struct desig *new_desig(int id);
extern struct desig *new_desig_name(const char *name, struct source src);
extern struct desig *new_desig_index(long index, struct source src);
extern struct desig *new_desig_field(struct field *field, struct source src);
extern struct desig *copy_desig(struct desig *desig);

#define isfuncdef(n)   (isfunc((n)->type) && (n)->defined)
#define isvardecl(n)   ((n)->sclass != TYPEDEF && !isfunc((n)->type))
#define isiliteral(n)  (OPID((n)->op) == CNST+I || OPID((n)->op) == CNST+U)
#define isfliteral(n)  (OPID((n)->op) == CNST+F)
#define issliteral(n)  (OPID((n)->op) == CNST+P)

// eval.c
extern struct expr *eval(struct expr *expr, struct type *ty);
 
// expr.c
extern struct expr *expression(void);
extern struct expr *assign_expr(void);
extern long intexpr1(struct type *ty);
extern long intexpr(void);
// for expression in conditional statement
extern struct expr *bool_expr(void);
// for expression in switch statement
extern struct expr *switch_expr(void);

// decl.c
extern void declaration(void);
extern void translation_unit(void);
extern struct type *typename(void);

// init.c
extern struct expr *initializer(struct type *ty);
extern struct expr *initializer_list(struct type *ty);

// stmt.c
extern void compound_stmt(void (*cb) (void), int cnt, int brk, struct swtch *swtch);

// sema.c
extern int first_decl(struct token *t);
extern int first_stmt(struct token *t);
extern int first_expr(struct token *t);
extern int first_typename(struct token *t);

extern void skip_to_brace(void);
extern void skip_to_bracket(void);
extern void skip_to_squarebracket(void);
extern void skip_to_decl(void);
extern void skip_to_stmt(void);
extern void skip_to_expr(void);

extern struct symbol *lookup_typedef(const char *id);
extern bool istypedef(const char *id);

extern struct desig *next_designator(struct desig *desig);

extern struct expr *cnsti(long i, struct type *ty);
extern struct expr *cnsts(const char *string);
extern struct expr *assign(struct symbol *sym, struct expr *r);
extern void check_case_duplicates(struct cse *cse, struct swtch *swtch);
extern void mark_goto(const char *id, struct source src);
extern struct symbol *mklocal(const char *name, struct type *ty, int sclass);

// type.c
extern void type_init(void);
extern struct type *qual(int t, struct type *ty);
extern struct type *unqual(struct type *ty);
extern bool eqtype(struct type *ty1, struct type *ty2);
extern bool eqarith(struct type *ty1, struct type *ty2);
extern struct type *array_type(struct type *ty);
extern struct type *ptr_type(struct type *ty);
extern struct type *func_type(struct type *ty);
extern struct type *tag_type(int t);
extern void set_typesize(struct type *ty);
extern struct field *find_field(struct type *ty, const char *name);
extern struct type *compose(struct type *ty1, struct type *ty2);
extern bool qual_contains(struct type *ty1, struct type *ty2);
extern int qual_union(struct type *ty1, struct type *ty2);
extern bool isincomplete(struct type *ty);
extern bool isstring(struct type *ty);
extern short ty2op(struct type *ty);

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

#define isconst(ty)     isconst1((ty)->kind)
#define isvolatile(ty)  isvolatile1((ty)->kind)
#define isrestrict(ty)  isrestrict1((ty)->kind)
#define isinline(ty)    ((ty)->inlined)
#define isqual(ty)      (isconst(ty) || isvolatile(ty) || isrestrict(ty))

#define isfunc(ty)         (TYPE_OP(ty) == FUNCTION)
#define isarray(ty)        (TYPE_OP(ty) == ARRAY)
#define isptr(ty)          (TYPE_OP(ty) == POINTER)
#define isvoid(ty)         (TYPE_OP(ty) == VOID)
#define isenum(ty)         (TYPE_OP(ty) == ENUM)
#define isstruct(ty)       (TYPE_OP(ty) == STRUCT)
#define isunion(ty)        (TYPE_OP(ty) == UNION)
#define isrecord(ty)       (isstruct(ty) || isunion(ty))
#define istag(ty)          (isstruct(ty) || isunion(ty) || isenum(ty))
#define isint(ty)          (TYPE_OP(ty) == INT || TYPE_OP(ty) == UNSIGNED || isenum(ty))
#define isfloat(ty)        (TYPE_OP(ty) == FLOAT)
#define isarith(ty)        (isint(ty) || isfloat(ty))
#define isscalar(ty)       (isarith(ty) || isptr(ty))
#define isbool(ty)         (unqual(ty) == booltype)
#define isptrto(ty, kind)  (isptr(ty) && TYPE_KIND(rtype(ty)) == kind)

// alias
#define rtype(ty)  TYPE_TYPE(ty)

// error.c
extern unsigned int errors(void);
extern unsigned int warnings(void);
extern void warning_at(struct source src, const char *fmt, ...);
extern void error_at(struct source src, const char *fmt, ...);
extern void fatal_at(struct source src, const char *fmt, ...);

#define warning(...)         warning_at(source, __VA_ARGS__)
#define error(...)           error_at(source, __VA_ARGS__)
#define fatal(...)           fatal_at(source, __VA_ARGS__)

// print.c
extern void ast_dump_vardecl(struct symbol *);
extern void ast_dump_typedecl(struct symbol *);
extern void ast_dump_funcdecl(struct symbol *);
extern void ast_dump_funcdef(struct symbol *);
extern const char *type2s(struct type *ty);
extern const char *desig2s(struct desig *desig);

#define ERR_INCOMPATIBLE_TYPES  "incompatible type conversion from '%s' to '%s'"
#define ERR_REDEFINITION  "redefinition of '%s', previous definition at %s:%u:%u"
#define ERR_CONFLICTING_TYPES  "conflicting types for '%s', previous at %s:%u:%u"
#define ERR_DUPLICATE_MEMBER  "duplicate member '%s', previous declaration at %s:%u:%u"

#define BUILTIN_VA_START    "__builtin_va_start"
#define BUILTIN_VA_ARG_P    "__builtin_va_arg_p"

#define CC_UNAVAILABLE  assert(0 && "unavailable code");


///
/// external variables
///

extern struct options opts;
extern struct func func;
extern struct actions actions;
extern struct interface *IR;
extern struct table *identifiers;
extern struct table *constants;
extern struct table *tags;
extern struct table *globals;
extern struct table *externals;
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
extern struct type *voidptype;
extern struct type *funcptype;

#endif
