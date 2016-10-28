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
// pointer
#define TYPE_P_DECAY(ty)         (unqual(ty)->u.p.decay)

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
        // pointer
        struct {
            struct type *decay; // original type in parameter
        } p;
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
    bool isbit;
    int bitsize;
    int bitoff;
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
    int defined:1;
    int predefine:1;
    int anonymous:1;
    int temporary:1;
    int nonnull:1;
    int literal:1;
    unsigned int refs;
    union {
        // varibale initializer
        struct expr *init;
        // literal/enum id
        union {
            struct symbol *cnst;  // string literal
            union value value;   // integer/floating literal
        } c;
        // function
        struct {
            struct expr *xcall; // call with max parameter size
            struct stmt *stmt;
            struct symbol *lvars; // all local vars
        } f;
        // enum/struct/union
        struct {
            struct field *flist; // first field
            struct symbol **ids; // enum ids
        } s;
    } u;
    struct {
        const char *name;
        int label;              // for goto labels
    } x;
    struct symbol *link;        // link in symtab
    struct symbol *local;       // link of local vars
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
    int op;
    struct type *type;
    struct expr *kids[2];
    struct {
        bool paren;
        struct symbol *sym;
        union {
            struct expr **args; // for CALL
            struct init *ilist; // for COMPOUND
            struct field *field; // for BFIELD
        } u;
        union value value;
    } x;
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
    struct init *link;
};

/// operator
/*
  zzzz zzxx xxxx yyyy

  zzzz-zz: op size
  xx-xxxx: op kind
  yy-yy:   op type
 */
#define OPSIZE(op)    ((op) >> 10)
#define OPKIND(op)    ((op) & 0x3F0)
#define OPTYPE(op)    ((op) & 0xF)

#define OPINDEX(op)   (((op) >> 4) & 0x3F)
#define OPID(op)      ((op) & 0x3FF)
#define MKOPSIZE(op)  ((op) << 10)
#define mkop(op, ty)  OPID((op) + ty2op(ty))
#define MKINDEX(i)    ((i) << 4)

/// op size
// 1,2,4,8,16

/// op type
// I - integer
// U - unsigned
// F - floating
// P - pointer
// S - struct/union/array
enum { I = 1, U, F, P, S };

// op kind
enum {
    /// comma
    RIGHT = MKINDEX(1),
    /// cond
    COND = MKINDEX(2),
    /// constant
    CNST = MKINDEX(3),
    /// address
    ADDRG = MKINDEX(4),
    ADDRP = MKINDEX(5),
    ADDRL = MKINDEX(6),
    /// indirection
    INDIR = MKINDEX(7),
    /// binary
    ASGN = MKINDEX(8),
    MUL = MKINDEX(9),
    DIV = MKINDEX(10),
    ADD = MKINDEX(11),
    SUB = MKINDEX(12),
    MOD = MKINDEX(13),
    SHL = MKINDEX(14),
    SHR = MKINDEX(15),
    BAND = MKINDEX(16),
    BOR = MKINDEX(17),
    XOR = MKINDEX(18),
    EQ = MKINDEX(19),
    NE = MKINDEX(20),
    GT = MKINDEX(21),
    GE = MKINDEX(22),
    LT = MKINDEX(23),
    LE = MKINDEX(24),
    AND = MKINDEX(25),
    OR = MKINDEX(26),
    /// unary
    NEG = MKINDEX(27),
    BNOT = MKINDEX(28),
    /// postfix
    COMPOUND = MKINDEX(29),
    CALL = MKINDEX(30),
    BFIELD = MKINDEX(31),
    /// conversion
    CVI = MKINDEX(32),
    CVU = MKINDEX(33),
    CVF = MKINDEX(34),
    CVP = MKINDEX(35),
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

/// goto info
struct goinfo {
    const char *id;
    struct source src;
    struct goinfo *link;
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
    struct goinfo *gotos;
    struct table *labels;
    struct expr *xcall;
    struct stmt **stmt;
    struct symbol **lvars;
};

typedef struct symbol * (*decl_fp) (const char *, struct type *, int, int, struct expr *, struct source);

// sema actions
struct actions {
    void (*init) (int argc, char *argv[]);
    void (*finalize) (void);

    /// decl
    void (*enumdecl) (struct symbol *, struct symbol *ids[]);
    void (*recorddecl) (struct symbol *);
    void (*tagdecl) (struct type *, int sclass, int fspec, struct source);
    decl_fp globaldecl, localdecl, paramdecl;
    void (*typedefdecl) (const char *, struct type *, int, int, struct source);
    
    void (*array_index) (struct type *aty, struct expr *assign, struct source);
    struct symbol ** (*prototype) (struct type *fty, struct symbol *params[]);
    struct symbol * (*enum_id) (const char *name, int val, struct symbol *sym, struct source);
    void (*direct_field) (struct symbol *sym, struct field *field);
    void (*indirect_field) (struct symbol *sym, struct field *field);
    
    /// expr
    struct expr * (*comma) (struct expr *l, struct expr *r, struct source);
    struct expr * (*assign) (int t, struct expr *l, struct expr *r, struct source);
    struct expr * (*cond) (struct expr *cond, struct expr *then, struct expr *els, struct source);
    struct expr * (*logical) (int t, struct expr *l, struct expr *r, struct source);
    struct expr * (*bop) (int t, struct expr *l, struct expr *r, struct source);
    struct expr * (*cast) (struct type *ty, struct expr *, struct source);
    // unary
    struct expr * (*pre_increment) (int t, struct expr *, struct source);
    struct expr * (*minus_plus) (int t, struct expr *, struct source);
    struct expr * (*bitwise_not) (struct expr *, struct source);
    struct expr * (*logical_not) (struct expr *, struct source);
    struct expr * (*address) (struct expr *, struct source);
    // postfix
    struct expr * (*indirection) (struct expr *, struct source);
    struct expr * (*sizeofop) (struct type *ty, struct expr *, struct source);
    struct expr * (*subscript) (struct expr *base, struct expr *index, struct source);
    struct expr * (*funcall) (struct expr *expr, struct expr **args, struct source);
    struct expr * (*direction) (int t, const char *name, struct expr *, struct source);
    struct expr * (*post_increment) (int t, struct expr *, struct source);
    struct expr * (*compound_literal) (struct type *ty, struct expr *init, struct source);
    // primary
    struct expr * (*id) (struct token *);
    struct expr * (*iconst) (struct token *);
    struct expr * (*fconst) (struct token *);
    struct expr * (*sconst) (struct token *);
    struct expr * (*paren) (struct expr *node, struct source);

    long (*intexpr) (struct expr *expr, struct type *ty, struct source);
    struct expr * (*bool_expr) (struct expr *expr, struct source);
    struct expr * (*switch_expr) (struct expr *expr, struct source);

    /// stmt
    void (*branch) (struct expr *expr, int tlab, int flab);
    void (*jump) (int label);
    void (*ret) (struct expr *expr, bool isnull, struct source);
    void (*label) (int label);
    void (*gen) (struct expr *expr);

    /// initializer
    void (*element_init) (struct desig **pdesig, struct expr *expr, struct init **pinit);
    struct desig * (*designator) (struct desig *desig, struct desig **ds);
    struct expr * (*initializer_list) (struct type *ty, struct init *ilist);
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
#define unuse(sym)  ((sym)->refs--)
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
extern struct expr *zinit(struct type *ty);
extern struct expr *ast_expr(int op, struct type *ty, struct expr *l, struct expr *r);
extern struct stmt *ast_stmt(int id);
extern const char *gen_tmpname(void);
extern const char *gen_compound_label(void);
extern const char *gen_string_label(void);
extern int genlabel(int count);
extern struct desig *new_desig(int id);
extern struct desig *new_desig_name(const char *name, struct source src);
extern struct desig *new_desig_index(long index, struct source src);
extern struct desig *new_desig_field(struct field *field, struct source src);
extern struct desig *copy_desig(struct desig *desig);

#define isfuncdef(n)   (isfunc((n)->type) && (n)->defined)
#define isiliteral(n)  (OPID((n)->op) == CNST+I || OPID((n)->op) == CNST+U)
#define isfliteral(n)  (OPID((n)->op) == CNST+F)
#define ispliteral(n)  (OPID((n)->op) == CNST+P)
#define issliteral(n)  ((OPID((n)->op) == ADDRG+P) && (n)->x.sym->literal)
#define iszinit(n)     ((n)->op == 0)

// tree.c
extern struct expr *reduce(struct expr *expr);
extern struct expr *addrof(struct expr *expr);

// eval.c
extern struct expr *eval(struct expr *expr, struct type *ty);
extern struct expr *simplify(int op, struct type *ty, struct expr *l, struct expr *r);
 
// parser.c
extern void translation_unit(void);
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

extern struct symbol *tag_symbol(int t, const char *tag, struct source src);
extern struct symbol *lookup_typedef(const char *id);
extern bool istypedef(const char *id);
extern struct desig *next_designator(struct desig *desig);
extern struct expr *cnsti(long i, struct type *ty);
extern struct expr *cnsts(const char *string);
extern void check_case_duplicates(struct cse *cse, struct swtch *swtch);
extern void mark_goto(const char *id, struct source src);
extern struct expr *mkref(struct symbol *sym);
extern void funcdef(const char *, struct type *, int, int, struct symbol *[], struct source);

// type.c
extern void type_init(void);
extern struct type *qual(int t, struct type *ty);
extern struct type *unqual(struct type *ty);
extern bool compatible(struct type *ty1, struct type *ty2);
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
extern const char *type2s(struct type *);
extern const char *desig2s(struct desig *);

#define BUILTIN_VA_START    "__builtin_va_start"
#define BUILTIN_VA_ARG_P    "__builtin_va_arg_p"

#define CC_UNAVAILABLE()  assert(0 && "unavailable code")


///
/// external variables
///

extern struct options opts;
extern struct func func;
extern struct actions actions;
extern struct interface *IR;
extern struct table *identifiers;
extern struct table *strings;
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
extern struct type *unsignedptrtype;
extern struct type *signedptrtype;

#endif
