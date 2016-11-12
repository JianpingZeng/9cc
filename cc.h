#ifndef CC_H
#define CC_H

// for bool
#include <stdbool.h>
// for size_t
#include <stddef.h>
// for FILE *
#include <stdio.h>

#include "config.h"
#include "libutils/utils.h"
#include "libcpp/lex.h"
#include "gen.h"

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
            struct tree *assign;
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
    int string:1;               // string literal
    int compound:1;             // compound literal
    int refs;
    union {
        // varibale initializer
        struct tree *init;
        // literal/enum id
        union {
            union value value;  // integer/floating literal
        } c;
        // function
        struct {
            struct tree *xcall; // call with max parameter size
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
    struct entry {
        struct symbol sym;
        struct entry *link;
    } *buckets[256];
    struct table *up;
    struct symbol *all;
};

// expression tree
struct tree {
    int op;
    struct type *type;
    struct tree *kids[2];
    struct {
        bool paren;
        struct symbol *sym;
        union {
            struct tree **args; // for CALL
            struct init *ilist; // for INITS
            struct field *field; // for BFIELD
        } u;
        union value value;
    } s;
    struct {
        void *state;
    } x;
};

// designator kind
enum { DESIG_NONE, DESIG_FIELD, DESIG_INDEX };

// designator
struct desig {
    int kind;                   // designator kind
    int braces;                 // braces count
    long offset;                // destination offset (absolute)
    struct type *type;          // destination type
    union {
        struct field *field;    // struct/union field
        long index;             // array index
        const char *name;       // designator identifier
    } u;
    struct source src;          // source location
    struct desig *prev;
    struct desig *all;
};

// initializer
struct init {
    struct desig *desig;
    struct tree *body;
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
    RIGHT = 1<<4,
    /// cond
    COND = 2<<4,
    /// constant
    CNST = 3<<4,
    /// address
    ADDRG = 4<<4,
    ADDRP = 5<<4,
    ADDRL = 6<<4,
    /// indirection
    INDIR = 7<<4,
    /// binary
    ASGN = 8<<4,
    MUL = 9<<4,
    DIV = 10<<4,
    ADD = 11<<4,
    SUB = 12<<4,
    MOD = 13<<4,
    SHL = 14<<4,
    SHR = 15<<4,
    BAND = 16<<4,
    BOR = 17<<4,
    XOR = 18<<4,
    EQ = 19<<4,
    NE = 20<<4,
    GT = 21<<4,
    GE = 22<<4,
    LT = 23<<4,
    LE = 24<<4,
    AND = 25<<4,
    OR = 26<<4,
    /// unary
    NEG = 27<<4,
    BNOT = 28<<4,
    /// postfix
    INITS = 29<<4,
    CALL = 30<<4,
    BFIELD = 31<<4,
    /// conversion
    CVI = 32<<4,
    CVU = 33<<4,
    CVF = 34<<4,
    CVP = 35<<4,
};

#define isaddrop(op)  (OPKIND(op) == ADDRL || \
                       OPKIND(op) == ADDRG || \
                       OPKIND(op) == ADDRP)

/// stmt

// stmt id
enum { LABEL = 1, GEN, JMP, CBR, RET };

struct stmt {
    int id;
    union {
        int label;              // LABEL/JMP
        struct tree *expr;      // GEN/RET
        struct {
            struct tree *expr;
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
    struct tree *xcall;
    struct stmt **stmt;
    struct symbol **lvars;
};

typedef struct symbol * (*decl_fp) (const char *, struct type *, int, int,
                                    struct tree *, struct source);

// sema actions
struct actions {
    void (*init) (int, char *[]);
    void (*finalize) (void);

    /// decl
    void (*enumdcl) (struct symbol *, struct symbol *[]);
    void (*structdcl) (struct symbol *);
    void (*tagdcl) (struct type *, int, int, struct source);
    decl_fp globaldcl, localdcl, paramdcl;
    void (*tydefdcl) (const char *, struct type *, int, int, struct source);
    
    void (*arrayidx) (struct type *, struct tree *, struct source);
    struct symbol ** (*prototype) (struct type *, struct symbol *[]);
    struct symbol * (*enumid) (const char *, int, struct symbol *, struct source);
    void (*direct_field) (struct symbol *, struct field *);
    void (*indirect_field) (struct symbol *, struct field *);
    
    /// expr
    struct tree * (*comma) (struct tree *, struct tree *, struct source);
    struct tree * (*assign) (int, struct tree *, struct tree *, struct source);
    struct tree * (*cond) (struct tree *, struct tree *, struct tree *, struct source);
    struct tree * (*bop) (int, struct tree *, struct tree *, struct source);
    struct tree * (*cast) (struct type *, struct tree *, struct source);
    // unary
    struct tree * (*preincr) (int, struct tree *, struct source);
    struct tree * (*minus_plus) (int, struct tree *, struct source);
    struct tree * (*bitnot) (struct tree *, struct source);
    struct tree * (*lognot) (struct tree *, struct source);
    struct tree * (*address) (struct tree *, struct source);
    // postfix
    struct tree * (*indirection) (struct tree *, struct source);
    struct tree * (*sizeofop) (struct type *, struct tree *, struct source);
    struct tree * (*subscript) (struct tree *, struct tree *, struct source);
    struct tree * (*funcall) (struct tree *, struct tree **, struct source);
    struct tree * (*direction) (int, const char *, struct tree *, struct source);
    struct tree * (*postincr) (int, struct tree *, struct source);
    struct tree * (*cpliteral) (struct type *, struct tree *, struct source);
    // primary
    struct tree * (*id) (struct token *);
    struct tree * (*iconst) (struct token *);
    struct tree * (*fconst) (struct token *);
    struct tree * (*sconst) (struct token *);
    struct tree * (*paren) (struct tree *, struct source);

    long (*intexpr) (struct tree *, struct type *, struct source);
    struct tree * (*boolexpr) (struct tree *, struct source);
    struct tree * (*swtchexpr) (struct tree *, struct source);

    /// stmt
    void (*branch) (struct tree *, int, int);
    void (*jump) (int);
    void (*ret) (struct tree *, bool, struct source);
    void (*label) (int);
    void (*gen) (struct tree *);

    /// initializer
    void (*eleminit) (struct desig **, struct tree *, struct init **);
    struct desig * (*designator) (struct desig *, struct desig **);
    struct tree * (*initlist) (struct type *, struct init *);
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
    struct xface x;
};


///
/// external functions
///

#define use(sym)  ((sym)->refs++)
#define deuse(sym)  ((sym)->refs--)
#define isindirect(field)  ((field)->indir)
#define direct(field)  (isindirect(field) ? (field)->indir : (field))
#define has_static_extent(sym)  ((sym)->sclass == EXTERN || \
                                 (sym)->sclass == STATIC || \
                                 (sym)->scope == GLOBAL)

// symtab.c
extern struct table *new_table(struct table *, int);
extern void free_table(struct table *);
extern void symbol_init(void);
extern void enter_scope(void);
extern void exit_scope(void);
extern void foreach(struct table *, int,
                    void (*) (struct symbol *, void *), void *);
extern bool is_current_scope(struct symbol *);
// create an anonymous symbol
extern struct symbol *anonymous(struct table **, int, int);
// create an temporary symbol
extern struct symbol *temporary(int, int);
// look up a symbol from this table to previous one, and so on
extern struct symbol *lookup(const char *, struct table *);
// install a symbol with specified scope
extern struct symbol *install(const char *, struct table **, int, int);

/// ast.c
extern struct field *alloc_field(void);
extern struct field *new_indirect_field(struct field *);
extern struct tree *zinit(struct type *);
extern struct tree *ast_expr(int, struct type *,
                             struct tree *, struct tree *);
extern struct stmt *ast_stmt(int);
extern const char *gen_string_label(void);
extern int genlabel(int);
extern struct desig *new_desig(int);
extern struct desig *new_desig_name(const char *, struct source);
extern struct desig *new_desig_index(long, struct source);
extern struct desig *new_desig_field(struct field *, struct source);
extern struct desig *copy_desig(struct desig *);

#define isfuncdef(n)   (isfunc((n)->type) && (n)->defined)
#define isiliteral(n)  (OPID((n)->op) == CNST+I || OPID((n)->op) == CNST+U)
#define isfliteral(n)  (OPID((n)->op) == CNST+F)
#define ispliteral(n)  (OPID((n)->op) == CNST+P)
#define issliteral(n)  (OPID((n)->op) == ADDRG+P && \
                        isarray((n)->type) && \
                        (n)->s.sym->string)
#define iszinit(n)     ((n)->op == 0)
// compound literal
#define iscpliteral(n)  (OPKIND((n)->op) == INDIR && \
                         isaddrop((n)->kids[0]->op) &&  \
                         (n)->kids[0]->s.sym->compound)
#define COMPOUND_SYM(n)  ((n)->kids[0]->s.sym)

// tree.c
extern struct tree *root(struct tree *);
extern struct tree *addrof(struct tree *);
extern struct tree *rightkid(struct tree *);

// eval.c
extern struct tree *eval(struct tree *, struct type *);
extern struct tree *fold(int, struct type *, struct tree *, struct tree *);
 
// parser.c
extern void translation_unit(void);
extern void compound_stmt(void (*) (void), int, int, struct swtch *);

// sema.c
extern int first_decl(struct token *);
extern int first_stmt(struct token *);
extern int first_expr(struct token *);
extern int first_typename(struct token *);

extern void skip_to_brace(void);
extern void skip_to_bracket(void);
extern void skip_to_squarebracket(void);
extern void skip_to_decl(void);
extern void skip_to_stmt(void);
extern void skip_to_expr(void);

extern struct symbol *tag_symbol(int, const char *, struct source);
extern struct desig *next_designator(struct desig *, int);
extern struct tree *cnsti(long, struct type *);
extern struct tree *cnsts(const char *);
extern void check_case_duplicates(struct cse *, struct swtch *);
extern void mark_goto(const char *, struct source);
extern struct tree *mkref(struct symbol *);
extern void funcdef(const char *, struct type *, int, int,
                    struct symbol *[], struct source);

// type.c
extern void type_init(void);
extern struct type *qual(int, struct type *);
extern struct type *unqual(struct type *);
extern bool compatible(struct type *, struct type *);
extern bool eqtype(struct type *, struct type *);
extern bool eqarith(struct type *, struct type *);
extern struct type *array_type(struct type *);
extern struct type *ptr_type(struct type *);
extern struct type *func_type(struct type *);
extern struct type *tag_type(int);
extern void set_typesize(struct type *);
extern struct field *find_field(struct type *, const char *);
extern struct type *compose(struct type *, struct type *);
extern bool qual_contains(struct type *, struct type *);
extern int qual_union(struct type *, struct type *);
extern bool isincomplete(struct type *);
extern bool isstring(struct type *);
extern short ty2op(struct type *);

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
#define isint(ty)          (TYPE_OP(ty) == INT ||       \
                            TYPE_OP(ty) == UNSIGNED ||  \
                            isenum(ty))
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
extern void warning_at(struct source, const char *, ...);
extern void error_at(struct source, const char *, ...);
extern void fatal_at(struct source, const char *, ...);

#define warning(...)         warning_at(source, __VA_ARGS__)
#define error(...)           error_at(source, __VA_ARGS__)
#define fatal(...)           fatal_at(source, __VA_ARGS__)

// print.c
extern void ast_dump_vardecl(struct symbol *);
extern void ast_dump_typedecl(struct symbol *);
extern void ast_dump_funcdecl(struct symbol *);
extern void ast_dump_funcdef(struct symbol *);
extern void print_expr(FILE *, struct tree *);
extern void vfprint(FILE *, const char *, va_list);
extern void fprint(FILE *, const char *, ...);
extern void print(const char *, ...);

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
extern struct type *unsignedptrtype;
extern struct type *signedptrtype;

#endif
