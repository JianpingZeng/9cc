#ifndef _AST_H
#define _AST_H

// node ids
enum {
#define _ns(a)   a,
#define _n(a, b) a,
#include "node.def"
};

#define AST_ID(NODE)            ((NODE)->common.id)
#define AST_NAME(NODE)          ((NODE)->common.name)
#define AST_TYPE(NODE)          ((NODE)->common.type)
#define AST_SRC(NODE)           ((NODE)->common.src)

struct ast_common {
    int id;
    const char *name;
    node_t *type;
    struct source src;
};

/* Handle carefully for qual/unqual types.
 *
 * macros begin with '_' is for 'atom' access,
 * others use the unqual version of the type.
 * See cc.h for more details.
 */
#define _TYPE_KIND(NODE)         ((NODE)->type.kind)
#define _TYPE_NAME(NODE)         AST_NAME(NODE)
#define _TYPE_SIZE(NODE)         ((NODE)->type.size)
#define _TYPE_ALIGN(NODE)        ((NODE)->type.align)
#define _TYPE_LEN(NODE)          ((NODE)->type.u.a.len)
#define _TYPE_RANK(NODE)         ((NODE)->type.rank)
#define _TYPE_INLINE(NODE)       ((NODE)->type.inlined)
#define _TYPE_TYPE(NODE)         AST_TYPE(NODE)
#define _TYPE_TAG(NODE)          ((NODE)->type.u.s.tag)
#define _TYPE_PROTO(NODE)        ((NODE)->type.u.f.proto)
#define _TYPE_PARAMS(NODE)       ((NODE)->type.u.f.params)
#define _TYPE_OLDSTYLE(NODE)     ((NODE)->type.u.f.oldstyle)
#define _TYPE_VARG(NODE)         ((NODE)->type.u.f.varg)
#define _TYPE_TSYM(NODE)         ((NODE)->type.u.s.tsym)
#define _TYPE_FIELDS(NODE)       ((NODE)->type.u.s.fields)
#define _TYPE_LIMITS_MAX(NODE)   ((NODE)->type.limits.max)
#define _TYPE_LIMITS_MIN(NODE)   ((NODE)->type.limits.min)
#define _TYPE_A_ASSIGN(NODE)     ((NODE)->type.u.a.assign)
#define _TYPE_A_CONST(NODE)      ((NODE)->type.u.a.is_const)
#define _TYPE_A_VOLATILE(NODE)   ((NODE)->type.u.a.is_volatile)
#define _TYPE_A_RESTRICT(NODE)   ((NODE)->type.u.a.is_restrict)
#define _TYPE_A_STATIC(NODE)     ((NODE)->type.u.a.is_static)
#define _TYPE_A_STAR(NODE)       ((NODE)->type.u.a.star)

struct ast_type {
    struct ast_common common;
    int kind;
    size_t size;
    unsigned align;                // align in bytes
    unsigned rank:8;
    unsigned inlined:1;
    union {
        // function
        struct {
            node_t **proto;
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

#define FIELD_NAME(NODE)        AST_NAME(NODE)
#define FIELD_TYPE(NODE)        AST_TYPE(NODE)
#define FIELD_ISBIT(NODE)       ((NODE)->field.isbit)
#define FIELD_OFFSET(NODE)      ((NODE)->field.offset)
#define FIELD_BITSIZE(NODE)     ((NODE)->field.bitsize)
#define FIELD_BITOFF(NODE)      ((NODE)->field.bitoff)

struct ast_field {
    struct ast_common common;
    size_t offset;
    int isbit : 1;
    int bitsize : 10;
    int bitoff : 10;
};

#define SYM_SCOPE(NODE)       ((NODE)->symbol.scope)
#define SYM_NAME(NODE)        AST_NAME(NODE)
#define SYM_SCLASS(NODE)      ((NODE)->symbol.sclass)
#define SYM_TYPE(NODE)        AST_TYPE(NODE)
#define SYM_DEFINED(NODE)     ((NODE)->symbol.defined)
#define SYM_PREDEFINE(NODE)   ((NODE)->symbol.predefine)
#define SYM_VALUE(NODE)       ((NODE)->symbol.value)
#define SYM_REFS(NODE)        ((NODE)->symbol.refs)
#define SYM_LINK(NODE)        ((NODE)->symbol.link)
// convenience
#define SYM_VALUE_U(NODE)     (VALUE_U(SYM_VALUE(NODE)))
#define SYM_VALUE_I(NODE)     (VALUE_I(SYM_VALUE(NODE)))
#define SYM_VALUE_D(NODE)     (VALUE_D(SYM_VALUE(NODE)))

struct ast_symbol {
    struct ast_common common;
    int scope;
    int sclass;
    unsigned defined : 1;
    unsigned predefine : 1;
    union value value;
    unsigned refs;
    node_t *link;
    union x x;
};

#define DECL_SYM(NODE)          ((NODE)->decl.sym)
#define DECL_BODY(NODE)         ((NODE)->decl.body)
#define DECL_EXTS(NODE)         ((NODE)->decl.exts)

struct ast_decl {
    struct ast_common common;
    node_t *sym;                // the symbol
    node_t *body;                // the initializer expr or func body
    node_t **exts;
    union x x;
};

#define EXPR_OP(NODE)           ((NODE)->expr.op)
#define EXPR_PREFIX(NODE)       ((NODE)->expr.prefix)
#define EXPR_OPERAND(NODE, I)   ((NODE)->expr.operands[I])
#define EXPR_ARGS(NODE)         ((NODE)->expr.list)
#define EXPR_INITS(NODE)        ((NODE)->expr.list)
#define EXPR_SYM(NODE)          ((NODE)->expr.sym)
// conditional expr
#define EXPR_COND(NODE)         EXPR_OPERAND(NODE, 0)
#define EXPR_THEN(NODE)         EXPR_OPERAND(NODE, 1)
#define EXPR_ELSE(NODE)         EXPR_OPERAND(NODE, 2)
// literal
#define ILITERAL_VALUE(NODE)    (SYM_VALUE_U(EXPR_SYM(NODE)))
#define FLITERAL_VALUE(NODE)    (SYM_VALUE_D(EXPR_SYM(NODE)))
// va_arg
#define EXPR_VA_ARG_TYPE(NODE)  EXPR_OPERAND(NODE, 1)
    
struct ast_expr {
    struct ast_common common;
    int op;
    bool prefix;
    node_t *sym;
    node_t *operands[3];
    node_t **list;
    union x x;
};

// compound stmt
#define STMT_BLKS(NODE)    ((NODE)->stmt.blks)

// if stmt
#define STMT_COND(NODE)    ((NODE)->stmt.list[0])
#define STMT_THEN(NODE)    ((NODE)->stmt.list[1])
#define STMT_ELSE(NODE)    ((NODE)->stmt.list[2])

// for stmt
#define STMT_FOR_INIT(NODE)    ((NODE)->stmt.list[0])
#define STMT_FOR_DECL(NODE)    ((NODE)->stmt.blks)
#define STMT_FOR_COND(NODE)    ((NODE)->stmt.list[1])
#define STMT_FOR_CTRL(NODE)    ((NODE)->stmt.list[2])
#define STMT_FOR_BODY(NODE)    ((NODE)->stmt.list[3])

// case stmt
#define STMT_CASE_INDEX(NODE)    ((NODE)->stmt.index)
#define STMT_CASE_BODY(NODE)     ((NODE)->stmt.list[0])
#define STMT_CASE_NAME(NODE)     AST_NAME(NODE)

// switch stmt
#define STMT_SWITCH_EXPR(NODE)     ((NODE)->stmt.list[0])
#define STMT_SWITCH_BODY(NODE)     ((NODE)->stmt.list[1])
#define STMT_SWITCH_CASES(NODE)    ((NODE)->stmt.blks)
#define STMT_SWITCH_DEFAULT(NODE)  ((NODE)->stmt.list[2])

// label stmt
#define STMT_LABEL_NAME(NODE)   AST_NAME(NODE)
#define STMT_LABEL_BODY(NODE)   ((NODE)->stmt.list[0])
#define STMT_LABEL_REFS(NODE)   ((NODE)->stmt.index)

// while stmt
#define STMT_WHILE_COND(NODE)   ((NODE)->stmt.list[0])
#define STMT_WHILE_BODY(NODE)   ((NODE)->stmt.list[1])

// return stmt
#define STMT_RETURN_EXPR(NODE)  ((NODE)->stmt.list[0])

// expr stmt
#define STMT_EXPR_BODY(NODE)    ((NODE)->stmt.list[0])

struct ast_stmt {
    struct ast_common common;
    long index;
    node_t **blks;
    node_t *list[4];
    union x x;
};

union ast_node {
    struct ast_common common;
    struct ast_decl decl;
    struct ast_expr expr;
    struct ast_stmt stmt;
    struct ast_type type;
    struct ast_field field;
    struct ast_symbol symbol;
};

// ast.c
extern void *alloc_symbol(void);
extern void *alloc_type(void);
extern void *alloc_field(void);

extern const char *nname(node_t * node);
// decl
extern node_t *ast_decl(int id);
// expr
extern node_t *ast_expr(int id, node_t * ty, node_t * l, node_t * r);
extern node_t *ast_uop(int op, node_t * ty, node_t * l);
extern node_t *ast_bop(int op, node_t * ty, node_t * l, node_t * r);
extern node_t *ast_conv(node_t * ty, node_t * l, const char *name);
extern node_t *ast_inits(node_t * ty, struct source src);
extern node_t *ast_vinit(void);
// stmt
extern node_t *ast_stmt(int id, struct source src);

extern const char *gen_label(void);
extern const char *gen_tmpname(void);
extern const char *gen_tmpname_r(void);
extern const char *gen_static_label(void);
extern const char *gen_compound_label(void);
extern const char *gen_sliteral_label(void);
extern const char *gen_block_label(void);

extern node_t *copy_node(node_t * node);

// kind
#define isexpr(n)   (AST_ID(n) > BEGIN_EXPR_ID && AST_ID(n) < END_EXPR_ID)
#define isdecl(n)   (AST_ID(n) > BEGIN_DECL_ID && AST_ID(n) < END_DECL_ID)
#define isstmt(n)   (AST_ID(n) > BEGIN_STMT_ID && AST_ID(n) < END_STMT_ID)
#define istype(n)   (AST_ID(n) == TYPE_NODE)
#define isfield(n)  (AST_ID(n) == FIELD_NODE)
#define issymbol(n) (AST_ID(n) == SYMBOL_NODE)

// decl
#define istudecl(n)    (AST_ID(n) == TU_DECL)
#define isfuncdecl(n)  (AST_ID(n) == FUNC_DECL)
#define isfuncdef(n)   (isfuncdecl(n) && DECL_BODY(n))
#define isvardecl(n)   (AST_ID(n) == VAR_DECL)

// expr
#define isiliteral(n)  (AST_ID(n) == INTEGER_LITERAL)
#define isfliteral(n)  (AST_ID(n) == FLOAT_LITERAL)
#define issliteral(n)  (AST_ID(n) == STRING_LITERAL)

// stmt
#define isnullstmt(n)  (AST_ID(n) == NULL_STMT)

// cast name
#define BitCast                 "BitCast"
#define LValueToRValue          "LValueToRValue"
#define FunctionToPointerDecay  "FunctionToPointerDecay"
#define ArrayToPointerDecay     "ArrayToPointerDecay"
#define IntegralCast            "IntegralCast"
#define FloatCast               "FloatingCast"
#define IntegerToFloatCast      "IntegralToFloating"
#define FloatToIntegerCast      "FloatingToIntegral"
#define PointerToBoolean        "PointerToBoolean"
#define IntegerToPointerCast    "IntegerToPointer"
#define PointerToIntegerCast    "PointerToInteger"

#endif
