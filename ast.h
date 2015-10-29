#ifndef _AST_H
#define _AST_H

// node ids
enum {
#define _ns(a)   a,
#define _n(a, b) a,
#include "node.def"
};

/**
 * This is the _ONLY_ typedef used.
 *
 * The coding style tends to avoid typedefs, because
 * typedefs reduce readability, but it's not a hard rule,
 * I don't mind a few of them.
 *
 * The fields of ast_node are designed to be accessed _ONLY_
 * via macros, so it's useful to use typedef to hide the
 * implementation details.
 */
typedef union ast_node node_t;

#define AST_ID(NODE)            ((NODE)->common.id)
#define AST_NAME(NODE)          ((NODE)->common.name)
#define AST_TYPE(NODE)          ((NODE)->common.type)
#define AST_KID(NODE, I)        ((NODE)->common.kids[I])
#define AST_SRC(NODE)           ((NODE)->common.src)

struct ast_common {
    int id;
    const char *name;
    node_t *type;
    node_t *kids[2];
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
#define _TYPE_PARAMS(NODE)       ((NODE)->type.u.f.params)
#define _TYPE_OLDSTYLE(NODE)     ((NODE)->type.u.f.oldstyle)
#define _TYPE_TSYM(NODE)         ((NODE)->type.u.s.tsym)
#define _TYPE_IDS(NODE)          ((NODE)->type.u.s.ids)
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
    unsigned align;			// align in bytes
    unsigned rank : 8;
    bool inlined : 1;
    union {
        // function
        struct {
            node_t **params;
            unsigned oldstyle : 1;
        }f;
        // enum/struct/union
        struct {
	    const char *tag;
	    node_t *tsym;
            node_t **ids;
            node_t **fields;
        }s;
	// array
        struct {
	    size_t len;			// array length
            node_t *assign;
            unsigned is_const : 1;
            unsigned is_volatile : 1;
            unsigned is_restrict : 1;
            unsigned is_static : 1;
            unsigned star : 1;
        }a;
    }u;
    struct {
        union value max;
        union value min;
    }limits;
};

#define FIELD_NAME(NODE)        AST_NAME(NODE)
#define FIELD_TYPE(NODE)        AST_TYPE(NODE)
#define FIELD_ISBIT(NODE)       ((NODE)->field.isbit)
#define FIELD_OFFSET(NODE)      ((NODE)->field.offset)
#define FIELD_BITSIZE(NODE)     ((NODE)->field.bitsize)

struct ast_field {
    struct ast_common common;
    bool isbit;
    int offset;
    int bitsize;
};

#define SYM_SCOPE(NODE)         ((NODE)->symbol.scope)
#define SYM_NAME(NODE)          AST_NAME(NODE)
#define SYM_SCLASS(NODE)        ((NODE)->symbol.sclass)
#define SYM_TYPE(NODE)          AST_TYPE(NODE)
#define SYM_DEFINED(NODE)       ((NODE)->symbol.defined)
#define SYM_VALUE(NODE)         ((NODE)->symbol.value)
#define SYM_REFS(NODE)          ((NODE)->symbol.refs)
// convenience
#define SYM_VALUE_U(NODE)       (VALUE_U(SYM_VALUE(NODE)))
#define SYM_VALUE_I(NODE)       (VALUE_I(SYM_VALUE(NODE)))
#define SYM_VALUE_D(NODE)       (VALUE_D(SYM_VALUE(NODE)))

struct ast_symbol {
    struct ast_common common;
    int scope;
    int sclass;
    bool defined : 1;
    struct source src;
    union value value;
    unsigned refs;
};

#define DECL_SCOPE(NODE)        ((NODE)->decl.scope)
#define DECL_SYM(NODE)          ((NODE)->decl.sym)
#define DECL_BODY(NODE)         ((NODE)->decl.body)
#define DECL_EXTS(NODE)         ((NODE)->decl.exts)
#define DECL_VARS(NODE)         ((NODE)->decl.exts)

struct ast_decl {
    struct ast_common common;
    int scope;
    node_t *sym;
    node_t *body;
    node_t **exts;
};

#define EXPR_OP(NODE)           ((NODE)->expr.op)
#define EXPR_PREFIX(NODE)       ((NODE)->expr.prefix)
#define EXPR_OPERAND0(NODE)     (AST_KID(NODE, 0))
#define EXPR_OPERAND1(NODE)     (AST_KID(NODE, 1))
#define EXPR_OPERAND2(NODE)     ((NODE)->expr.operands[0])
#define EXPR_OPERAND(NODE, I)   EXPR_OPERAND##I(NODE)
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

struct ast_expr {
    struct ast_common common;
    int op;
    bool prefix;
    node_t *sym;
    node_t *operands[1];
    node_t **list;
};

// compound stmt
#define STMT_BLKS(NODE)         ((NODE)->stmt.blks)
// case
#define STMT_LABEL(NODE)        AST_NAME(NODE)
#define STMT_CASE_INDEX(NODE)   ((NODE)->stmt.index)
#define STMT_CASE_BODY(NODE)    AST_KID(NODE, 0)
// for
#define STMT_FOR_DECL(NODE)     ((NODE)->stmt.blks)
#define STMT_FOR_INIT(NODE)     AST_KID(NODE, 0)
#define STMT_FOR_COND(NODE)     AST_KID(NODE, 1)
#define STMT_FOR_CTRL(NODE)     ((NODE)->stmt.operands[0])
#define STMT_FOR_BODY(NODE)     ((NODE)->stmt.operands[1])
// if
#define STMT_IF_COND(NODE)      AST_KID(NODE, 0)
#define STMT_IF_THEN(NODE)      AST_KID(NODE, 1)
#define STMT_IF_ELSE(NODE)      ((NODE)->stmt.operands[0])
// while
#define STMT_WHILE_COND(NODE)   AST_KID(NODE, 0)
#define STMT_WHILE_BODY(NODE)   AST_KID(NODE, 1)
// switch
#define STMT_SWITCH_EXPR(NODE)  AST_KID(NODE, 0)
#define STMT_SWITCH_BODY(NODE)  AST_KID(NODE, 1)
// return
#define STMT_RETURN_BODY(NODE)  AST_KID(NODE, 0)
// gen
#define STMT_GEN(NODE)          ((NODE)->stmt.gen)

struct ast_stmt {
    struct ast_common common;
    int index;
    node_t *operands[2];
    node_t **blks;
    node_t *gen;
};

#define GEN_OPERAND(NODE)       ((NODE)->gen.operand)
#define GEN_COND(NODE)          GEN_OPERAND(NODE)
#define GEN_THEN(NODE)          AST_KID(NODE, 0)
#define GEN_ELSE(NODE)          AST_KID(NODE, 1)
#define GEN_LABEL(NODE)         AST_NAME(NODE)
#define GEN_LIST(NODE)          ((NODE)->gen.list)

struct ast_gen {
    struct ast_common common;
    node_t *operand;
    node_t **list;
};

union ast_node {
    struct ast_common common;
    struct ast_decl   decl;
    struct ast_stmt   stmt;
    struct ast_expr   expr;
    struct ast_type   type;
    struct ast_field  field;
    struct ast_symbol symbol;
    struct ast_gen    gen;
};

// ast.c
extern void * alloc_symbol(void);
extern void * alloc_type(void);
extern void * alloc_field(void);

extern const char *nname(node_t *node);
// decl
extern node_t * ast_decl(int id, int scope);
// stmt
extern node_t * ast_stmt(int id, struct source src, node_t *gen);
extern node_t * ast_null_stmt(void);
// expr
extern node_t * ast_expr(int id, node_t *ty, node_t *l, node_t *r);
extern node_t * ast_uop(int op, node_t *ty, node_t *l);
extern node_t * ast_bop(int op, node_t *ty, node_t *l, node_t *r);
extern node_t * ast_conv(node_t *ty, node_t *l, const char *name);
extern node_t * ast_inits(void);
extern node_t * ast_vinit(void);
// gen
extern const char * gen_label(void);
extern const char * gen_tmpname(void);
extern node_t * ast_if(node_t *cond, node_t *then, node_t *els);
extern node_t * ast_jump(const char *label);
extern node_t * ast_label(const char *label);
extern node_t * ast_return(node_t *node);
extern node_t * ast_compound(node_t **list);
extern node_t * ast_gen(node_t *node);

extern node_t * copy_node(node_t *node);

// kind
#define isexpr(n)           (AST_ID(n) > BEGIN_EXPR_ID && AST_ID(n) < END_EXPR_ID)
#define isdecl(n)           (AST_ID(n) > BEGIN_DECL_ID && AST_ID(n) < END_DECL_ID)
#define isstmt(n)           (AST_ID(n) > BEGIN_STMT_ID && AST_ID(n) < END_STMT_ID)
#define istype(n)           (AST_ID(n) == TYPE_NODE)
#define isfield(n)          (AST_ID(n) == FIELD_NODE)
#define issymbol(n)         (AST_ID(n) == SYMBOL_NODE)
#define isgen(n)            (AST_ID(n) > BEGIN_GEN_ID && AST_ID(n) < END_GEN_ID)

// decl
#define istudecl(n)         (AST_ID(n) == TU_DECL)
#define isfuncdecl(n)       (AST_ID(n) == FUNC_DECL)
#define isfuncdef(n)        (isfuncdecl(n) && DECL_BODY(n) && AST_ID(DECL_BODY(n)) == COMPOUND_STMT)
#define isvardecl(n)        (AST_ID(n) == VAR_DECL)
#define istypedefdecl(n)    (AST_ID(n) == TYPEDEF_DECL)
#define isenumdecl(n)       (AST_ID(n) == ENUM_DECL)
#define isstructdecl(n)     (AST_ID(n) == STRUCT_DECL)
#define isuniondecl(n)      (AST_ID(n) == UNION_DECL)

// expr
#define isiliteral(n)       (AST_ID(n) == INTEGER_LITERAL)
#define isfliteral(n)       (AST_ID(n) == FLOAT_LITERAL)
#define issliteral(n)       (AST_ID(n) == STRING_LITERAL)

// stmt
#define isnullstmt(n)     (AST_ID(n) == NULL_STMT)

#define isbitfield(field)   (FIELD_ISBIT(field))

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
