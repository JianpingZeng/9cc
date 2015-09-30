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
#define _TYPE_INLINE(NODE)       ((NODE)->type.u.f.inlined)
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
#define _TYPE_A_WILDCARD(NODE)   ((NODE)->type.u.a.wildcard)
#define _TYPE_A_HASEXPR(NODE)    ((NODE)->type.u.a.has_expr)

struct ast_type {
    struct ast_common common;
    int kind;
    size_t size;
    int align;			// align in bytes
    unsigned rank;
    union {
        // function
        struct {
	    bool inlined;
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
            unsigned wildcard : 1;
	    unsigned has_expr : 1;
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
#define SYM_SRC(NODE)           ((NODE)->symbol.src)
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
    bool defined;
    struct source src;
    union value value;
    unsigned refs;
};

#define DECL_SCOPE(NODE)        ((NODE)->decl.scope)
#define DECL_SYM(NODE)          ((NODE)->decl.sym)
#define DECL_BODY(NODE)         ((NODE)->decl.body)
#define DECL_EXTS(NODE)         ((NODE)->decl.exts)

struct ast_decl {
    struct ast_common common;
    int scope;
    node_t *sym;
    node_t *body;
    node_t **exts;
};

// compound stmt
#define STMT_BLKS(NODE)         ((NODE)->stmt.blks)

// if stmt
#define STMT_COND(NODE)         (AST_KID(NODE, 0))
#define STMT_THEN(NODE)         (AST_KID(NODE, 1))
#define STMT_ELSE(NODE)         ((NODE)->stmt.list[0])

// for stmt
#define STMT_CTRL(NODE)         (AST_KID(NODE, 0))
#define STMT_INIT(NODE)         (AST_KID(NODE, 1))
#define STMT_DECL(NODE)         ((NODE)->stmt.blks)

// case stmt
#define STMT_CASE_INDEX(NODE)   ((NODE)->stmt.index)
// label stmt
#define STMT_LABEL_NAME(NODE)   AST_NAME(NODE)

struct ast_stmt {
    struct ast_common common;
    int index;
    node_t **blks;
    node_t *list[1];
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

union ast_node {
    struct ast_common common;
    struct ast_decl decl;
    struct ast_stmt stmt;
    struct ast_expr expr;
    struct ast_type type;
    struct ast_field field;
    struct ast_symbol symbol;
};

// ast.c
extern const char *nname(node_t *node);
extern node_t * ast_expr(int id, int op, node_t *l, node_t *r);
extern node_t * ast_decl(int id, int scope);
extern node_t * ast_stmt(int id, struct source src);
extern node_t * ast_null_stmt(void);
extern node_t * ast_uop(int op, node_t *ty, node_t *l);
extern node_t * ast_bop(int op, node_t *ty, node_t *l, node_t *r);
extern node_t * ast_conv(node_t *ty, node_t *l, const char *name);
extern node_t * ast_inits(void);
extern node_t * ast_vinit(void);
extern const char * gen_label(void);

#define isexpr(n)           (AST_ID(n) > BEGIN_EXPR_ID && AST_ID(n) < END_EXPR_ID)
#define isdecl(n)           (AST_ID(n) > BEGIN_DECL_ID && AST_ID(n) < END_DECL_ID)
#define isstmt(n)           (AST_ID(n) > BEGIN_STMT_ID && AST_ID(n) < END_STMT_ID)
#define istype(n)           (AST_ID(n) == TYPE_NODE)
#define isfield(n)          (AST_ID(n) == FIELD_NODE)
#define issymbol(n)         (AST_ID(n) == SYMBOL_NODE)

#define isfuncdecl(n)       (AST_ID(n) == FUNC_DECL)
#define isfuncdef(n)        (isfuncdecl(n) && DECL_BODY(n) && AST_ID(DECL_BODY(n)) == COMPOUND_STMT)
#define isiliteral(n)       (AST_ID(n) == INTEGER_LITERAL)
#define isfliteral(n)       (AST_ID(n) == FLOAT_LITERAL)
#define issliteral(n)       (AST_ID(n) == STRING_LITERAL)
#define isliteral(n)        (AST_ID(n) > BEGIN_LITERAL_ID && AST_ID(n) < END_LITERAL_ID)
#define is_switch_stmt(n)   (AST_ID(n) == SWITCH_STMT)
#define is_for_stmt(n)      (AST_ID(n) == FOR_STMT)
#define is_while_stmt(n)    (AST_ID(n) == WHILE_STMT)
#define is_dowhile_stmt(n)  (AST_ID(n) == DO_WHILE_STMT)
#define is_iteration_stmt(n) (is_for_stmt(n) || is_while_stmt(n) || is_dowhile_stmt(n))

#define isbitfield(field)    (FIELD_ISBIT(field))

#define BitCast                 "BitCast"
#define LValueToRValue          "LValueToRValue"
#define FunctionToPointerDecay  "FunctionToPointerDecay"
#define ArrayToPointerDecay     "ArrayToPointerDecay"
#define IntegralCast            "IntegralCast"
#define FloatCast               "FloatingCast"
#define IntegerToFloatCast      "IntegralToFloating"
#define FloatToIntegerCast      "FloatingToIntegral"
#define PointerToBoolean        "PointerToBoolean"

#endif
