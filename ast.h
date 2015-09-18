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
#define AST_KID(NODE, I)        ((NODE)->common.kids[I])

struct ast_common {
    int id;
    const char *name;
    union node *type;
    union node *kids[2];
};

#define TYPE_KIND(NODE)         ((NODE)->type.kind)
#define TYPE_NAME(NODE)         ((NODE)->type.name)
#define TYPE_SIZE(NODE)         ((NODE)->type.size)
#define TYPE_RANK(NODE)         ((NODE)->type.rank)
#define TYPE_INLINE(NODE)       ((NODE)->type.inlined)
#define TYPE_TYPE(NODE)         ((NODE)->type.type)
#define TYPE_TAG(NODE)          ((NODE)->type.tag)
#define TYPE_PARAMS(NODE)       ((NODE)->type.u.f.params)
#define TYPE_OLDSTYLE(NODE)     ((NODE)->type.u.f.oldstyle)
#define TYPE_TSYM(NODE)         ((NODE)->type.u.s.tsym)
#define TYPE_IDS(NODE)          ((NODE)->type.u.s.ids)
#define TYPE_FIELDS(NODE)       ((NODE)->type.u.s.fields)
#define TYPE_LIMITS_MAX(NODE)   ((NODE)->type.limits.max)
#define TYPE_LIMITS_MIN(NODE)   ((NODE)->type.limits.min)
#define TYPE_A_ASSIGN(NODE)     ((NODE)->type.u.a.assign)
#define TYPE_A_CONST(NODE)      ((NODE)->type.u.a.is_const)
#define TYPE_A_VOLATILE(NODE)   ((NODE)->type.u.a.is_volatile)
#define TYPE_A_RESTRICT(NODE)   ((NODE)->type.u.a.is_restrict)
#define TYPE_A_STATIC(NODE)     ((NODE)->type.u.a.is_static)
#define TYPE_A_WILDCARD(NODE)   ((NODE)->type.u.a.wildcard)

struct ast_type {
    struct ast_common common;
    int kind;
    const char *name;
    size_t size;
    unsigned rank;
    bool inlined;
    union node *type;
    const char *tag;
    union {
        // function
        struct {
            union node **params;
            unsigned oldstyle : 1;
        }f;
        // enum/struct/union
        struct {
	    union node *tsym;
            union node **ids;
            union node **fields;
        }s;
	// array
        struct {
            union node *assign;
            unsigned is_const : 1;
            unsigned is_volatile : 1;
            unsigned is_restrict : 1;
            unsigned is_static : 1;
            unsigned wildcard : 1;
        }a;
    }u;
    struct {
        union value max;
        union value min;
    }limits;
};

#define FIELD_NAME(NODE)        ((NODE)->field.name)
#define FIELD_TYPE(NODE)        ((NODE)->field.type)
#define FIELD_OFFSET(NODE)      ((NODE)->field.offset)
#define FIELD_BITSIZE(NODE)     ((NODE)->field.bitsize)

struct ast_field {
    struct ast_common common;
    const char *name;
    union node *type;
    int offset;
    int bitsize;
};

#define SYM_SCOPE(NODE)         ((NODE)->symbol.scope)
#define SYM_NAME(NODE)          ((NODE)->symbol.name)
#define SYM_SCLASS(NODE)        ((NODE)->symbol.sclass)
#define SYM_TYPE(NODE)          ((NODE)->symbol.type)
#define SYM_DEFINED(NODE)       ((NODE)->symbol.defined)
#define SYM_SRC(NODE)           ((NODE)->symbol.src)
#define SYM_VALUE(NODE)         ((NODE)->symbol.value)
#define SYM_REFS(NODE)          ((NODE)->symbol.refs)

struct ast_symbol {
    int scope;
    const char *name;
    int sclass;
    union node *type;
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
    union node *sym;
    union node *body;
    union node **exts;
};

#define STMT_UP(NODE)           ((NODE)->stmt.up)

// compound stmt
#define STMT_BLKS(NODE)         ((NODE)->stmt.blks)

// if stmt
#define STMT_COND(NODE)         ((NODE)->stmt.list[0])
#define STMT_THEN(NODE)         ((NODE)->stmt.list[1])
#define STMT_ELSE(NODE)         ((NODE)->stmt.list[2])

// for stmt
#define STMT_CTRL(NODE)         ((NODE)->stmt.list[1])
#define STMT_INIT(NODE)         ((NODE)->stmt.list[2])
#define STMT_DECL(NODE)         ((NODE)->stmt.blks)

// case stmt
#define STMT_CASE_INDEX(NODE)   ((NODE)->stmt.index)

struct ast_stmt {
    struct ast_common common;
    union node *up;
    int index;
    union node *list[3];
    union node **blks;
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

struct ast_expr {
    struct ast_common common;
    int op;
    bool prefix;
    union node *sym;
    union node *operands[1];
    union node **list;
};

union node {
    struct ast_common common;
    struct ast_decl decl;
    struct ast_stmt stmt;
    struct ast_expr expr;
    struct ast_type type;
    struct ast_field field;
    struct ast_symbol symbol;
};

// ast.c
extern const char *nname(union node *node);
extern union node * ast_expr(int id, int op, union node *l, union node *r);
extern union node * ast_decl(int id, int scope);
extern union node * ast_stmt(int id, union node *l, union node *r);
extern union node * ast_uop(int op, union node *ty, union node *l);
extern union node * ast_bop(int op, union node *l, union node *r);
extern union node * ast_conv(union node *ty, union node *l, const char *name);
extern union node * ast_inits();
extern union node * ast_vinit();

#define isexpr(n)           (AST_ID(n) > BEGIN_EXPR_ID && AST_ID(n) < END_EXPR_ID)
#define isdecl(n)           (AST_ID(n) > BEGIN_DECL_ID && AST_ID(n) < END_DECL_ID)
#define isstmt(n)           (AST_ID(n) > BEGIN_STMT_ID && AST_ID(n) < END_STMT_ID)
#define istype(n)           (AST_ID(n) == TYPE_NODE)
#define isfield(n)          (AST_ID(n) == FIELD_NODE)
#define issymbol(n)         (AST_ID(n) == SYMBOL_NODE)

#define isfuncdecl(n)       (AST_ID(n) == FUNC_DECL)
#define isfuncdef(n)        (isfuncdecl(n) && DECL_BODY(n) && AST_ID(DECL_BODY(n)) == COMPOUND_STMT)
#define isliteral(n)        (AST_ID(n) > BEGIN_LITERAL_ID && AST_ID(n) < END_LITERAL_ID)
#define is_switch_stmt(n)   (AST_ID(n) == SWITCH_STMT)
#define is_for_stmt(n)      (AST_ID(n) == FOR_STMT)
#define is_while_stmt(n)    (AST_ID(n) == WHILE_STMT)
#define is_dowhile_stmt(n)  (AST_ID(n) == DO_WHILE_STMT)
#define is_iteration_stmt(n) (is_for_stmt(n) || is_while_stmt(n) || is_dowhile_stmt(n))

#define BitCast                 "BitCast"
#define LValueToRValue          "LValueToRValue"
#define FunctionToPointerDecay  "FunctionToPointerDecay"
#define ArrayToPointerDecay     "ArrayToPointerDecay"
#define IntegralCast            "IntegralCast"
#define FloatCast               "FloatingCast"
#define IntegerToFloatCast      "IntegralToFloatingCast"
#define FloatToIntegerCast      "FloatingToIntegralCast"
#define PointerToBoolean        "PointerToBoolean"

#endif
