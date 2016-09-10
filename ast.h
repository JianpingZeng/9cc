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
    struct type *type;
    struct source src;
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
#define EXPR_VA_ARG_TYPE(NODE)  ((NODE)->expr.type)

// expr
#define EXPR_X_ADDR(NODE)     ((NODE)->expr.x.addr)
#define EXPR_X_TRUE(NODE)     ((NODE)->expr.x.btrue)
#define EXPR_X_FALSE(NODE)    ((NODE)->expr.x.bfalse)
#define EXPR_X_ARRAY(NODE)    ((NODE)->expr.x.array)
#define EXPR_X_XVALUES(NODE)  ((NODE)->expr.x.xvalues)
    
struct ast_expr {
    struct ast_common common;
    int op;
    bool prefix;
    struct symbol *sym;
    node_t *operands[3];
    node_t **list;
    struct type *type;
    struct {
        struct operand *addr;
        struct operand *array;

        // label
        const char *btrue;
        const char *bfalse;
        struct vector *xvalues;
    } x;
};

// compound stmt
#define STMT_BLKS(NODE)    ((NODE)->stmt.blks)

// if stmt
#define STMT_COND(NODE)    ((NODE)->stmt.list[0])
#define STMT_THEN(NODE)    ((NODE)->stmt.list[1])
#define STMT_ELSE(NODE)    ((NODE)->stmt.list[2])

// for stmt
#define STMT_FOR_INIT(NODE)    ((NODE)->stmt.list[0])
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

// stmt
#define STMT_X_LABEL(NODE)    ((NODE)->stmt.x.label)
#define STMT_X_NEXT(NODE)     ((NODE)->stmt.x.next)

struct ast_stmt {
    struct ast_common common;
    long index;
    node_t **blks;
    node_t *list[4];
    struct {
        const char *label;

        // label
        const char *next;
    } x;
};

union ast_node {
    struct ast_common common;
    struct ast_expr expr;
    struct ast_stmt stmt;
};

extern const char *nname(node_t * node);
// expr
extern node_t *ast_expr(int id, struct type * ty, node_t * l, node_t * r);
extern node_t *ast_uop(int op, struct type * ty, node_t * l);
extern node_t *ast_bop(int op, struct type * ty, node_t * l, node_t * r);
extern node_t *ast_conv(struct type * ty, node_t * l, const char *name);
extern node_t *ast_inits(struct type * ty, struct source src);
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

// kind
#define isexpr(n)   (AST_ID(n) > BEGIN_EXPR_ID && AST_ID(n) < END_EXPR_ID)
#define isstmt(n)   (AST_ID(n) > BEGIN_STMT_ID && AST_ID(n) < END_STMT_ID)

// decl
#define isfuncdef(n)   (isfunc(SYM_TYPE(n)) && SYM_INIT(n))
#define isvardecl(n)   (SYM_SCLASS(n) != TYPEDEF && !isfunc(SYM_TYPE(n)))

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
