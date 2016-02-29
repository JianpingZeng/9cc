#ifndef _GEN_H
#define _GEN_H

// operand size
enum {
    Zero = 0,
    Byte = 1,
    Word = 2,
    Long = 4,
    Quad = 8,
};

enum {
    B = 0,
    W = 1,
    L = 2,
    Q = 3
};

struct reg {
    int freg:1;
    const char *r[4];
    struct vector *vars;
};

// op
enum {
#define _rop(a, b) a,
#include "rop.def"
    IR_END
};

enum {
    GDATA_BSS,
    GDATA_DATA,
    GDATA_TEXT
};

struct xvalue {
    int size;
    const char *name;
};

struct gdata {
    int id:3;
    int global:1;
    int align:6;
    const char *label;
    size_t size;
    union {
        // data
        struct xvalue **xvalues;
        // decl
        node_t *decl;
    } u;
};

enum {
    SYM_KIND_LABEL,
    SYM_KIND_ILITERAL,
    SYM_KIND_FLITERAL,
    SYM_KIND_REF,
    SYM_KIND_TMP,
};

enum {
    ADDR_STACK,
    ADDR_REGISTER,
};

#define MAX_STRUCT_PARAM_SIZE  16

struct addr {
    int kind;
    struct reg *reg;
    long offset;
    size_t size;
};

// REGS type
enum {
    REG_INT,
    REG_SSE_D,
    REG_SSE_F,
    REG_SSE_FF
};

struct paddr {
    int kind;
    size_t size;
    union {
        struct {
            int type;
            struct reg *reg;
        } regs[MAX_STRUCT_PARAM_SIZE >> 3];
        long offset;
    } u;
};

/*
  op = IR_NONE:        sym
  op = IR_SUBSCRIPT:   sym[index]
  op = IR_INDIRECTION: *sym
 */
struct uses {
    bool live;
    struct tac *use_tac;
};

// displacement(base, index, scale)
// address = base + displacement + index * scale
// base: register
// index: register
// scale: 1,2,4,8
// displacement: integer number
//
// cases:
// - Direct operand:            displacement
// - Indirect operand:          (base)
// - Base+displacement:         displacement(base)
//     - index into an array
//     - access a field of a record
// - (index*scale)+dispacement: displacement(,index,scale)
//     - index into an array
// - Base+index+displacement:   displacement(base, index)
//     - two dimensional array
//     - one dimensional array of records
// - Base+(index*scale)+displacement:  displacement(base,index,scale)
//     - two dimensional array

struct operand {
    int op;
    int scale;
    long disp;
    node_t *sym;                // base symbol
    node_t *index;              // index symbol
    struct uses uses;
};

// three-address code
struct tac {
    int op;
    int relop;
    int opsize:6;
    int from_opsize:6;
    int to_opsize:6;
    struct operand *args[2];
    struct operand *result;
    struct tac *next, *prev;
    node_t *call;               // funcall expr
};

// externals
struct externals {
    struct vector *gdatas;
    struct dict *strings;
    struct dict *compounds;
    struct dict *floats;
};

// sym
#define SYM_X_LABEL(NODE)     ((NODE)->symbol.x.sym.label)
#define SYM_X_USES(NODE)      ((NODE)->symbol.x.sym.uses)
#define SYM_X_REG(NODE)       ((NODE)->symbol.x.sym.reg)
#define SYM_X_KIND(NODE)      ((NODE)->symbol.x.sym.kind)
#define SYM_X_LOFF(NODE)      ((NODE)->symbol.x.sym.loff)
#define SYM_X_PADDR(NODE)     ((NODE)->symbol.x.sym.paddr)
// decl
#define DECL_X_SVARS(NODE)    ((NODE)->decl.x.decl.svars)
#define DECL_X_LVARS(NODE)    ((NODE)->decl.x.decl.lvars)
#define DECL_X_CALLS(NODE)    ((NODE)->decl.x.decl.calls)
#define DECL_X_HEAD(NODE)     ((NODE)->decl.x.decl.head)
// expr
#define EXPR_X_ADDR(NODE)     ((NODE)->expr.x.expr.addr)
#define EXPR_X_TRUE(NODE)     ((NODE)->expr.x.expr.btrue)
#define EXPR_X_FALSE(NODE)    ((NODE)->expr.x.expr.bfalse)
#define EXPR_X_ARRAY(NODE)    ((NODE)->expr.x.expr.array)
#define EXPR_X_PADDR(NODE)    ((NODE)->expr.x.expr.paddr)
#define EXPR_X_PARAM_ALLOCED(NODE)     ((NODE)->expr.x.expr.param_allocated)
#define EXPR_X_STACK_PARAM_SIZE(NODE)  ((NODE)->expr.x.expr.stack_param_size)
// stmt
#define STMT_X_LABEL(NODE)    ((NODE)->stmt.x.stmt.label)
#define STMT_X_NEXT(NODE)     ((NODE)->stmt.x.stmt.next)

union x {
    struct {
        const char *label;
        long loff;              // local offset (<0)
        int kind;               // kind
        struct uses uses;       // uses
        struct reg *reg;        // reg addr
        struct paddr *paddr;    // param addr
    }sym;
    
    struct {
        node_t **lvars;        // function local vars
        node_t **svars;        // function static vars
        node_t **calls;        // function calls
        struct tac *head;
    }decl;
    
    struct {
        struct operand *addr;
        struct operand *array;

        // label
        const char *btrue;
        const char *bfalse;

        struct paddr *paddr;    // arg addr

        int param_allocated:1;
        size_t stack_param_size;
    }expr;

    struct {
        const char *label;

        // label
        const char *next;
    }stmt;
};

// gen.c
extern void gen(struct externals *externals, FILE * fp);

// ir.c
extern const char *rop2s(int op);
extern struct externals * ir(node_t *tree);
extern node_t * reduce(node_t *expr);

#endif
