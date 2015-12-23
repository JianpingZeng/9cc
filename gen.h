#ifndef _GEN_H
#define _GEN_H

#define NUM_IARG_REGS  6
#define NUM_FARG_REGS  8

struct reg {
    const char *r64;
    const char *r32;
    const char *r16;
    const char *r8;
};

// op
enum {
#define _rop(a, b) a,
#include "rop.def"
    IR_END
};

// operand size
enum {
    Byte = 1,
    Word = 2,
    Long = 4,
    Quad = 8,
};

// gdata

enum {
    XVALUE_BYTES,
    XVALUE_LABEL,
};

struct xvalue {
    int kind;
    int size;
    union {
        union value v;
        struct {
            const char *name;
            long offset;
        } p;
    } u;
};

struct gdata {
    int bss:1;
    int string:1;
    int globl:1;
    int align:6;
    size_t size;
    const char *label;
    union {
        struct {
            struct xvalue **values;
            size_t zeros;
        } v;
        const char *string;
    } u;
};

struct gtext {
    node_t *decl;
};

/*
  op = IR_NONE:        sym
  op = IR_SUBSCRIPT:   sym[index]
  op = IR_INDIRECTION: *sym
 */
struct operand {
    int op;
    node_t *type;
    node_t *sym;
    node_t *index;
};

// three-address code
struct ir {
    int op;
    int relop;
    struct operand *args[2];
    struct operand *result;
};

// basic block && flow graph
struct basic_block {
    struct ir **irs;
};

struct flow_graph {
    struct basic_block **blks;
};

// sym
#define SYM_X_LABEL(NODE)     ((NODE)->symbol.x.sym.label)
#define SYM_X_LOFF(NODE)      ((NODE)->symbol.x.sym.loff)
// decl
#define DECL_X_SVARS(NODE)             ((NODE)->decl.x.decl.svars)
#define DECL_X_LVARS(NODE)             ((NODE)->decl.x.decl.lvars)
#define DECL_X_EXTRA_STACK_SIZE(NODE)  ((NODE)->decl.x.decl.extra_stack_size)
#define DECL_X_IRS(NODE)               ((NODE)->decl.x.decl.irs)
#define DECL_X_FLOW_GRAPH(NODE)        ((NODE)->decl.x.decl.flow_graph)
#define DECL_X_GDATA(NODE)             ((NODE)->decl.x.decl.gdata)
#define DECL_X_GTEXT(NODE)             ((NODE)->decl.x.decl.gtext)
// expr
#define EXPR_X_ADDR(NODE)       ((NODE)->expr.x.expr.addr)
#define EXPR_X_TRUE(NODE)       ((NODE)->expr.x.expr.btrue)
#define EXPR_X_FALSE(NODE)      ((NODE)->expr.x.expr.bfalse)
#define EXPR_X_ARRAY(NODE)      ((NODE)->expr.x.expr.array)
// stmt
#define STMT_X_LABEL(NODE)    ((NODE)->stmt.x.stmt.label)
#define STMT_X_NEXT(NODE)     ((NODE)->stmt.x.stmt.next)

union x {
    struct {
        const char *label;
        long loff;             // stack offset
    }sym;
    
    struct {
        node_t **lvars;        // function local vars
        node_t **svars;        // function static vars
        size_t extra_stack_size;
        struct vector *irs;
        struct flow_graph *flow_graph;
        struct gdata **gdata;
        struct gtext **gtext;
    }decl;
    
    struct {
        struct operand *addr;
        struct operand *array;

        // label
        const char *btrue;
        const char *bfalse;
    }expr;

    struct {
        const char *label;

        // label
        const char *next;
    }stmt;
};

// register.c
extern void init_regs(void);
extern void print_register_state(void);

// gen.c
extern void gen(node_t * tree, FILE * fp);

// ir.c
extern const char *rop2s(int op);
extern node_t * ir(node_t *tree);
extern node_t * reduce(node_t *expr);

#endif
