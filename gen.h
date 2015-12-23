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
    Zero = 0,
    Byte = 1,
    Word = 2,
    Long = 4,
    Quad = 8,
};

enum {
    GDATA_BSS,
    GDATA_DATA,
    GDATA_TEXT
};

#define GDATA_ID(gdata)        ((gdata)->common.id)
#define GDATA_GLOBAL(gdata)    ((gdata)->common.global)
#define GDATA_LABEL(gdata)     ((gdata)->common.label)
#define GDATA_ALIGN(gdata)     ((gdata)->common.align)
#define GDATA_SIZE(gdata)      ((gdata)->common.size)

struct gdata_common {
    int id:3;
    int global:1;
    int align:6;
    const char *label;
    size_t size;
};

struct gdata_bss {
    struct gdata_common common;
};

struct xvalue {
    int size;
    const char *name;
};

#define GDATA_DATA_XVALUES(gdata)    ((gdata)->data.xvalues)

struct gdata_data {
    struct gdata_common common;
    struct xvalue **xvalues;
};

#define GDATA_TEXT_DECL(gdata)       ((gdata)->text.decl)

struct gdata_text {
    struct gdata_common common;
    node_t *decl;
};

typedef union {
    struct gdata_common common;
    struct gdata_bss bss;
    struct gdata_data data;
    struct gdata_text text;
} gdata_t;

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
#define DECL_X_GDATAS(NODE)            ((NODE)->decl.x.decl.gdatas)
#define DECL_X_STRINGS(NODE)           ((NODE)->decl.x.decl.strings)
#define DECL_X_COMPOUNDS(NODE)         ((NODE)->decl.x.decl.compounds)
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
        gdata_t **gdatas;
        struct dict *strings;
        struct dict *compounds;
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
