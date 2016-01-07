#ifndef _GEN_H
#define _GEN_H

#define NUM_IARG_REGS  6
#define NUM_FARG_REGS  8

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

enum {
    SYM_KIND_LABEL,
    SYM_KIND_LITERAL,
    SYM_KIND_REF,
    SYM_KIND_TMP,
};

enum {
    ADDR_MEMORY,
    ADDR_STACK,
    ADDR_REGISTER,
    ADDR_NUM
};

struct addr {
    int kind;
    struct reg *reg;
    long offset;
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
struct operand {
    unsigned op:8;
    node_t *sym;
    node_t *index;
    struct uses uses;
};

// three-address code
struct tac {
    unsigned op:8;
    unsigned opsize:6;
    unsigned from_opsize:6;
    unsigned to_opsize:6;
    int relop;
    struct operand *args[2];
    struct operand *result;
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
#define SYM_X_ADDRS(NODE)     ((NODE)->symbol.x.sym.addrs)
#define SYM_X_KIND(NODE)      ((NODE)->symbol.x.sym.kind)
#define SYM_X_LOFF(NODE)      ((NODE)->symbol.x.sym.loff)
// decl
#define DECL_X_SVARS(NODE)    ((NODE)->decl.x.decl.svars)
#define DECL_X_LVARS(NODE)    ((NODE)->decl.x.decl.lvars)
#define DECL_X_CALLS(NODE)    ((NODE)->decl.x.decl.calls)
#define DECL_X_TACS(NODE)     ((NODE)->decl.x.decl.tacs)
// expr
#define EXPR_X_ADDR(NODE)     ((NODE)->expr.x.expr.addr)
#define EXPR_X_TRUE(NODE)     ((NODE)->expr.x.expr.btrue)
#define EXPR_X_FALSE(NODE)    ((NODE)->expr.x.expr.bfalse)
#define EXPR_X_ARRAY(NODE)    ((NODE)->expr.x.expr.array)
// stmt
#define STMT_X_LABEL(NODE)    ((NODE)->stmt.x.stmt.label)
#define STMT_X_NEXT(NODE)     ((NODE)->stmt.x.stmt.next)

union x {
    struct {
        const char *label;
        long loff;              // local offset (<0)
        // kind
        int kind;
        // uses
        struct uses uses;
        // addrs
        struct addr *addrs[ADDR_NUM];
    }sym;
    
    struct {
        node_t **lvars;        // function local vars
        node_t **svars;        // function static vars
        node_t **calls;        // function calls
        struct vector *tacs;
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

// gen.c
extern void gen(struct externals *externals, FILE * fp);

// ir.c
extern const char *rop2s(int op);
extern struct externals * ir(node_t *tree);
extern node_t * reduce(node_t *expr);

#endif
