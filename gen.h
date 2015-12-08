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

enum {
    ADDR_LITERAL,
    ADDR_REGISTER,
    ADDR_MEMORY,
    ADDR_STACK
};

struct addr {
    int kind;
    union {
        long loff;              // stack offset
        const char *name;       // data/literal
        struct reg *reg;        // register
    }u;
};

// sym
#define SYM_X_LABEL(NODE)     ((NODE)->symbol.x.sym.label)
#define SYM_X_LOFF(NODE)      ((NODE)->symbol.x.sym.loff)
// decl
#define DECL_X_SVARS(NODE)             ((NODE)->decl.x.decl.svars)
#define DECL_X_LVARS(NODE)             ((NODE)->decl.x.decl.lvars)
#define DECL_X_EXTRA_STACK_SIZE(NODE)  ((NODE)->decl.x.decl.extra_stack_size)
#define DECL_X_IRS(NODE)               ((NODE)->decl.x.decl.irs)
// expr
#define EXPR_X_ADDR(NODE)       ((NODE)->expr.x.expr.addr)
#define EXPR_X_ARG(NODE)        ((NODE)->expr.x.expr.arg)
// stmt
#define STMT_X_LABEL(NODE)    ((NODE)->stmt.x.stmt.label)

union code {
    struct {
        const char *label;
        long loff;             // stack offset
    }sym;
    
    struct {
        node_t **lvars;        // function local vars
        node_t **svars;        // function static vars
        size_t extra_stack_size;
        struct vector *irs;
    }decl;
    
    struct {
        struct addr *addr;
        struct addr *arg;
    }expr;

    struct {
        const char *label;
    }stmt;
};

struct operand {
    const char *name;
};

struct ir {
    int op;
    struct operand *l;          // left operand
    struct operand *r;          // right operand
};

// register.c
extern void init_regs(void);
extern void print_register_state(void);

// gen.c
extern void emit(const char *fmt, ...);
extern void gen(node_t * tree, FILE * fp);

// ir.c
extern node_t * ir(node_t *tree);

#endif
