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
#define DECL_X_CODES(NODE)             ((NODE)->decl.x.decl.codes)
// expr
#define EXPR_X_ADDR(NODE)       ((NODE)->expr.x.expr.addr)
#define EXPR_X_ARG(NODE)        ((NODE)->expr.x.expr.arg)
#define EXPR_X_TRUE(NODE)       ((NODE)->expr.x.expr.btrue)
#define EXPR_X_FALSE(NODE)      ((NODE)->expr.x.expr.bfalse)
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
        struct vector *codes;
    }decl;
    
    struct {
        struct addr *addr;
        struct addr *arg;

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

// op
enum {
#define _rop(a, b) a,
#include "rop.def"
    IR_END
};

struct operand {
    node_t *sym;
};

/**
 *  - binary:  result = arg1 op arg2
 *  - unary:   result = op arg1
 *  - jump:    op result  (op == jump operator)
 *  - label:   op result  (op == label operator)
 */

struct code {
    int op;
    struct operand *arg1;          // left operand
    struct operand *arg2;          // right operand
    struct operand *result;
};

// register.c
extern void init_regs(void);
extern void print_register_state(void);

// gen.c
extern void emit(const char *fmt, ...);
extern void emit_noindent(const char *fmt, ...);
extern void gen(node_t * tree, FILE * fp);
extern void emit_initializer(node_t * init);
extern struct dict *compound_lits;
extern struct dict *string_lits;

// gen-decl.c
extern void gen_decl(node_t *decl);

// ir.c
extern node_t * ir(node_t *tree);
extern void print_codes(struct vector *codes);
extern node_t * reduce(node_t *expr);

#endif
