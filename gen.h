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
    const char *r[4];
    struct vector *vars;
};

// op
enum {
#define _rop(a, b, c) a,
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
    SYM_KIND_LITERAL,
    SYM_KIND_REF,
    SYM_KIND_TMP,
};

enum {
    ADDR_MEMORY,
    ADDR_STACK,
    ADDR_REGISTER,
    ADDRS                    // count
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
    int op;
    node_t *sym;
    node_t *index;
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
#define DECL_X_HEAD(NODE)     ((NODE)->decl.x.decl.head)
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
        struct addr *addrs[ADDRS];
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
