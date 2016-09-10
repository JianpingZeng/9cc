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

// operand size index
enum {
    B = 0,
    W = 1,
    L = 2,
    Q = 3
};

struct rvar {
    struct symbol *sym;
    int size;
};

/*
  If the callee wishes to use _preserved_ registers, it must restore
  their original values before returning control to the caller.
  All others must be saved by the caller if it wishes to preserve
  their values.

  See the AMD64 ABI documentation for more details.
 */

struct reg {
    int freg:1;
    int preserved:1;              // preserved across function calls
    const char *r[4];
    struct set *vars;
};

// op
enum {
#define _rop(a, b) a,
#include "rop.def"
    IR_END
};

enum {
    ASCIZ = Quad + 1
};

struct xvalue {
    int size;
    const char *name;
};

enum {
    ADDR_STACK,
    ADDR_REGISTER,
};

enum {
    SYM_KIND_LABEL,
    SYM_KIND_IMM,
    SYM_KIND_GREF,
    SYM_KIND_LREF,
    SYM_KIND_TMP
};

#define MAX_STRUCT_PARAM_SIZE  16

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

struct uses {
    bool live;
    struct tac *next;
};

/*
  displacement(base, index, scale)
  address = base + displacement + index * scale
  base: register
  index: register
  scale: 1,2,4,8
  displacement: integer number
  
  cases:
  - Direct operand:            displacement
  - Indirect operand:          (base)
  - Base+displacement:         displacement(base)
      - index into an array
      - access a field of a record
  - (index*scale)+dispacement: displacement(,index,scale)
      - index into an array
  - Base+index+displacement:   displacement(base, index)
      - two dimensional array
      - one dimensional array of records
  - Base+(index*scale)+displacement:  displacement(base,index,scale)
      - two dimensional array
*/

/*
  op = IR_NONE:        sym
  op = IR_SUBSCRIPT:   disp(sym,index,scale)
  op = IR_INDIRECTION: *sym
 */
struct operand {
    int op;
    int scale;
    long disp;
    struct symbol *sym;                // base symbol
    struct symbol *index;              // index symbol
};

// three-address code
struct tac {
    int op;
    int relop;
    int opsize:6;
    int from_opsize:6;
    int to_opsize:6;
    int sign:1;
    struct operand *operands[3];
    struct uses uses[6];
    struct tac *next, *prev;
    node_t *call;               // funcall expr
};

// basic block tag
enum {
    BLOCK_NONE,
    BLOCK_START,               // entry
    BLOCK_END,                 // exit
    BLOCK_JUMPING_DEST,        // jumping destination
};

// basic block
struct basic_block {
    int tag;
    const char *label;
    struct tac *head;
    struct basic_block *successors[2];
    struct set *use;
    struct set *def;
    struct set *in;
    struct set *out;
};

// reladdr kind
enum {
    RELADDR_NONE,
    RELADDR_IMM,
    RELADDR_SUBSCRIPT,
};

// gen.c
struct reladdr {
    int kind;
    int scale;
    long disp;
    const char *base;
    const char *index;
};

// opcode kind
enum {
    OPCODE_NONE,
    OPCODE_LABEL,
    OPCODE_MACRO,
};

struct opcode {
    int kind;
    const char *op;
    const char *suffix;
    const char *comment;
    struct reladdr *operands[2];
};

// ir.c
extern const char *rop2s(int op);
extern bool is_tmp_operand(struct operand *operand);
extern bool is_mem_operand(struct operand *operand);
extern bool is_imm_operand(struct operand *operand);
extern bool is_direct_mem_operand(struct operand *operand);
extern struct symbol * make_label_sym(const char *name);

// block.c
extern void construct_basic_blocks(struct symbol *sym, struct tac *head);
#define REF_SYM(sym)  (SYM_X_KIND(sym) == SYM_KIND_GREF ||\
                       SYM_X_KIND(sym) == SYM_KIND_LREF ||\
                       SYM_X_KIND(sym) == SYM_KIND_TMP)

#endif
