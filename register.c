#include "cc.h"

/**
 *  X86_64 registers
 *
 *  64-bit  32-bit  16-bit  8-bit
 *  rax     eax     ax      al,ah
 *  rbx     ebx     bx      bl,bh
 *  rcx     ecx     cx      cl,ch
 *  rdx     edx     dx      dl,dh
 *  rbp     ebp     bp
 *  rsp     esp     sp
 *  rsi     esi     si
 *  rdi     edi     di
 *  rip     eip     ip
 *  r8~r15
 *
 *  Segment registers
 *
 *  cs,ds,es,fs,gs,ss
 */

enum {
    RAX, RBX, RCX, RDX,
    RSI, RDI,
    R8, R9,
    R10, R11, R12, R13, R14, R15,
    INT_REGS
};

enum {
    XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7,
    XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15,
    FLOAT_REGS
};

static struct reg *iarg_regs[NUM_IARG_REGS];
static struct reg *farg_regs[NUM_FARG_REGS];
static struct reg *int_regs[INT_REGS];
static struct reg *float_regs[FLOAT_REGS];
struct reg * rsp = &(struct reg){
    .r64 = "%rsp",
    .r32 = "%esp",
    .r16 = "%sp"
};
struct reg * rbp = &(struct reg){
    .r64 = "%rbp",
    .r32 = "%ebp",
    .r16 = "%bp"
};
struct reg * rip = &(struct reg){
    .r64 = "%rip",
    .r32 = "%eip",
    .r16 = "%ip"
};

static inline struct reg *mkreg(struct reg *r)
{
    struct reg *reg = xmalloc(sizeof(struct reg));
    *reg = *r;
    return reg;
}

void init_regs(void)
{
    int_regs[RAX] = mkreg(&(struct reg)
                          {.r64 = "%rax", .r32 = "%eax", .r16 = "%ax", .r8 = "%al"});

    int_regs[RBX] = mkreg(&(struct reg)
                          {.r64 = "%rbx", .r32 = "%ebx", .r16 = "%bx", .r8 = "%bl"});
    
    int_regs[RCX] = mkreg(&(struct reg)
                          {.r64 = "%rcx", .r32 = "%ecx", .r16 = "%cx", .r8 = "%cl"});
    
    int_regs[RDX] = mkreg(&(struct reg)
                          {.r64 = "%rdx", .r32 = "%edx", .r16 = "%dx", .r8 = "%dl"});

    int_regs[RSI] = mkreg(&(struct reg)
                          {.r64 = "%rsi", .r32 = "%esi", .r16 = "%si", .r8 = "%sil"});

    int_regs[RDI] = mkreg(&(struct reg)
                          {.r64 = "%rdi", .r32 = "%edi", .r16 = "%di", .r8 = "%dil"});

    for (int i = R8; i <= R15; i++) {
        int index = i - R8 + 8;
        int_regs[i] = mkreg(&(struct reg){
                                .r64 = format("%%r%d", index),
                                .r32 = format("%%r%dd", index),
                                .r16 = format("%%r%dw", index),
                                .r8 = format("%%r%db", index)
                            });
    }

    // init integer regs
    iarg_regs[0] = int_regs[RDI];
    iarg_regs[1] = int_regs[RSI];
    iarg_regs[2] = int_regs[RDX];
    iarg_regs[3] = int_regs[RCX];
    iarg_regs[4] = int_regs[R8];
    iarg_regs[5] = int_regs[R9];

    // init floating regs
    for (int i = XMM0; i <= XMM15; i++) {
        const char *name = format("%%xmm%d", i - XMM0);
        float_regs[i] = mkreg(&(struct reg){.r64 = name, .r32 = name});
        if (i <= XMM7)
            farg_regs[i - XMM0] = float_regs[i];
    }
}

static struct reg * get_int_reg(struct tac *tac)
{
    
}

static struct reg * get_float_reg(struct tac *tac)
{
    
}

struct reg * get_reg(struct tac *tac)
{
    switch (tac->op) {
    case IR_ADDI:
    case IR_ADDF:
    case IR_SUBI:
    case IR_SUBF:
    case IR_MULI:
    case IR_MULF:
    case IR_IMULI:
    case IR_DIVI:
    case IR_DIVF:
    case IR_IDIVI:

    case IR_CONV_FF:
    case IR_CONV_F_SI:
    case IR_CONV_F_UI:
    case IR_CONV_SI_F:
    case IR_CONV_SI_SI:
    case IR_CONV_SI_UI:
    case IR_CONV_UI_F:
    case IR_CONV_UI_SI:
    case IR_CONV_UI_UI:
        break;

    case IR_LABEL:
    case IR_NONE:
        break;
    }
}

static struct addr * make_addr_with_type(int kind)
{
    struct addr *addr = zmalloc(sizeof(struct addr));
    addr->kind = kind;
    return addr;
}

struct addr * make_literal_addr(void)
{
    return make_addr_with_type(ADDR_TYPE_LITERAL);
}

struct addr * make_memory_addr(void)
{
    return make_addr_with_type(ADDR_TYPE_MEMORY);
}

struct addr * make_stack_addr(void)
{
    return make_addr_with_type(ADDR_TYPE_STACK);
}

struct addr * make_register_addr(void)
{
    return make_addr_with_type(ADDR_TYPE_REGISTER);
}
