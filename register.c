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
static struct vector *reg_stack;

static inline struct reg *make_reg(void)
{
    return zmalloc(sizeof(struct reg));
}

static struct reg *get_free_reg(struct reg **regs, int count)
{
    for (int i = 0; i < count; i++) {
        struct reg *r = regs[i];
        if (r->uses)
            continue;
        return r;
    }
    return NULL;
}

static void push_reg(struct reg *r)
{
    emit("pushq %s", r->r64);
    vec_push(reg_stack, r);
}

static void pop_reg(struct reg *r)
{
    struct reg *reg = vec_tail(reg_stack);
    if (reg != r) {
        print_register_state();
        die("reg_stack conflicts");
    }
    emit("popq %s", r->r64);
    vec_pop(reg_stack);
}

static inline void use_reg(struct reg *r)
{
    r->uses++;
    if (r->uses > 1)
        push_reg(r);
}

void free_reg(struct reg *r)
{
    r->uses--;
    if (r->uses)
        pop_reg(r);
}

/**
 * According to the ABI, the first 6 integer or pointer
 * arguments to a function are passed in registers:
 * rdi, rsi, rdx, rcx, r8, r9
 */
struct reg *get_iarg_reg(void)
{
    struct reg *reg = get_free_reg(iarg_regs, ARRAY_SIZE(iarg_regs));
    if (reg)
        use_reg(reg);
    return reg;
}

struct reg *get_farg_reg(void)
{
    struct reg *reg = get_free_reg(farg_regs, ARRAY_SIZE(farg_regs));
    if (reg)
        use_reg(reg);
    return reg;
}

struct reg *use_int_reg(void)
{
    struct reg *reg = get_free_reg(int_regs, ARRAY_SIZE(int_regs));
    if (reg) {
        use_reg(reg);
        return reg;
    } else {
        struct reg *reg0 = int_regs[0];
        use_reg(reg0);
        return reg0;
    }
}

struct reg *use_float_reg(void)
{
    struct reg *reg = get_free_reg(float_regs, ARRAY_SIZE(float_regs));
    if (reg) {
        use_reg(reg);
        return reg;
    } else {
        struct reg *reg0 = float_regs[0];
        use_reg(reg0);
        return reg0;
    }
}

static const char *get_reg_name(struct reg *r, int size)
{
    switch (size) {
    case 1:
        return r->r8;
    case 2:
        return r->r16;
    case 4:
        return r->r32;
    case 8:
        return r->r64;
    default:
        cc_assert(0);
        return NULL;
    }
}

static struct operand *make_operand(int kind)
{
    struct operand *operand = zmalloc(sizeof(struct operand));
    operand->kind = kind;
    return operand;
}

struct operand *make_literal_operand(const char *name)
{
    struct operand *operand = make_operand(OPERAND_LITERAL);
    operand->u.name = name;
    return operand;
}

struct operand *make_memory_operand(const char *name)
{
    struct operand *operand = make_operand(OPERAND_MEMORY);
    operand->u.name = name;
    return operand;
}

struct operand *make_register_operand(struct reg *reg)
{
    struct operand *operand = make_operand(OPERAND_REGISTER);
    operand->u.reg = reg;
    return operand;
}

const char *get_operand_name(struct operand *operand, int size)
{
    if (operand->kind == OPERAND_REGISTER)
        return get_reg_name(operand->u.reg, size);
    else
        return operand->u.name;
}

void use_operand(struct operand *operand)
{
    if (operand->kind == OPERAND_REGISTER)
        use_reg(operand->u.reg);
}

void free_operand(struct operand *operand)
{
    if (operand->kind == OPERAND_REGISTER)
        free_reg(operand->u.reg);
}

void init_regs(void)
{
    int_regs[RAX] = make_reg();
    int_regs[RAX]->r64 = "%rax";
    int_regs[RAX]->r32 = "%eax";
    int_regs[RAX]->r16 = "%ax";
    int_regs[RAX]->r8 = "%al";

    int_regs[RBX] = make_reg();
    int_regs[RBX]->r64 = "%rbx";
    int_regs[RBX]->r32 = "%ebx";
    int_regs[RBX]->r16 = "%bx";
    int_regs[RBX]->r8 = "%bl";
    
    int_regs[RCX] = make_reg();
    int_regs[RCX]->r64 = "%rcx";
    int_regs[RCX]->r32 = "%ecx";
    int_regs[RCX]->r16 = "%cx";
    int_regs[RCX]->r8 = "%cl";
    
    int_regs[RDX] = make_reg();
    int_regs[RDX]->r64 = "%rdx";
    int_regs[RDX]->r32 = "%edx";
    int_regs[RDX]->r16 = "%dx";
    int_regs[RDX]->r8 = "%dl";

    int_regs[RSI] = make_reg();
    int_regs[RSI]->r64 = "%rsi";
    int_regs[RSI]->r32 = "%esi";
    int_regs[RSI]->r16 = "%si";
    int_regs[RSI]->r8 = "%sil";

    int_regs[RDI] = make_reg();
    int_regs[RDI]->r64 = "%rdi";
    int_regs[RDI]->r32 = "%edi";
    int_regs[RDI]->r16 = "%di";
    int_regs[RDI]->r8 = "%dil";

    for (int i = R8; i <= R15; i++) {
        int index = i - R8 + 8;
        int_regs[i] = make_reg();
        int_regs[i]->r64 = format("%%r%d", index);
        int_regs[i]->r32 = format("%%r%dd", index);
        int_regs[i]->r16 = format("%%r%dw", index);
        int_regs[i]->r8 = format("%%r%db", index);
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
        float_regs[i] = make_reg();
        float_regs[i]->r64 = name;
        float_regs[i]->r32 = name;
        if (i <= XMM7)
            farg_regs[i - XMM0] = float_regs[i];
    }

    reg_stack = vec_new();
}

void print_register_state(void)
{
    for (int i = 0; i < ARRAY_SIZE(int_regs); i++) {
        struct reg *reg = int_regs[i];
        println("%s: uses[%u]", reg->r64, reg->uses);
    }
    for (int i = 0; i < ARRAY_SIZE(float_regs); i++) {
        struct reg *reg = float_regs[i];
        println("%s: uses[%u]", reg->r64, reg->uses);
    }
    for (int i = vec_len(reg_stack) - 1; i >= 0; i--) {
        struct reg *reg = vec_at(reg_stack, i);
        println("reg_stack[%d]: %s: uses[%u]", i, reg->r64, reg->uses);
    }
}
