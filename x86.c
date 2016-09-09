#include "cc.h"

#define NUM_IARG_REGS  6
#define NUM_FARG_REGS  8
#define REGISTER_SAVE_AREA_SIZE  (NUM_IARG_REGS * 8 + NUM_FARG_REGS * 16)
#define NUM_RET_IREGS  2
#define NUM_RET_FREGS  2
#define STACK_PARAM_BASE_OFF  16

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
static struct reg *ret_iregs[NUM_RET_IREGS];
static struct reg *ret_fregs[NUM_RET_FREGS];
static struct reg *rsp, *rbp, *rip;
static int idx[] = {
    -1, B, W, -1, L, -1, -1, -1, Q
};
static const char *suffixi[] = {
    "b", "w", "l", "q"
};
static const char *suffixf[] = {
    "", "", "ss", "sd"
};
static const char *suffixp[] = {
    "", "", "ps", "pd"
};
// Register Allocation
static void reset_regs(void);
static struct reg * get_one_ireg(struct set *excepts);
static struct reg * get_one_freg(struct set *excepts);
static struct reg * dispatch_ireg(node_t *sym, struct set *excepts, int opsize);
static struct reg * dispatch_freg(node_t *sym, struct set *excepts, int opsize);
static void drain_reg(struct reg *reg);
static void clear_reg(struct reg *reg);
static bool is_preserved(struct reg *reg);
static bool is_nonpreserved(struct reg *reg);
static void for_each_reg(bool (*cond) (struct reg *), void (*action) (struct reg *));
static void load(struct reg *reg, node_t *sym, int opsize);
static void store(node_t *sym);
static struct rvar *find_var(struct reg *reg, node_t *sym);
static void update_use(struct tac *tac);
static void push_excepts(struct set *excepts);
static void pop_excepts(void);
static struct set * operand_regs(struct operand *operand);
static struct reladdr * operand2s(struct operand *operand, int opsize);
static struct reladdr *operand2s_none_ex(struct operand *operand, int opsize, bool mem);
static void at_exit_basic_block(void);

// Parameter Classification
static int *no_class = (int *)1;
static int *integer_class = (int *)2;
static int *sse_class = (int *)3;
static int *memory_class = (int *)4;

struct ptype {
    size_t offset;
    node_t *type;
};
static struct vector * get_types(node_t *ty, size_t offset);
static struct vector * get_elements(struct vector *ptypes);
static struct vector * get_classes(struct vector *elements);

struct pnode {
    node_t *param;              // symbol or expr
    struct paddr *paddr;        // paddr of the param
};

struct pinfo {
    int fp;                     // number of fp used
    int gp;                     // number of gp used
    size_t size;                // size of params on stack
    struct vector *pnodes;      // param nodes
    struct paddr *retaddr;      // return addr
};
static struct pinfo * alloc_addr_for_funcall(node_t *ftype, node_t **params);
static struct pinfo * alloc_addr_for_funcdef(node_t *ftype, node_t **params);

#define COMMENT(str)    "\t\t## " str
#define COMMENT1(str)   "## " str

#define OP_MOV    "mov"
#define OP_ADD    "add"
#define OP_SUB    "sub"
#define OP_DIV    "div"
#define OP_IDIV   "idiv"
#define OP_MUL    "mul"
#define OP_IMUL   "imul"
#define OP_SHR    "shr"
#define OP_SHL    "shl"
#define OP_SAR    "sar"
#define OP_OR     "or"
#define OP_AND    "and"
#define OP_XOR    "xor"
#define OP_LEA    "lea"
#define OP_CALL   "call"
#define OP_CMP    "cmp"
#define OP_JNB    "jnb"
#define OP_JMP    "jmp"
#define OP_JLE    "jle"
#define OP_JBE    "jbe"
#define OP_JG     "jg"
#define OP_JA     "ja"
#define OP_JGE    "jge"
#define OP_JAE    "jae"
#define OP_JL     "jl"
#define OP_JB     "jb"
#define OP_JE     "je"
#define OP_JNE    "jne"
#define OP_UCOMI  "ucomi"
#define OP_NOT    "not"
#define OP_NEG    "neg"
#define OP_CVTSI  "cvtsi"
#define OP_CVTSS  "cvtss"
#define OP_CVTSD  "cvtsd"
#define OP_TEST   "test"
#define OP_LEAVE  "leave"
#define OP_RET    "ret"
#define OP_PUSH   "push"
#define OP_POP    "pop"
#define OP_SETNE  "setne"
#define OP_SETP   "setp"

// placeholder instruction id
enum {
    INST_PRESERVED_REG_PUSH = 1,
    INST_PRESERVED_REG_POP,
    INST_STACK_SUB,
    INST_STACK_ADD,
};

// function context
static struct {
    // function end label
    const char *end_label;
    // return value by stack (provided by caller, negative)
    long calls_return_loff;
    struct pinfo *pinfo;
    struct basic_block *current_block;
    struct tac *current_tac;
    node_t *current_ftype;
    // stack size (positive)
    size_t orig_localsize, localsize;
    // allocated preserved registers
    struct set *preserved_regs;
    // opcodes
    struct vector *opcodes;
} fcon;

static void emit(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    printf("\t");
    vprintf(fmt, ap);
    printf("\n");
    va_end(ap);
}

static void emit_noindent(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vprintf(fmt, ap);
    printf("\n");
    va_end(ap);
}

static struct reladdr *imm(long value)
{
    struct reladdr *operand = NEWS0(struct reladdr, PERM);
    operand->kind = RELADDR_IMM;
    operand->disp = value;
    return operand;
}

static struct reladdr *str(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    char *text = vformat(fmt, ap);
    va_end(ap);
    struct reladdr *operand = NEWS0(struct reladdr, PERM);
    operand->kind = RELADDR_NONE;
    operand->base = text;
    return operand;
}

static struct reladdr *rs(const char *text)
{
    struct reladdr *operand = NEWS0(struct reladdr, PERM);
    operand->kind = RELADDR_NONE;
    operand->base = text;
    return operand;
}

static struct reladdr *subscript(const char *base, const char *index,
                                 int scale, long disp)
{
    struct reladdr *operand = NEWS0(struct reladdr, PERM);
    operand->kind = RELADDR_SUBSCRIPT;
    operand->base = base;
    operand->index = index;
    operand->scale = scale;
    operand->disp = disp;
    return operand;
}

static struct reladdr *subst(const char *base, long disp)
{
    return subscript(base, NULL, 0, disp);
}

static void yy(const char *op, const char *suffix,
               struct reladdr *src, struct reladdr *dst,
               const char *comment)
{
    struct opcode *opcode = NEWS0(struct opcode, PERM);
    opcode->op = op;
    opcode->suffix = suffix;
    opcode->comment = comment;
    opcode->operands[0] = src;
    opcode->operands[1] = dst;
    vec_push(fcon.opcodes, opcode);
}

static void xx(const char *op, const char *suffix,
               struct reladdr *src, struct reladdr *dst)
{
    yy(op, suffix, src, dst, NULL);
}

// label
static void lab(const char *label)
{
    struct opcode *opcode = NEWS0(struct opcode, PERM);
    opcode->kind = OPCODE_LABEL;
    opcode->op = label;
    vec_push(fcon.opcodes, opcode);
}

// jmp
static void jmp(const char *label)
{
    xx(OP_JMP, NULL, rs(label), NULL);
}

// jump to
static void jump(const char *op, const char *label)
{
    at_exit_basic_block();
    xx(op, NULL, rs(label), NULL);
}

// macro
static void macro(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    char *text = vformat(fmt, ap);
    va_end(ap);
    struct opcode *opcode = NEWS0(struct opcode, PERM);
    opcode->kind = OPCODE_MACRO;
    opcode->op = text;
    vec_push(fcon.opcodes, opcode);
}

static void emit_placeholder(int id)
{
    char *message = format("<<<%d", id);
    xx(message, NULL, NULL, NULL);
}

static const char *reladdr2s(struct reladdr *addr, long offset)
{
    switch (addr->kind) {
    case RELADDR_NONE:
        return addr->base;
    case RELADDR_IMM:
        return format("$%ld", addr->disp);
    case RELADDR_SUBSCRIPT:
        if (addr->base) {
            bool isrbp = !strcmp(addr->base, rbp->r[Q]);
            long disp = addr->disp;
            if (isrbp && disp < 0)
                disp -= offset;
            if (addr->index) {
                if (disp)
                    return format("%ld(%s,%s,%s)",
                                  disp, addr->base, addr->index, addr->scale);
                else
                    return format("(%s,%s,%s)",
                                  addr->base, addr->index, addr->scale);
            } else {
                if (disp)
                    return format("%ld(%s)", disp, addr->base);
                else
                    return format("(%s)", addr->base);
            }
        } else {
            assert(0);
        }
    default:
        assert(0);
    }
}

static void finalize_text(void)
{
    struct vector *preserved_regs = set_objects(fcon.preserved_regs);
    size_t len = vec_len(preserved_regs);
    size_t localsize = ROUNDUP(fcon.localsize, 16);
    long extra_offset = len << 3;

    for (size_t i = 0; i < vec_len(fcon.opcodes); i++) {
        struct opcode *opcode = vec_at(fcon.opcodes, i);
        switch (opcode->kind) {
        case OPCODE_NONE:
            if (has_prefix(opcode->op, "<<<")) {
                int id = atoi(opcode->op + 3);
                switch (id) {
                case INST_STACK_SUB:
                    if (fcon.orig_localsize == fcon.localsize)
                        emit("subq $%lu, %s", localsize, rsp->r[Q]);
                    else
                        emit("subq $%lu, %s" COMMENT("extended"), localsize, rsp->r[Q]);
                    break;
                case INST_PRESERVED_REG_PUSH:
                    for (size_t i = 0; i < len; i++) {
                        struct reg *reg = vec_at(preserved_regs, i);
                        emit("pushq %s" COMMENT("push preserved regs"), reg->r[Q]);
                    }
                    break;
                case INST_PRESERVED_REG_POP:
                    for (int i = len - 1; i >= 0; i--) {
                        struct reg *reg = vec_at(preserved_regs, i);
                        emit("popq %s" COMMENT("pop preserved regs"), reg->r[Q]);
                    }
                    break;
                case INST_STACK_ADD:
                    if (len)
                        emit("addq $%lu, %s", localsize, rsp->r[Q]);
                    break;
                default:
                    assert(0);
                }
            } else {
                struct reladdr *src = opcode->operands[0];
                struct reladdr *dst = opcode->operands[1];
                if (src && dst) {
                    const char *src_label = reladdr2s(src, extra_offset);
                    const char *dst_label = reladdr2s(dst, extra_offset);
                    if (opcode->suffix) {
                        if (opcode->comment)
                            emit("%s%s %s, %s \t\t## %s",
                                 opcode->op, opcode->suffix,
                                 src_label, dst_label, opcode->comment);
                        else
                            emit("%s%s %s, %s",
                                 opcode->op, opcode->suffix,
                                 src_label, dst_label);
                    } else {
                        if (opcode->comment)
                            emit("%s %s, %s \t\t## %s",
                                 opcode->op,
                                 src_label, dst_label, opcode->comment);
                        else
                            emit("%s %s, %s",
                                 opcode->op,
                                 src_label, dst_label);
                    }
                } else if (src || dst) {
                    struct reladdr *addr = src ? src : dst;
                    const char *label = reladdr2s(addr, extra_offset);
                    if (opcode->suffix) {
                        if (opcode->comment)
                            emit("%s%s %s \t\t## %s",
                                 opcode->op, opcode->suffix,
                                 label, opcode->comment);
                        else
                            emit("%s%s %s",
                                 opcode->op, opcode->suffix,
                                 label);
                    } else {
                        if (opcode->comment)
                            emit("%s %s \t\t## %s",
                                 opcode->op,
                                 label, opcode->comment);
                        else
                            emit("%s %s",
                                 opcode->op,
                                 label);
                    }
                } else {
                    if (opcode->suffix) {
                        if (opcode->comment)
                            emit("%s%s \t\t## %s",
                                 opcode->op, opcode->suffix, opcode->comment);
                        else
                            emit("%s%s", opcode->op, opcode->suffix);
                    } else {
                        if (opcode->comment)
                            emit("%s \t\t## %s", opcode->op, opcode->comment);
                        else
                            emit("%s", opcode->op);
                    }
                }
            }
            break;
        case OPCODE_LABEL:
            emit_noindent("%s:", opcode->op);
            break;
        case OPCODE_MACRO:
            emit("%s", opcode->op);
            break;
        default:
            assert(0);
        }
    }
}

static struct operand * make_offset_operand(struct operand *operand, long offset)
{
    switch (operand->op) {
    case IR_NONE:
    case IR_SUBSCRIPT:
        {
            struct operand *ret = NEWS0(struct operand, PERM);
            *ret = *operand;
            ret->op = IR_SUBSCRIPT;
            ret->disp += offset;
            return ret;
        }
        break;
    default:
        assert(0);
    }
}

/*
  integer params(6): rdi, rsi, rdx, rcx, r8, r9
  floating params(8): xmm0~xmm7
 */
static void emit_param_scalar(struct tac *tac, struct pnode *pnode)
{
    struct operand *operand = tac->operands[1];
    node_t *arg = pnode->param;
    struct paddr *paddr = pnode->paddr;
    node_t *ty = AST_TYPE(arg);
    int i = idx[TYPE_SIZE(ty)];
    struct reladdr *src_label = operand2s(operand, TYPE_SIZE(ty));

    if (paddr->kind == ADDR_REGISTER) {
        struct reg *dst = paddr->u.regs[0].reg;
        if (isfloat(ty))
            xx(OP_MOV, suffixf[i], src_label, rs(dst->r[i]));
        else
            xx(OP_MOV, suffixi[i], src_label, rs(dst->r[i]));
    } else if (paddr->kind == ADDR_STACK) {
        struct reladdr *stack;
        if (paddr->u.offset)
            stack = str("%ld(%s)", paddr->u.offset, rsp->r[Q]);
        else
            stack = str("(%s)", rsp->r[Q]);

        struct set *excepts = operand_regs(operand);
        if (isfloat(ty)) {
            struct reg *tmp = get_one_freg(excepts);
            xx(OP_MOV, suffixf[i], src_label, rs(tmp->r[i]));
            xx(OP_MOV, suffixf[i], rs(tmp->r[i]), stack);
        } else {
            struct reg *tmp = get_one_ireg(excepts);
            xx(OP_MOV, suffixi[i], src_label, rs(tmp->r[i]));
            xx(OP_MOV, suffixi[i], rs(tmp->r[i]), stack);
        }
    }
}

static void emit_param_record(struct tac *tac, struct pnode *pnode)
{
    struct operand *operand = tac->operands[1];
    struct paddr *paddr = pnode->paddr;
    size_t size = paddr->size;
    int cnt = ROUNDUP(size, 8) >> 3;
    
    if (paddr->kind == ADDR_STACK) {
        struct set *excepts = operand_regs(operand);
        struct reg *tmp = get_one_ireg(excepts);
        excepts = set_new1(tmp);
        
        long loff = paddr->u.offset;
        for (int i = 0; i < cnt; i++) {
            long offset = i << 3;
            struct operand *src = make_offset_operand(operand, offset);
            push_excepts(excepts);
            struct reladdr *src_label = operand2s(src, Quad);
            pop_excepts();
            xx(OP_MOV, suffixi[Q], src_label, rs(tmp->r[Q]));
            if (loff + offset)
                xx(OP_MOV, suffixi[Q], rs(tmp->r[Q]), str("%ld(%s)", loff + offset, rsp->r[Q]));
            else
                xx(OP_MOV, suffixi[Q], rs(tmp->r[Q]), str("(%s)", rsp->r[Q]));
        }
    } else if (paddr->kind == ADDR_REGISTER) {
        long loff = 0;
        for (int i = 0; i < cnt; i++ , loff += 8, size -= 8) {
            struct reg *reg = paddr->u.regs[i].reg;
            int type = paddr->u.regs[i].type;
            struct operand *src = make_offset_operand(operand, loff);
            switch (type) {
            case REG_INT:
                if (size > 4)
                    xx(OP_MOV, suffixi[Q], operand2s(src, Quad), rs(reg->r[Q]));
                else if (size > 2)
                    xx(OP_MOV, suffixi[L], operand2s(src, Long), rs(reg->r[L]));
                else if (size == 2)
                    xx(OP_MOV, "zwl", operand2s(src, Word), rs(reg->r[L]));
                else if (size == 1)
                    xx(OP_MOV, "zbl", operand2s(src, Quad), rs(reg->r[L]));
                else
                    assert(0);
                break;
            case REG_SSE_F:
                xx(OP_MOV, suffixf[L], operand2s(src, Long), rs(reg->r[Q]));
                break;
            case REG_SSE_D:
                xx(OP_MOV, suffixf[Q], operand2s(src, Quad), rs(reg->r[Q]));
                break;
            case REG_SSE_FF:
                xx(OP_MOV, suffixi[Q], operand2s(src, Quad), rs(reg->r[Q]));
                break;
            }
        }
    }
}

static void emit_param(struct tac *tac, struct pnode *pnode)
{
    node_t *ty = AST_TYPE(pnode->param);
    if (isint(ty) || isptr(ty) || isfloat(ty))
        emit_param_scalar(tac, pnode);
    else if (isstruct(ty) || isunion(ty))
        emit_param_record(tac, pnode);
    else
        assert(0);
}

static struct set * get_pinfo_regs(struct pinfo *pinfo)
{
    struct set *set = set_new();
    for (int i = 0; i < pinfo->gp; i++)
        set_add(set, iarg_regs[i]);
    for (int i = 0; i < pinfo->fp; i++)
        set_add(set, farg_regs[i]);
    return set;
}

static void copy_retval_to_register_record(node_t *rty,
                                           struct paddr *retaddr,
                                           struct operand *result)
{
    // move from regs to stack
    size_t size = retaddr->size;
    long loff = 0;
    long base_loff = fcon.calls_return_loff;
    int cnt = ROUNDUP(retaddr->size, 8) >> 3;
    for (int i = 0; i < cnt; i++, loff += 8, size -= 8) {
        struct reg *reg = retaddr->u.regs[i].reg;
        int type = retaddr->u.regs[i].type;
        switch (type) {
        case REG_INT:
            switch (size) {
            case 1:
            case 2:
                {
                    int opsize = size == 1 ? Byte : Word;
                    int i = idx[opsize];
                    xx(OP_MOV, suffixi[i],
                       rs(reg->r[i]),
                       subst(rbp->r[Q], loff + base_loff));
                }
                break;
            case 3:
                {
                    xx(OP_MOV, suffixi[W],
                       rs(reg->r[W]),
                       subst(rbp->r[Q], loff + base_loff));
                    xx(OP_SHR, suffixi[L],
                       imm(16),
                       rs(reg->r[L]));
                    xx(OP_MOV, suffixi[B],
                       rs(reg->r[B]),
                       subst(rbp->r[Q], loff + base_loff + 2));
                }
                break;
            case 4:
                xx(OP_MOV, suffixi[L],
                   rs(reg->r[L]),
                   subst(rbp->r[Q], loff + base_loff));
                break;
            case 5:
            case 6:
                {
                    int opsize = size == 5 ? Byte : Word;
                    int i = idx[opsize];
                    xx(OP_MOV, suffixi[L],
                       rs(reg->r[L]),
                       subst(rbp->r[Q], loff + base_loff));
                    xx(OP_SHR, suffixi[Q],
                       imm(32),
                       rs(reg->r[Q]));
                    xx(OP_MOV, suffixi[i],
                       rs(reg->r[i]),
                       subst(rbp->r[Q], loff + base_loff + 4));
                }
                break;
            case 7:
                {
                    xx(OP_MOV, suffixi[L],
                       rs(reg->r[L]),
                       subst(rbp->r[Q], loff + base_loff));
                    xx(OP_SHR, suffixi[Q],
                       imm(32),
                       rs(reg->r[Q]));
                    xx(OP_MOV, suffixi[W],
                       rs(reg->r[W]),
                       subst(rbp->r[Q], loff + base_loff + 4));
                    xx(OP_SHR, suffixi[L],
                       imm(16),
                       rs(reg->r[L]));
                    xx(OP_MOV, suffixi[B],
                       rs(reg->r[B]),
                       subst(rbp->r[Q], loff + base_loff + 6));
                }
                break;
                // >= 8
            default:
                xx(OP_MOV, suffixi[Q],
                   rs(reg->r[Q]),
                   subst(rbp->r[Q], loff + base_loff));
                break;
            }
            break;
        case REG_SSE_F:
            xx(OP_MOV, suffixf[L],
               rs(reg->r[Q]),
               subst(rbp->r[Q], loff + base_loff));
            break;
        case REG_SSE_D:
            xx(OP_MOV, suffixf[Q],
               rs(reg->r[Q]),
               subst(rbp->r[Q], loff + base_loff));
            break;
        case REG_SSE_FF:
            xx(OP_MOV, "lps",
               rs(reg->r[Q]),
               subst(rbp->r[Q], loff + base_loff));
            break;
        }
    }
    SYM_X_LOFF(result->sym) = fcon.calls_return_loff;
    SYM_X_KIND(result->sym) = SYM_KIND_LREF;
}

static void copy_retval_to_register_scalar(node_t *rty,
                                           struct paddr *retaddr,
                                           struct operand *result)
{
    struct reg *reg = retaddr->u.regs[0].reg;
    if (TYPE_SIZE(rty) == Quad)
        load(reg, result->sym, Quad);
    else
        load(reg, result->sym, Long);
}

static void copy_retval_to_stack(struct paddr *retaddr, struct operand *result)
{
    SYM_X_LOFF(result->sym) = fcon.calls_return_loff;
    SYM_X_KIND(result->sym) = SYM_KIND_LREF;
    load(int_regs[RAX], result->sym, Quad);
}

static void emit_call_epilogue(node_t *ftype, struct paddr *retaddr, struct operand *result)
{
    if (retaddr->kind == ADDR_STACK) {
        // memory
        // rax contains the address
        copy_retval_to_stack(retaddr, result);
    } else if (retaddr->kind == ADDR_REGISTER) {
        // register
        node_t *rty = rtype(ftype);
        if (isstruct(rty) || isunion(rty))
            copy_retval_to_register_record(rty, retaddr, result);
        else
            copy_retval_to_register_scalar(rty, retaddr, result);
    }
}

static void emit_nonbuiltin_call(struct tac *tac)
{
    node_t *call = tac->call;
    node_t **args = EXPR_ARGS(call);
    struct operand *result = tac->operands[0];
    struct operand *l = tac->operands[1];
    size_t len = length(args);
    node_t *ftype = rtype(AST_TYPE(EXPR_OPERAND(call, 0)));
    struct vector *params = vec_new();
    struct tac *t = tac->prev;
    for (size_t i = 0; i < len; i++, t = t->prev) {
        assert(t->op == IR_PARAM);
        vec_push(params, t);    // in reverse order
    }

    struct pinfo *pinfo = alloc_addr_for_funcall(ftype, args);

    // drain all non-preserved registers
    for_each_reg(is_nonpreserved, drain_reg);
    // push excepts
    struct set *excepts = get_pinfo_regs(pinfo);
    push_excepts(excepts);
    
    // emit args
    size_t k = 0;
    if (pinfo->retaddr && pinfo->retaddr->kind == ADDR_STACK) {
        struct pnode *pnode = vec_at(pinfo->pnodes, 0);
        struct reg *reg = pnode->paddr->u.regs[0].reg;
        assert(reg == iarg_regs[0]);
        xx(OP_LEA, suffixi[Q],
           subst(rbp->r[Q], fcon.calls_return_loff),
           rs(reg->r[Q]));
        k = 1;
    }
    for (size_t i = k; i < vec_len(pinfo->pnodes); i++) {
        struct pnode *pnode = vec_at(pinfo->pnodes, i);
        struct tac *param = vec_at(params, (len-1) - (i-k));
        emit_param(param, pnode);
    }

    // set fp to rax (al)
    if (TYPE_VARG(ftype) || TYPE_OLDSTYLE(ftype))
        xx(OP_MOV, suffixi[B], imm(pinfo->fp), rs(int_regs[RAX]->r[B]));

    // direct / indirect
    if (l->op == IR_NONE && SYM_X_KIND(l->sym) == SYM_KIND_GREF)
        xx(OP_CALL, NULL, rs(SYM_X_LABEL(l->sym)), NULL);
    else
        xx(OP_CALL, " *", operand2s(l, Quad), NULL);

    // pop excepts
    pop_excepts();
    // clear nonpreserved regs
    for_each_reg(is_nonpreserved, clear_reg);

    if (result)
        emit_call_epilogue(ftype, pinfo->retaddr, result);

    // update use of params
    for (int i = vec_len(params) - 1; i >= 0; i--) {
        struct tac *tac = vec_at(params, i);
        update_use(tac);
    }
}

/*
  Initialize the va_list structure.

  typedef struct __builtin_va_list_tag {
    unsigned int gp_offset;
    unsigned int fp_offset;
    void *overflow_arg_area;
    void *reg_save_area;
  } __builtin_va_list[1];
 */
// void __builtin_va_start(__builtin_va_list, ...);
static void emit_builtin_va_start(struct tac *tac)
{
    node_t *call = tac->call;
    node_t **args = EXPR_ARGS(call);
    node_t *arg0 = args[0];
    struct operand *l = EXPR_X_ADDR(arg0);
    struct pinfo *pinfo = fcon.pinfo;
    unsigned int gp_offset = pinfo->gp << 3;
    unsigned int fp_offset = 48 + (pinfo->fp << 4);
    long overflow_arg_area = STACK_PARAM_BASE_OFF;
    long reg_save_area = -REGISTER_SAVE_AREA_SIZE;

    emit(COMMENT1("va_start begin"));

    // gp_offset
    struct operand *operand1 = make_offset_operand(l, 0);
    struct reladdr *label1 = operand2s(operand1, Long);
    xx(OP_MOV, suffixi[L], imm(gp_offset), label1);

    // fp_offset
    struct operand *operand2 = make_offset_operand(l, 4);
    struct reladdr *label2 = operand2s(operand2, Long);
    xx(OP_MOV, suffixi[L], imm(fp_offset), label2);

    struct set *excepts = operand_regs(l);
    struct reg *reg = get_one_ireg(excepts);
    
    // overflow_arg_area
    struct operand *operand3 = make_offset_operand(l, 8);
    struct reladdr *label3 = operand2s(operand3, Quad);
    xx(OP_LEA, suffixi[Q], subst(rbp->r[Q], overflow_arg_area), rs(reg->r[Q]));
    xx(OP_MOV, suffixi[Q], rs(reg->r[Q]), label3);

    // reg_save_area
    struct operand *operand4 = make_offset_operand(l, 16);
    struct reladdr *label4 = operand2s(operand4, Quad);

    xx(OP_LEA, suffixi[Q], subst(rbp->r[Q], reg_save_area), rs(reg->r[Q]));
    xx(OP_MOV, suffixi[Q], rs(reg->r[Q]), label4);

    emit(COMMENT1("va_start end"));
}

/*
  va_arg(ap, type)
  
  tmp = ap.overflow_arg_area;
  ap.overflow_arg_area += sizeof type;
  result = tmp;
*/
static void emit_builtin_va_arg_p_memory(struct tac *tac)
{
    node_t *call = tac->call;
    node_t **args = EXPR_ARGS(call);
    node_t *arg0 = args[0];
    struct operand *l = EXPR_X_ADDR(arg0);
    struct operand *result = tac->operands[0];
    node_t *ty = EXPR_VA_ARG_TYPE(call);
    size_t size = ROUNDUP(TYPE_SIZE(ty), 8);

    struct operand *operand = make_offset_operand(l, 8);
    struct reladdr *dst_label = operand2s(operand, Quad);
    struct set *excepts = operand_regs(l);
    struct reg *tmp1 = get_one_ireg(excepts);
    set_add(excepts, tmp1);
    struct reg *tmp2 = get_one_ireg(excepts);

    xx(OP_MOV, suffixi[Q], dst_label, rs(tmp1->r[Q]));
    xx(OP_MOV, suffixi[Q], rs(tmp1->r[Q]), rs(tmp2->r[Q]));
    xx(OP_ADD, suffixi[Q], imm(size), rs(tmp1->r[Q]));
    xx(OP_MOV, suffixi[Q], rs(tmp1->r[Q]), dst_label);
    load(tmp2, result->sym, Quad);
}

/**
 count record type's gp, fp;
 size = type size of 'ty' (arg1)

 if (gp > 0) {
     gp_offset_max = 48 - (gp-1)*8;
     if (ap.gp_offset >= gp_offset_max)
         goto mem;
 }
 if (fp > 0) {
     fp_offset_max = 176 - (fp-1)*16;
     if (ap.fp_offset >= fp_offset_max)
         goto mem;
 }

 if (gp > 0 && fp > 0) {
     tmp2 = ap.reg_save_area + ap.gp_offset;
     tmp3 = ap.reg_save_area + ap.fp_offset;
     dst[0] = *tmp2;
     dst[1] = *tmp3;
     tmp = dst;
 } else if (gp > 0) {
     tmp = ap.reg_save_area + ap.gp_offset;
 } else if (fp > 0) {
     tmp = ap.ref_save_area + ap.fp_offset;
 }

 if (gp > 0) {
     ap.gp_offset += gp << 3;
 }
 if (fp > 0) {
     ap.fp_offset += fp << 4;
 }
 goto out;

 mem:
 tmp = ap.overflow_arg_area;
 ap.overflow_arg_area += ROUNDUP(size, 8);

 out:
 result = tmp;
 
 */
static void emit_builtin_va_arg_p_record(struct tac *tac)
{
    node_t *call = tac->call;
    node_t **args = EXPR_ARGS(call);
    node_t *arg0 = args[0];
    struct operand *l = EXPR_X_ADDR(arg0);
    struct operand *result = tac->operands[0];
    node_t *ty = EXPR_VA_ARG_TYPE(call);
    
    struct operand *gp_operand = make_offset_operand(l, 0);
    struct operand *fp_operand = make_offset_operand(l, 4);
    struct operand *over_area_operand = make_offset_operand(l, 8);
    struct operand *reg_area_operand = make_offset_operand(l, 16);
        
    struct reladdr *gp_label = operand2s(gp_operand, Long);
    struct reladdr *fp_label = operand2s(fp_operand, Long);
    struct reladdr *over_area_label = operand2s(over_area_operand, Quad);
    struct reladdr *reg_area_label = operand2s(reg_area_operand, Quad);
        
    const char *mem_label = gen_label();
    const char *out_label = gen_label();

    // struct/union
    node_t *arg1 = args[1];
    struct operand *r = EXPR_X_ADDR(arg1);
    struct set *excepts1 = operand_regs(l);
    struct set *excepts2 = operand_regs(r);
    struct set *excepts = set_union(excepts1, excepts2);
    struct reg *tmp1 = get_one_ireg(excepts);
    set_add(excepts, tmp1);
    struct reg *tmp2 = get_one_ireg(excepts);
            
    struct vector *ptypes = get_types(ty, 0);
    struct vector *elements = get_elements(ptypes);
    struct vector *classes = get_classes(elements);
    size_t size = TYPE_SIZE(ty);
    int gp = 0;
    int fp = 0;
    for (int i = 0; i < vec_len(classes); i++) {
        int *class = vec_at(classes, i);
        if (class == integer_class)
            gp++;
        else if (class == sse_class)
            fp++;
        else
            assert(0);
    }
    
    if (gp > 0) {
        unsigned gp_offset_max = 48 - (gp-1) * 8;
        xx(OP_MOV, suffixi[L], gp_label, rs(tmp1->r[L]));
        xx(OP_CMP, suffixi[L], imm(gp_offset_max), rs(tmp1->r[L]));
        xx(OP_JNB, NULL, rs(mem_label), NULL);
    }
    if (fp > 0) {
        unsigned fp_offset_max = 176 - (fp-1) * 16;
        xx(OP_MOV, suffixi[L], fp_label, rs(tmp1->r[L]));
        xx(OP_CMP, suffixi[L], imm(fp_offset_max), rs(tmp1->r[L]));
        xx(OP_JNB, NULL, rs(mem_label), NULL);
    }
    // register
    if (gp > 0 && fp > 0) {
        set_add(excepts, tmp2);
        struct reg *tmp3 = get_one_ireg(excepts);
        // TODO: assume vec_len == 2
        assert(vec_len(classes) == 2);
        bool tmp2_used = false;
        for (int i = 0; i < vec_len(classes); i++) {
            int *class = vec_at(classes, i);
            if (class == integer_class) {
                xx(OP_MOV, suffixi[Q], reg_area_label, rs(tmp1->r[Q]));
                struct reg *tmp = tmp2_used ? tmp3 : tmp2;
                xx(OP_MOV, suffixi[L], gp_label, rs(tmp->r[L]));
                xx(OP_ADD, suffixi[Q], rs(tmp1->r[Q]), rs(tmp->r[Q]));
                tmp2_used = true;
            } else if (class == sse_class) {
                xx(OP_MOV, suffixi[Q], reg_area_label, rs(tmp1->r[Q]));
                struct reg *tmp = tmp2_used ? tmp3 : tmp2;
                xx(OP_MOV, suffixi[L], fp_label, rs(tmp->r[L]));
                xx(OP_ADD, suffixi[Q], rs(tmp1->r[Q]), rs(tmp->r[Q]));
                tmp2_used = true;
            }
        }
        struct reg *dst_reg = SYM_X_REG(r->sym);
        xx(OP_MOV, suffixi[Q],
           str("(%s)", tmp2->r[Q]),
           rs(tmp2->r[Q]));
        xx(OP_MOV, suffixi[Q],
           rs(tmp2->r[Q]),
           str("(%s)", dst_reg->r[Q]));
        xx(OP_MOV, suffixi[Q],
           str("(%s)", tmp3->r[Q]),
           rs(tmp3->r[Q]));
        xx(OP_MOV, suffixi[Q],
           rs(tmp3->r[Q]),
           str("8(%s)", dst_reg->r[Q]));
        xx(OP_MOV, suffixi[Q],
           rs(dst_reg->r[Q]),
           rs(tmp1->r[Q]));
    } else if (gp > 0) {
        xx(OP_MOV, suffixi[Q],
           reg_area_label,
           rs(tmp1->r[Q]));
        xx(OP_MOV, suffixi[L],
           gp_label,
           rs(tmp2->r[L]));
        xx(OP_ADD, suffixi[Q],
           rs(tmp2->r[Q]),
           rs(tmp1->r[Q]));
    } else if (fp > 0) {
        xx(OP_MOV, suffixi[Q],
           reg_area_label,
           rs(tmp1->r[Q]));
        xx(OP_MOV, suffixi[L],
           fp_label,
           rs(tmp2->r[L]));
        xx(OP_ADD, suffixi[Q],
           rs(tmp2->r[Q]),
           rs(tmp1->r[Q]));
    }

    if (gp > 0) {
        xx(OP_MOV, suffixi[L],
           gp_label, rs(tmp2->r[L]));
        xx(OP_ADD, suffixi[L],
           imm(gp << 3),
           rs(tmp2->r[L]));
        xx(OP_MOV, suffixi[L],
           rs(tmp2->r[L]),
           gp_label);
    }
    if (fp > 0) {
        xx(OP_MOV, suffixi[L],
           fp_label,
           rs(tmp2->r[L]));
        xx(OP_ADD, suffixi[L],
           imm(fp << 4),
           rs(tmp2->r[L]));
        xx(OP_MOV, suffixi[L],
           rs(tmp2->r[L]),
           fp_label);
    }
    jmp(out_label);
            
    // memory
    lab(mem_label);
    xx(OP_MOV, suffixi[Q], over_area_label, rs(tmp1->r[Q]));
    xx(OP_LEA, suffixi[Q],
       str("%lu(%s)", ROUNDUP(size, 8), tmp1->r[Q]),
       rs(tmp2->r[Q]));
    xx(OP_MOV, suffixi[Q], rs(tmp2->r[Q]), over_area_label);
            
    // end
    lab(out_label);
    load(tmp1, result->sym, Quad);
}

/*
  integer:

  if (ap.gp_offset <= 48) {
      // register
      tmp = ap.reg_save_area + ap.gp_offset;
      ap.gp_offset += 8;
      result = tmp;
  } else {
      // memory
      tmp = ap.overflow_arg_area;
      ap.overflow_arg_area += 8;
      result = tmp;
  }

  floating:
  
 */
static void emit_builtin_va_arg_p_scalar(struct tac *tac)
{
    node_t *call = tac->call;
    node_t **args = EXPR_ARGS(call);
    node_t *arg0 = args[0];
    struct operand *l = EXPR_X_ADDR(arg0);
    struct operand *result = tac->operands[0];
    node_t *ty = EXPR_VA_ARG_TYPE(call);
    
    struct operand *gp_operand = make_offset_operand(l, 0);
    struct operand *fp_operand = make_offset_operand(l, 4);
    struct operand *over_area_operand = make_offset_operand(l, 8);
    struct operand *reg_area_operand = make_offset_operand(l, 16);
        
    struct reladdr *gp_label = operand2s(gp_operand, Long);
    struct reladdr *fp_label = operand2s(fp_operand, Long);
    struct reladdr *over_area_label = operand2s(over_area_operand, Quad);
    struct reladdr *reg_area_label = operand2s(reg_area_operand, Quad);
        
    const char *mem_label = gen_label();
    const char *out_label = gen_label();

    // scalar
    struct set *excepts = operand_regs(l);
    struct reg *tmp1 = get_one_ireg(excepts);
    set_add(excepts, tmp1);
    struct reg *tmp2 = get_one_ireg(excepts);
            
    struct reladdr *offset_label;
    unsigned offset_max;
    int add_size;
    if (isfloat(ty)) {
        offset_label = fp_label;
        offset_max = 176;
        add_size = 16;
    } else {
        offset_label = gp_label;
        offset_max = 48;
        add_size = 8;
    }

    xx(OP_MOV, suffixi[L], offset_label, rs(tmp1->r[L]));
    xx(OP_CMP, suffixi[L], imm(offset_max), rs(tmp1->r[L]));
    xx(OP_JNB, NULL, rs(mem_label), NULL);

    // register
    xx(OP_MOV, suffixi[Q], reg_area_label, rs(tmp1->r[Q]));
    xx(OP_MOV, suffixi[L], offset_label, rs(tmp2->r[L]));
    xx(OP_ADD, suffixi[Q], rs(tmp2->r[Q]), rs(tmp1->r[Q]));
    xx(OP_ADD, suffixi[L], imm(add_size), rs(tmp2->r[L]));
    xx(OP_MOV, suffixi[L], rs(tmp2->r[L]), offset_label);
    jmp(out_label);

    // memory
    lab(mem_label);
    xx(OP_MOV, suffixi[Q], over_area_label, rs(tmp1->r[Q]));
    xx(OP_LEA, suffixi[Q], str("8(%s)", tmp1->r[Q]), rs(tmp2->r[Q]));
    xx(OP_MOV, suffixi[Q], rs(tmp2->r[Q]), over_area_label);

    // end
    lab(out_label);
    load(tmp1, result->sym, TYPE_SIZE(ty));
}

// return the address of the argument.
// void * __builtin_va_arg_p(__builtin_va_list, ...);
static void emit_builtin_va_arg_p(struct tac *tac)
{
    node_t *call = tac->call;
    node_t *ty = EXPR_VA_ARG_TYPE(call);
    if (TYPE_SIZE(ty) > MAX_STRUCT_PARAM_SIZE) {
        // by meory
        emit_builtin_va_arg_p_memory(tac);
    } else {
        // by registers or memory (no enough registers)
        if (isrecord(ty))
            emit_builtin_va_arg_p_record(tac);
        else
            emit_builtin_va_arg_p_scalar(tac);
    }
}

static void emit_call(struct tac *tac)
{
    const char *name = SYM_NAME(tac->operands[1]->sym);
    if (!strcmp(name, BUILTIN_VA_START))
        emit_builtin_va_start(tac);
    else if (!strcmp(name, BUILTIN_VA_ARG_P))
        emit_builtin_va_arg_p(tac);
    else
        emit_nonbuiltin_call(tac);
}

static void emit_return_by_stack(struct operand *l, struct paddr *retaddr)
{
    // by memory
    // copy l to the address pointed by the implicit first argument
    // rax will contain the address that has passed in by caller in rdi.
    struct pnode *pnode = vec_head(fcon.pinfo->pnodes);
    node_t *sym = pnode->param;
    struct reg *rax = int_regs[RAX];    
    struct set *excepts = operand_regs(l);
    struct reg *tmp = get_one_ireg(excepts);
    set_add(excepts, tmp);

    // set destination base address
    xx(OP_MOV, suffixi[Q],
       subst(rbp->r[Q], SYM_X_LOFF(sym)), rs(rax->r[Q]));

    // Can't be IR_INDIRECTION (see ir.c: emit_uop_indirection)
    assert(l->op == IR_NONE || l->op == IR_SUBSCRIPT);
    // emit bytes
    // rounded by 8 bytes. (see emit_function_prologue)
    size_t size = ROUNDUP(retaddr->size, 8);
    for (size_t i = 0; i < size; i += 8) {
        struct operand *operand = make_offset_operand(l, i);
        struct reladdr *dst_label;
        push_excepts(excepts);
        struct reladdr *src_label = operand2s(operand, Quad);
        pop_excepts();
        if (i)
            dst_label = str("%ld(%s)", i, rax->r[Q]);
        else
            dst_label = str("(%s)", rax->r[Q]);
        xx(OP_MOV, suffixi[Q], src_label, rs(tmp->r[Q]));
        xx(OP_MOV, suffixi[Q], rs(tmp->r[Q]), dst_label);
    }
}

static void emit_return_by_registers_scalar(struct operand *l, struct paddr *retaddr)
{
    // scalar
    node_t *rtype = rtype(fcon.current_ftype);
    int opsize = TYPE_SIZE(rtype);
    int i = idx[opsize];
    struct reladdr *l_label = operand2s(l, opsize);
    struct reg *reg = retaddr->u.regs[0].reg;

    if (isint(rtype) || isptr(rtype)) {
        if (opsize < 4) {
            // extend to 32bits
            bool sign = TYPE_OP(rtype) == INT;
            if (sign)
                xx(OP_MOV, format("s%sl", suffixi[i]), l_label, rs(reg->r[L]));
            else
                xx(OP_MOV, format("z%sl", suffixi[i]), l_label, rs(reg->r[L]));
        } else {
            xx(OP_MOV, suffixi[i], l_label, rs(reg->r[i]));
        }
    } else if (isfloat(rtype)) {
        xx(OP_MOV, suffixf[i], l_label, rs(reg->r[i]));
    } else {
        assert(0);
    }
}

static void emit_return_by_registers_record(struct operand *l, struct paddr *retaddr)
{
    size_t size = retaddr->size;
    int cnt = ROUNDUP(retaddr->size, 8) >> 3;
    long loff = 0;
    
    for (int i = 0; i < cnt; i++, loff += 8, size -= 8) {
        int type = retaddr->u.regs[i].type;
        struct reg *reg = retaddr->u.regs[i].reg;
        struct operand *operand = make_offset_operand(l, loff);
        switch (type) {
        case REG_INT:
            switch (size) {
            case 1:
            case 2:
                {
                    int opsize = size == 1 ? Byte : Word;
                    int i = idx[opsize];
                    xx(OP_MOV, suffixi[i], operand2s(operand, opsize), rs(reg->r[i]));
                }
                break;
            case 3:
                {
                    struct operand *operand1 = make_offset_operand(l, loff + 2);
                    struct reladdr *label = operand2s(operand, Long);
                    struct reladdr *label1 = operand2s(operand1, Byte);
                    struct set *excepts = operand_regs(operand);
                    struct reg *tmp = get_one_ireg(excepts);

                    xx(OP_MOV, "zbl", label1, rs(tmp->r[L]));
                    xx(OP_SHL, suffixi[L], imm(16), rs(tmp->r[L]));

                    xx(OP_MOV, "zwl", label, rs(reg->r[L]));
                    xx(OP_OR, suffixi[L], rs(tmp->r[L]), rs(reg->r[L]));
                }
                break;
            case 4:
                xx(OP_MOV, suffixi[L], operand2s(operand, Long), rs(reg->r[L]));
                break;
            case 5:
            case 6:
                {
                    int opsize = size == 5 ? Byte : Word;
                    int i = idx[opsize];
                    struct operand *operand1 = make_offset_operand(l, loff + 4);
                    struct set *excepts = operand_regs(operand);
                    struct reg *tmp1 = get_one_ireg(excepts);
                    xx(OP_MOV, format("z%sl", suffixi[i]), operand2s(operand1, opsize), rs(tmp1->r[L]));
                    xx(OP_SHL, suffixi[Q], imm(32), rs(tmp1->r[Q]));

                    set_add(excepts, tmp1);
                    struct reg *tmp2 = get_one_ireg(excepts);
                    xx(OP_MOV, suffixi[L], operand2s(operand, Long), rs(tmp2->r[L]));
                    xx(OP_OR, suffixi[Q], rs(tmp2->r[Q]), rs(tmp1->r[Q]));

                    xx(OP_MOV, suffixi[Q], rs(tmp1->r[Q]), rs(reg->r[Q]));
                }
                break;
            case 7:
                {
                    struct operand *operand1 = make_offset_operand(l, loff + 6);
                    struct set *excepts = operand_regs(operand);
                    struct reg *tmp1 = get_one_ireg(excepts);
                    xx(OP_MOV, "zbl", operand2s(operand1, Byte), rs(tmp1->r[L]));
                    xx(OP_SHL, suffixi[Q], imm(16), rs(tmp1->r[Q]));

                    set_add(excepts, tmp1);
                    struct operand *operand2 = make_offset_operand(l, loff + 4);
                    struct reg *tmp2 = get_one_ireg(excepts);
                    xx(OP_MOV, "zwl", operand2s(operand2, Word), rs(tmp2->r[L]));

                    xx(OP_OR, suffixi[L], rs(tmp1->r[L]), rs(tmp2->r[L]));
                    xx(OP_SHL, suffixi[Q], imm(32), rs(tmp2->r[Q]));

                    // reuse tmp1 here
                    xx(OP_MOV, suffixi[L], operand2s(operand, Long), rs(tmp1->r[L]));
                    xx(OP_OR, suffixi[Q], rs(tmp2->r[Q]), rs(tmp1->r[Q]));

                    xx(OP_MOV, suffixi[Q], rs(tmp1->r[Q]), rs(reg->r[Q]));
                }
                break;
                // >=8
            default:
                xx(OP_MOV, suffixi[Q], operand2s(operand, Quad), rs(reg->r[Q]));
                break;
            }
            break;
        case REG_SSE_F:
            xx(OP_MOV, suffixf[L], operand2s(operand, Quad), rs(reg->r[Q]));
            break;
        case REG_SSE_D:
            xx(OP_MOV, suffixf[Q], operand2s(operand, Quad), rs(reg->r[Q]));
            break;
        case REG_SSE_FF:
            xx(OP_MOV, "lps", operand2s(operand, Quad), rs(reg->r[Q]));
            break;
        }
    }
}

struct set * get_return_regs(struct paddr *retaddr)
{
    struct set *regs = set_new();
    int cnt = ROUNDUP(retaddr->size, 8) >> 3;
    for (int i = 0; i < cnt; i++) {
        struct reg *reg = retaddr->u.regs[i].reg;
        set_add(regs, reg);
    }
    return regs;
}

// return regs: rax, rdx, xmm0, xmm1
static void emit_return(struct tac *tac)
{
    struct operand *l = tac->operands[1];
    // non-void return
    if (l) {
        struct paddr *retaddr = fcon.pinfo->retaddr;
        if (retaddr->kind == ADDR_STACK) {
            // by stack
            // rax contains the address
            struct reg *rax = int_regs[RAX];
            drain_reg(rax);
            push_excepts(set_new1(rax));
            emit_return_by_stack(l, retaddr);
            pop_excepts();
        } else if (retaddr->kind == ADDR_REGISTER) {
            // by register
            // integer registers: rax, rdx
            // sse registers: xmm0, xmm1
            struct set *excepts = get_return_regs(retaddr);
            struct vector *regs = set_objects(excepts);
            for (int i = 0; i < vec_len(regs); i++) {
                struct reg *reg = vec_at(regs, i);
                drain_reg(reg);
            }
            push_excepts(excepts);

            node_t *rtype = rtype(fcon.current_ftype);
            if (isstruct(rtype) || isunion(rtype))
                emit_return_by_registers_record(l, retaddr);
            else
                emit_return_by_registers_scalar(l, retaddr);

            pop_excepts();
        }
    }
    jump(OP_JMP, fcon.end_label);
}

// if x relop y goto dest
static void emit_if_relop(struct tac *tac, bool reverse, bool floating)
{
    struct operand *result = tac->operands[0];
    struct operand *l = tac->operands[1];
    struct operand *r = tac->operands[2];
    int i = idx[tac->opsize];
    bool sign = tac->sign;

    struct set *vl = operand_regs(l);
    struct set *vr = operand_regs(r);
    struct set *excepts = set_union(vl, vr);

    // l label
    push_excepts(excepts);
    struct reladdr *l_label = operand2s(l, tac->opsize);
    pop_excepts();

    // r label
    vl = operand_regs(l);
    excepts = set_union(excepts, vl);
    push_excepts(excepts);
    struct reladdr *r_label = operand2s(r, tac->opsize);
    pop_excepts();

    vr = operand_regs(r);
    excepts = set_union(excepts, vr);
    
    struct reg *reg;
    if (floating) {
        reg = get_one_freg(excepts);
        xx(OP_MOV, suffixf[i], l_label, rs(reg->r[i]));
        xx(OP_UCOMI, suffixf[i], r_label, rs(reg->r[i]));
    } else {
        reg = get_one_ireg(excepts);
        xx(OP_MOV, suffixi[i], l_label, rs(reg->r[i]));
        xx(OP_CMP, suffixi[i], r_label, rs(reg->r[i]));
    }
        
    const char *jop;
    switch (tac->relop) {
    case '>':
        if (reverse)
            jop = sign ? OP_JLE : OP_JBE;
        else
            jop = sign ? OP_JG : OP_JA;
        break;
    case '<':
        if (reverse)
            jop = sign ? OP_JGE : OP_JAE;
        else
            jop = sign ? OP_JL : OP_JB;
        break;
    case GEQ:
        if (reverse)
            jop = sign ? OP_JL : OP_JB;
        else
            jop = sign ? OP_JGE : OP_JAE;
        break;
    case LEQ:
        if (reverse)
            jop = sign ? OP_JG : OP_JA;
        else
            jop = sign ? OP_JLE : OP_JBE;
        break;
    case NEQ:
        jop = reverse ? OP_JE : OP_JNE;
        break;
    case EQ:
        jop = reverse ? OP_JNE : OP_JE;
        break;
    default:
        assert(0);
    }
    jump(jop, SYM_X_LABEL(result->sym));
}

// if x goto dest
static void emit_if_simple(struct tac *tac, bool reverse, bool floating)
{
    struct operand *result = tac->operands[0];
    struct operand *l = tac->operands[1];
    int i = idx[tac->opsize];
    struct reladdr *l_label = operand2s(l, tac->opsize);
    
    if (floating) {
        struct set *excepts = operand_regs(l);
        struct reg *r = get_one_freg(excepts);
        xx(OP_XOR, suffixp[i], rs(r->r[i]), rs(r->r[i]));
        xx(OP_UCOMI, suffixf[i], l_label, rs(r->r[i]));
        const char *jop = reverse ? OP_JE : OP_JNE;
        jump(jop, SYM_X_LABEL(result->sym));
    } else {
        if (is_imm_operand(l)) {
            long value = SYM_VALUE_I(l->sym);
            if (reverse) {
                if (!value)
                    jump(OP_JMP, SYM_X_LABEL(result->sym));
            } else {
                if (value)
                    jump(OP_JMP, SYM_X_LABEL(result->sym));
            }
        } else {
            xx(OP_CMP, suffixi[i], imm(0), l_label);
            const char *jop = reverse ? OP_JE : OP_JNE;
            jump(jop, SYM_X_LABEL(result->sym));
        }
    }
}

static void emit_if(struct tac *tac, bool reverse, bool floating)
{
    if (tac->relop)
        emit_if_relop(tac, reverse, floating);
    else
        emit_if_simple(tac, reverse, floating);
}

static void emit_if_i(struct tac *tac)
{
    emit_if(tac, false, false);
}

static void emit_iffalse_i(struct tac *tac)
{
    emit_if(tac, true, false);
}

static void emit_if_f(struct tac *tac)
{
    emit_if(tac, false, true);
}

static void emit_iffalse_f(struct tac *tac)
{
    emit_if(tac, true, true);
}

static void emit_goto(struct tac *tac)
{
    jump(OP_JMP, SYM_X_LABEL(tac->operands[0]->sym));
}

static void emit_assign_basic(struct operand *l, struct operand *r,
                              int opsize, bool assignf)
{
    struct set *vl = operand_regs(l);
    struct set *vr = operand_regs(r);
    struct set *excepts = set_union(vl, vr);
    
    push_excepts(excepts);
    struct reladdr *dst;
    if (is_direct_mem_operand(l))
        dst = operand2s_none_ex(l, opsize, true);
    else
        dst = operand2s(l, opsize);
    pop_excepts();

    vl = operand_regs(l);
    excepts = set_union(excepts, vl);
    push_excepts(excepts);
    struct reladdr *src = operand2s(r, opsize);
    pop_excepts();
    
    const char **suffix = assignf ? suffixf : suffixi;
    int i = idx[opsize];
    xx(OP_MOV, suffix[i], src, dst);
}

static void emit_assign_spill_tmp(struct operand *l, struct operand *r,
                                  int opsize, bool assignf)
{
    struct reladdr *src = operand2s(r, opsize);
    struct reladdr *dst = subst(rbp->r[Q], SYM_X_LOFF(l->sym));
    const char **suffix = assignf ? suffixf : suffixi;
    int i = idx[opsize];

    if (is_mem_operand(r)) {
        struct set *vl = operand_regs(l);
        struct set *vr = operand_regs(r);
        struct set *excepts = set_union(vl, vr);
        struct reg *reg;
        if (assignf)
            reg = get_one_freg(excepts);
        else
            reg = get_one_ireg(excepts);
        xx(OP_MOV, suffix[i], src, rs(reg->r[i]));
        src = rs(reg->r[i]);
    }
    
    xx(OP_MOV, suffix[i], src, dst);
}

static void emit_assign(struct tac *tac)
{
    bool assignf = tac->op == IR_ASSIGNF;
    struct operand *l = tac->operands[0];
    struct operand *r = tac->operands[1];
    if (is_mem_operand(l)) {
        // mem = tmp
        // mem = imm
        emit_assign_basic(l, r, tac->opsize, assignf);
        // POST load
        if (is_direct_mem_operand(l)) {
            store(l->sym);
            if (is_tmp_operand(r))
                load(SYM_X_REG(r->sym), l->sym, tac->opsize);
        }
    } else if (is_tmp_operand(l)) {
        // tmp = mem
        // tmp = tmp
        // tmp = imm
        if (SYM_X_LOFF(l->sym)) {
            // a spill tmp (treated as mem)
            emit_assign_spill_tmp(l, r, tac->opsize, assignf);
        } else if (SYM_X_REG(l->sym)) {
            emit_assign_basic(l, r, tac->opsize, assignf);
        } else if (is_direct_mem_operand(r) && SYM_X_REG(r->sym)) {
            load(SYM_X_REG(r->sym), l->sym, tac->opsize);
        } else {
            // alloc register for tmp operand
            int i = idx[tac->opsize];
            struct reladdr *src = operand2s(r, tac->opsize);
            struct set *excepts = operand_regs(r);
            struct reg *reg;
            if (assignf)
                reg = dispatch_freg(l->sym, excepts, tac->opsize);
            else
                reg = dispatch_ireg(l->sym, excepts, tac->opsize);
            
            const char **suffix = assignf ? suffixf : suffixi;
            xx(OP_MOV, suffix[i], src, rs(reg->r[i]));
            // POST load
            if (is_direct_mem_operand(r) && !SYM_X_REG(r->sym))
                load(reg, r->sym, tac->opsize);
        }
    } else {
        assert(0);
    }
}

static void emit_uop_int(struct tac *tac, const char *op)
{
    struct operand *result = tac->operands[0];
    struct operand *l = tac->operands[1];
    int i = idx[tac->opsize];
    struct reladdr *l_label = operand2s(l, tac->opsize);
    struct set *excepts = operand_regs(l);
    struct reg *reg = dispatch_ireg(result->sym, excepts, tac->opsize);
    xx(OP_MOV, suffixi[i], l_label, rs(reg->r[i]));
    xx(op, suffixi[i], rs(reg->r[i]), NULL);
}

// int: bitwise not ~
static void emit_uop_not(struct tac *tac)
{
    emit_uop_int(tac, OP_NOT);
}

static void emit_uop_minus_i(struct tac *tac)
{
    emit_uop_int(tac, OP_NEG);
}

static void emit_uop_minus_f(struct tac *tac)
{
    struct operand *result = tac->operands[0];
    struct operand *l = tac->operands[1];
    int i = idx[tac->opsize];
    struct reladdr *l_label = operand2s(l, tac->opsize);
    struct set *excepts = operand_regs(l);
    struct reg *reg = dispatch_freg(result->sym, excepts, tac->opsize);
    xx(OP_XOR, suffixp[i], rs(reg->r[i]), rs(reg->r[i]));
    xx(OP_SUB, suffixf[i], l_label, rs(reg->r[i]));
}

static void emit_uop_address(struct tac *tac)
{
    struct operand *result = tac->operands[0];
    struct operand *l = tac->operands[1];
    struct set *excepts = operand_regs(l);
    struct reg *reg = get_one_ireg(excepts);
    load(reg, result->sym, Quad);
    // gref func
    switch (l->op) {
    case IR_NONE:
        if (SYM_X_KIND(l->sym) == SYM_KIND_GREF)
            xx(OP_LEA, suffixi[Q], str("%s(%s)", SYM_X_LABEL(l->sym), rip->r[Q]), rs(reg->r[Q]));
        else if (SYM_X_KIND(l->sym) == SYM_KIND_LREF)
            xx(OP_LEA, suffixi[Q], subst(rbp->r[Q], SYM_X_LOFF(l->sym)), rs(reg->r[Q]));
        else
            assert(0);
        break;
    case IR_SUBSCRIPT:
    case IR_INDIRECTION:
        {
            struct set *excepts = set_new1(reg);
            push_excepts(excepts);
            struct reladdr *src_label = operand2s(l, Quad);
            pop_excepts();
            xx(OP_LEA, suffixi[Q], src_label, rs(reg->r[Q]));
        }
        break;
    default:
        assert(0);
    }
}

static void emit_bop_arith(struct tac *tac, const char *op, bool floating)
{
    struct operand *result = tac->operands[0];
    struct operand *l = tac->operands[1];
    struct operand *r = tac->operands[2];
    const char **suffix = floating ? suffixf : suffixi;
    int i = idx[tac->opsize];

    struct set *vl = operand_regs(l);
    struct set *vr = operand_regs(r);
    struct set *excepts = set_union(vl, vr);

    push_excepts(excepts);
    struct reladdr *l_label = operand2s(l, tac->opsize);
    pop_excepts();

    if (is_imm_operand(l) && tac->opsize == Quad) {
        struct reg *reg;
        if (floating)
            reg = dispatch_freg(l->sym, excepts, tac->opsize);
        else
            reg = dispatch_ireg(l->sym, excepts, tac->opsize);
        
        xx(OP_MOV, suffix[i], l_label, rs(reg->r[i]));
        l_label = rs(reg->r[i]);

        set_add(excepts, reg);
    }

    vl = operand_regs(l);
    excepts = set_union(excepts, vl);
    push_excepts(excepts);
    struct reladdr *r_label = operand2s(r, tac->opsize);
    pop_excepts();

    if (is_imm_operand(r) && tac->opsize == Quad) {
        struct reg *reg;
        if (floating)
            reg = dispatch_freg(r->sym, excepts, tac->opsize);
        else
            reg = dispatch_ireg(r->sym, excepts, tac->opsize);

        xx(OP_MOV, suffix[i], r_label, rs(reg->r[i]));
        r_label = rs(reg->r[i]);

        set_add(excepts, reg);
    }
    
    vr = operand_regs(r);
    excepts = set_union(excepts, vr);
    struct reg *reg;
    if (floating)
        reg = dispatch_freg(result->sym, excepts, tac->opsize);
    else
        reg = dispatch_ireg(result->sym, excepts, tac->opsize);

    xx(OP_MOV, suffix[i], l_label, rs(reg->r[i]));
    xx(op, suffix[i], r_label, rs(reg->r[i]));
}

static void emit_bop_int(struct tac *tac, const char *op)
{
    emit_bop_arith(tac, op, false);
}

static void emit_bop_float(struct tac *tac, const char *op)
{
    emit_bop_arith(tac, op, true);
}

// result in rax, rdx
static void emit_int_mul_div(struct tac *tac, const char *op)
{
    struct operand *l = tac->operands[1];
    struct operand *r = tac->operands[2];
    int i = idx[tac->opsize];
    struct reg *rax = int_regs[RAX];
    struct reg *rdx = int_regs[RDX];
    struct set *excepts = set_new();

    // drain rax
    drain_reg(rax);
    set_add(excepts, rax);

    // drain rdx
    if (tac->opsize > Byte) {
        drain_reg(rdx);
        set_add(excepts, rdx);
        // clear rdx
        if (tac->op == IR_DIVI || tac->op == IR_IDIVI ||
            tac->op == IR_MOD || tac->op == IR_IMOD)
            xx(OP_MOV, suffixi[i], imm(0), rs(rdx->r[i]));
    }

    // l label
    struct set *vl = operand_regs(l);
    struct set *vr = operand_regs(r);
    struct set *v = set_union(vl, vr);
    excepts = set_union(excepts, v);
    push_excepts(excepts);
    struct reladdr *l_label = operand2s(l, tac->opsize);
    pop_excepts();

    // r label
    vl = operand_regs(l);
    excepts = set_union(excepts, vl);
    push_excepts(excepts);
    struct reladdr *r_label = operand2s(r, tac->opsize);
    pop_excepts();

    xx(OP_MOV, suffixi[i], l_label, rs(rax->r[i]));
    if (is_imm_operand(r)) {
        vr = operand_regs(r);
        excepts = set_union(excepts, vr);
        struct reg *reg = dispatch_ireg(r->sym, excepts, tac->opsize);
        xx(OP_MOV, suffixi[i], r_label, rs(reg->r[i]));
        xx(op, suffixi[i], rs(reg->r[i]), NULL);
    } else {
        xx(op, suffixi[i], r_label, NULL);
    }
}

static void emit_int_imul(struct tac *tac)
{
    emit_int_mul_div(tac, OP_IMUL);
    load(int_regs[RAX], tac->operands[0]->sym, tac->opsize);
}

static void emit_int_mul(struct tac *tac)
{
    // the same as 'imul'
    emit_int_imul(tac);
}

static void emit_int_div(struct tac *tac)
{
    emit_int_mul_div(tac, OP_DIV);
    load(int_regs[RAX], tac->operands[0]->sym, tac->opsize);
}

static void emit_int_idiv(struct tac *tac)
{
    emit_int_mul_div(tac, OP_IDIV);
    load(int_regs[RAX], tac->operands[0]->sym, tac->opsize);
}

static void emit_mod(struct tac *tac)
{
    emit_int_mul_div(tac, OP_DIV);
    load(int_regs[RDX], tac->operands[0]->sym, tac->opsize);
}

static void emit_imod(struct tac *tac)
{
    emit_int_mul_div(tac, OP_IDIV);
    load(int_regs[RDX], tac->operands[0]->sym, tac->opsize);
}

static void emit_shift(struct tac *tac, const char *op)
{
    struct operand *result = tac->operands[0];
    struct operand *l = tac->operands[1];
    struct operand *r = tac->operands[2];
    int i = idx[tac->opsize];
    // drain rcx
    struct reg *rcx = int_regs[RCX];
    drain_reg(rcx);

    // get labels    
    struct set *vl = operand_regs(l);
    struct set *vr = operand_regs(r);
    struct set *excepts = set_union(vl, vr);
    set_add(excepts, rcx);
    
    push_excepts(excepts);
    struct reladdr *l_label = operand2s(l, tac->opsize);
    pop_excepts();

    vl = operand_regs(l);
    excepts = set_union(excepts, vl);
    push_excepts(excepts);
    struct reladdr *r_label = operand2s(r, tac->opsize);
    pop_excepts();

    // dispatch reg for result
    vr = operand_regs(r);
    excepts = set_union(excepts, vr);
    struct reg *reg = dispatch_ireg(result->sym, excepts, tac->opsize);

    xx(OP_MOV, suffixi[i], l_label, rs(reg->r[i]));
    xx(OP_MOV, suffixi[i], r_label, rs(rcx->r[i]));
    xx(op, suffixi[i], rs(rcx->r[B]), rs(reg->r[i]));
}

static void emit_conv_ii_widden(struct tac *tac, int typeop)
{
    struct operand *result = tac->operands[0];
    struct operand *l = tac->operands[1];
    int from_size = tac->from_opsize;
    int to_size = tac->to_opsize;
    int from_i = idx[from_size];
    int to_i = idx[to_size];
    // widden
    struct reladdr *src_label = operand2s(l, from_size);
    struct set *excepts = operand_regs(l);
    struct reg *reg = dispatch_ireg(result->sym, excepts, to_size);
    if (is_imm_operand(l)) {
        set_add(excepts, reg);
        struct reg *src_reg = dispatch_ireg(l->sym, excepts, from_size);
        xx(OP_MOV, suffixi[from_i], src_label, rs(src_reg->r[from_i]));
        // reset
        src_label = rs(src_reg->r[from_i]);
    }
    if (typeop == INT) {
        xx(OP_MOV, format("s%s%s", suffixi[from_i], suffixi[to_i]),
           src_label, rs(reg->r[to_i]));
    } else {
        if (from_size == Long)
            xx(OP_MOV, suffixi[L], src_label, rs(reg->r[from_i]));
        else
            xx(OP_MOV, format("z%s%s", suffixi[from_i], suffixi[to_i]),
               src_label, rs(reg->r[to_i]));
    }
}

static void emit_conv_ii_narrow(struct tac *tac, int typeop)
{
    struct operand *result = tac->operands[0];
    struct operand *l = tac->operands[1];
    int to_size = tac->to_opsize;
    int to_i = idx[to_size];
    // narrow
    struct reladdr *src_label = operand2s(l, to_size);
    struct set *excepts = operand_regs(l);
    struct reg *reg = dispatch_ireg(result->sym, excepts, to_size);
    xx(OP_MOV, suffixi[to_i], src_label, rs(reg->r[to_i]));
}

static void emit_conv_i2i(struct tac *tac, int typeop)
{
    if (tac->from_opsize < tac->to_opsize)
        emit_conv_ii_widden(tac, typeop);
    else if (tac->from_opsize > tac->to_opsize)
        emit_conv_ii_narrow(tac, typeop);
    else
        assert(0);
}

static void emit_conv_f2i(struct tac *tac)
{
    struct operand *result = tac->operands[0];
    struct operand *l = tac->operands[1];
    int from_size = tac->from_opsize;
    int from_i = idx[from_size];
    struct reladdr *src_label = operand2s(l, from_size);
    struct set *excepts = operand_regs(l);
    struct reg *reg = dispatch_ireg(result->sym, excepts, from_size);
    xx(format("cvtt%s2si", suffixf[from_i]), NULL, src_label, rs(reg->r[from_i]));
}

static void emit_conv_tof(struct tac *tac, const char *op)
{
    struct operand *result = tac->operands[0];
    struct operand *l = tac->operands[1];
    int from_size = tac->from_opsize;
    int to_size = tac->to_opsize;
    int from_i = idx[from_size];
    int to_i = idx[to_size];
    struct reladdr *src_label = operand2s(l, from_size);
    if (is_imm_operand(l)) {
        struct reg *reg = dispatch_ireg(l->sym, NULL, from_size);
        xx(OP_MOV, suffixi[from_i], src_label, rs(reg->r[from_i]));
        // reset
        src_label = rs(reg->r[from_i]);
    }
    struct set *excepts = operand_regs(l);
    struct reg *reg = dispatch_freg(result->sym, excepts, to_size);
    xx(format("%s2%s", op, suffixf[to_i]), NULL, src_label, rs(reg->r[to_i]));
}

static void emit_conv_i2f(struct tac *tac)
{
    emit_conv_tof(tac, OP_CVTSI);
}

static void emit_conv_f2f(struct tac *tac)
{
    if (tac->from_opsize == Long && tac->to_opsize == Quad)
        emit_conv_tof(tac, OP_CVTSS);
    else if (tac->from_opsize == Quad && tac->to_opsize == Long)
        emit_conv_tof(tac, OP_CVTSD);
    else
        assert(0);
}

static void emit_conv_p2b(struct tac *tac)
{
    struct operand *result = tac->operands[0];
    struct operand *l = tac->operands[1];
    int from_size = tac->from_opsize;
    int to_size = tac->to_opsize;
    int from_i = idx[from_size];
    int to_i = idx[to_size];
    struct reladdr *src_label = operand2s(l, from_size);
    struct set *excepts = operand_regs(l);
    struct reg *reg = dispatch_ireg(result->sym, excepts, to_size);
    struct reladdr *dst_label = rs(reg->r[from_i]);
    xx(OP_MOV, suffixi[from_i], src_label, dst_label);
    xx(OP_TEST, suffixi[from_i], dst_label, dst_label);
    xx(OP_SETNE, NULL, rs(reg->r[to_i]), NULL);
}

static void emit_conv_i2b(struct tac *tac)
{
    struct operand *result = tac->operands[0];
    struct operand *l = tac->operands[1];
    int from_size = tac->from_opsize;
    int to_size = tac->to_opsize;
    int from_i = idx[from_size];
    int to_i = idx[to_size];
    struct reladdr *src_label = operand2s(l, from_size);
    if (is_imm_operand(l)) {
        struct reg *reg = dispatch_ireg(l->sym, NULL, from_size);
        xx(OP_MOV, suffixi[from_i], src_label, rs(reg->r[from_i]));
        // reset
        src_label = rs(reg->r[from_i]);
    }
    struct set *excepts = operand_regs(l);
    struct reg *reg = dispatch_ireg(result->sym, excepts, to_size);
    xx(OP_CMP, suffixi[from_i], imm(0), src_label);
    xx(OP_SETNE, NULL, rs(reg->r[to_i]), NULL);
}

static void emit_conv_f2b(struct tac *tac)
{
    struct operand *result = tac->operands[0];
    struct operand *l = tac->operands[1];
    int from_size = tac->from_opsize;
    int to_size = tac->to_opsize;
    int from_i = idx[from_size];
    int to_i = idx[to_size];
    struct reladdr *src_label = operand2s(l, from_size);
    struct set *excepts = operand_regs(l);
    struct reg *ftmp = get_one_freg(excepts);
    xx(OP_XOR, suffixp[from_i], rs(ftmp->r[from_i]), rs(ftmp->r[from_i]));
    xx(OP_UCOMI, suffixf[from_i], rs(ftmp->r[from_i]), src_label);
    set_add(excepts, ftmp);
    struct reg *reg = dispatch_ireg(result->sym, excepts, to_size);
    set_add(excepts, reg);
    struct reg *tmp = get_one_ireg(excepts);
    // handle negative zero
    xx(OP_SETNE, NULL, rs(reg->r[to_i]), NULL);
    xx(OP_SETP, NULL, rs(tmp->r[to_i]), NULL);
    xx(OP_OR, suffixi[to_i], rs(tmp->r[to_i]), rs(reg->r[to_i]));
    xx(OP_AND, suffixi[to_i], imm(1), rs(reg->r[to_i]));
}

static void emit_tac(struct tac *tac)
{
    switch (tac->op) {
    case IR_GOTO:
        emit_goto(tac);
        break;
    case IR_IF_I:
        emit_if_i(tac);
        break;
    case IR_IF_F:
        emit_if_f(tac);
        break;
    case IR_IF_FALSE_I:
        emit_iffalse_i(tac);
        break;
    case IR_IF_FALSE_F:
        emit_iffalse_f(tac);
        break;
    case IR_RETURNI:
    case IR_RETURNF:
        emit_return(tac);
        break;
        // bop
    case IR_ADDI:
        emit_bop_int(tac, OP_ADD);
        break;
    case IR_SUBI:
        emit_bop_int(tac, OP_SUB);
        break;
    case IR_DIVI:
        emit_int_div(tac);
        break;
    case IR_IDIVI:
        emit_int_idiv(tac);
        break;
    case IR_MOD:
        emit_mod(tac);
        break;
    case IR_IMOD:
        emit_imod(tac);
        break;
    case IR_MULI:
        emit_int_mul(tac);
        break;
    case IR_IMULI:
        emit_int_imul(tac);
        break;
    case IR_OR:
        emit_bop_int(tac, OP_OR);
        break;
    case IR_AND:
        emit_bop_int(tac, OP_AND);
        break;
    case IR_XOR:
        emit_bop_int(tac, OP_XOR);
        break;
    case IR_LSHIFT:
    case IR_ILSHIFT:
        emit_shift(tac, OP_SHL);
        break;
    case IR_RSHIFT:
        emit_shift(tac, OP_SHR);
        break;
    case IR_IRSHIFT:
        emit_shift(tac, OP_SAR);
        break;
    case IR_ADDF:
        emit_bop_float(tac, OP_ADD);
        break;
    case IR_SUBF:
        emit_bop_float(tac, OP_SUB);
        break;
    case IR_DIVF:
        emit_bop_float(tac, OP_DIV);
        break;
    case IR_MULF:
        emit_bop_float(tac, OP_MUL);
        break;
        // uop
    case IR_NOT:
        emit_uop_not(tac);
        break;
    case IR_MINUSI:
        emit_uop_minus_i(tac);
        break;
    case IR_MINUSF:
        emit_uop_minus_f(tac);
        break;
    case IR_ADDRESS:
        emit_uop_address(tac);
        break;
    case IR_ASSIGNI:
    case IR_ASSIGNF:
        emit_assign(tac);
        break;
    case IR_CALL:
        emit_call(tac);
        break;
    case IR_CONV_UI_UI:
    case IR_CONV_UI_SI:
        emit_conv_i2i(tac, UNSIGNED);
        break;
    case IR_CONV_SI_UI:
    case IR_CONV_SI_SI:
        emit_conv_i2i(tac, INT);
        break;
    case IR_CONV_SI_F:
    case IR_CONV_UI_F:
        emit_conv_i2f(tac);
        break;
    case IR_CONV_F_SI:
    case IR_CONV_F_UI:
        emit_conv_f2i(tac);
        break;
    case IR_CONV_FF:
        emit_conv_f2f(tac);
        break;
    case IR_CONV_P_B:
        emit_conv_p2b(tac);
        break;
    case IR_CONV_I_B:
        emit_conv_i2b(tac);
        break;
    case IR_CONV_F_B:
        emit_conv_f2b(tac);
        break;
    case IR_NONE:
    case IR_PARAM:
    case IR_LABEL:
    default:
        // skip
        break;
    }
}

static void do_spill(struct rvar *v)
{
    node_t *sym = v->sym;
    struct reg *reg = SYM_X_REG(sym);
    int i = idx[v->size];
    
    switch (SYM_X_KIND(sym)) {
    case SYM_KIND_GREF:
        {
            yy(OP_MOV, suffixi[i],
               rs(reg->r[i]),
               str("%s(%s)", SYM_X_LABEL(sym), rip->r[Q]),
               format("%d-byte spill", v->size));
            SYM_X_INMEM(sym) = true;
            SYM_X_FREG(sym) = reg->freg;
        }
        break;
    case SYM_KIND_TMP:
        if (!SYM_X_LOFF(sym)) {
            fcon.localsize = ROUNDUP(fcon.localsize + v->size, 4);
            SYM_X_LOFF(sym) = -fcon.localsize;
        }
        // fall through
    case SYM_KIND_LREF:
        {
            yy(OP_MOV, suffixi[i],
               rs(reg->r[i]),
               subst(rbp->r[Q], SYM_X_LOFF(sym)),
               format("%d-byte spill", v->size));
            SYM_X_INMEM(sym) = true;
            SYM_X_FREG(sym) = reg->freg;
        }
        break;
    default:
        break;
    }
}

static void spillv(struct rvar *v)
{
    node_t *sym = v->sym;
    struct reg *reg = SYM_X_REG(sym);
    if (!reg)
        return;
    if (SYM_X_INMEM(sym))
        goto done;
    if (!SYM_X_USES(sym).live &&
        !set_has(fcon.current_block->out, sym))
        goto done;
    // not in memory and in register
    do_spill(v);
 done:
    // clear SYM_X_REG
    SYM_X_REG(sym) = NULL;
}

static void spill(node_t *sym)
{
    struct reg *reg = SYM_X_REG(sym);
    if (!reg)
        return;
    if (SYM_X_INMEM(sym))
        goto done;
    struct rvar *v = find_var(reg, sym);
    do_spill(v);
 done:
    // clear SYM_X_REG
    SYM_X_REG(sym) = NULL;
}

static int sort_symbol(node_t *sym1, node_t *sym2)
{
    return strcmp(SYM_X_LABEL(sym1), SYM_X_LABEL(sym2));
}

static int sort_out(const void *val1, const void *val2)
{
    node_t *sym1 = * (node_t **) val1;
    node_t *sym2 = * (node_t **) val2;
    return sort_symbol(sym1, sym2);
}

static void finalize_basic_block(struct basic_block *block)
{
    struct vector *outs = set_objects(block->out);
    struct vector *sorted = vec_sort(outs, sort_out);
    for (size_t i = 0; i < vec_len(sorted); i++) {
        node_t *sym = vec_at(sorted, i);
        spill(sym);
    }
}

static void at_exit_basic_block(void)
{
    struct basic_block *block = fcon.current_block;
    if (block->tag != BLOCK_START && block->tag != BLOCK_END)
        finalize_basic_block(block);
}

static void init_sym_addrs(node_t *sym)
{
    if (SYM_X_KIND(sym) == SYM_KIND_GREF ||
        SYM_X_KIND(sym) == SYM_KIND_LREF) {
        SYM_X_INMEM(sym) = true;
        SYM_X_REG(sym) = NULL;
    }
}

static void init_basic_blocks(struct basic_block *start)
{
    for (struct basic_block *block = start; block; block = block->successors[0]) {
        for (struct tac *tac = block->head; tac; tac = tac->next) {
            for (int i = 0; i < ARRAY_SIZE(tac->operands); i++) {
                struct operand *operand = tac->operands[i];
                if (operand) {
                    if (operand->sym)
                        init_sym_addrs(operand->sym);
                    if (operand->index)
                        init_sym_addrs(operand->index);
                }
            }
        }
    }
}

/**
 * Next-use information
 */
static void mark_die(node_t *sym)
{
    if (REF_SYM(sym)) {
        SYM_X_USES(sym).live = false;
        SYM_X_USES(sym).next = NULL;
    }
}

static void mark_live(node_t *sym, struct tac *tac)
{
    if (REF_SYM(sym)) {
        SYM_X_USES(sym).live = true;
        SYM_X_USES(sym).next = tac;
    }
}

static void init_use(node_t *sym)
{
    if (SYM_X_KIND(sym) == SYM_KIND_GREF ||
        SYM_X_KIND(sym) == SYM_KIND_LREF) {
        SYM_X_USES(sym).live = true;
        SYM_X_USES(sym).next = NULL;
    } else if (SYM_X_KIND(sym) == SYM_KIND_TMP) {
        SYM_X_USES(sym).live = false;
        SYM_X_USES(sym).next = NULL;
    }
}

static void init_next_use(struct basic_block *block)
{
    for (struct tac *tac = block->head; tac; tac = tac->next) {
        for (int i = 0; i < ARRAY_SIZE(tac->operands); i++) {
            struct operand *operand = tac->operands[i];
            if (operand) {
                if (operand->sym)
                    init_use(operand->sym);
                if (operand->index)
                    init_use(operand->index);
            }
        }
    }
}

static void scan_next_use(struct basic_block *block)
{
    struct tac *tail;
    for (tail = block->head; tail; tail = tail->next) {
        if (tail->next == NULL)
            break;
    }
    for (struct tac *tac = tail; tac; tac = tac->prev) {
        // set
        for (int i = 0; i < ARRAY_SIZE(tac->operands); i++) {
            struct operand *operand = tac->operands[i];
            if (operand) {
                if (operand->sym)
                    tac->uses[i*2] = SYM_X_USES(operand->sym);
                if (operand->index)
                    tac->uses[i*2+1] = SYM_X_USES(operand->index);
            }
        }
        
        // mark
        for (int i = 0; i < ARRAY_SIZE(tac->operands); i++) {
            struct operand *operand = tac->operands[i];
            if (operand) {
                if (i == 0) {
                    // die
                    if (operand->sym) {
                        if (operand->op == IR_NONE)
                            mark_die(operand->sym);
                        else
                            mark_live(operand->sym, tac);
                    }
                    if (operand->index)
                        mark_live(operand->index, tac);
                } else {
                    // live at tac
                    if (operand->sym)
                        mark_live(operand->sym, tac);
                    if (operand->index)
                        mark_live(operand->index, tac);
                }
            }
        }
    }
}

static void calculate_next_use(struct basic_block *block)
{
    init_next_use(block);
    scan_next_use(block);
}

static void emit_basic_blocks(struct basic_block *start)
{
    for (struct basic_block *block = start; block; block = block->successors[0]) {
        fcon.current_block = block;
        calculate_next_use(block);
        if (block->label && block->tag == BLOCK_JUMPING_DEST)
            lab(block->label);
        for (struct tac *tac = block->head; tac; tac = tac->next) {
            // set current tac
            fcon.current_tac = tac;
            emit_tac(tac);
            if (tac->op != IR_PARAM)
                update_use(tac);
        }
        if (block->tag != BLOCK_START && block->tag != BLOCK_END)
            finalize_basic_block(block);
    }
}

static size_t call_returns_size(node_t *sym)
{
    size_t extra_stack_size = 0;
    struct vector *calls = SYM_X_CALLS(sym);
    for (int i = 0; i < vec_len(calls); i++) {
        node_t *call = vec_at(calls, i);
        node_t *rty = AST_TYPE(call);
        if (isrecord(rty))
            extra_stack_size = MAX(extra_stack_size, TYPE_SIZE(rty));
    }
    return extra_stack_size;
}

static size_t call_params_size(node_t *sym)
{
    size_t extra_stack_size = 0;
    struct vector *calls = SYM_X_CALLS(sym);
    for (int i = 0; i < vec_len(calls); i++) {
        node_t *call = vec_at(calls, i);
        node_t *ftype = rtype(AST_TYPE(EXPR_OPERAND(call, 0)));
        struct pinfo *pinfo = alloc_addr_for_funcall(ftype, EXPR_ARGS(call));
        extra_stack_size = MAX(extra_stack_size, pinfo->size);
    }
    return extra_stack_size;
}

static void emit_register_save_area(void)
{
    long offset = -REGISTER_SAVE_AREA_SIZE;
    for (int i = 0; i < ARRAY_SIZE(iarg_regs); i++, offset += 8) {
        struct reg *r = iarg_regs[i];
        xx(OP_MOV, suffixi[Q], rs(r->r[Q]), subst(rbp->r[Q], offset));
    }
    const char *label = gen_label();
    xx(OP_TEST, suffixi[B], rs(int_regs[RAX]->r[B]), rs(int_regs[RAX]->r[B]));
    xx(OP_JE, NULL, rs(label), NULL);
    for (int i = 0; i < ARRAY_SIZE(farg_regs); i++, offset += 16) {
        struct reg *r = farg_regs[i];
        xx(OP_MOV, "aps", rs(r->r[Q]), subst(rbp->r[Q], offset));
    }
    lab(label);
    assert(offset == 0);
}

static long get_reg_offset(struct reg *reg)
{
    for (int i = 0; i < ARRAY_SIZE(iarg_regs); i++) {
        struct reg *ireg = iarg_regs[i];
        if (reg == ireg)
            return -176 + (i << 3);
    }
    for (int i = 0; i < ARRAY_SIZE(farg_regs); i++) {
        struct reg *freg = farg_regs[i];
        if (reg == freg)
            return -128 + (i << 4);
    }
    assert(0);
}

static void emit_register_params(void)
{
    for (int i = 0; i < vec_len(fcon.pinfo->pnodes); i++) {
        struct pnode *pnode = vec_at(fcon.pinfo->pnodes, i);
        node_t *sym = pnode->param;
        struct paddr *paddr = pnode->paddr;
        if (paddr->kind == ADDR_REGISTER) {
            long loff = SYM_X_LOFF(sym);
            size_t size = paddr->size;
            int cnt = ROUNDUP(paddr->size, 8) >> 3;
            for (int i = 0; i < cnt; i++, loff += 8, size -= 8) {
                int type = paddr->u.regs[i].type;
                struct reg *reg = paddr->u.regs[i].reg;
                switch (type) {
                case REG_INT:
                    if (size > 4)
                        xx(OP_MOV, suffixi[Q], rs(reg->r[Q]), subst(rbp->r[Q], loff));
                    else if (size > 2)
                        xx(OP_MOV, suffixi[L], rs(reg->r[L]), subst(rbp->r[Q], loff));
                    else if (size == 2)
                        xx(OP_MOV, suffixi[W], rs(reg->r[W]), subst(rbp->r[Q], loff));
                    else if (size == 1)
                        xx(OP_MOV, suffixi[B], rs(reg->r[B]), subst(rbp->r[Q], loff));
                    else
                        assert(0);
                    break;
                case REG_SSE_F:
                    xx(OP_MOV, suffixf[L], rs(reg->r[Q]), subst(rbp->r[Q], loff));
                    break;
                case REG_SSE_D:
                    xx(OP_MOV, suffixf[Q], rs(reg->r[Q]), subst(rbp->r[Q], loff));
                    break;
                case REG_SSE_FF:
                    xx(OP_MOV, "lps", rs(reg->r[Q]), subst(rbp->r[Q], loff));
                    break;
                }
            }
        }
    }
}

static void emit_function_epilogue(void)
{
    // function epilogue
    emit_placeholder(INST_STACK_ADD);
    emit_placeholder(INST_PRESERVED_REG_POP);
    /*
      leave instruction

      move rbp to rsp
      pop rbp
    */
    xx(OP_LEAVE, NULL, NULL, NULL);
    xx(OP_RET, NULL, NULL, NULL);
}

static void emit_function_prologue(node_t *fsym)
{
    node_t *ftype = SYM_TYPE(fsym);
    bool global = SYM_SCLASS(fsym) == STATIC ? false : true;
    
    if (global)
        macro(".globl %s", SYM_X_LABEL(fsym));
    macro(".text");
    lab(SYM_X_LABEL(fsym));
    xx(OP_PUSH, suffixi[Q], rs(rbp->r[Q]), NULL);
    xx(OP_MOV, suffixi[Q], rs(rsp->r[Q]), rs(rbp->r[Q]));

    size_t localsize = 0;

    // register save area
    if (TYPE_VARG(ftype))
        localsize += REGISTER_SAVE_AREA_SIZE;
    
    // local vars
    for (int i = 0; i < vec_len(SYM_X_LVARS(fsym)); i++) {
        node_t *lvar = vec_at(SYM_X_LVARS(fsym), i);
        node_t *ty = SYM_TYPE(lvar);
        size_t size = TYPE_SIZE(ty);
        localsize = ROUNDUP(localsize + size, 4);
        SYM_X_LOFF(lvar) = -localsize;
    }

    // params
    for (int i = 0; i < vec_len(fcon.pinfo->pnodes); i++) {
        struct pnode *pnode = vec_at(fcon.pinfo->pnodes, i);
        node_t *sym = pnode->param;
        struct paddr *paddr = pnode->paddr;
        node_t *ty = SYM_TYPE(sym);
        size_t size = TYPE_SIZE(ty);
        if (paddr->kind == ADDR_REGISTER) {
            if (TYPE_VARG(ftype)) {
                long offset = get_reg_offset(paddr->u.regs[0].reg);
                SYM_X_LOFF(sym) = offset;
            } else {
                localsize = ROUNDUP(localsize + size, 4);
                SYM_X_LOFF(sym) = -localsize;
            }
        } else if (paddr->kind == ADDR_STACK) {
            SYM_X_LOFF(sym) = paddr->u.offset + STACK_PARAM_BASE_OFF;
        } else {
            die("unexpected paddr type: %d", paddr->kind);
        }
    }

    // call returns
    size_t returns_size = call_returns_size(fsym);
    if (returns_size) {
        // rounded by 8 bytes. (see emit_return)
        localsize = ROUNDUP(localsize + returns_size, 8);
        fcon.calls_return_loff = -localsize;
    }
    
    // call params
    localsize += call_params_size(fsym);
    fcon.localsize = fcon.orig_localsize = localsize;
    
    emit_placeholder(INST_PRESERVED_REG_PUSH);
    emit_placeholder(INST_STACK_SUB);
}

static void emit_text(node_t *fsym)
{
    node_t *ftype = SYM_TYPE(fsym);
    node_t **params = TYPE_PARAMS(ftype);
    
    // reset registers
    reset_regs();
    // reset function context
    fcon.end_label = STMT_X_NEXT(SYM_INIT(fsym));
    fcon.calls_return_loff = 0;
    fcon.current_block = NULL;
    fcon.current_tac = NULL;
    fcon.current_ftype = ftype;
    fcon.opcodes = vec_new();
    fcon.localsize = fcon.orig_localsize = 0;
    fcon.preserved_regs = set_new();
    fcon.pinfo = alloc_addr_for_funcdef(ftype, params);
    
    emit_function_prologue(fsym);
    if (TYPE_VARG(ftype))
        emit_register_save_area();
    else
        emit_register_params();
    init_basic_blocks(SYM_X_BASIC_BLOCK(fsym));
    emit_basic_blocks(SYM_X_BASIC_BLOCK(fsym));
    emit_function_epilogue();
    
    finalize_text();
}

static void emit_data(const char *label, bool global, bool array,
                      int align, int size, struct vector *xvalues)
{
    if (global)
        emit(".globl %s", label);
    emit(".data");
    if (align > 1)
        emit(".align %d", align);
    if (array)
        emit(".size %s, %lu", label, size);
    emit_noindent("%s:", label);
    for (int i = 0; i < vec_len(xvalues); i++) {
        struct xvalue *value = vec_at(xvalues, i);
        switch (value->size) {
        case Zero:
            emit(".zero %s", value->name);
            break;
        case Byte:
            emit(".byte %s", value->name);
            break;
        case Word:
            emit(".short %s", value->name);
            break;
        case Long:
            emit(".long %s", value->name);
            break;
        case Quad:
            emit(".quad %s", value->name);
            break;
        case ASCIZ:
            emit(".asciz %s", value->name);
            break;
        default:
            die("unknown size");
            break;
        }
    }
}

static void emit_bss(node_t *sym)
{
    node_t *ty = SYM_TYPE(sym);
    bool global = SYM_SCLASS(sym) == STATIC ? false : true;
    
    if (!global)
        emit(".local %s", SYM_X_LABEL(sym));
    emit(".comm %s,%llu,%d",
         SYM_X_LABEL(sym), TYPE_SIZE(ty), TYPE_ALIGN(ty));
}

static void emit_compounds(struct map *compounds)
{
    struct vector *keys = map_keys(compounds);
    if (vec_len(keys)) {
        for (int i = 0; i < vec_len(keys); i++) {
            const char *label = vec_at(keys, i);
            node_t *init = map_get(compounds, label);
            node_t *ty = AST_TYPE(init);
            emit_data(label, false, false, TYPE_ALIGN(ty), TYPE_SIZE(ty), EXPR_X_XVALUES(init));
        }
    }
}

static void emit_strings(struct map *strings)
{
    struct vector *keys = map_keys(strings);
    if (vec_len(keys)) {
        emit(".section .rodata");
        for (int i = 0; i < vec_len(keys); i++) {
            const char *name = vec_at(keys, i);
            const char *label = map_get(strings, name);
            emit_noindent("%s:", label);
            emit(".asciz %s", name);
        }
    }
}

static void emit_floats(struct map *floats)
{
    struct vector *keys = map_keys(floats);
    if (vec_len(keys)) {
        emit(".section .rodata");
        for (int i = 0; i < vec_len(keys); i++) {
            const char *name = vec_at(keys, i);
            const char *label = map_get(floats, name);
            node_t *sym = lookup(name, constants);
            assert(sym);
            node_t *ty = SYM_TYPE(sym);
            emit(".align %d", TYPE_ALIGN(ty));
            emit_noindent("%s:", label);
            switch (TYPE_KIND(ty)) {
            case FLOAT:
                {
                    float f = SYM_VALUE_D(sym);
                    emit(".long %u", *(uint32_t *)&f);
                }
                break;
            case DOUBLE:
            case LONG+DOUBLE:
                {
                    double d = SYM_VALUE_D(sym);
                    emit(".quad %llu", *(uint64_t *)&d);
                }
                break;
            default:
                assert(0);
            }
        }
    }
}

///
/// Register Allocation
///
static struct vector *rexcepts;

static inline struct reg * mkreg(void)
{
    return zmalloc(sizeof(struct reg));
}

static void init_regs(void)
{
    rsp = mkreg();
    rsp->r[Q] = "%rsp";
    rsp->r[L] = "%esp";
    rsp->r[W] = "%sp";

    rbp = mkreg();
    rbp->r[Q] = "%rbp";
    rbp->r[L] = "%ebp";
    rbp->r[W] = "%bp";
    
    rip = mkreg();
    rip->r[Q] = "%rip";
    rip->r[L] = "%eip";
    rip->r[W] = "%ip";
    
    int_regs[RAX] = mkreg();
    int_regs[RAX]->r[Q] = "%rax";
    int_regs[RAX]->r[L] = "%eax";
    int_regs[RAX]->r[W] = "%ax";
    int_regs[RAX]->r[B] = "%al";

    int_regs[RBX] = mkreg();
    int_regs[RBX]->r[Q] = "%rbx";
    int_regs[RBX]->r[L] = "%ebx";
    int_regs[RBX]->r[W] = "%bx";
    int_regs[RBX]->r[B] = "%bl";
    
    int_regs[RCX] = mkreg();
    int_regs[RCX]->r[Q] = "%rcx";
    int_regs[RCX]->r[L] = "%ecx";
    int_regs[RCX]->r[W] = "%cx";
    int_regs[RCX]->r[B] = "%cl";
    
    int_regs[RDX] = mkreg();
    int_regs[RDX]->r[Q] = "%rdx";
    int_regs[RDX]->r[L] = "%edx";
    int_regs[RDX]->r[W] = "%dx";
    int_regs[RDX]->r[B] = "%dl";

    int_regs[RSI] = mkreg();
    int_regs[RSI]->r[Q] = "%rsi";
    int_regs[RSI]->r[L] = "%esi";
    int_regs[RSI]->r[W] = "%si";
    int_regs[RSI]->r[B] = "%sil";

    int_regs[RDI] = mkreg();
    int_regs[RDI]->r[Q] = "%rdi";
    int_regs[RDI]->r[L] = "%edi";
    int_regs[RDI]->r[W] = "%di";
    int_regs[RDI]->r[B] = "%dil";

    for (int i = R8; i <= R15; i++) {
        int index = i - R8 + 8;
        int_regs[i] = mkreg();
        int_regs[i]->r[Q] = format("%%r%d", index);
        int_regs[i]->r[L] = format("%%r%dd", index);
        int_regs[i]->r[W] = format("%%r%dw", index);
        int_regs[i]->r[B] = format("%%r%db", index);
    }

    // init integer regs
    iarg_regs[0] = int_regs[RDI];
    iarg_regs[1] = int_regs[RSI];
    iarg_regs[2] = int_regs[RDX];
    iarg_regs[3] = int_regs[RCX];
    iarg_regs[4] = int_regs[R8];
    iarg_regs[5] = int_regs[R9];

    // init integer return regs
    ret_iregs[0] = int_regs[RAX];
    ret_iregs[1] = int_regs[RDX];

    // preserved regs
    rsp->preserved = true;
    rbp->preserved = true;
    int_regs[RBX]->preserved = true;
    int_regs[R12]->preserved = true;
    int_regs[R13]->preserved = true;
    int_regs[R14]->preserved = true;
    int_regs[R15]->preserved = true;

    // init floating regs
    for (int i = XMM0; i <= XMM15; i++) {
        const char *name = format("%%xmm%d", i - XMM0);
        float_regs[i] = mkreg();
        float_regs[i]->freg = true;
        float_regs[i]->r[Q] = name;
        float_regs[i]->r[L] = name;
        if (i <= XMM7)
            farg_regs[i - XMM0] = float_regs[i];
    }

    // init floating return regs
    ret_fregs[0] = float_regs[XMM0];
    ret_fregs[1] = float_regs[XMM1];
}

static void reset_regs(void)
{
    for (int i = 0; i < ARRAY_SIZE(int_regs); i++) {
        struct reg *reg = int_regs[i];
        reg->vars = NULL;
    }
    for (int i = 0; i < ARRAY_SIZE(float_regs); i++) {
        struct reg *reg = float_regs[i];
        reg->vars = NULL;
    }
}

static struct rvar * new_rvar(node_t *sym, int size)
{
    struct rvar *var = zmalloc(sizeof(struct rvar));
    var->sym = sym;
    var->size = size;
    return var;
}

// LD reg, sym
static void load(struct reg *reg, node_t *sym, int opsize)
{
    assert(SYM_X_REG(sym) == NULL);
    if (!REF_SYM(sym))
        return;
    
    if (!reg->vars)
        reg->vars = set_new();

    struct rvar *var = new_rvar(sym, opsize);
    set_add(reg->vars, var);
    SYM_X_REG(sym) = reg;
}

// ST sym, reg
static void store(node_t *sym)
{
    SYM_X_REG(sym) = NULL;
    SYM_X_INMEM(sym) = true;
}

struct rvar *find_var(struct reg *reg, node_t *sym)
{
    struct vector *objs = set_objects(reg->vars);
    for (size_t i = 0; i < vec_len(objs); i++) {
        struct rvar *var = vec_at(objs, i);
        if (var->sym == sym)
            return var;
    }
    return NULL;
}

static void push_excepts(struct set *excepts)
{
    if (!rexcepts)
        rexcepts = vec_new();
    vec_push(rexcepts, excepts);
}

static void pop_excepts(void)
{
    if (rexcepts)
        vec_pop(rexcepts);
}

static struct reg * return_reg(struct reg *reg)
{
    // preserved regs
    if (is_preserved(reg))
        set_add(fcon.preserved_regs, reg);
    return reg;
}

static struct reg * get_reg(struct reg **regs, int count, struct set *excepts)
{
    // filter excepts out
    struct vector *candicates = vec_new();
    for (int i = 0; i < count; i++) {
        struct reg *ri = regs[i];
        if (set_has(excepts, ri))
            continue;
        for (int j = 0; j < vec_len(rexcepts); j++) {
            struct set *re = vec_at(rexcepts, j);
            if (set_has(re, ri))
                goto next;
        }
        vec_push(candicates, ri);
    next:;
    }

    struct reg *ret = NULL;
    int mincost = 0;
    for (int i = 0; i < vec_len(candicates); i++) {
        struct reg *reg = vec_at(candicates, i);
        // if exists an empty reg, return directly
        if (set_empty(reg->vars))
            return return_reg(reg);
        // dirty
        int cost = 0;
        struct vector *vars = set_objects(reg->vars);
        for (int j = 0; j < vec_len(vars); j++) {
            struct rvar *v = vec_at(vars, j);
            struct uses uses = SYM_X_USES(v->sym);
            if (SYM_X_INMEM(v->sym))
                continue;       // done
            if (!uses.live)
                continue;       // done
            // spill
            cost += 1;
        }
        if (ret == NULL || cost < mincost) {
            ret = reg;
            mincost = cost;
        }
    }

    // oops...
    if (ret == NULL)
        die("no enough registers");
    // spill out
    drain_reg(ret);
    return return_reg(ret);
}

static struct reg * dispatch_reg(struct reg **regs, int count, struct set *excepts,
                                 node_t *sym, int opsize)
{
    // already in reg, return directly
    if (SYM_X_REG(sym))
        return SYM_X_REG(sym);
    struct reg *reg = get_reg(regs, count, excepts);
    load(reg, sym, opsize);
    return reg;
}

static struct reg * dispatch_ireg(node_t *sym, struct set *excepts, int opsize)
{
    return dispatch_reg(int_regs, ARRAY_SIZE(int_regs), excepts, sym, opsize);
}

static struct reg * dispatch_freg(node_t *sym, struct set *excepts, int opsize)
{
    return dispatch_reg(float_regs, ARRAY_SIZE(float_regs), excepts, sym, opsize);
}

static struct reg * get_one_freg(struct set *excepts)
{
    return get_reg(float_regs, ARRAY_SIZE(float_regs), excepts);
}

static struct reg * get_one_ireg(struct set *excepts)
{
    return get_reg(int_regs, ARRAY_SIZE(int_regs), excepts);
}

static int sort_var(const void *val1, const void *val2)
{
    struct rvar *v1 = *(struct rvar **)val1;
    struct rvar *v2 = *(struct rvar **)val2;
    return sort_symbol(v1->sym, v2->sym);
}

static void drain_reg(struct reg *reg)
{
    struct vector *vars = set_objects(reg->vars);
    struct vector *sorted = vec_sort(vars, sort_var);
    for (size_t i = 0; i < vec_len(sorted); i++) {
        struct rvar *v = vec_at(sorted, i);
        spillv(v);
    }
    reg->vars = NULL;
}

static void clear_reg(struct reg *reg)
{
    struct vector *vars = set_objects(reg->vars);
    for (size_t i = 0; i < vec_len(vars); i++) {
        struct rvar *v = vec_at(vars, i);
        node_t *sym = v->sym;
        // must be in memory
        assert(SYM_X_INMEM(sym));
        SYM_X_REG(sym) = NULL;
    }
    reg->vars = NULL;
}

static bool is_preserved(struct reg *reg)
{
    return reg->preserved;
}

static bool is_nonpreserved(struct reg *reg)
{
    return !reg->preserved;
}

static void for_each_reg(bool (*cond) (struct reg *), void (*action) (struct reg *))
{
    for (int i = 0; i < ARRAY_SIZE(int_regs); i++) {
        struct reg *reg = int_regs[i];
        if (cond(reg))
            action(reg);
    }
    for (int i = 0; i < ARRAY_SIZE(float_regs); i++) {
        struct reg *reg = float_regs[i];
        if (cond(reg))
            action(reg);
    }
}

static struct set * operand_regs(struct operand *operand)
{
    struct set *v = set_new();
    if (operand->sym && SYM_X_REG(operand->sym))
        set_add(v, SYM_X_REG(operand->sym));
    if (operand->index && SYM_X_REG(operand->index))
        set_add(v, SYM_X_REG(operand->index));
    return v;
}

static void try_load_tmp(node_t *sym, struct set *excepts, int opsize)
{
    assert(SYM_X_KIND(sym) == SYM_KIND_TMP);
    if (!SYM_X_REG(sym)) {
        assert(SYM_X_INMEM(sym));
        if (SYM_X_FREG(sym))
            dispatch_freg(sym, excepts, opsize);
        else
            dispatch_ireg(sym, excepts, opsize);
        // clear
        SYM_X_FREG(sym) = false;
        int i = idx[opsize];
        yy(OP_MOV, suffixi[i],
           subst(rbp->r[Q], SYM_X_LOFF(sym)),
           rs(SYM_X_REG(sym)->r[i]),
           format("%d-byte reload", opsize));
    }
}

// sym
static struct reladdr *operand2s_none_ex(struct operand *operand, int opsize, bool mem)
{
    int i = idx[opsize];
    if (SYM_X_REG(operand->sym) && !mem) {
        return str("%s", SYM_X_REG(operand->sym)->r[i]);
    } else if (SYM_X_KIND(operand->sym) == SYM_KIND_IMM) {
        return imm(SYM_VALUE_U(operand->sym));
    } else if (SYM_X_KIND(operand->sym) == SYM_KIND_LREF) {
        return subst(rbp->r[Q], SYM_X_LOFF(operand->sym));
    } else if (SYM_X_KIND(operand->sym) == SYM_KIND_GREF) {
        if (isfunc(SYM_TYPE(operand->sym)))
            return str("$%s", SYM_X_LABEL(operand->sym));
        else
            return str("%s(%s)", SYM_X_LABEL(operand->sym), rip->r[Q]);
    } else if (SYM_X_KIND(operand->sym) == SYM_KIND_TMP) {
        try_load_tmp(operand->sym, NULL, opsize);
        return str("%s", SYM_X_REG(operand->sym)->r[i]);
    } else {
        assert(0);
    }
}

static struct reladdr *operand2s_none(struct operand *operand, int opsize)
{
    return operand2s_none_ex(operand, opsize, false);
}

// disp(sym, index, scale)
static struct reladdr *operand2s_subscript(struct operand *operand, int opsize)
{
    node_t *sym = operand->sym;
    node_t *index = operand->index;
    switch (SYM_X_KIND(sym)) {
    case SYM_KIND_GREF:
        {
            long offset = operand->disp;
            if (index) {
                try_load_tmp(index, NULL, Quad);
                if (offset > 0)
                    return str("%s+%ld(,%s,%d)",
                               SYM_X_LABEL(sym), offset, SYM_X_REG(index)->r[Q], operand->scale);
                else if (offset < 0)
                    return str("%s%ld(,%s,%d)",
                               SYM_X_LABEL(sym), offset, SYM_X_REG(index)->r[Q], operand->scale);
                else
                    return str("%s(,%s,%d)",
                               SYM_X_LABEL(sym), SYM_X_REG(index)->r[Q], operand->scale);
            } else {
                if (offset > 0)
                    return str("%s+%ld(%s)", SYM_X_LABEL(sym), offset, rip->r[Q]);
                else if (offset < 0)
                    return str("%s%ld(%s)", SYM_X_LABEL(sym), offset, rip->r[Q]);
                else
                    return str("%s(%s)", SYM_X_LABEL(sym), rip->r[Q]);
            }
        }
        break;
    case SYM_KIND_LREF:
        {
            long offset = SYM_X_LOFF(sym) + operand->disp;
            if (index) {
                try_load_tmp(index, NULL, Quad);
                return subscript(rbp->r[Q], SYM_X_REG(index)->r[Q], operand->scale, offset);
            } else {
                return subscript(rbp->r[Q], NULL, 0, offset);
            }
        }
        break;
    case SYM_KIND_TMP:
        {
            long offset = operand->disp;
            if (index) {
                struct set *excepts = set_new();
                if (SYM_X_REG(index))
                    set_add(excepts, SYM_X_REG(index));
                try_load_tmp(sym, excepts, Quad);
                set_add(excepts, SYM_X_REG(sym));
                try_load_tmp(index, excepts, Quad);
                if (offset)
                    return str("%ld(%s,%s,%d)",
                               offset, SYM_X_REG(sym)->r[Q], SYM_X_REG(index)->r[Q], operand->scale);
                else
                    return str("(%s,%s,%d)",
                               SYM_X_REG(sym)->r[Q], SYM_X_REG(index)->r[Q], operand->scale);
            } else {
                try_load_tmp(sym, NULL, Quad);
                if (offset)
                    return str("%ld(%s)", offset, SYM_X_REG(sym)->r[Q]);
                else
                    return str("(%s)", SYM_X_REG(sym)->r[Q]);
            }
        }
        break;
    default:
        assert(0);
    }
}

// *sym
static struct reladdr * operand2s_indirection(struct operand *operand, int opsize)
{
    assert(SYM_X_KIND(operand->sym) == SYM_KIND_TMP);
    assert(operand->index == NULL);
    try_load_tmp(operand->sym, NULL, Quad);
    return str("(%s)", SYM_X_REG(operand->sym)->r[Q]);
}

static struct reladdr * operand2s(struct operand *operand, int opsize)
{
    switch (operand->op) {
    case IR_NONE:
        return operand2s_none(operand, opsize);
    case IR_SUBSCRIPT:
        return operand2s_subscript(operand, opsize);
    case IR_INDIRECTION:
        return operand2s_indirection(operand, opsize);
    default:
        assert(0);
    }
}

static void update_use(struct tac *tac)
{
    for (int i = 0; i < ARRAY_SIZE(tac->operands); i++) {
        struct operand *operand = tac->operands[i];
        if (operand) {
            if (operand->sym)
                SYM_X_USES(operand->sym) = tac->uses[i*2];
            if (operand->index)
                SYM_X_USES(operand->index) = tac->uses[i*2+1];
        }
    }
}

///
/// Parameter Classification
///

static struct ptype * new_ptype(node_t *ty, size_t offset)
{
    struct ptype *ptype = zmalloc(sizeof(struct ptype));
    ptype->offset = offset;
    ptype->type = ty;
    return ptype;
}

static int * get_class(struct vector *ptypes)
{
    int *class = no_class;
    for (int i = 0; i < vec_len(ptypes); i++) {
        struct ptype *ptype = vec_at(ptypes, i);
        node_t *ty = ptype->type;
        int *class1 = no_class;
        if (isint(ty) || isptr(ty))
            class1 = integer_class;
        else if (isfloat(ty))
            class1 = sse_class;
        else
            assert(0);
        // reduce
        if (class == class1)
            continue;
        else if (class1 == no_class)
            continue;
        else if (class == no_class)
            class = class1;
        else if (class == memory_class || class1 == memory_class)
            class = memory_class;
        else if (class == integer_class || class1 == integer_class)
            class = integer_class;
        else
            class = sse_class;
    }
    if (class == no_class)
        die("%s: no_class", __func__);
    return class;
}

static struct vector * get_classes(struct vector *elements)
{
    struct vector *v = vec_new();
    for (int i = 0; i < vec_len(elements); i++) {
        struct vector *ptypes = vec_at(elements, i);
        int *class = get_class(ptypes);
        vec_push(v, class);
    }
    return v;
}

// merge two eightbyte elements.
static struct vector * merge_element(struct vector *v1, struct vector *v2)
{
    size_t len1 = vec_len(v1);
    size_t len2 = vec_len(v2);
    if (len1 == 0)
        return v2;
    else if (len2 == 0)
        return v1;

    int *class1 = get_class(v1);
    int *class2 = get_class(v2);
    if (class1 == integer_class)
        return v1;
    else if (class2 == integer_class)
        return v2;
    // both sse_class
    assert(class1 == sse_class && class2 == sse_class);
    assert(len1 <= 2 && len2 <= 2);
    if (len1 == 1) {
        struct ptype *ptype = vec_head(v1);
        size_t size = TYPE_SIZE(ptype->type);
        if (size == 8)
            return v1;
        else
            return v2;
    } else if (len1 == 2) {
        if (len2 == 1) {
            struct ptype *ptype = vec_head(v2);
            size_t size = TYPE_SIZE(ptype->type);
            if (size == 8)
                return v2;
            else
                return v1;
        } else {
            return v1;
        }
    } else {
        assert(0);
    }
}

static struct vector * merge_elements(struct vector *v1, struct vector *v2)
{
    assert(vec_len(v1) == vec_len(v2));

    struct vector *v = vec_new();
    for (int i = 0; i < vec_len(v1); i++) {
        struct vector *va = vec_at(v1, i);
        struct vector *vb = vec_at(v2, i);
        struct vector *v2 = merge_element(va, vb);
        vec_push(v, v2);
    }
    return v;
}

// return a vector of vector of struct ptype.
static struct vector * get_elements(struct vector *ptypes)
{
    struct vector *v = vec_new();
    struct vector *v2 = vec_new();
    struct ptype *first = vec_head(ptypes);
    size_t offset = first->offset;
    for (int i = 0; i < vec_len(ptypes); i++) {
        struct ptype *ptype = vec_at(ptypes, i);
        if (ptype->offset < offset + 8) {
            vec_push(v2, ptype);
        } else {
            vec_push(v, v2);
            v2 = vec_new1(ptype);
            offset += 8;
        }
    }
    vec_push(v, v2);
    return v;
}

static struct vector * get_types_for_union(node_t *ty, size_t offset)
{
    size_t cnt = ROUNDUP(TYPE_SIZE(ty), 8) >> 3;
    struct vector *v1 = vec_new();
    for (int i = 0; TYPE_FIELDS(ty)[i]; i++) {
        node_t *field = TYPE_FIELDS(ty)[i];
        node_t *fty = FIELD_TYPE(field);
        struct vector *v2 = get_types(fty, offset);
        struct vector *v3 = get_elements(v2);
        // padding with empty elements
        for (int j = vec_len(v3); j < cnt; j++)
            vec_push(v3, vec_new());
        vec_push(v1, v3);
    }
    struct vector *v0 = vec_head(v1);
    for (int j = 1; j < vec_len(v1); j++)
        v0 = merge_elements(v0, vec_at(v1, j));
    struct vector *v = vec_new();
    for (int i = 0; i < vec_len(v0); i++)
        vec_add(v, vec_at(v0, i));
    return v;
}

static struct vector * get_types_for_struct(node_t *ty, size_t offset)
{
    struct vector *v = vec_new();
    for (int i = 0; TYPE_FIELDS(ty)[i]; i++) {
        node_t *field = TYPE_FIELDS(ty)[i];
        node_t *fty = FIELD_TYPE(field);
        size_t off = offset + FIELD_OFFSET(field);
        if (FIELD_ISBIT(field) && FIELD_BITSIZE(field) == 0)
            continue;
        struct vector *v2 = get_types(fty, off);
        vec_add(v, v2);
    }
    return v;
}

static struct vector * get_types_for_array(node_t *ty, size_t offset)
{
    struct vector *v = vec_new();
    node_t *rty = rtype(ty);
    for (int i = 0; i < TYPE_LEN(ty); i++) {
        size_t off = offset + i * TYPE_SIZE(rty);
        struct vector *v2 = get_types(rty, off);
        vec_add(v, v2);
    }
    return v;
}

static struct vector * get_types(node_t *ty, size_t offset)
{
    if (isint(ty) || isptr(ty) || isfloat(ty))
        return vec_new1(new_ptype(ty, offset));
    else if (isstruct(ty))
        return get_types_for_struct(ty, offset);
    else if (isunion(ty))
        return get_types_for_union(ty, offset);
    else if (isarray(ty))
        return get_types_for_array(ty, offset);
    else
        assert(0);
}

static bool is_type_aligned(node_t *ty)
{
    // Now all types are aligned.
    return true;
}

static struct pinfo * alloc_pinfo(void)
{
    return zmalloc(sizeof(struct pinfo));
}

static struct paddr * alloc_paddr(void)
{
    return zmalloc(sizeof(struct paddr));
}

static struct pnode * new_pnode(node_t *param)
{
    struct pnode *pnode = zmalloc(sizeof(struct pnode));
    pnode->param = param;
    pnode->paddr = alloc_paddr();
    return pnode;
}

static node_t * new_tmp_param(bool call)
{
    node_t *sym = gen_tmp_sym(FUNC);
    SYM_TYPE(sym) = ptr_type(voidtype);
    SYM_SCOPE(sym) = call ? LOCAL : PARAM;
    SYM_X_KIND(sym) = SYM_KIND_LREF;
    SYM_X_INMEM(sym) = true;
    
    if (call) {
        // expr
        node_t *expr = ast_expr(REF_EXPR, SYM_TYPE(sym), NULL, NULL);
        EXPR_SYM(expr) = sym;
        return expr;
    } else {
        // symbol
        return sym;
    }
}

static void set_param_stack_addr(struct paddr *paddr, long offset, size_t size)
{
    paddr->kind = ADDR_STACK;
    paddr->size = size;
    paddr->u.offset = offset;
}

static void set_param_reg_addr(struct paddr *paddr, int index,
                               struct reg *reg, int type)
{
    paddr->kind = ADDR_REGISTER;
    paddr->u.regs[index].reg = reg;
    paddr->u.regs[index].type = type;
}

static void traverse_classes(struct vector *classes,
                             struct vector *elements,
                             struct paddr *paddr,
                             struct reg **iregs, int *gp,
                             struct reg **fregs, int *fp)
{
    for (int i = 0; i < vec_len(classes); i++) {
        int *class = vec_at(classes, i);
        struct vector *element = vec_at(elements, i);
        if (class == integer_class) {
            set_param_reg_addr(paddr, i, iregs[*gp], REG_INT);
            (*gp)++;
        } else if (class == sse_class) {
            int len = vec_len(element);
            if (len == 1) {
                struct ptype *ptype = vec_head(element);
                if (TYPE_SIZE(ptype->type) == 4)
                    set_param_reg_addr(paddr, i, fregs[*fp], REG_SSE_F);
                else
                    set_param_reg_addr(paddr, i, fregs[*fp], REG_SSE_D);
            } else if (len == 2) {
                set_param_reg_addr(paddr, i, fregs[*fp], REG_SSE_FF);
            } else {
                assert(0);
            }
            (*fp)++;
        }
    }
}

// params: list of symbols or expressions, may be NULL
static struct pinfo * alloc_addr_for_params(node_t *ftype, node_t **params, bool call)
{
    int gp = 0;
    int fp = 0;
    size_t offset = 0;
    const int OFFSET_ALIGN = 8;
    struct vector *pnodes = vec_new();
    struct paddr *retaddr = NULL;

    // return type
    node_t *rtype = rtype(ftype);
    if (!isvoid(rtype)) {
        retaddr = alloc_paddr();
        retaddr->size = TYPE_SIZE(rtype);
        
        if (TYPE_SIZE(rtype) > MAX_STRUCT_PARAM_SIZE) {
            // memory, passing as the first argument (pointer)
            node_t *param = new_tmp_param(call);
            struct pnode *pnode = new_pnode(param);
            vec_push(pnodes, pnode);

            // param addr
            pnode->paddr->size = TYPE_SIZE(AST_TYPE(param));
            set_param_reg_addr(pnode->paddr, 0, iarg_regs[gp], REG_INT);
            gp++;

            // ret addr
            retaddr->kind = ADDR_STACK;
        } else {
            // registers
            struct vector *ptypes = get_types(rtype, 0);
            struct vector *elements = get_elements(ptypes);
            struct vector *classes = get_classes(elements);
            int ret_gp = 0;
            int ret_fp = 0;
            
            retaddr->kind = ADDR_REGISTER;
            traverse_classes(classes, elements, retaddr,
                             ret_iregs, &ret_gp, ret_fregs, &ret_fp);
        }
    }
    
    for (int i = 0; params[i]; i++) {
        node_t *param = params[i];
        struct pnode *pnode = new_pnode(param);
        vec_push(pnodes, pnode);
        
        node_t *ty = AST_TYPE(param);
        size_t size = TYPE_SIZE(ty);
        if (size > MAX_STRUCT_PARAM_SIZE || !is_type_aligned(ty)) {
            // pass by memory
            set_param_stack_addr(pnode->paddr, offset, size);
            offset = ROUNDUP(offset + size, OFFSET_ALIGN);
        } else {
            struct vector *ptypes = get_types(ty, 0);
            struct vector *elements = get_elements(ptypes);
            struct vector *classes = get_classes(elements);
            assert(vec_len(elements) == vec_len(classes));

            int left_gp = NUM_IARG_REGS - gp;
            int left_fp = NUM_FARG_REGS - fp;
            int num_gp = 0;
            int num_fp = 0;
            for (int i = 0; i < vec_len(classes); i++) {
                int *class = vec_at(classes, i);
                if (class == integer_class)
                    num_gp++;
                else if (class == sse_class)
                    num_fp++;
                else
                    assert(0);
            }
            if (num_gp > left_gp || num_fp > left_fp) {
                // no enough registers
                set_param_stack_addr(pnode->paddr, offset, size);
                offset = ROUNDUP(offset + size, OFFSET_ALIGN);
            } else {
                pnode->paddr->kind = ADDR_REGISTER;
                pnode->paddr->size = size;
                traverse_classes(classes, elements, pnode->paddr,
                                 iarg_regs, &gp, farg_regs, &fp);
            }
        }
    }

    struct pinfo *pinfo = alloc_pinfo();
    pinfo->fp = fp;
    pinfo->gp = gp;
    pinfo->size = offset;
    pinfo->pnodes = pnodes;
    pinfo->retaddr = retaddr;
    return pinfo;
}

static struct pinfo * alloc_addr_for_funcall(node_t *ftype, node_t **params)
{
    return alloc_addr_for_params(ftype, params, true);
}

static struct pinfo * alloc_addr_for_funcdef(node_t *ftype, node_t **params)
{
    return alloc_addr_for_params(ftype, params, false);
}

static void metrics_init(void)
{
#define METRICS(m, size, align, rank)  IM->m = (struct metrics) { size, align, rank }

    // size  align  rank
    METRICS(boolmetrics, 1, 1, 10);
    METRICS(charmetrics, 1, 1, 20);
    METRICS(shortmetrics, 2, 2, 30);
    METRICS(wcharmetrics, 4, 4, 40);
    METRICS(intmetrics, 4, 4, 40);
    METRICS(longmetrics, 8, 8, 50);
    METRICS(longlongmetrics, 8, 8, 60);
    METRICS(floatmetrics, 4, 4, 70);
    METRICS(doublemetrics, 8, 8, 80);
    METRICS(longdoublemetrics, 8, 8, 90);
    METRICS(ptrmetrics, 8, 8, 0);
    METRICS(zerometrics, 0, 1, 0);

#undef METRICS
}

static void init(int argc, char *argv[])
{
    metrics_init();
    init_regs();
}

static void finalize(void)
{
    emit(".ident \"7cc: %s\"", VERSION);
}

static void defvar(node_t *sym, int seg)
{
    node_t *ty = SYM_TYPE(sym);
    int align = TYPE_ALIGN(ty);
    bool global = SYM_SCLASS(sym) == STATIC ? false : true;
    bool array = isarray(ty);
    struct vector *xvalues = SYM_X_XVALUES(sym);
    
    if (seg == DATA)
        emit_data(SYM_X_LABEL(sym), global, array, align, TYPE_SIZE(ty), xvalues);
    else if (seg == BSS)
        emit_bss(sym);
}

static void defun(node_t *sym)
{
    emit_text(sym);
}

struct im *IM = &(struct im) {
    .init = init,
    .finalize = finalize,
    .defvar = defvar,
    .defun = defun,
    .emit_compounds = emit_compounds,
    .emit_strings = emit_strings,
    .emit_floats = emit_floats
};
