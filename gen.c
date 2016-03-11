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

// Parameter classification
static int *no_class = (int *)1;
static int *integer_class = (int *)2;
static int *sse_class = (int *)3;
static int *memory_class = (int *)4;

struct ptype {
    size_t offset;
    node_t *type;
};
static struct vector * get_types(node_t *ty, size_t offset);

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
static void drain_reg(struct reg *reg);
static struct pinfo * alloc_addr_for_funcall(node_t *ftype, node_t **params);
static struct pinfo * alloc_addr_for_funcdef(node_t *ftype, node_t **params);
static struct uses * get_uses(node_t *sym, struct tac *tac);
#define get_current_uses(sym)  get_uses(sym, current_tac)

#define COMMENT(str)    "\t\t## " str

static FILE *outfp;
static const char *func_end_label;
static size_t func_returns;
static long calls_return_loff;
static struct pinfo *func_pinfo;
static struct dict *next_info;
static struct tac *current_tac;
static node_t *current_ftype;

static void emit(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    fprintf(outfp, "\t");
    vfprintf(outfp, fmt, ap);
    fprintf(outfp, "\n");
    va_end(ap);
}

static void emit_noindent(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vfprintf(outfp, fmt, ap);
    fprintf(outfp, "\n");
    va_end(ap);
}

static struct reg * mkreg(struct reg *r)
{
    struct reg *reg = zmalloc(sizeof(struct reg));
    *reg = *r;
    return reg;
}

static void init_regs(void)
{
    rsp = mkreg(&(struct reg){
        .r[Q] = "%rsp",
        .r[L] = "%esp",
        .r[W] = "%sp"
    });
    
    rbp = mkreg(&(struct reg){
        .r[Q] = "%rbp",
        .r[L] = "%ebp",
        .r[W] = "%bp"
    });
    
    rip = mkreg(&(struct reg){
        .r[Q] = "%rip",
        .r[L] = "%eip",
        .r[W] = "%ip"
    });
    
    int_regs[RAX] = mkreg(&(struct reg){
        .r[Q] = "%rax",
        .r[L] = "%eax",
        .r[W] = "%ax",
        .r[B] = "%al"
    });

    int_regs[RBX] = mkreg(&(struct reg){
        .r[Q] = "%rbx",
        .r[L] = "%ebx",
        .r[W] = "%bx",
        .r[B] = "%bl"
    });
    
    int_regs[RCX] = mkreg(&(struct reg){
        .r[Q] = "%rcx",
        .r[L] = "%ecx",
        .r[W] = "%cx",
        .r[B] = "%cl"
    });
    
    int_regs[RDX] = mkreg(&(struct reg){
        .r[Q] = "%rdx",
        .r[L] = "%edx",
        .r[W] = "%dx",
        .r[B] = "%dl"
    });

    int_regs[RSI] = mkreg(&(struct reg){
        .r[Q] = "%rsi",
        .r[L] = "%esi",
        .r[W] = "%si",
        .r[B] = "%sil"
    });

    int_regs[RDI] = mkreg(&(struct reg){
        .r[Q] = "%rdi",
        .r[L] = "%edi",
        .r[W] = "%di",
        .r[B] = "%dil"
    });

    for (int i = R8; i <= R15; i++) {
        int index = i - R8 + 8;
        int_regs[i] = mkreg(&(struct reg){
            .r[Q] = format("%%r%d", index),
            .r[L] = format("%%r%dd", index),
            .r[W] = format("%%r%dw", index),
            .r[B] = format("%%r%db", index)
        });
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
        float_regs[i] = mkreg(&(struct reg){
            .freg = true,
            .r[Q] = name,
            .r[L] = name
        });
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

static void dump_operand(struct operand *operand)
{
    println("%p: op:%s,%ld(%s,%s,%d), kind: %d, reg: %s",
            operand, rop2s(operand->op),operand->disp,
            SYM_X_LABEL(operand->sym),
            operand->index ? SYM_X_LABEL(operand->index) : "",
            operand->scale,
            SYM_X_KIND(operand->sym),
            SYM_X_REG(operand->sym) ? SYM_X_REG(operand->sym)->r[Q] : "");
}

static void dump_reg(struct reg *reg)
{
    println("dump %s:", reg->r[Q]);
    for (int i = 0; i < vec_len(reg->vars); i++) {
        struct rvar *v = vec_at(reg->vars, i);
        println("[%d] %s, %d", i, SYM_X_LABEL(v->sym), v->size);
    }
}

static struct vector * operand_regs(struct operand *operand)
{
    struct vector *v = vec_new();
    if (operand->sym && SYM_X_REG(operand->sym))
        vec_push(v, SYM_X_REG(operand->sym));
    if (operand->index && SYM_X_REG(operand->index))
        vec_push(v, SYM_X_REG(operand->index));
    return v;
}

static const char * operand2s(struct operand *operand, int opsize)
{
    switch (operand->op) {
    case IR_NONE:
        if (SYM_X_REG(operand->sym)) {
            return format("%s", SYM_X_REG(operand->sym)->r[idx[opsize]]);
        } else if (SYM_X_KIND(operand->sym) == SYM_KIND_IMM) {
            return format("$%lu", SYM_VALUE_U(operand->sym));
        } else if (SYM_X_KIND(operand->sym) == SYM_KIND_LREF) {
            return format("%ld(%s)", SYM_X_LOFF(operand->sym), rbp->r[Q]);
        } else if (SYM_X_KIND(operand->sym) == SYM_KIND_GREF) {
            if (isfunc(SYM_TYPE(operand->sym)))
                return format("$%s", SYM_X_LABEL(operand->sym));
            else
                return format("%s(%s)", SYM_X_LABEL(operand->sym), rip->r[Q]);
        } else if (SYM_X_KIND(operand->sym) == SYM_KIND_TMP) {
            cc_assert(SYM_X_REG(operand->sym));
            return format("%s", SYM_X_REG(operand->sym)->r[idx[opsize]]);
        } else {
            cc_assert(0);
        }
        break;
    case IR_SUBSCRIPT:
        {
            node_t *sym = operand->sym;
            node_t *index = operand->index;
            if (SYM_X_KIND(sym) == SYM_KIND_LREF) {
                long offset = SYM_X_LOFF(sym) + operand->disp;
                if (index) {
                    if (offset)
                        return format("%ld(%s,%s,%d)",
                                      offset, rbp->r[Q],
                                      SYM_X_REG(index)->r[Q], operand->scale);
                    else
                        return format("(%s,%s,%d)",
                                      rbp->r[Q],
                                      SYM_X_REG(index)->r[Q], operand->scale);
                } else {
                    if (offset)
                        return format("%ld(%s)", offset, rbp->r[Q]);
                    else
                        return format("(%s)", rbp->r[Q]);
                }
            } else if (SYM_X_KIND(sym) == SYM_KIND_TMP) {
                long offset = operand->disp;
                if (index) {
                    if (offset)
                        return format("%ld(%s,%s,%d)",
                                      offset, SYM_X_REG(sym)->r[Q],
                                      SYM_X_REG(index)->r[Q], operand->scale);
                    else
                        return format("(%s,%s,%d)",
                                      SYM_X_REG(sym)->r[Q],
                                      SYM_X_REG(index)->r[Q], operand->scale);
                } else {
                    if (offset)
                        return format("%ld(%s)", offset, SYM_X_REG(sym)->r[Q]);
                    else
                        return format("(%s)", SYM_X_REG(sym)->r[Q]);
                }
            } else {
                cc_assert(0);
            }
        }
        break;
    case IR_INDIRECTION:
        return format("(%s)", SYM_X_REG(operand->sym)->r[Q]);
    default:
        cc_assert(0);
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
    cc_assert(SYM_X_REG(sym) == NULL);

    struct rvar *var = new_rvar(sym, opsize);
    
    if (!reg->vars)
        reg->vars = vec_new1(var);
    else
        vec_push(reg->vars, var);
    SYM_X_REG(sym) = reg;
}

// ST sym, reg
static void store(node_t *sym)
{
    SYM_X_REG(sym) = NULL;
    SYM_X_INMEM(sym) = true;
}

static struct reg * get_reg(struct reg **regs, int count, struct vector *excepts)
{
    // filter excepts out
    struct vector *candicates = vec_new();
    for (int i = 0; i < count; i++) {
        struct reg *ri = regs[i];
        bool found = false;
        for (int j = 0; j < vec_len(excepts); j++) {
            struct reg *rj = vec_at(excepts, j);
            if (ri == rj) {
                found = true;
                break;
            }
        }
        if (!found)
            vec_push(candicates, ri);
    }
    
    // if exists an empty reg, return directly
    for (int i = 0; i < vec_len(candicates); i++) {
        struct reg *reg = vec_at(candicates, i);
        if (vec_empty(reg->vars))
            return reg;
    }

    cc_assert(vec_len(candicates));
    
    // all regs are dirty, select one.
    struct reg *ret = NULL;
    int mincost = 0;
    for (int i = 0; i < vec_len(candicates); i++) {
        int cost = 0;
        bool sticky = false;
        struct reg *reg = vec_at(candicates, i);
        for (int j = 0; j < vec_len(reg->vars); j++) {
            struct rvar *v = vec_at(reg->vars, j);
            struct uses *uses = get_current_uses(v->sym);
            if (SYM_X_INMEM(v->sym)) {
                // ok
            } else if (uses->live == false) {
                // ok
            } else if (SYM_X_KIND(v->sym) == SYM_KIND_TMP) {
                // if contains a live tmp symbol, skip the whole reg
                sticky = true;
                break;
            } else {
                // spill
                cost += 1;
            }
        }

        if (sticky == false) {
            if (ret == NULL || cost < mincost) {
                ret = reg;
                mincost = cost;
            }
        }
    }
    if (ret == NULL)
        die("no enough registers");
    // spill out
    drain_reg(ret);
    return ret;
}

static struct reg * dispatch_reg(struct reg **regs, int count, struct vector *excepts,
                                 node_t *sym, int opsize)
{
    // already in reg, return directly
    if (SYM_X_REG(sym))
        return SYM_X_REG(sym);
    struct reg *reg = get_reg(regs, count, excepts);
    load(reg, sym, opsize);
    return reg;
}

static struct reg * dispatch_ireg(node_t *sym, struct vector *excepts, int opsize)
{
    return dispatch_reg(int_regs, ARRAY_SIZE(int_regs), excepts, sym, opsize);
}

static struct reg * dispatch_freg(node_t *sym, struct vector *excepts, int opsize)
{
    return dispatch_reg(float_regs, ARRAY_SIZE(float_regs), excepts, sym, opsize);
}

static struct reg * get_one_freg(struct vector *excepts)
{
    return get_reg(float_regs, ARRAY_SIZE(float_regs), excepts);
}

static struct reg * get_one_ireg(struct vector *excepts)
{
    return get_reg(int_regs, ARRAY_SIZE(int_regs), excepts);
}

static bool is_in_tac(node_t *sym, struct tac *tac)
{
    for (int i = 0; i < ARRAY_SIZE(tac->operands); i++) {
        struct operand *operand = tac->operands[i];
        if (operand && (sym == operand->sym || sym == operand->index))
            return true;
    }
    return false;
}

static void do_drain_reg(struct reg *reg, struct vector *excepts)
{
    struct reg *new_reg = NULL;
    for (int j = 0; j < vec_len(reg->vars); j++) {
        struct rvar *v = vec_at(reg->vars, j);
        node_t *sym = v->sym;
        int i = idx[v->size];
        // always clear
        SYM_X_REG(sym) = NULL;
        struct uses *uses = get_current_uses(sym);

        switch (SYM_X_KIND(sym)) {
        case SYM_KIND_GREF:
            if (!SYM_X_INMEM(sym)) {
                emit("mov%s %s, %s(%s)" COMMENT("%d-byte spill"),
                     suffixi[i], reg->r[i], SYM_X_LABEL(sym), rip->r[Q], v->size);
                store(sym);
            }
            break;
        case SYM_KIND_LREF:
            if (!SYM_X_INMEM(sym)) {
                emit("mov%s %s, %ld(%s)" COMMENT("%d-byte spill"),
                     suffixi[i], reg->r[i], SYM_X_LOFF(sym), rbp->r[Q], v->size);
                store(sym);
            }
            break;
        case SYM_KIND_TMP:
            if (is_in_tac(sym, current_tac) || uses->live) {
                // sticky
                if (!new_reg) {
                    const char **suffix;
                    if (reg->freg) {
                        new_reg = dispatch_freg(sym, excepts, v->size);
                        suffix = suffixf;
                    } else {
                        new_reg = dispatch_ireg(sym, excepts, v->size);
                        suffix = suffixi;
                    }
                    emit("mov%s %s, %s" COMMENT("%d-byte spill"),
                         suffix[i], reg->r[i], new_reg->r[i], v->size);
                } else {
                    load(new_reg, sym, v->size);
                }
            }
            break;
        default:
            break;
        }
    }
    if (reg->vars)
        vec_clear(reg->vars);
}

static void drain_regs(struct vector *regs)
{
    for (int i = 0; i < vec_len(regs); i++) {
        struct reg *reg = vec_at(regs, i);
        do_drain_reg(reg, regs);
    }
}

// maybe sticky
static void drain_reg(struct reg *reg)
{
    do_drain_reg(reg, vec_new1(reg));
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
    const char *src_label = operand2s(l, from_size);
    struct vector *excepts = operand_regs(l);
    struct reg *reg = dispatch_ireg(result->sym, excepts, to_size);
    if (is_imm_operand(l)) {
        vec_push(excepts, reg);
        struct reg *src_reg = dispatch_ireg(l->sym, excepts, from_size);
        emit("mov%s %s, %s", suffixi[from_i], src_label, src_reg->r[from_i]);
        // reset
        src_label = operand2s(l, from_size);
    }
    if (typeop == INT) {
        emit("movs%s%s %s, %s",
             suffixi[from_i], suffixi[to_i], src_label, reg->r[to_i]);
    } else {
        if (from_size == Long)
            emit("movl %s, %s", src_label, reg->r[from_i]);
        else
            emit("movz%s%s %s, %s",
                 suffixi[from_i], suffixi[to_i], src_label, reg->r[to_i]);
    }
}

static void emit_conv_ii_narrow(struct tac *tac, int typeop)
{
    struct operand *result = tac->operands[0];
    struct operand *l = tac->operands[1];
    int from_size = tac->from_opsize;
    int to_size = tac->to_opsize;
    int to_i = idx[to_size];
    // narrow
    const char *src_label = operand2s(l, from_size);
    struct vector *excepts = operand_regs(l);
    struct reg *reg = dispatch_ireg(result->sym, excepts, to_size);
    emit("mov%s %s, %s", suffixi[to_i], src_label, reg->r[to_i]);
}

static void emit_conv_i2i(struct tac *tac, int typeop)
{
    if (tac->from_opsize < tac->to_opsize)
        emit_conv_ii_widden(tac, typeop);
    else if (tac->from_opsize > tac->to_opsize)
        emit_conv_ii_narrow(tac, typeop);
    else
        cc_assert(0);
}

static void emit_conv_tof(struct tac *tac, const char *op)
{
    struct operand *result = tac->operands[0];
    struct operand *l = tac->operands[1];
    int from_size = tac->from_opsize;
    int to_size = tac->to_opsize;
    int from_i = idx[from_size];
    int to_i = idx[to_size];
    const char *src_label = operand2s(l, from_size);
    if (is_imm_operand(l)) {
        struct reg *reg = dispatch_ireg(l->sym, NULL, from_size);
        emit("mov%s %s, %s", suffixi[from_i], src_label, reg->r[from_i]);
        // reset
        src_label = operand2s(l, from_size);
    }
    struct vector *excepts = operand_regs(l);
    struct reg *reg = dispatch_freg(result->sym, excepts, to_size);
    emit("%s2%s %s, %s", op, suffixf[to_i], src_label, reg->r[to_i]);
}

static void emit_conv_si2f(struct tac *tac)
{
    emit_conv_tof(tac, "cvtsi");
}

static void emit_conv_ui2f(struct tac *tac)
{
    emit_conv_tof(tac, "cvtsi");
}

static void emit_conv_f2si(struct tac *tac)
{
    struct operand *result = tac->operands[0];
    struct operand *l = tac->operands[1];
    int from_size = tac->from_opsize;
    int from_i = idx[from_size];
    const char *src_label = operand2s(l, from_size);
    struct vector *excepts = operand_regs(l);
    struct reg *reg = dispatch_ireg(result->sym, excepts, from_size);
    emit("cvtt%s2si %s, %s", suffixf[from_i], src_label, reg->r[from_i]);
}

static void emit_conv_f2ui(struct tac *tac)
{
    emit_conv_f2si(tac);
}

static void emit_conv_f2f(struct tac *tac)
{
    if (tac->from_opsize == Long && tac->to_opsize == Quad)
        emit_conv_tof(tac, "cvtss");
    else if (tac->from_opsize == Quad && tac->to_opsize == Long)
        emit_conv_tof(tac, "cvtsd");
    else
        cc_assert(0);
}

static struct operand * make_ret_offset_operand(struct operand *operand, long offset)
{
    switch (operand->op) {
    case IR_NONE:
    case IR_SUBSCRIPT:
        {
            struct operand *ret = alloc_operand();
            *ret = *operand;
            ret->op = IR_SUBSCRIPT;
            ret->disp += offset;
            return ret;
        }
        break;
    default:
        cc_assert(0);
    }
}

/*
  integer params(6): rdi, rsi, rdx, rcx, r8, r9
  floating params(8): xmm0~xmm7
 */
// TODO: first struct param, get regs excepts
static void emit_param_scalar(struct tac *tac, struct pnode *pnode)
{
    struct operand *operand = tac->operands[1];
    node_t *arg = pnode->param;
    struct paddr *paddr = pnode->paddr;
    node_t *ty = AST_TYPE(arg);
    int i = idx[TYPE_SIZE(ty)];
    node_t *sym = operand->sym;
    struct reg *src = SYM_X_REG(sym);
    switch (paddr->kind) {
    case ADDR_REGISTER:
        {
            const char *src_label = operand2s(operand, TYPE_SIZE(ty));
            struct reg *dst = paddr->u.regs[0].reg;
            if (src == NULL || src != dst) {
                if (isfloat(ty)) {
                    if (TYPE_SIZE(ty) == 4)
                        emit("movss %s, %s", src_label, dst->r[i]);
                    else
                        emit("movsd %s, %s", src_label, dst->r[i]);
                } else {
                    emit("mov%s %s, %s", suffixi[i], src_label, dst->r[i]);
                }
            }
        }
        break;
    case ADDR_STACK:
        {
            const char *stack;
            if (paddr->u.offset)
                stack = format("%ld(%s)", paddr->u.offset, rsp->r[Q]);
            else
                stack = format("(%s)", rsp->r[Q]);
            if (src) {
                if (isfloat(ty)) {
                    if (TYPE_SIZE(ty) == 4)
                        emit("movss %s, %s", src->r[i], stack);
                    else
                        emit("movsd %s, %s", src->r[i], stack);
                } else {
                    emit("mov%s %s, %s", suffixi[i], src->r[i], stack);
                }
            } else {
                const char *src_label = operand2s(operand, TYPE_SIZE(ty));
                if (isfloat(ty)) {
                    struct reg *tmp = get_one_freg(NULL);
                    if (TYPE_SIZE(ty) == 4) {
                        emit("movss %s, %s", src_label, tmp->r[Q]);
                        emit("movss %s, %s", tmp->r[Q], stack);
                    } else {
                        emit("movsd %s, %s", src_label, tmp->r[Q]);
                        emit("movsd %s, %s", tmp->r[Q], stack);
                    }
                } else {
                    struct reg *tmp = get_one_ireg(NULL);
                    emit("mov%s %s, %s", suffixi[i], src_label, tmp->r[i]);
                    emit("mov%s %s, %s", suffixi[i], tmp->r[i], stack);
                }
            }
        }
        break;
    default:
        cc_assert(0);
    }
}

static void emit_param_record(struct tac *tac, struct pnode *pnode)
{
    struct operand *operand = tac->operands[1];
    struct paddr *paddr = pnode->paddr;
    size_t size = paddr->size;
    int cnt = ROUNDUP(size, 8) >> 3;
    
    if (paddr->kind == ADDR_STACK) {
        struct vector *excepts = operand_regs(operand);
        struct reg *tmp = get_one_ireg(excepts);
        
        long loff = paddr->u.offset;
        for (int i = 0; i < cnt; i++) {
            long offset = i << 3;
            struct operand *src = make_ret_offset_operand(operand, offset);
            const char *src_label = operand2s(src, Quad);
            emit("movq %s, %s", src_label, tmp->r[Q]);
            if (loff + offset)
                emit("movq %s, %ld(%s)", tmp->r[Q], loff + offset, rsp->r[Q]);
            else
                emit("movq %s, (%s)", tmp->r[Q], rsp->r[Q]);
        }
    } else if (paddr->kind == ADDR_REGISTER) {
        long loff = 0;
        for (int i = 0; i < cnt; i++ , loff += 8, size -= 8) {
            struct reg *reg = paddr->u.regs[i].reg;
            int type = paddr->u.regs[i].type;
            struct operand *src = make_ret_offset_operand(operand, loff);
            switch (type) {
            case REG_INT:
                if (size > 4)
                    emit("movq %s, %s", operand2s(src, Quad), reg->r[Q]);
                else if (size > 2)
                    emit("movl %s, %s", operand2s(src, Long), reg->r[L]);
                else if (size == 2)
                    emit("movzwl %s, %s", operand2s(src, Word), reg->r[L]);
                else if (size == 1)
                    emit("movzbl %s, %s", operand2s(src, Quad), reg->r[L]);
                else
                    cc_assert(0);
                break;
            case REG_SSE_F:
                emit("movss %s, %s", operand2s(src, Long), reg->r[Q]);
                break;
            case REG_SSE_D:
                emit("movsd %s, %s", operand2s(src, Quad), reg->r[Q]);
                break;
            case REG_SSE_FF:
                emit("movq %s, %s ", operand2s(src, Quad), reg->r[Q]);
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
        cc_assert(0);
}

static void drain_args_regs(int gp, int fp)
{
    struct vector *gv = vec_new();
    for (int i = 0; i < gp; i++)
        vec_push(gv, iarg_regs[i]);
    drain_regs(gv);

    struct vector *fv = vec_new();
    for (int i = 0; i < fp; i++)
        vec_push(fv, farg_regs[i]);
    drain_regs(fv);
}

static void emit_call_epilogue(node_t *ftype, struct paddr *retaddr, struct operand *result)
{
    if (retaddr->kind == ADDR_STACK) {
        // memory
        // rax contains the address
        SYM_X_LOFF(result->sym) = calls_return_loff;
        SYM_X_KIND(result->sym) = SYM_KIND_LREF;
        load(int_regs[RAX], result->sym, Quad);
    } else if (retaddr->kind == ADDR_REGISTER) {
        node_t *rty = rtype(ftype);
        if (isstruct(rty) || isunion(rty)) {
            // move from regs to stack
            size_t size = retaddr->size;
            long loff = 0;
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
                            emit("mov%s %s, %ld(%s)",
                                 suffixi[i], reg->r[i], loff + calls_return_loff, rbp->r[Q]);
                        }
                        break;
                    case 3:
                        {
                            emit("movw %s, %ld(%s)",
                                 reg->r[W], loff + calls_return_loff, rbp->r[Q]);
                            emit("shrl $16, %s", reg->r[L]);
                            emit("movb %s, %ld(%s)",
                                 reg->r[B], loff + calls_return_loff + 2, rbp->r[Q]);
                        }
                        break;
                    case 4:
                        emit("movl %s, %ld(%s)",
                             reg->r[L], loff + calls_return_loff, rbp->r[Q]);
                        break;
                    case 5:
                    case 6:
                        {
                            int opsize = size == 5 ? Byte : Word;
                            int i = idx[opsize];
                            emit("movl %s, %ld(%s)",
                                 reg->r[L], loff + calls_return_loff, rbp->r[Q]);
                            emit("shrq $32, %s", reg->r[Q]);
                            emit("mov%s %s, %ld(%s)",
                                 suffixi[i], reg->r[i], loff + calls_return_loff + 4, rbp->r[Q]);
                        }
                        break;
                    case 7:
                        {
                            emit("movl %s, %ld(%s)",
                                 reg->r[L], loff + calls_return_loff, rbp->r[Q]);
                            emit("shrq $32, %s", reg->r[Q]);
                            emit("movw %s, %ld(%s)",
                                 reg->r[W], loff + calls_return_loff + 4, rbp->r[Q]);
                            emit("shrl $16, %s", reg->r[L]);
                            emit("movb %s, %ld(%s)",
                                 reg->r[B], loff + calls_return_loff + 6, rbp->r[Q]);
                        }
                        break;
                        // >= 8
                    default:
                        emit("movq %s, %ld(%s)",
                             reg->r[Q], loff + calls_return_loff, rbp->r[Q]);
                        break;
                    }
                    break;
                case REG_SSE_F:
                    emit("movss %s, %ld(%s)",
                         reg->r[Q], loff + calls_return_loff, rbp->r[Q]);
                    break;
                case REG_SSE_D:
                    emit("movsd %s, %ld(%s)",
                         reg->r[Q], loff + calls_return_loff, rbp->r[Q]);
                    break;
                case REG_SSE_FF:
                    emit("movlps %s, %ld(%s)",
                         reg->r[Q], loff + calls_return_loff, rbp->r[Q]);
                    break;
                }
            }
            SYM_X_LOFF(result->sym) = calls_return_loff;
            SYM_X_KIND(result->sym) = SYM_KIND_LREF;
        } else {
            // scalar
            struct reg *reg = retaddr->u.regs[0].reg;
            if (TYPE_SIZE(rty) == Quad)
                load(reg, result->sym, Quad);
            else
                load(reg, result->sym, Long);
        }
    }
}

static void emit_nonbuiltin_call(struct tac *tac)
{
    node_t *call = tac->call;
    node_t **args = EXPR_ARGS(call);
    struct operand *result = tac->operands[0];
    struct operand *l = tac->operands[1];
    int len = tac->relop;
    node_t *ftype = rtype(AST_TYPE(EXPR_OPERAND(call, 0)));
    struct vector *params = vec_new();
    struct tac *t = tac->prev;
    for (int i = 0; i < len; i++, t = t->prev) {
        cc_assert(t->op == IR_PARAM);
        vec_push(params, t);
    }

    // TODO: reset current tac here to make next uses live for params
    current_tac = t;
    
    // emit args
    struct pinfo *pinfo = alloc_addr_for_funcall(ftype, args);
    // drain regs
    drain_args_regs(pinfo->gp, pinfo->fp);
    for (int i = 0, sub = 0; i < vec_len(pinfo->pnodes); i++) {
        struct pnode *pnode = vec_at(pinfo->pnodes, i);
        if (i == 0 && pinfo->retaddr && pinfo->retaddr->kind == ADDR_STACK) {
            struct reg *reg = iarg_regs[0];
            emit("leaq %ld(%s), %s", calls_return_loff, rbp->r[Q], reg->r[Q]);
            sub = 1;
        } else {
            struct tac *param = vec_at(params, i - sub);
            emit_param(param, pnode);
        }
    }
    
    if (TYPE_VARG(ftype) || TYPE_OLDSTYLE(ftype)) {
        drain_reg(int_regs[RAX]);
        emit("movb $%d, %%al", pinfo->fp);
    }

    // TODO: drain regs in retaddr

    // direct / indirect
    if (l->op == IR_NONE && SYM_X_KIND(l->sym) == SYM_KIND_GREF)
        emit("call %s", SYM_X_LABEL(l->sym));
    else
        emit("call *%s", operand2s(l, Quad));

    if (result)
        emit_call_epilogue(ftype, pinfo->retaddr, result);
}

static void emit_builtin_va_start(struct tac *tac)
{
    die("not implemented yet");
}

static void emit_builtin_va_arg_class(struct tac *tac)
{
    die("not implemented yet");
}

static void emit_builtin_va_end(struct tac *tac)
{
    die("not implemented yet");
}

static void emit_builtin_va_copy(struct tac *tac)
{
    die("not implemented yet");
}

static void emit_call(struct tac *tac)
{
    const char *name = SYM_NAME(tac->operands[1]->sym);
    if (!strcmp(name, "__builtin_va_start"))
        emit_builtin_va_start(tac);
    else if (!strcmp(name, "__builtin_va_arg_class"))
        emit_builtin_va_arg_class(tac);
    else if (!strcmp(name, "__builtin_va_end"))
        emit_builtin_va_end(tac);
    else if (!strcmp(name, "__builtin_va_copy"))
        emit_builtin_va_copy(tac);
    else
        emit_nonbuiltin_call(tac);
}

static void emit_return_by_stack(struct operand *l, struct paddr *retaddr)
{
    // by memory
    // copy l to the address pointed by the implicit first argument
    // rax will contain the address that has passed in by caller in rdi.
    struct pnode *pnode = vec_head(func_pinfo->pnodes);
    node_t *sym = pnode->param;
    struct reg *rax = int_regs[RAX];

    struct vector *excepts = operand_regs(l);
    // drain rax
    do_drain_reg(rax, excepts);
    
    vec_push(excepts, rax);
    struct reg *tmp = get_one_ireg(excepts);

    // set destination base address
    emit("movq %ld(%s), %s", SYM_X_LOFF(sym), rbp->r[Q], rax->r[Q]);

    // Can't be IR_INDIRECTION (see ir.c: emit_uop_indirection)
    cc_assert(l->op == IR_NONE || l->op == IR_SUBSCRIPT);
    // emit bytes
    // rounded by 8 bytes. (see emit_function_prologue)
    size_t size = ROUNDUP(retaddr->size, 8);
    for (size_t i = 0; i < size; i += 8) {
        struct operand *operand = make_ret_offset_operand(l, i);
        const char *src_label = operand2s(operand, Quad);
        const char *dst_label;
        if (i)
            dst_label = format("%ld(%s)", i, rax->r[Q]);
        else
            dst_label = format("(%s)", rax->r[Q]);
        emit("movq %s, %s", src_label, tmp->r[Q]);
        emit("movq %s, %s", tmp->r[Q], dst_label);
    }
}

static void emit_return_by_registers_scalar(struct operand *l, struct paddr *retaddr)
{
    // scalar
    node_t *rtype = rtype(current_ftype);
    struct reg *reg = retaddr->u.regs[0].reg;
    // drain reg
    drain_reg(reg);
    
    int opsize = TYPE_SIZE(rtype);
    int i = idx[opsize];
    if (isint(rtype) || isptr(rtype)) {
        if (opsize < 4) {
            // extend to 32bits
            bool sign = TYPE_OP(rtype) == INT;
            if (sign)
                emit("movs%sl %s, %s", suffixi[i], operand2s(l, opsize), reg->r[L]);
            else
                emit("movz%sl %s, %s", suffixi[i], operand2s(l, opsize), reg->r[L]);
        } else {
            emit("mov%s %s, %s", suffixi[i], operand2s(l, opsize), reg->r[i]);
        }
    } else if (isfloat(rtype)) {
        emit("mov%s %s, %s", suffixf[i], operand2s(l, opsize), reg->r[i]);
    } else {
        cc_assert(0);
    }
}

static void emit_return_by_registers_record(struct operand *l, struct paddr *retaddr)
{
    size_t size = retaddr->size;
    int cnt = ROUNDUP(retaddr->size, 8) >> 3;
    long loff = 0;

    // calculate excepts
    struct vector *excepts = vec_new();
    vec_add(excepts, operand_regs(l));
    for (int i = 0; i < cnt; i++) {
        struct reg *reg = retaddr->u.regs[i].reg;
        // drain regs
        do_drain_reg(reg, excepts);
        vec_push(excepts, reg);
    }
    
    for (int i = 0; i < cnt; i++, loff += 8, size -= 8) {
        int type = retaddr->u.regs[i].type;
        struct reg *reg = retaddr->u.regs[i].reg;
        struct operand *operand = make_ret_offset_operand(l, loff);
        switch (type) {
        case REG_INT:
            switch (size) {
            case 1:
            case 2:
                {
                    int opsize = size == 1 ? Byte : Word;
                    int i = idx[opsize];
                    emit("mov%s %s, %s", suffixi[i], operand2s(operand, opsize), reg->r[i]);
                }
                break;
            case 3:
                {
                    struct operand *operand1 = make_ret_offset_operand(l, loff + 2);
                    struct reg *tmp1 = get_one_ireg(excepts);
                    emit("movzbl %s, %s", operand2s(operand1, Byte), tmp1->r[L]);
                    emit("shll $16, %s", tmp1->r[L]);

                    emit("movzwl %s, %s", operand2s(operand, Long), reg->r[L]);
                    emit("orl %s, %s", tmp1->r[L], reg->r[L]);
                }
                break;
            case 4:
                emit("movl %s, %s", operand2s(operand, Long), reg->r[L]);
                break;
            case 5:
            case 6:
                {
                    int opsize = size == 5 ? Byte : Word;
                    int i = idx[opsize];
                    struct operand *operand1 = make_ret_offset_operand(l, loff + 4);
                    struct reg *tmp1 = get_one_ireg(excepts);
                    emit("movz%sl %s, %s", suffixi[i], operand2s(operand1, opsize), tmp1->r[L]);
                    emit("shlq $32, %s", tmp1->r[Q]);

                    vec_push(excepts, tmp1);
                    struct reg *tmp2 = get_one_ireg(excepts);
                    emit("movl %s, %s", operand2s(operand, Long), tmp2->r[L]);
                    emit("orq %s, %s", tmp2->r[Q], tmp1->r[Q]);

                    emit("movq %s, %s", tmp1->r[Q], reg->r[Q]);
                }
                break;
            case 7:
                {
                    struct operand *operand1 = make_ret_offset_operand(l, loff + 6);
                    struct reg *tmp1 = get_one_ireg(excepts);
                    emit("movzbl %s, %s", operand2s(operand1, Byte), tmp1->r[L]);
                    emit("shlq $16, %s", tmp1->r[Q]);

                    vec_push(excepts, tmp1);
                    struct operand *operand2 = make_ret_offset_operand(l, loff + 4);
                    struct reg *tmp2 = get_one_ireg(excepts);
                    emit("movzwl %s, %s", operand2s(operand2, Word), tmp2->r[L]);

                    emit("orl %s, %s", tmp1->r[L], tmp2->r[L]);
                    emit("shlq $32, %s", tmp2->r[Q]);

                    // reuse tmp1 here
                    emit("movl %s, %s", operand2s(operand, Long), tmp1->r[L]);
                    emit("orq %s, %s", tmp2->r[Q], tmp1->r[Q]);

                    emit("movq %s, %s", tmp1->r[Q], reg->r[Q]);
                }
                break;
                // >=8
            default:
                emit("movq %s, %s", operand2s(operand, Quad), reg->r[Q]);
                break;
            }
            break;
        case REG_SSE_F:
            emit("movss %s, %s", operand2s(operand, Quad), reg->r[Q]);
            break;
        case REG_SSE_D:
            emit("movsd %s, %s", operand2s(operand, Quad), reg->r[Q]);
            break;
        case REG_SSE_FF:
            emit("movlps %s, %s", operand2s(operand, Quad), reg->r[Q]);
            break;
        }
    }
}

static void emit_return(struct tac *tac)
{
    struct operand *l = tac->operands[1];
    // non-void return
    if (l) {
        struct paddr *retaddr = func_pinfo->retaddr;
        if (retaddr->kind == ADDR_STACK) {
            emit_return_by_stack(l, retaddr);
        } else if (retaddr->kind == ADDR_REGISTER) {
            // by register
            // integer registers: rax, rdx
            // sse registers: xmm0, xmm1
            node_t *rtype = rtype(current_ftype);
            if (isstruct(rtype) || isunion(rtype))
                emit_return_by_registers_record(l, retaddr);
            else
                emit_return_by_registers_scalar(l, retaddr);
        }
    }
    // if it's the last tac, don't emit a jump
    if (tac->next) {
        emit("jmp %s", func_end_label);
        func_returns++;
    }
}

static void emit_if(struct tac *tac, bool reverse, bool floating)
{
    struct operand *result = tac->operands[0];
    struct operand *l = tac->operands[1];
    struct operand *r = tac->operands[2];
    int i = idx[tac->opsize];
    bool sign = tac->sign;
    if (tac->relop) {
        struct vector *vl = operand_regs(l);
        struct vector *vr = operand_regs(r);
        vec_add(vl, vr);
        const char *l_label = operand2s(l, tac->opsize);
        const char *r_label = operand2s(r, tac->opsize);
        struct reg *reg;
        if (floating) {
            reg = get_one_freg(vl);
            emit("mov%s %s, %s", suffixf[i], l_label, reg->r[i]);
            emit("ucomi%s %s, %s", suffixf[i], r_label, reg->r[i]);
        } else {
            reg = get_one_ireg(vl);
            emit("mov%s %s, %s", suffixi[i], l_label, reg->r[i]);
            emit("cmp%s %s, %s", suffixi[i], r_label, reg->r[i]);
        }
        
        const char *jop;
        switch (tac->relop) {
        case '>':
            if (reverse)
                jop = sign ? "jle" : "jbe";
            else
                jop = sign ? "jg" : "ja";
            break;
        case '<':
            if (reverse)
                jop = sign ? "jge" : "jae";
            else
                jop = sign ? "jl" : "jb";
            break;
        case GEQ:
            if (reverse)
                jop = sign ? "jl" : "jb";
            else
                jop = sign ? "jge" : "jae";
            break;
        case LEQ:
            if (reverse)
                jop = sign ? "jg" : "ja";
            else
                jop = sign ? "jle" : "jbe";
            break;
        case NEQ:
            jop = reverse ? "je" : "jne";
            break;
        case EQ:
            jop = reverse ? "jne" : "je";
            break;
        default:
            cc_assert(0);
        }
        emit("%s %s", jop, SYM_X_LABEL(result->sym));
    } else {
        if (floating) {
            struct reg *r = get_one_freg(NULL);
            emit("xor%s %s, %s", suffixp[i], r->r[i], r->r[i]);
            emit("ucomi%s %s, %s", suffixf[i], operand2s(l, tac->opsize), r->r[i]);
        } else {
            emit("cmp%s $0, %s", suffixi[i], operand2s(l, tac->opsize));
        }
        
        const char *jop = reverse ? "je" : "jne";
        emit("%s %s", jop, SYM_X_LABEL(result->sym));
    }
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
    emit("jmp %s", SYM_X_LABEL(tac->operands[0]->sym));
}

static void emit_label(struct tac *tac)
{
    emit_noindent("%s:", SYM_X_LABEL(tac->operands[0]->sym));
}

static void emit_assign_basic(struct operand *l, struct operand *r,
                              int opsize, bool assignf)
{
    const char *dst = operand2s(l, opsize);
    const char *src = operand2s(r, opsize);
    int i = idx[opsize];
    const char **suffix = assignf ? suffixf : suffixi;
    emit("mov%s %s, %s", suffix[i], src, dst);
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
        if (SYM_X_REG(l->sym)) {
            emit_assign_basic(l, r, tac->opsize, assignf);
        } else if (is_direct_mem_operand(r) && SYM_X_REG(r->sym)) {
            load(SYM_X_REG(r->sym), l->sym, tac->opsize);
        } else {
            // alloc register for tmp operand
            struct vector *excepts = operand_regs(r);
            struct reg *reg;
            if (assignf)
                reg = dispatch_freg(l->sym, excepts, tac->opsize);
            else
                reg = dispatch_ireg(l->sym, excepts, tac->opsize);
            int i = idx[tac->opsize];
            const char *src = operand2s(r, tac->opsize);
            const char **suffix = assignf ? suffixf : suffixi;
            emit("mov%s %s, %s", suffix[i], src, reg->r[i]);
            // POST load
            if (is_direct_mem_operand(r) && !SYM_X_REG(r->sym))
                load(reg, r->sym, tac->opsize);
        }
    } else {
        cc_assert(0);
    }
}

static void emit_uop_int(struct tac *tac, const char *op)
{
    struct operand *result = tac->operands[0];
    struct operand *l = tac->operands[1];
    int i = idx[tac->opsize];
    struct vector *excepts = operand_regs(l);
    struct reg *reg = dispatch_ireg(result->sym, excepts, tac->opsize);
    const char *l_label = operand2s(l, tac->opsize);
    emit("mov%s %s, %s", suffixi[i], l_label, reg->r[i]);
    emit("%s%s %s", op, suffixi[i], reg->r[i]);
}

// int: bitwise not ~
static void emit_uop_not(struct tac *tac)
{
    emit_uop_int(tac, "not");
}

static void emit_uop_minus_i(struct tac *tac)
{
    emit_uop_int(tac, "neg");
}

static void emit_uop_minus_f(struct tac *tac)
{
    struct operand *result = tac->operands[0];
    struct operand *l = tac->operands[1];
    int i = idx[tac->opsize];
    struct vector *excepts = operand_regs(l);
    struct reg *reg = dispatch_freg(result->sym, excepts, tac->opsize);
    const char *l_label = operand2s(l, tac->opsize);
    emit("xor%s %s, %s", suffixp[i], reg->r[i], reg->r[i]);
    emit("sub%s %s, %s", suffixf[i], l_label, reg->r[i]);
}

static void emit_uop_address(struct tac *tac)
{
    struct operand *result = tac->operands[0];
    struct operand *l = tac->operands[1];
    const char *src_label = operand2s(l, Quad);
    struct reg *reg = get_one_ireg(NULL);
    load(reg, result->sym, Quad);
    emit("leaq %s, %s", src_label, reg->r[Q]);
}

static void emit_bop_arith(struct tac *tac, const char *op, bool floating)
{
    struct operand *result = tac->operands[0];
    struct operand *l = tac->operands[1];
    struct operand *r = tac->operands[2];
    struct vector *vl = operand_regs(l);
    struct vector *vr = operand_regs(r);
    vec_add(vl, vr);
    struct reg *reg;
    if (floating)
        reg = dispatch_freg(result->sym, vl, tac->opsize);
    else
        reg = dispatch_ireg(result->sym, vl, tac->opsize);
    int i = idx[tac->opsize];
    const char *l_label = operand2s(l, tac->opsize);
    const char *r_label = operand2s(r, tac->opsize);
    const char **suffix = floating ? suffixf : suffixi;
    emit("mov%s %s, %s", suffix[i], l_label, reg->r[i]);
    emit("%s%s %s, %s", op, suffix[i], r_label, reg->r[i]);
}

static void emit_bop_int(struct tac *tac, const char *op)
{
    emit_bop_arith(tac, op, false);
}

static void emit_bop_int_mul_div(struct tac *tac, const char *op)
{
    struct operand *l = tac->operands[1];
    struct operand *r = tac->operands[2];
    struct reg *rax = int_regs[RAX];
    struct reg *rdx = int_regs[RDX];
    int i = idx[tac->opsize];
    if (tac->opsize > Byte) {
        struct vector *regs = vec_new();
        vec_push(regs, rax);
        vec_push(regs, rdx);
        drain_regs(regs);
        if (tac->op == IR_DIVI || tac->op == IR_IDIVI ||
            tac->op == IR_MOD || tac->op == IR_IMOD)
            emit("mov%s $0, %s", suffixi[i], rdx->r[i]);
    } else {
        drain_reg(rax);
    }
    const char *l_label = operand2s(l, tac->opsize);
    const char *r_label = operand2s(r, tac->opsize);
    emit("mov%s %s, %s", suffixi[i], l_label, rax->r[i]);
    emit("%s%s %s", op, suffixi[i], r_label);
}

static void emit_bop_int_imul(struct tac *tac)
{
    emit_bop_int_mul_div(tac, "imul");
    load(int_regs[RAX], tac->operands[0]->sym, tac->opsize);
}

static void emit_bop_int_mul(struct tac *tac)
{
    emit_bop_int_imul(tac);
}

static void emit_bop_int_div(struct tac *tac)
{
    emit_bop_int_mul_div(tac, "div");
    load(int_regs[RAX], tac->operands[0]->sym, tac->opsize);
}

static void emit_bop_int_idiv(struct tac *tac)
{
    emit_bop_int_mul_div(tac, "idiv");
    load(int_regs[RAX], tac->operands[0]->sym, tac->opsize);
}

static void emit_bop_mod(struct tac *tac)
{
    emit_bop_int_mul_div(tac, "div");
    load(int_regs[RDX], tac->operands[0]->sym, tac->opsize);
}

static void emit_bop_imod(struct tac *tac)
{
    emit_bop_int_mul_div(tac, "idiv");
    load(int_regs[RDX], tac->operands[0]->sym, tac->opsize);
}

static void emit_bop_shift(struct tac *tac, const char *op)
{
    struct operand *result = tac->operands[0];
    struct operand *l = tac->operands[1];
    struct operand *r = tac->operands[2];
    struct reg *rcx = int_regs[RCX];
    int i = idx[tac->opsize];
    drain_reg(rcx);
    struct vector *vl = operand_regs(l);
    struct vector *vr = operand_regs(r);
    const char *l_label = operand2s(l, tac->opsize);
    const char *r_label = operand2s(r, tac->opsize);
    struct vector *excepts = vec_new();
    vec_push(excepts, rcx);
    vec_add(excepts, vl);
    vec_add(excepts, vr);
    struct reg *reg = dispatch_ireg(result->sym, excepts, tac->opsize);
    emit("mov%s %s, %s", suffixi[i], l_label, reg->r[i]);
    emit("mov%s %s, %s", suffixi[i], r_label, rcx->r[i]);
    emit("%s%s %s, %s", op, suffixi[i], rcx->r[B], reg->r[i]);
}

static void emit_bop_float(struct tac *tac, const char *op)
{
    emit_bop_arith(tac, op, true);
}

static void emit_tac(struct tac *tac)
{
    switch (tac->op) {
    case IR_LABEL:
        emit_label(tac);
        break;
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
        emit_bop_int(tac, "add");
        break;
    case IR_SUBI:
        emit_bop_int(tac, "sub");
        break;
    case IR_DIVI:
        emit_bop_int_div(tac);
        break;
    case IR_IDIVI:
        emit_bop_int_idiv(tac);
        break;
    case IR_MOD:
        emit_bop_mod(tac);
        break;
    case IR_IMOD:
        emit_bop_imod(tac);
        break;
    case IR_MULI:
        emit_bop_int_mul(tac);
        break;
    case IR_IMULI:
        emit_bop_int_imul(tac);
        break;
    case IR_OR:
        emit_bop_int(tac, "or");
        break;
    case IR_AND:
        emit_bop_int(tac, "and");
        break;
    case IR_XOR:
        emit_bop_int(tac, "xor");
        break;
    case IR_LSHIFT:
    case IR_ILSHIFT:
        emit_bop_shift(tac, "shl");
        break;
    case IR_RSHIFT:
        emit_bop_shift(tac, "shr");
        break;
    case IR_IRSHIFT:
        emit_bop_shift(tac, "sar");
        break;
    case IR_ADDF:
        emit_bop_float(tac, "add");
        break;
    case IR_SUBF:
        emit_bop_float(tac, "sub");
        break;
    case IR_DIVF:
        emit_bop_float(tac, "div");
        break;
    case IR_MULF:
        emit_bop_float(tac, "mul");
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
        emit_conv_si2f(tac);
        break;
    case IR_CONV_UI_F:
        emit_conv_ui2f(tac);
        break;
    case IR_CONV_F_SI:
        emit_conv_f2si(tac);
        break;
    case IR_CONV_F_UI:
        emit_conv_f2ui(tac);
        break;
    case IR_CONV_FF:
        emit_conv_f2f(tac);
        break;
    case IR_NONE:
    case IR_PARAM:
    default:
        // skip
        break;
    }
}

static void emit_tacs(struct tac *head)
{
    for (struct tac *tac = head; tac; tac = tac->next) {
        // set current tac
        current_tac = tac;
        emit_tac(tac);
    }
}

static void mark_die(node_t *sym)
{
    int kind = SYM_X_KIND(sym);
    if (kind == SYM_KIND_GREF ||
        kind == SYM_KIND_LREF ||
        kind == SYM_KIND_TMP) {
        SYM_X_USES(sym).live = false;
        SYM_X_USES(sym).next = NULL;
    }
}

static void mark_live(node_t *sym, struct tac *tac)
{
    int kind = SYM_X_KIND(sym);
    if (kind == SYM_KIND_GREF ||
        kind == SYM_KIND_LREF ||
        kind == SYM_KIND_TMP) {
        SYM_X_USES(sym).live = true;
        SYM_X_USES(sym).next = tac;
    }
}

static struct uses * get_uses(node_t *sym, struct tac *tac)
{
    struct map *tuple = dict_get(next_info, sym);
    cc_assert(tuple);
    struct uses *uses = map_get(tuple, tac);
    cc_assert(uses);
    return uses;
}

static void scan_uses(struct tac *tail)
{
    struct vector *keys = next_info->keys;
    for (struct tac *tac = tail; tac; tac = tac->prev) {
        // set
        for (int i = 0; i < vec_len(keys); i++) {
            node_t *sym = vec_at(keys, i);
            struct map *tuple = dict_get(next_info, sym);
            struct uses *uses = map_get(tuple, tac);
            *uses = SYM_X_USES(sym);
        }
        
        // mark
        for (int i = 0; i < ARRAY_SIZE(tac->operands); i++) {
            struct operand *operand = tac->operands[i];
            if (operand) {
                if (i == 0) {
                    switch (operand->op) {
                    case IR_NONE:
                        if (operand->sym)
                            mark_die(operand->sym);
                        break;
                    case IR_SUBSCRIPT:
                    case IR_INDIRECTION:
                        if (operand->sym)
                            mark_live(operand->sym, tac);
                        if (operand->index)
                            mark_live(operand->index, tac);
                        break;
                    }
                } else {
                    if (operand->sym)
                        mark_live(operand->sym, tac);
                    if (operand->index)
                        mark_live(operand->index, tac);
                }
            }
        }
    }
}

static void init_sym_uses(node_t *sym)
{
    mark_die(sym);
    struct map *tuple = dict_get(next_info, sym);
    if (!tuple) {
        tuple = map_new();
        tuple->cmpfn = nocmp;
        dict_put(next_info, sym, tuple);
    }
}

static void init_sym_addrs(node_t *sym)
{
    if (SYM_X_KIND(sym) == SYM_KIND_GREF ||
        SYM_X_KIND(sym) == SYM_KIND_LREF)
        SYM_X_INMEM(sym) = true;
}

static void init_tacs(struct tac *head)
{
    for (struct tac *tac = head; tac; tac = tac->next) {
        for (int i = 0; i < ARRAY_SIZE(tac->operands); i++) {
            struct operand *operand = tac->operands[i];
            if (operand) {
                if (operand->sym) {
                    init_sym_addrs(operand->sym);
                    init_sym_uses(operand->sym);
                }
                if (operand->index) {
                    init_sym_addrs(operand->index);
                    init_sym_uses(operand->index);
                }
            }
        }
    }
    struct vector *values = dict_values(next_info);
    for (int i = 0; i < vec_len(values); i++) {
        struct map *tuple = vec_at(values, i);
        for (struct tac *tac = head; tac; tac = tac->next) {
            struct uses *uses = zmalloc(sizeof(struct uses));
            map_put(tuple, tac, uses);
        }
    }
}

// Parameter classification

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
            cc_assert(0);
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
    cc_assert(class1 == sse_class && class2 == sse_class);
    cc_assert(len1 <= 2 && len2 <= 2);
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
        cc_assert(0);
    }
}

static struct vector * merge_elements(struct vector *v1, struct vector *v2)
{
    cc_assert(vec_len(v1) == vec_len(v2));

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
    for (int i = 0; i < LIST_LEN(TYPE_FIELDS(ty)); i++) {
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
    for (int i = 0; i < LIST_LEN(TYPE_FIELDS(ty)); i++) {
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
        cc_assert(0);
}

static bool is_type_aligned(node_t *ty)
{
    // Now all types are aligned.
    return true;
}

static struct pinfo * new_pinfo(struct pinfo *pinfo)
{
    struct pinfo *ret = zmalloc(sizeof(struct pinfo));
    *ret = *pinfo;
    return ret;
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
    node_t *sym = gen_tmp_sym();
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
                cc_assert(0);
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
    
    for (int i = 0; i < LIST_LEN(params); i++) {
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
            cc_assert(vec_len(elements) == vec_len(classes));

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
                    cc_assert(0);
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
    return new_pinfo(&(struct pinfo) {
        .fp = fp,
        .gp = gp,
        .size = offset,
        .pnodes = pnodes,
        .retaddr = retaddr
    });
}

static struct pinfo * alloc_addr_for_funcall(node_t *ftype, node_t **params)
{
    return alloc_addr_for_params(ftype, params, true);
}

static struct pinfo * alloc_addr_for_funcdef(node_t *ftype, node_t **params)
{
    return alloc_addr_for_params(ftype, params, false);
}

static size_t call_returns_size(node_t *decl)
{
    size_t extra_stack_size = 0;
    node_t **calls = DECL_X_CALLS(decl);
    for (int i = 0; i < LIST_LEN(calls); i++) {
        node_t *call = calls[i];
        node_t *rty = AST_TYPE(call);
        if (isrecord(rty))
            extra_stack_size = MAX(extra_stack_size, TYPE_SIZE(rty));
    }
    return extra_stack_size;
}

static size_t call_params_size(node_t *decl)
{
    size_t extra_stack_size = 0;
    node_t **calls = DECL_X_CALLS(decl);
    for (int i = 0; i < LIST_LEN(calls); i++) {
        node_t *call = calls[i];
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
        emit("movq %s, %ld(%s)", r->r[Q], offset, rbp->r[Q]);
    }
    const char *label = gen_label();
    emit("testb %%al, %%al");
    emit("je %s", label);
    for (int i = 0; i < ARRAY_SIZE(farg_regs); i++, offset += 16) {
        struct reg *r = farg_regs[i];
        emit("movaps %s, %ld(%s)", r->r[Q], offset, rbp->r[Q]);
    }
    emit_noindent("%s:", label);
    cc_assert(offset == 0);
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
    cc_assert(0);
}

static void emit_register_params(node_t *decl)
{
    for (int i = 0; i < vec_len(func_pinfo->pnodes); i++) {
        struct pnode *pnode = vec_at(func_pinfo->pnodes, i);
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
                        emit("movq %s, %ld(%s)", reg->r[Q], loff, rbp->r[Q]);
                    else if (size > 2)
                        emit("movl %s, %ld(%s)", reg->r[L], loff, rbp->r[Q]);
                    else if (size == 2)
                        emit("movw %s, %ld(%s)", reg->r[W], loff, rbp->r[Q]);
                    else if (size == 1)
                        emit("movb %s, %ld(%s)", reg->r[B], loff, rbp->r[Q]);
                    else
                        cc_assert(0);
                    break;
                case REG_SSE_F:
                    emit("movss %s, %ld(%s)", reg->r[Q], loff, rbp->r[Q]);
                    break;
                case REG_SSE_D:
                    emit("movsd %s, %ld(%s)", reg->r[Q], loff, rbp->r[Q]);
                    break;
                case REG_SSE_FF:
                    emit("movlps %s, %ld(%s)", reg->r[Q], loff, rbp->r[Q]);
                    break;
                }
            }
        }
    }
}

static void emit_function_prologue(struct gdata *gdata)
{
    node_t *decl = gdata->u.decl;
    node_t *fsym = DECL_SYM(decl);
    node_t *ftype = SYM_TYPE(fsym);
    
    if (gdata->global)
        emit(".globl %s", gdata->label);
    emit(".text");
    emit_noindent("%s:", gdata->label);
    emit("pushq %s", rbp->r[Q]);
    emit("movq %s, %s", rsp->r[Q], rbp->r[Q]);

    size_t localsize = 0;

    // register save area
    if (TYPE_VARG(ftype))
        localsize += REGISTER_SAVE_AREA_SIZE;
    
    // local vars
    for (int i = 0; i < LIST_LEN(DECL_X_LVARS(decl)); i++) {
        node_t *lvar = DECL_X_LVARS(decl)[i];
        node_t *sym = DECL_SYM(lvar);
        node_t *ty = SYM_TYPE(sym);
        size_t size = TYPE_SIZE(ty);
        localsize = ROUNDUP(localsize + size, 4);
        SYM_X_LOFF(sym) = -localsize;
    }

    // params
    node_t **params = TYPE_PARAMS(ftype);
    func_pinfo = alloc_addr_for_funcdef(ftype, params);
    for (int i = 0; i < vec_len(func_pinfo->pnodes); i++) {
        struct pnode *pnode = vec_at(func_pinfo->pnodes, i);
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
    size_t returns_size = call_returns_size(decl);
    if (returns_size) {
        // rounded by 8 bytes. (see emit_return)
        localsize = ROUNDUP(localsize + returns_size, 8);
        calls_return_loff = -localsize;
    }
    
    // call params
    localsize += call_params_size(decl);
    localsize = ROUNDUP(localsize, 16);
    
    if (localsize > 0)
        emit("subq $%llu, %s", localsize, rsp->r[Q]);
}

static void emit_text(struct gdata *gdata)
{
    node_t *decl = gdata->u.decl;
    node_t *fsym = DECL_SYM(decl);
    node_t *ftype = SYM_TYPE(fsym);

    // reset func context
    func_end_label = STMT_X_NEXT(DECL_BODY(decl));
    func_returns = 0;
    next_info = dict_new();
    next_info->map->cmpfn = nocmp;
    reset_regs();
    calls_return_loff = 0;
    func_pinfo = NULL;
    current_ftype = ftype;

    emit_function_prologue(gdata);
    if (TYPE_VARG(ftype))
        emit_register_save_area();
    else
        emit_register_params(decl);
    init_tacs(DECL_X_HEAD(decl));
    scan_uses(DECL_X_TAIL(decl));
    emit_tacs(DECL_X_HEAD(decl));
    // function epilogue
    if (func_returns)
        emit_noindent("%s:", func_end_label);
    /*
      leave instruction

      move rbp to rsp
      pop rbp
    */
    emit("leave");
    emit("ret");
}

static void emit_data(struct gdata *gdata)
{
    if (gdata->global)
        emit(".globl %s", gdata->label);
    emit(".data");
    if (gdata->align > 1)
        emit(".align %d", gdata->align);
    emit_noindent("%s:", gdata->label);
    for (int i = 0; i < LIST_LEN(gdata->u.xvalues); i++) {
        struct xvalue *value = gdata->u.xvalues[i];
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
        default:
            die("unknown size");
            break;
        }
    }
}

static void emit_bss(struct gdata *gdata)
{
    if (!gdata->global)
        emit(".local %s", gdata->label);
    emit(".comm %s,%llu,%d",
         gdata->label,
         gdata->size,
         gdata->align);
}

static void emit_compounds(struct dict *compounds)
{
    struct vector *keys = compounds->keys;
    if (vec_len(keys)) {
        for (int i = 0; i < vec_len(keys); i++) {
            const char *label = vec_at(compounds->keys, i);
            struct gdata *gdata = dict_get(compounds, label);
            emit_data(gdata);
        }
    }
}

static void emit_strings(struct dict *strings)
{
    struct vector *keys = strings->keys;
    if (vec_len(keys)) {
        emit(".section .rodata");
        for (int i = 0; i < vec_len(keys); i++) {
            const char *name = vec_at(strings->keys, i);
            const char *label = dict_get(strings, name);
            emit_noindent("%s:", label);
            emit(".asciz %s", name);
        }
    }
}

static void emit_floats(struct dict *floats)
{
    struct vector *keys = floats->keys;
    if (vec_len(keys)) {
        emit(".section .rodata");
        for (int i = 0; i < vec_len(keys); i++) {
            const char *name = vec_at(floats->keys, i);
            const char *label = dict_get(floats, name);
            node_t *sym = lookup(name, constants);
            cc_assert(sym);
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
                cc_assert(0);
            }
        }
    }
}

static void gen_init(FILE *fp)
{
    outfp = fp;
    init_regs();
}

void gen(struct externals *exts, FILE * fp)
{
    cc_assert(errors == 0 && fp);
    
    gen_init(fp);
    for (int i = 0; i < vec_len(exts->gdatas); i++) {
        struct gdata *gdata = vec_at(exts->gdatas, i);
        switch (gdata->id) {
        case GDATA_BSS:
            emit_bss(gdata);
            break;
        case GDATA_DATA:
            emit_data(gdata);
            break;
        case GDATA_TEXT:
            emit_text(gdata);
            break;
        default:
            cc_assert(0);
        }
    }
    emit_compounds(exts->compounds);
    emit_strings(exts->strings);
    emit_floats(exts->floats);
    emit(".ident \"mcc: %d.%d\"", MAJOR(version), MINOR(version));
}
