#include "cc.h"

static FILE *outfp;
static const char *func_end_label;
static int func_returns;

static const char * oplabel(struct operand *operand);
static void alloc_params(node_t *ftype);

#define NUM_IARG_REGS  6
#define NUM_FARG_REGS  8
#define REGISTER_SAVE_AREA_SIZE  (NUM_IARG_REGS * 8 + NUM_FARG_REGS * 16)

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
static struct reg *rsp, *rbp, *rip;
static int idx[] = {
    -1, B, W, -1, L, -1, -1, -1, Q
};
static const char *suffix[] = {
    "b", "w", "l", "q"
};

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

static struct addr * make_addr_with_type(int kind)
{
    struct addr *addr = zmalloc(sizeof(struct addr));
    addr->kind = kind;
    return addr;
}

static struct addr * make_register_addr(struct reg *reg)
{
    struct addr *addr = make_addr_with_type(ADDR_REGISTER);
    addr->reg = reg;
    return addr;
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

    // init floating regs
    for (int i = XMM0; i <= XMM15; i++) {
        const char *name = format("%%xmm%d", i - XMM0);
        float_regs[i] = mkreg(&(struct reg){
            .r[Q] = name,
            .r[L] = name
        });
        if (i <= XMM7)
            farg_regs[i - XMM0] = float_regs[i];
    }
}

static void reg_add_var(struct reg *reg, node_t *sym)
{
    if (!reg->vars)
        reg->vars = vec_new();

    vec_push(reg->vars, sym);
}

static void load_operand_to_reg(struct operand *operand,
                                struct reg *reg,
                                int opsize)
{
    int index = idx[opsize];
    emit("mov%s %s, %s", suffix[index], oplabel(operand), reg->r[index]);
}

static void dispatch_ireg_for(struct operand *operand, int opsize)
{
    node_t *sym = operand->sym;

    if (SYM_X_ADDRS(sym)[ADDR_REGISTER])
        return;

    for (int i = 0; i < ARRAY_SIZE(int_regs); i++) {
        struct reg *reg = int_regs[i];
        if (vec_len(reg->vars) == 0) {
            // clean reg
            reg_add_var(reg, sym);
            SYM_X_ADDRS(sym)[ADDR_REGISTER] = make_register_addr(reg);
            load_operand_to_reg(operand, reg, opsize);
            return;
        }
    }

    die("not implemented yet");
}

static void dispatch_ireg_uop(struct tac *tac)
{
    
}

static void dispatch_ireg_bop(struct tac *tac)
{
    struct operand *l = tac->args[0];
    struct operand *r = tac->args[1];
    struct operand *result = tac->result;
    int opsize = tac->opsize;

    if (SYM_X_KIND(l->sym) == SYM_KIND_ILITERAL) {
        dispatch_ireg_for(r, opsize);
    } else if (SYM_X_KIND(r->sym) == SYM_KIND_ILITERAL) {
        dispatch_ireg_for(l, opsize);
    } else {
        dispatch_ireg_for(l, opsize);
        dispatch_ireg_for(r, opsize);
    }
}

static void emit_conv_i2i(struct tac *tac)
{
    
}

static void emit_conv_i2f(struct tac *tac)
{
    
}

static void emit_conv_f2i(struct tac *tac)
{
    
}

static void emit_conv_f2f(struct tac *tac)
{
    
}

/*
  integer params(6): rdi, rsi, rdx, rcx, r8, r9
  floating params(8): xmm0~xmm7
 */

static void emit_param(struct tac *tac)
{
    struct operand *operand = tac->args[0];
}

static void emit_call(struct tac *tac)
{
    emit("callq %s", SYM_X_LABEL(tac->args[0]->sym));
    // TODO: clear uses
}

static void emit_return(struct tac *tac)
{
    struct operand *operand = tac->args[0];
    // TODO:
    func_returns++;
}

static void emit_if(struct tac *tac)
{
    
}

static void emit_goto(struct tac *tac)
{
    emit("jmpq %s", SYM_X_LABEL(tac->result->sym));
}

static void emit_label(struct tac *tac)
{
    emit_noindent("%s:", SYM_X_LABEL(tac->result->sym));
}

static void emit_assign(struct tac *tac)
{
    
}

static const char * oplabel(struct operand *operand)
{
    return "";
}

static void emit_uop_not(struct tac *tac)
{
    
}

static void emit_uop_minus(struct tac *tac)
{
    
}

static void emit_uop_address(struct tac *tac)
{
    
}

static void emit_bop_mod(struct tac *tac)
{
    
}

static void emit_bop_int(struct tac *tac, const char *op)
{
}

static void emit_bop_float(struct tac *tac)
{
    
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
    case IR_IF_F:
    case IR_IF_FALSE_I:
    case IR_IF_FALSE_F:
        emit_if(tac);
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
    case IR_DIVI:
    case IR_IDIVI:
    case IR_MULI:
    case IR_IMULI:
    case IR_OR:
    case IR_AND:
    case IR_XOR:
    case IR_LSHIFT:
    case IR_RSHIFT:
        emit_bop_int(tac, "shl");
        break;
    case IR_MOD:
        emit_bop_mod(tac);
        break;
    case IR_ADDF:
    case IR_SUBF:
    case IR_DIVF:
    case IR_MULF:
        emit_bop_float(tac);
        break;
        // uop
    case IR_NOT:
        emit_uop_not(tac);
        break;
    case IR_MINUSI:
    case IR_MINUSF:
        emit_uop_minus(tac);
        break;
    case IR_ADDRESS:
        emit_uop_address(tac);
        break;
    case IR_ASSIGNI:
    case IR_ASSIGNF:
        emit_assign(tac);
        break;
    case IR_PARAM:
        emit_param(tac);
        break;
    case IR_CALL:
        emit_call(tac);
        break;
    case IR_CONV_UI_UI:
    case IR_CONV_UI_SI:
    case IR_CONV_SI_UI:
    case IR_CONV_SI_SI:
        emit_conv_i2i(tac);
        break;
    case IR_CONV_SI_F:
    case IR_CONV_UI_F:
        emit_conv_i2f(tac);
        break;
    case IR_CONV_FF:
        emit_conv_f2f(tac);
        break;
    case IR_CONV_F_SI:
    case IR_CONV_F_UI:
        emit_conv_f2i(tac);
        break;
    case IR_NONE:
    default:
        cc_assert(0);
    }
}

static void emit_tacs(struct tac *head)
{
    for (struct tac *tac = head; tac; tac = tac->next)
        emit_tac(tac);
}

static void alloc_params(node_t *ftype)
{
    if (TYPE_PARAM_ALLOCED(ftype))
        return;
    int gp = 0;
    int fp = 0;
    long offset = 0;
    node_t **params = TYPE_PARAMS(ftype);
    for (int i = 0; i < LIST_LEN(params); i++) {
        node_t *sym = params[i];
        node_t *ty = SYM_TYPE(sym);
        size_t size = ROUNDUP(TYPE_SIZE(ty), 8);
        if (isint(ty) || isptr(ty)) {
            gp++;
            if (gp > NUM_IARG_REGS) {
                
            }
        } else if (isfloat(ty)) {
            fp++;
            if (fp > NUM_FARG_REGS) {
                
            }
        } else if (isstruct(ty) || isunion(ty)) {
            if (size > 16) {
                // memory
            } else {
                
            }
        } else {
            cc_assert(0);
        }
    }
}

static size_t call_stack_size(node_t *call)
{
    node_t **args = EXPR_ARGS(call);
    size_t extra_size = 0;
    int num_int = 0;
    int num_float = 0;
    for (int i = 0; i < LIST_LEN(args); i++) {
        node_t *ty = AST_TYPE(args[i]);
        size_t size = ROUNDUP(TYPE_SIZE(ty), 8);
        
        if (isint(ty) || isptr(ty)) {
            num_int++;
            if (num_int > NUM_IARG_REGS)
                extra_size += size;
        } else if (isfloat(ty)) {
            num_float++;
            if (num_float > NUM_FARG_REGS)
                extra_size += size;
        } else if (isstruct(ty) || isunion(ty)) {
            extra_size += size;
        } else {
            cc_assert(0);
        }
    }
    return extra_size;
}

static size_t extra_stack_size(node_t *decl)
{
    size_t extra_stack_size = 0;
    node_t **calls = DECL_X_CALLS(decl);
    for (int i = 0; i < LIST_LEN(calls); i++) {
        node_t *call = calls[i];
        size_t size = call_stack_size(call);
        extra_stack_size = MAX(extra_stack_size, size);
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
    for (int i = 0; i < ARRAY_SIZE(farg_regs); i++, offset += 16) {
        struct reg *r = farg_regs[i];
        emit("movaps %s, %ld(%s)", r->r[Q], offset, rbp->r[Q]);
    }
    cc_assert(offset == 0);
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
    for (int i = LIST_LEN(DECL_X_LVARS(decl)) - 1; i >= 0; i--) {
        node_t *lvar = DECL_X_LVARS(decl)[i];
        node_t *sym = DECL_SYM(lvar);
        node_t *ty = SYM_TYPE(sym);
        size_t size = TYPE_SIZE(ty);
        int align = TYPE_ALIGN(ty);
        localsize = ROUNDUP(localsize, align) + size;
        SYM_X_LOFF(sym) = - localsize;
    }
    localsize = ROUNDUP(localsize, 8);

    // params
    node_t *ty = SYM_TYPE(DECL_SYM(decl));
    node_t **args = TYPE_PARAMS(ty);
    size_t stack_off = 16;      // rbp+16
    int num_int = 0;
    int num_float = 0;
    for (int i = 0; i < LIST_LEN(args); i++) {
        node_t *sym = args[i];
        node_t *ty = SYM_TYPE(sym);
        size_t size = TYPE_SIZE(ty);
        int align = TYPE_ALIGN(ty);
        if (isint(ty) || isptr(ty)) {
            num_int++;
            if (num_int > NUM_IARG_REGS) {
                SYM_X_LOFF(sym) = stack_off;
                stack_off += ROUNDUP(size, 8);
            } else {
                localsize = ROUNDUP(localsize, align) + size;
                SYM_X_LOFF(sym) = - localsize;
                struct reg *ireg = iarg_regs[num_int-1];
                SYM_X_ADDRS(sym)[ADDR_REGISTER] = make_register_addr(ireg);
            }
        } else if (isfloat(ty)) {
            num_float++;
            if (num_float > NUM_FARG_REGS) {
                SYM_X_LOFF(sym) = stack_off;
                stack_off += ROUNDUP(size, 8);
            } else {
                localsize = ROUNDUP(localsize, align) + size;
                SYM_X_LOFF(sym) = - localsize;
                struct reg *freg = farg_regs[num_float-1];
                SYM_X_ADDRS(sym)[ADDR_REGISTER] = make_register_addr(freg);
            }
        } else if (isstruct(ty) || isunion(ty)) {
            SYM_X_LOFF(sym) = stack_off;
            stack_off += ROUNDUP(size, 8);
        } else {
            cc_assert(0);
        }
    }
    localsize = ROUNDUP(localsize, 8);

    // calls
    localsize += extra_stack_size(decl);
    localsize = ROUNDUP(localsize, 16);
    
    if (localsize > 0)
        emit("subq $%llu, %s", localsize, rsp->r[Q]);
}

static void emit_function_params(node_t *decl)
{
    node_t *ty = SYM_TYPE(DECL_SYM(decl));
    node_t **args = TYPE_PARAMS(ty);
    for (int i = 0; i < LIST_LEN(args); i++) {
        node_t *sym = args[i];
        node_t *ty = SYM_TYPE(sym);
        size_t size = TYPE_SIZE(ty);
        if (SYM_X_ADDRS(sym)[ADDR_REGISTER]) {
            struct reg *reg = SYM_X_ADDRS(sym)[ADDR_REGISTER]->reg;
            if (isint(ty) || isptr(ty)) {
                emit("mov%s %s, %ld(%s)",
                     suffix[idx[size]],
                     reg->r[idx[size]],
                     SYM_X_LOFF(sym), rbp->r[Q]);
            } else if (isfloat(ty)) {
                if (TYPE_KIND(ty) == FLOAT) {
                    emit("movss %s, %ld(%s)",
                         reg->r[idx[size]],
                         SYM_X_LOFF(sym), rbp->r[Q]);
                } else {
                    emit("movsd %s, %ld(%s)",
                         reg->r[idx[size]],
                         SYM_X_LOFF(sym), rbp->r[Q]);
                }
            }
            // reset
            SYM_X_ADDRS(sym)[ADDR_REGISTER] = NULL;
        }
    }
}

/*
  leave instruction

  move rbp to rsp
  pop rbp
 */

static void emit_function_epilogue(struct gdata *gdata)
{
    emit("leave");
    emit("retq");
}

static void emit_text(struct gdata *gdata)
{
    node_t *decl = gdata->u.decl;
    node_t *fsym = DECL_SYM(decl);
    node_t *ftype = SYM_TYPE(fsym);

    func_end_label = STMT_X_NEXT(DECL_BODY(decl));
    func_returns = 0;
    
    emit_function_prologue(gdata);
    if (TYPE_VARG(ftype))
        emit_register_save_area();
    emit_function_params(decl);
    emit_tacs(DECL_X_HEAD(decl));
    if (func_returns)
        emit_noindent("%s:", func_end_label);
    emit_function_epilogue(gdata);
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
    emit("%s %s,%llu,%d",
         gdata->global ? ".comm" : ".lcomm",
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
