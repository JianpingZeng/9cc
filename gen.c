#include "cc.h"

static FILE *outfp;

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

static struct bblock * alloc_bblock(void)
{
    struct bblock *blk = zmalloc(sizeof(struct bblock));
    blk->tacs = vec_new();
    return blk;
}

static struct bblock * make_bblock(struct vector *bblks)
{
    cc_assert(bblks);

    struct bblock *current = vec_tail(bblks);
    if (current == NULL || vec_len(current->tacs) > 0) {
        struct bblock *blk = alloc_bblock();
        vec_push(bblks, blk);
        return blk;
    } else {
        return current;
    }
}

static struct vector * construct_flow_graph(struct vector *tacs)
{
    struct vector *v = vec_new();
    struct bblock *blk;
    
    for (int i = 0; i < vec_len(tacs); i++) {
        struct tac *tac = vec_at(tacs, i);
        if (i == 0) {
            // new block
            blk = make_bblock(v);
        }

        if (tac->op == IR_IF_I ||
            tac->op == IR_IF_F ||
            tac->op == IR_IF_FALSE_I ||
            tac->op == IR_IF_FALSE_F ||
            tac->op == IR_GOTO ||
            tac->op == IR_RETURNI ||
            tac->op == IR_RETURNF) {
            vec_push(blk->tacs, tac);
            // new block
            blk = make_bblock(v);
        } else if (tac->op == IR_LABEL) {
            // new block
            blk = make_bblock(v);
            do {
                vec_push(blk->tacs, tac);
                i++;
                if (i < vec_len(tacs))
                    tac = vec_at(tacs, i);
                else
                    tac = NULL;
            } while (tac && tac->op == IR_LABEL);
            i--;
        } else if (tac->op == IR_CALL) {
            // new block
            blk = make_bblock(v);
            vec_push(blk->tacs, tac);
        } else {
            vec_push(blk->tacs, tac);
        }
    }
    return v;
}

static int addr_type(node_t *sym)
{
    struct vector *addrs = SYM_X_ADDRS(sym);
    struct addr *addr = vec_at(addrs, 0);
    return addr->kind;
}

static void mark_die(node_t *sym)
{
    int kind = addr_type(sym);
    if (kind == ADDR_TYPE_MEMORY ||
        kind == ADDR_TYPE_STACK ||
        kind == ADDR_TYPE_REGISTER) {
        SYM_X_USES(sym).live = false;
        SYM_X_USES(sym).use_tac = NULL;
    }
}

static void mark_live(node_t *sym, struct tac *tac)
{
    int kind = addr_type(sym);
    if (kind == ADDR_TYPE_MEMORY ||
        kind == ADDR_TYPE_STACK ||
        kind == ADDR_TYPE_REGISTER) {
        SYM_X_USES(sym).live = true;
        SYM_X_USES(sym).use_tac = tac;
    }
}

static void scan_tac_uses(struct tac *tac)
{
    struct operand *result = tac->result;
    struct operand *l = tac->args[0];
    struct operand *r = tac->args[1];

    if (result)
        result->uses = SYM_X_USES(result->sym);
    if (l)
        l->uses = SYM_X_USES(l->sym);
    if (r)
        r->uses = SYM_X_USES(r->sym);
    // mark
    if (result)
        mark_die(result->sym);
    if (l)
        mark_live(l->sym, tac);
    if (r)
        mark_live(r->sym, tac);
}

static void init_sym_uses(node_t *sym)
{
    int kind = addr_type(sym);
    if (kind == ADDR_TYPE_MEMORY ||
        kind == ADDR_TYPE_STACK) {
        SYM_X_USES(sym).live = true;
        SYM_X_USES(sym).use_tac = NULL;
    } else {
        SYM_X_USES(sym).live = false;
        SYM_X_USES(sym).use_tac = NULL;
    }
}

static void scan_init_blk(struct bblock *blk)
{
    for (int i = 0; i < vec_len(blk->tacs); i++) {
        struct tac *tac = vec_at(blk->tacs, i);
        struct operand *result = tac->result;
        struct operand *l = tac->args[0];
        struct operand *r = tac->args[1];
        if (result)
            init_sym_uses(result->sym);
        if (l)
            init_sym_uses(l->sym);
        if (r)
            init_sym_uses(r->sym);
    }
}

static void scan_uses(struct vector *blks)
{
    for (int i = 0; i < vec_len(blks); i++) {
        struct bblock *blk = vec_at(blks, i);
        scan_init_blk(blk);
        for (int j = vec_len(blk->tacs) - 1; j >= 0; j--) {
            struct tac *tac = vec_at(blk->tacs, j);
            scan_tac_uses(tac);
        }
    }
}

static void optimize_blk(struct bblock *blk)
{
    // TODO: 
}

static void optimize_blks(struct vector *blks)
{
    for (int i = 0; i < vec_len(blks); i++)
        optimize_blk(vec_at(blks, i));
}

static void emit_tac(struct tac *tac)
{
    switch (tac->op) {
        // bop
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
    case IR_MOD:
    case IR_OR:
    case IR_AND:
    case IR_XOR:
    case IR_LSHIFT:
    case IR_RSHIFT:
        break;

        // uop
    case IR_NOT:
    case IR_MINUSI:
    case IR_MINUSF:
        break;

    case IR_ASSIGNI:
    case IR_ASSIGNF:
        break;

    case IR_PARAM:
    case IR_CALL:
        break;

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

    case IR_IF_I:
    case IR_IF_F:
    case IR_IF_FALSE_I:
    case IR_IF_FALSE_F:
        break;

    case IR_RETURNI:
    case IR_RETURNF:
        break;

    case IR_LABEL:
    case IR_GOTO:
        break;

    case IR_SUBSCRIPT:
    case IR_ADDRESS:
    case IR_INDIRECTION:
        break;
        
    case IR_NONE:
        break;

    default:
        cc_assert(0);
    }
}

static void emit_blks(struct vector *blks)
{
    for (int i = 0; i < vec_len(blks); i++) {
        struct bblock *blk = vec_at(blks, i);
        for (int j = 0; j < vec_len(blk->tacs); j++) {
            struct tac *tac = vec_at(blk->tacs, j);
            emit_tac(tac);
        }
    }
}

static struct vector * init_text(gdata_t *gdata)
{
    node_t *decl = GDATA_TEXT_DECL(gdata);
    struct vector *tacs = DECL_X_TACS(decl);
    struct vector *bblks = construct_flow_graph(tacs);
    scan_uses(bblks);
    optimize_blks(bblks);
    for (int i = 0; i < vec_len(bblks); i++) {
        struct bblock *blk = vec_at(bblks, i);
        println("BLOCK#%d {", i);
        for (int j = 0; j < vec_len(blk->tacs); j++) {
            struct tac *tac = vec_at(blk->tacs, j);
            print_tac(tac);
        }
        println("}\n");
    }
    return bblks;
}

static size_t call_stack_size(node_t *decl)
{
    size_t extra_stack_size = 0;
    struct vector *calls = DECL_X_CALLS(decl);
    for (int i = 0; i < vec_len(calls); i++) {
        node_t *call = vec_at(calls, i);
        node_t **args = EXPR_ARGS(call);

        size_t size = 0;
        int num_int = 0;
        int num_float = 0;
        for (int j = 0; j < LIST_LEN(args); j++) {
            node_t *ty = AST_TYPE(args[j]);
            size_t typesize = ROUNDUP(TYPE_SIZE(ty), 8);
            if (isint(ty) || isptr(ty)) {
                num_int++;
                if (num_int > NUM_IARG_REGS)
                    size += typesize;
            } else if (isfloat(ty)) {
                num_float++;
                if (num_float > NUM_FARG_REGS)
                    size += typesize;
            } else if (isstruct(ty) || isunion(ty)) {
                size += typesize;
            } else {
                cc_assert(0);
            }
        }
        extra_stack_size = MAX(extra_stack_size, size);
    }
    return extra_stack_size;
}

/*
  stack layout

  | ...           |
  +---------------+ <--- rbp+16
  | return address|
  +---------------+ <--- rbp+8
  | saved rbp     |
  +---------------+ <--- rbp
  | local vars    |
  +---------------+
  | params        |
  +---------------+
  | call params   |
  +---------------+ <--- rsp
 */

static void emit_text(gdata_t *gdata)
{
    node_t *decl = GDATA_TEXT_DECL(gdata);
    // init
    struct vector *bblks = init_text(gdata);
    
    if (GDATA_GLOBAL(gdata))
        emit(".globl %s", GDATA_LABEL(gdata));
    emit(".text");
    emit_noindent("%s:", GDATA_LABEL(gdata));
    emit("pushq %rbp");
    emit("movq %rsp, %rbp");

    size_t sub = 0;
    for (int i = 0; i < DECL_X_LVARS(decl); i++) {
        node_t *lvar = DECL_X_LVARS(decl)[i];
        node_t *sym = DECL_SYM(decl);
        size_t offset = sub + TYPE_SIZE(SYM_TYPE(sym));
        
    }
    
    emit_blks(bblks);
    emit("leave");
    emit("ret");
}

static void emit_data(gdata_t *gdata)
{
    if (GDATA_GLOBAL(gdata))
        emit(".globl %s", GDATA_LABEL(gdata));
    emit(".data");
    if (GDATA_ALIGN(gdata) > 1)
        emit(".align %d", GDATA_ALIGN(gdata));
    emit_noindent("%s:", GDATA_LABEL(gdata));
    for (int i = 0; i < LIST_LEN(GDATA_DATA_XVALUES(gdata)); i++) {
        struct xvalue *value = GDATA_DATA_XVALUES(gdata)[i];
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

static void emit_bss(gdata_t *gdata)
{
    emit("%s %s,%llu,%d",
         GDATA_GLOBAL(gdata) ? ".comm" : ".lcomm",
         GDATA_LABEL(gdata),
         GDATA_SIZE(gdata),
         GDATA_ALIGN(gdata));
}

static void emit_compounds(struct dict *compounds)
{
    struct vector *keys = compounds->keys;
    if (vec_len(keys)) {
        for (int i = 0; i < vec_len(keys); i++) {
            const char *label = vec_at(compounds->keys, i);
            gdata_t *gdata = dict_get(compounds, label);
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
        gdata_t *gdata = vec_at(exts->gdatas, i);
        switch (GDATA_ID(gdata)) {
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
            die("unknown gdata id '%d'", GDATA_ID(gdata));
            break;
        }
    }
    emit_compounds(exts->compounds);
    emit_strings(exts->strings);
    emit_floats(exts->floats);
    emit(".ident \"mcc: %d.%d\"", MAJOR(version), MINOR(version));
}
