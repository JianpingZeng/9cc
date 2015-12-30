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

static void scan_tac_uses(struct tac *tac)
{

}

static void scan_init_blk(struct bblock *blk)
{
    
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

static void emit_text(gdata_t *gdata)
{
    if (GDATA_GLOBAL(gdata))
        emit(".globl %s", GDATA_LABEL(gdata));
    emit(".text");
    emit_noindent("%s:", GDATA_LABEL(gdata));
    // TODO:
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
    // outfp = fp;
    outfp = stdout;
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
