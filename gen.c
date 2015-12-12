#include "cc.h"
#include "sys.h"

static FILE *outfp;
struct dict *compound_lits;
struct dict *string_lits;

void emit(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    fprintf(outfp, "\t");
    vfprintf(outfp, fmt, ap);
    fprintf(outfp, "\n");
    va_end(ap);
}

void emit_noindent(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vfprintf(outfp, fmt, ap);
    fprintf(outfp, "\n");
    va_end(ap);
}

static void emit_funcdef(node_t * n)
{
    node_t *sym = DECL_SYM(n);
    const char *label = SYM_X_LABEL(sym);
    if (SYM_SCLASS(sym) != STATIC)
        emit(".globl %s", label);
    emit_noindent("%s:", label);
    print_codes(DECL_X_CODES(n));
}

static void emit_compound_literals(void)
{
    if (vec_len(compound_lits->keys))
        emit(".data");
    for (int i = 0; i < vec_len(compound_lits->keys); i++) {
        const char *label = vec_at(compound_lits->keys, i);
        node_t *init = dict_get(compound_lits, label);
        emit_noindent("%s:", label);
        emit_initializer(init);
    }
}

static void emit_string_literals(void)
{
    if (vec_len(string_lits->keys))
        emit(".section .rodata");
    for (int i = 0; i < vec_len(string_lits->keys); i++) {
        const char *name = vec_at(string_lits->keys, i);
        const char *label = dict_get(string_lits, name);
        emit_noindent("%s:", label);
        emit(".asciz %s", name);
    }
}

static void gen_init(FILE *fp)
{
    outfp = fp;
    compound_lits = dict_new();
    string_lits = dict_new();
    init_regs();
}

void gen(node_t * tree, FILE * fp)
{
    cc_assert(errors == 0 && fp);
    
    gen_init(fp);
    for (int i = 0; i < LIST_LEN(DECL_EXTS(tree)); i++) {
        node_t *n = DECL_EXTS(tree)[i];
        if (isfuncdef(n))
            emit_funcdef(n);
        else if (isvardecl(n))
            gen_decl(n);
    }
    emit_compound_literals();
    emit_string_literals();
    emit(".ident \"mcc: %d.%d\"", MAJOR(version), MINOR(version));
}
