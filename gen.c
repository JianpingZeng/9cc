#include "cc.h"

static FILE *outfp;

#define emit(...)             emitf("    ",  __VA_ARGS__)
#define emit_noindent(...)    emitf(NULL, __VA_ARGS__)

static void emitf(const char *lead, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    if (lead)
	fprintf(outfp, "%s", lead);
    vfprintf(outfp, fmt, ap);
    fprintf(outfp, "\n");
    va_end(ap);
}

static void emit_globalvar(node_t *n)
{
    node_t *sym = DECL_SYM(n);
    if (SYM_SCLASS(sym) == STATIC)
	emit(".local %s", SYM_LABEL(sym));
    else
	emit(".globl %s", SYM_LABEL(sym));
}

static void emit_funcdef(node_t *n)
{
    node_t *sym = DECL_SYM(n);
    emit_noindent("%s:", SYM_LABEL(sym));
    emit(".cfi_startproc");
    emit(".cfi_endproc");
}

static void emit_begin(const char *ifile)
{
    emit(".file \"%s\"", basename(ifile));
}

static void emit_end(void)
{
}

void gen(node_t *tree, FILE *fp, const char *ifile)
{
    cc_assert(errors == 0 && fp);
    outfp = fp;
    emit_begin(strcopy(ifile));
    node_t **exts = DECL_EXTS(tree);
    for (int i = 0; i < LIST_LEN(exts); i++) {
	node_t *n = exts[i];
	if (isfuncdef(n))
	    emit_funcdef(n);
	else if (isvardecl(n))
	    emit_globalvar(n);
    }
    emit_end();
}
