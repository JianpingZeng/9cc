#include "cc.h"
#include "sys.h"

static FILE *outfp;

#define LEAD  "    "
#define emit(...)             emitf(LEAD,  __VA_ARGS__)
#define emit_noindent(...)    emitf(NULL, __VA_ARGS__)
#define pushq(reg)      emit("pushq %%" #reg)
#define popq(reg)       emit("popq %%" #reg)
#define movq(src, dst)  emit("movq %s, %s", src, dst)

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

static void emit_initializer(node_t *n)
{
    emit(".int 4");
}

static void emit_data(node_t *n)
{
    node_t *sym = DECL_SYM(n);
    node_t *ty = SYM_TYPE(sym);
    if (SYM_SCLASS(sym)) {
	emit(".globl %s", SYM_LABEL(sym));
	emit(".data");
    }
    emit(".align %d", TYPE_ALIGN(ty));
    emit_noindent("%s:", SYM_LABEL(sym));
    emit_initializer(n);
}

static void emit_bss(node_t *n)
{
    node_t *sym = DECL_SYM(n);
    node_t *ty = SYM_TYPE(sym);
    if (SYM_SCLASS(sym) == STATIC)
	emit(".lcomm %s,%llu,%d", SYM_LABEL(sym), TYPE_SIZE(ty), TYPE_ALIGN(ty));
    else
	emit(".comm  %s,%llu,%d", SYM_LABEL(sym), TYPE_SIZE(ty), TYPE_ALIGN(ty));
}

static void emit_funcdef(node_t *n)
{
    node_t *sym = DECL_SYM(n);
    const char *name = SYM_LABEL(sym);
    if (SYM_SCLASS(sym) != STATIC)
	emit(".globl %s", name);
    emit_noindent("%s:", name);
    pushq(rbp);
    movq("%rsp", "%rbp");
    popq(rbp);
    emit("ret");
}

static void emit_begin(const char *ifile)
{
    emit(".file \"%s\"", basename(ifile));
}

static void emit_end(void)
{
    emit(".ident \"mcc\"");
}

void gen(node_t *tree, FILE *fp, const char *ifile)
{
    cc_assert(errors == 0 && fp);
    outfp = fp;
    emit_begin(strcopy(ifile));
    node_t **exts = DECL_EXTS(tree);
    for (int i = 0; i < LIST_LEN(exts); i++) {
	node_t *n = exts[i];
	if (isfuncdef(n)) {
	    emit_funcdef(n);
	} else if (isvardecl(n)) {
	    if (DECL_BODY(n))
		emit_data(n);
	    else
		emit_bss(n);
	}
    }
    emit_end();
}
