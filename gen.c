#include "cc.h"
#include "sys.h"
#include <stdint.h>

static FILE *outfp;

#define LEAD  "    "
#define emit(...)             emitf(LEAD,  __VA_ARGS__)
#define emit_noindent(...)    emitf(NULL, __VA_ARGS__)
#define pushq(reg)      emit("pushq %s", reg)
#define popq(reg)       emit("popq %s", reg)
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
    int kind = TYPE_KIND(AST_TYPE(n));
    switch (kind) {
    case _BOOL:
    case CHAR:
	emit(".byte %d", ILITERAL_VALUE(n));
	break;
    case SHORT:
	emit(".short %d", ILITERAL_VALUE(n));
	break;
    case INT:
    case UNSIGNED:
	emit(".long %d", ILITERAL_VALUE(n));
	break;
    case LONG:
    case LONG+LONG:
        emit(".quad %llu", ILITERAL_VALUE(n));
	break;
    case FLOAT:
	{
	    float f = FLITERAL_VALUE(n);
	    emit(".long %d", *(uint32_t *)&f);
	}
	break;
    case DOUBLE:
    case LONG+DOUBLE:
	{
	    double d = FLITERAL_VALUE(n);
	    emit(".quad %llu", *(uint64_t *)&d);
	}
	break;
    case POINTER:
	break;
    default:
	error("unknown type '%s'", type2s(AST_TYPE(n)));
	break;
    }
}

static void emit_data(node_t *n)
{
    node_t *sym = DECL_SYM(n);
    node_t *ty = SYM_TYPE(sym);
    if (SYM_SCLASS(sym) != STATIC) {
	emit(".globl %s", SYM_LABEL(sym));
	emit(".data");
    }
    if (TYPE_ALIGN(ty) > 1)
	emit(".align %d", TYPE_ALIGN(ty));
    emit_noindent("%s:", SYM_LABEL(sym));
    emit_initializer(DECL_BODY(n));
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
    pushq("%rbp");
    movq("%rsp", "%rbp");
    popq("%rbp");
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
