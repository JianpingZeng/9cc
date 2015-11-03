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

static void emit_initializer(node_t *t);

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

static const char *gen_slabel(void)
{
    static size_t i;
    return format(".LC%llu", i++);
}

static const char * emit_string_literal(const char *name)
{
    static struct map *map;
    if (!map)
	map = map_new();
    const char *label = map_get(map, name);
    if (!label) {
	label = gen_slabel();
	map_put(map, name, (void *)label);
	emit(".section .rodata");
	emit_noindent("%s:", label);
	emit(".asciz %s", name);
    }
    return label;
}

static const char *gen_compound_label(void)
{
    static size_t i;
    return format("__compound_literal.%llu", i++);
}

static void emit_inits_expr(node_t *n)
{
    
}

static const char *emit_compound_literal(node_t *n)
{
    const char *label = gen_compound_label();
    emit_noindent("%s:", label);
    emit_inits_expr(n);
    return label;
}

static void emit_zero(node_t *ty)
{
    int size = TYPE_SIZE(ty);
    if (size == 1) emit(".byte 0");
    else if (size == 2) emit(".short 0");
    else if (size == 4) emit(".long 0");
    else if (size == 8) emit(".quad 0");
    else cc_assert(0);
}

static const char *get_ptr_label(node_t *n)
{
    const char *label = NULL;
    switch (AST_ID(n)) {
    case STRING_LITERAL:
	label = emit_string_literal(SYM_NAME(EXPR_SYM(n)));
	break;
    case REF_EXPR:
	label = SYM_LABEL(EXPR_SYM(n));
	break;
    case BINARY_OPERATOR:
	label = get_ptr_label(EXPR_OPERAND(n, 0));
	break;
    case UNARY_OPERATOR:
	cc_assert(EXPR_OP(n) == '&');
	label = get_ptr_label(EXPR_OPERAND(n, 0));
	break;
    case INITS_EXPR:
	label = emit_compound_literal(n);
	break;
    default: cc_assert(0);
    }
    return label;
}

static void emit_align_label(int align, const char *label)
{
    emit(".data");
    if (align > 1)
	emit(".align %d", align);
    emit_noindent("%s:", label);
}

static void emit_address_initializer(node_t *n)
{
    node_t *sym = DECL_SYM(n);
    node_t *ty = SYM_TYPE(sym);
    node_t *init = DECL_BODY(n);
    if (isiliteral(init)) {
	emit_align_label(TYPE_ALIGN(ty), SYM_LABEL(sym));
	emit(".quad %llu", ILITERAL_VALUE(init));
    } else {
	const char *label = get_ptr_label(init);
	emit_align_label(TYPE_ALIGN(ty), SYM_LABEL(sym));
	if (BINARY_OPERATOR) {
	    node_t *r = EXPR_OPERAND(init, 1);
	    int op = EXPR_OP(init);
	    if (op == '+') {
		if (TYPE_OP(AST_TYPE(r)) == INT) {
		    long long i = ILITERAL_VALUE(r);
		    if (i < 0)
			emit(".quad %s%lld", label, i*TYPE_SIZE(rtype(ty)));
		    else
			emit(".quad %s+%lld", label, i*TYPE_SIZE(rtype(ty)));
		} else {
		    emit(".quad %s+%llu", label, ILITERAL_VALUE(r)*TYPE_SIZE(rtype(ty)));
		}
	    } else {
		if (TYPE_OP(AST_TYPE(r)) == INT) {
		    long long i = ILITERAL_VALUE(r);
		    if (i < 0)
			emit(".quad %s+%lld", label, -i*TYPE_SIZE(rtype(ty)));
		    else
			emit(".quad %s-%lld", label, i*TYPE_SIZE(rtype(ty)));
		} else {
		    emit(".quad %s-%llu", label, ILITERAL_VALUE(r)*TYPE_SIZE(rtype(ty)));
		}
	    }
	} else {
	    emit(".quad %s", label);
	}
    }
}

static void emit_arith_initializer(node_t *n)
{
    node_t *sym = DECL_SYM(n);
    node_t *ty = SYM_TYPE(sym);
    node_t *init = DECL_BODY(n);
    emit_align_label(TYPE_ALIGN(ty), SYM_LABEL(sym));
    switch (TYPE_KIND(ty)) {
    case _BOOL:
    case CHAR:
	emit(".byte %d", ILITERAL_VALUE(init));
	break;
    case SHORT:
	emit(".short %d", ILITERAL_VALUE(init));
	break;
    case INT:
    case UNSIGNED:
	emit(".long %d", ILITERAL_VALUE(init));
	break;
    case LONG:
    case LONG+LONG:
        emit(".quad %llu", ILITERAL_VALUE(init));
	break;
    case FLOAT:
	{
	    float f = FLITERAL_VALUE(init);
	    emit(".long %d", *(uint32_t *)&f);
	}
	break;
    case DOUBLE:
    case LONG+DOUBLE:
	{
	    double d = FLITERAL_VALUE(init);
	    emit(".quad %llu", *(uint64_t *)&d);
	}
	break;
    default:
	error("unknown type '%s'", type2s(ty));
	break;
    }
}

static void emit_struct_initializer(node_t *n)
{
    node_t *ty = SYM_TYPE(DECL_SYM(n));
    node_t **fields = TYPE_FIELDS(ty);
    node_t *init = DECL_BODY(n);
    for (int i = 0; i < LIST_LEN(EXPR_INITS(n)); i++) {
	node_t *elem = EXPR_INITS(n)[i];
	if (AST_ID(elem) == VINIT_EXPR)
	    emit_zero(AST_TYPE(fields[i]));
	else
	    emit_initializer(elem);
    }
}

static void emit_initializer(node_t *n)
{
    node_t *ty = SYM_TYPE(DECL_SYM(n));
    if (isarith(ty))
	emit_arith_initializer(n);
    else if (isptr(ty))
	emit_address_initializer(n);
    else
	emit_struct_initializer(n);
}

static void emit_data(node_t *n)
{
    node_t *sym = DECL_SYM(n);
    if (SYM_SCLASS(sym) != STATIC)
	emit(".globl %s", SYM_LABEL(sym));
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

static void emit_expr(node_t *n)
{
    switch (AST_ID(n)) {
	
    }
}

static void emit_compound(node_t *n)
{
    for (int i = 0; i < LIST_LEN(GEN_LIST(n)); i++)
	emit_expr(GEN_LIST(n)[i]);
}

static void emit_funcbody(node_t *n)
{
    node_t *compound = STMT_GEN(n);
    cc_assert(AST_ID(compound) == AST_COMPOUND);
    emit_compound(compound);
}

static void emit_funcdef(node_t *n)
{
    node_t *sym = DECL_SYM(n);
    const char *name = SYM_LABEL(sym);
    if (SYM_SCLASS(sym) != STATIC)
	emit(".globl %s", name);
    size_t sub = 0;
    for (int i = 0; i < LIST_LEN(DECL_LVARS(n)); i++) {
	node_t *lvar = DECL_LVARS(n)[i];
	node_t *sym = DECL_SYM(lvar);
	size_t offset = sub + TYPE_SIZE(SYM_TYPE(sym));
	SYM_LOFF(sym) = offset;
	sub = ROUNDUP(offset, 8);
    }
    emit_noindent("%s:", name);
    pushq("%rbp");
    movq("%rsp", "%rbp");
    if (sub)
	emit("sub $%lld, %%rbp", sub);
    emit_funcbody(DECL_BODY(n));
    emit("leave");
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
