#include "cc.h"
#include "sys.h"
#include <stdint.h>

static FILE *outfp;
static struct dict *compound_lits;

#define LEAD  "    "
#define emit(...)             emitf(LEAD,  __VA_ARGS__)
#define emit_noindent(...)    emitf(NULL, __VA_ARGS__)
#define pushq(reg)      emit("pushq %s", reg)
#define popq(reg)       emit("popq %s", reg)
#define movq(src, dst)  emit("movq %s, %s", src, dst)
#define STR_PREFIX    ".LC"
#define IS_STRLIT(sym)  (!strncmp(STR_PREFIX, SYM_LABEL(sym), strlen(STR_PREFIX)))

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

static const char *gen_str_label(void)
{
    static size_t i;
    return format("%s%llu", STR_PREFIX, i++);
}

static const char * emit_string_literal(const char *name)
{
    node_t *sym = lookup(name, constants);
    cc_assert(sym);
    if (!IS_STRLIT(sym))
	SYM_LABEL(sym) = gen_str_label();
    return SYM_LABEL(sym);
}

static const char *gen_compound_label(void)
{
    static size_t i;
    return format("__compound_literal.%llu", i++);
}

static const char *emit_compound_literal(node_t *n)
{
    const char *label = gen_compound_label();
    dict_put(compound_lits, label, n);
    return label;
}

static void emit_zero(size_t bytes)
{
    emit(".zero %llu", bytes);
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
    default:
	die("unkown ptr node: %s", node2s(n));
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

static void emit_address_initializer(node_t *init)
{
    node_t *ty = AST_TYPE(init);
    if (isiliteral(init)) {
	emit(".quad %llu", ILITERAL_VALUE(init));
    } else {
	const char *label = get_ptr_label(init);
	if (AST_ID(init) == BINARY_OPERATOR) {
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

static void emit_arith_initializer(node_t *init)
{
    node_t *ty = AST_TYPE(init);
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
	die("unknown type '%s'", type2s(ty));
    }
}

static void emit_array_initializer(node_t *n)
{
    if (issliteral(n)) {
	const char *label = get_ptr_label(n);
	emit(".quad %s", label);
    } else {
	cc_assert(AST_ID(n) == INITS_EXPR);
	
    }
}

static void emit_struct_initializer(node_t *n)
{
    cc_assert(AST_ID(n) == INITS_EXPR);
    node_t *ty = AST_TYPE(n);
    node_t **fields = TYPE_FIELDS(ty);
    node_t **inits = EXPR_INITS(n);
    for (int i = 0; i < LIST_LEN(inits); i++) {
	node_t *init = inits[i];
	node_t *field = fields[i];
	node_t *next = i < LIST_LEN(inits) - 1 ? fields[i+1] : NULL;
	size_t offset = FIELD_OFFSET(field);
	if (FIELD_ISBIT(field)) {
	    int old_bits = 0;
	    unsigned long long old_byte = 0;
	    do {
		int bits = FIELD_BITSIZE(field);
		unsigned long long byte = 0;
		if (isiliteral(init))
		    byte = ILITERAL_VALUE(init);
		for (;;) {
		    if (bits + old_bits >= 8) {
			unsigned char val;
			unsigned char l = byte & ~(~0 << (8 - old_bits));
			unsigned char r = old_byte & ~(~0 << old_bits);
			val = (l << old_bits) | r;
			old_bits = 0;
			old_byte = 0;
			bits -= 8 - old_bits;
			byte >>= 8 - old_bits;
			emit(".byte %d", val);
		    } else {
			old_bits += bits;
			old_byte += byte;
			break;
		    }
		}
	    } while (FIELD_OFFSET(field) == FIELD_OFFSET(next));
	} else {
	    node_t *fty = FIELD_TYPE(field);
	    if (TYPE_SIZE(fty)) {
		if (AST_ID(init) == VINIT_EXPR)
		    emit_zero(TYPE_SIZE(fty));
		else
		    emit_initializer(init);
		offset += TYPE_SIZE(fty);
	    }
	}
	// pack
	size_t end;
	if (next)
	    end = FIELD_OFFSET(next);
	else
	    end = TYPE_SIZE(ty);
	if (end - offset)
	    emit_zero(end - offset);
    }
}

static void emit_initializer(node_t *init)
{
    node_t *ty = AST_TYPE(init);
    if (isarith(ty))
	emit_arith_initializer(init);
    else if (isptr(ty))
	emit_address_initializer(init);
    else if (isarray(ty))
	emit_array_initializer(init);
    else
	emit_struct_initializer(init);
}

static void emit_data(node_t *n)
{
    node_t *sym = DECL_SYM(n);
    node_t *ty = SYM_TYPE(sym);
    if (SYM_SCLASS(sym) != STATIC)
	emit(".globl %s", SYM_LABEL(sym));
    emit_align_label(TYPE_ALIGN(ty), SYM_LABEL(sym));
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

static void emit_literals(void)
{
    bool section = false;
    // compounds
    for (int i = 0; i < vec_len(compound_lits->keys); i++) {
	const char *label = vec_at(compound_lits->keys, i);
	node_t *init = dict_get(compound_lits, label);
	if (!section) {
	    emit(".data");
	    section = true;
	}
	emit_noindent("%s:", label);
	emit_initializer(init);
    }

    // strings
    section = false;
    for (int i = 0; i < vec_len(constants->dict->keys); i++) {
	node_t *sym = dict_get(constants->dict, vec_at(constants->dict->keys, i));
	if (sym && IS_STRLIT(sym)) {
	    if (!section) {
		emit(".section .rodata");
		section = true;
	    }
	    emit_noindent("%s:", SYM_LABEL(sym));
	    emit(".asciz %s", SYM_NAME(sym));
	}
    }
}

void gen(node_t *tree, FILE *fp)
{
    cc_assert(errors == 0 && fp);
    outfp = fp;
    compound_lits = dict_new();
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
    emit_literals();
    emit(".ident \"mcc: %s\"", IR->uname);
}
