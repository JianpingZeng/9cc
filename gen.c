#include "cc.h"
#include "sys.h"
#include <stdint.h>

/**
 *  X86_64 registers
 *
 *  64-bit  32-bit  16-bit  8-bit
 *  rax     eax     ax      al,ah
 *  rbx     ebx     bx      bl,bh
 *  rcx     ecx     cx      cl,ch
 *  rdx     edx     dx      dl,dh
 *  rbp     ebp     bp
 *  rsp     esp     sp
 *  rsi     esi     si
 *  rdi     edi     di
 *  rip     eip     ip
 *  r8~r15
 *
 *  Segment registers
 *
 *  cs,ds,es,fs,gs,ss
 */

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
static void emit_stmt(node_t *n);
static const char *func_ret_label;

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

static const char *glabel(const char *label)
{
    if (opts.fleading_underscore)
	return format("_%s", label);
    else
	return label;
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
	int i;
	for (i = 0; i < LIST_LEN(EXPR_INITS(n)); i++)
	    emit_initializer(EXPR_INITS(n)[i]);
	if (TYPE_LEN(AST_TYPE(n)) - i > 0)
	    emit_zero((TYPE_LEN(AST_TYPE(n)) - i) * TYPE_SIZE(rtype(AST_TYPE(n))));
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
	size_t offset = FIELD_OFFSET(field);
	if (FIELD_ISBIT(field)) {
	    int old_bits = 0;
	    unsigned long long old_byte = 0;
	    for (; i < LIST_LEN(inits); i++) {
		node_t *next = i < LIST_LEN(inits) - 1 ? fields[i+1] : NULL;
		field = fields[i];
		init = inits[i];
		if (next && FIELD_OFFSET(field) != FIELD_OFFSET(next))
		    break;
		int bits = FIELD_BITSIZE(field);
		unsigned long long byte = 0;
		if (isiliteral(init))
		    byte = ILITERAL_VALUE(init);
		while (bits + old_bits >= 8) {
		    unsigned char val;
		    unsigned char l = byte & ~(~0 << (8 - old_bits));
		    unsigned char r = old_byte & ~(~0 << old_bits);
		    val = (l << old_bits) | r;
		    old_bits = 0;
		    old_byte = 0;
		    bits -= 8 - old_bits;
		    byte >>= 8 - old_bits;
		    emit(".byte %d", val);
		    offset += 1;
		}
		old_bits += bits;
		old_byte += byte;
	    }
	    if (old_bits) {
		unsigned char r = old_byte & ~(~0 << old_bits);
		emit(".byte %d", r);
		offset += 1;
	    }
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
	node_t *next = i < LIST_LEN(inits) - 1 ? fields[i+1] : NULL;
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
    const char *label = glabel(SYM_LABEL(sym));
    if (SYM_SCLASS(sym) != STATIC)
	emit(".globl %s", label);
    emit(".data");
    if (TYPE_ALIGN(ty) > 1)
	emit(".align %d", TYPE_ALIGN(ty));
    emit_noindent("%s:", label);
    emit_initializer(DECL_BODY(n));
}

static void emit_bss(node_t *n)
{
    node_t *sym = DECL_SYM(n);
    node_t *ty = SYM_TYPE(sym);
    if (SYM_SCLASS(sym) == STATIC)
	emit(".lcomm %s,%llu,%d", glabel(SYM_LABEL(sym)), TYPE_SIZE(ty), TYPE_ALIGN(ty));
    else
	emit(".comm  %s,%llu,%d", glabel(SYM_LABEL(sym)), TYPE_SIZE(ty), TYPE_ALIGN(ty));
}

static void emit_expr(node_t *n)
{
    switch (AST_ID(n)) {
    case BINARY_OPERATOR:
	{
	    int op = EXPR_OP(n);
	    switch (op) {
	    case '=':
	    case ',':
		// int
	    case '%':
	    case '|':
	    case '&':
	    case '^':
	    case LSHIFT:
	    case RSHIFT:
		// arith
	    case '*':
	    case '/':
		// scalar
	    case '<':
	    case '>':
	    case GEQ:
	    case LEQ:
	    case EQ:
	    case NEQ:
	    case '+':
	    case '-':
	    case AND:
	    case OR:
		break;
	    default: cc_assert(0);
	    }
	}
	break;
    case UNARY_OPERATOR:
	{
	    int op = EXPR_OP(n);
	    switch (op) {
	    case INCR:
	    case DECR:
	    case '*':
	    case '&':
	    case '+':
	    case '-':
	    case '~':
	    case '!':
	    case SIZEOF:
		break;
	    default: cc_assert(0);
	    }
	}
	break;
    case PAREN_EXPR:
	emit_expr(EXPR_OPERAND(n, 0));
	break;
    case COND_EXPR:
    case MEMBER_EXPR:
    case REF_EXPR:
    case CAST_EXPR:
    case CONV_EXPR:
    case CALL_EXPR:
    case SUBSCRIPT_EXPR:
    case INTEGER_LITERAL:
    case FLOAT_LITERAL:
    case STRING_LITERAL:
    case COMPOUND_LITERAL:
	break;
    case INITS_EXPR:
    case VINIT_EXPR:
	break;
    default:
	die("unknown node '%s'", nname(n));
	break;
    }
}

static void emit_decl_init(node_t *init, size_t offset)
{

}

static void emit_decl(node_t *n)
{
    if (DECL_BODY(n))
	emit_decl_init(DECL_BODY(n), SYM_LOFF(DECL_SYM(n)));
}

static void emit_compound(node_t *n)
{
    cc_assert(AST_ID(n) == AST_COMPOUND);
    for (int i = 0; i < LIST_LEN(STMT_LIST(n)); i++)
	emit_stmt(STMT_LIST(n)[i]);
}

static void emit_label(const char *label)
{
    emit_noindent("%s:", label);
}

static void emit_jmp(const char *label)
{
    emit("jmp %s", label);
}

static void emit_je(const char *label)
{
    emit("je %s", label);
}

static void emit_if(node_t *n)
{
    emit_expr(STMT_COND(n));
    const char *ne = gen_label();
    emit_je(ne);
    if (STMT_THEN(n))
	emit_stmt(STMT_THEN(n));
    if (STMT_ELSE(n)) {
	const char *end = gen_label();
	emit_jmp(end);
	emit_label(ne);
	emit_stmt(STMT_ELSE(n));
	emit_label(end);
    } else {
	emit_label(ne);
    }
}

static void emit_stmt(node_t *n)
{
    if (isexpr(n)) {
	emit_expr(n);
    } else if (isdecl(n)) {
	emit_decl(n);
    } else {
	switch (AST_ID(n)) {
	case AST_IF:
	    emit_if(n);
	    break;
	case AST_COMPOUND:
	    emit_compound(n);
	    break;
	case AST_RETURN:
	    emit_stmt(STMT_OPERAND(n));
	    emit_jmp(func_ret_label);
	    break;
	case AST_LABEL:
	    emit_label(STMT_LABEL(n));
	    break;
	case AST_JUMP:
	    emit_jmp(STMT_LABEL(n));
	    break;
	default:
	    // null statement
	    cc_assert(AST_ID(n) == NULL_STMT);
	    break;
	}
    }
}

static void emit_funcdef(node_t *n)
{
    node_t *sym = DECL_SYM(n);
    const char *label = glabel(SYM_LABEL(sym));
    if (SYM_SCLASS(sym) != STATIC)
	emit(".globl %s", label);
    size_t sub = 0;
    for (int i = 0; i < LIST_LEN(DECL_LVARS(n)); i++) {
	node_t *lvar = DECL_LVARS(n)[i];
	node_t *sym = DECL_SYM(lvar);
	size_t offset = sub + TYPE_SIZE(SYM_TYPE(sym));
	SYM_LOFF(sym) = offset;
	sub = ROUNDUP(offset, 8);
    }
    func_ret_label = gen_label();
    emit_noindent("%s:", label);
    pushq("%rbp");
    movq("%rsp", "%rbp");
    if (sub)
	emit("sub $%lld, %%rbp", sub);
    emit_stmt(DECL_BODY(n));
    emit_label(func_ret_label);
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
    emit(".ident \"mcc: %d.%d\"", MAJOR(version), MINOR(version));
}
