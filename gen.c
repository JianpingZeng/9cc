#include "cc.h"
#include "sys.h"

static FILE *outfp;
static struct dict *compound_lits;
static struct dict *string_lits;
static void emit_initializer(node_t * init);
static void gen_decl(node_t *n);

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

static void emit_funcdef(node_t * n)
{
    node_t *sym = DECL_SYM(n);
    const char *label = SYM_X_LABEL(sym);
    if (SYM_SCLASS(sym) != STATIC)
        emit(".globl %s", label);
    emit_noindent("%s:", label);
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

// gen-decl

static const char *emit_string_literal_label(const char *name)
{
    const char *label = dict_get(string_lits, name);
    if (!label) {
        label = gen_sliteral_label();
        dict_put(string_lits, name, (void *)label);
    }
    return label;
}

static const char *emit_compound_literal_label(node_t * n)
{
    const char *label = gen_compound_label();
    dict_put(compound_lits, label, n);
    return label;
}

static void emit_zero(size_t bytes)
{
    emit(".zero %llu", bytes);
}

static const char *get_ptr_label(node_t * n)
{
    const char *label = NULL;
    switch (AST_ID(n)) {
    case STRING_LITERAL:
        label = emit_string_literal_label(SYM_NAME(EXPR_SYM(n)));
        break;
    case REF_EXPR:
        label = SYM_X_LABEL(EXPR_SYM(n));
        break;
    case BINARY_OPERATOR:
        label = get_ptr_label(EXPR_OPERAND(n, 0));
        break;
    case UNARY_OPERATOR:
        cc_assert(EXPR_OP(n) == '&');
        label = get_ptr_label(EXPR_OPERAND(n, 0));
        break;
    case INITS_EXPR:
        label = emit_compound_literal_label(n);
        break;
    default:
        die("unkown ptr node: %s", node2s(n));
    }
    return label;
}

static void emit_address_initializer(node_t * init)
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
                        emit(".quad %s%lld", label,
                             i * TYPE_SIZE(rtype(ty)));
                    else
                        emit(".quad %s+%lld", label,
                             i * TYPE_SIZE(rtype(ty)));
                } else {
                    emit(".quad %s+%llu", label,
                         ILITERAL_VALUE(r) *
                         TYPE_SIZE(rtype(ty)));
                }
            } else {
                if (TYPE_OP(AST_TYPE(r)) == INT) {
                    long long i = ILITERAL_VALUE(r);
                    if (i < 0)
                        emit(".quad %s+%lld", label,
                             -i * TYPE_SIZE(rtype(ty)));
                    else
                        emit(".quad %s-%lld", label,
                             i * TYPE_SIZE(rtype(ty)));
                } else {
                    emit(".quad %s-%llu", label,
                         ILITERAL_VALUE(r) *
                         TYPE_SIZE(rtype(ty)));
                }
            }
        } else {
            emit(".quad %s", label);
        }
    }
}

static void emit_arith_initializer(node_t * init)
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
    case LONG + LONG:
        emit(".quad %llu", ILITERAL_VALUE(init));
        break;
    case FLOAT:
        {
            float f = FLITERAL_VALUE(init);
            emit(".long %u", *(uint32_t *) & f);
        }
        break;
    case DOUBLE:
    case LONG + DOUBLE:
        {
            double d = FLITERAL_VALUE(init);
            emit(".quad %llu", *(uint64_t *) & d);
        }
        break;
    default:
        die("unknown type '%s'", type2s(ty));
    }
}

static void emit_array_initializer(node_t * n)
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
            emit_zero((TYPE_LEN(AST_TYPE(n)) -
                       i) * TYPE_SIZE(rtype(AST_TYPE(n))));
    }
}

static void emit_struct_initializer(node_t * n)
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
                node_t *next =
                    i <
                    LIST_LEN(inits) - 1 ? fields[i + 1] : NULL;
                field = fields[i];
                init = inits[i];
                if (next
                    && FIELD_OFFSET(field) !=
                    FIELD_OFFSET(next))
                    break;
                int bits = FIELD_BITSIZE(field);
                unsigned long long byte = 0;
                if (isiliteral(init))
                    byte = ILITERAL_VALUE(init);
                while (bits + old_bits >= 8) {
                    unsigned char val;
                    unsigned char l =
                        byte & ~(~0 << (8 - old_bits));
                    unsigned char r =
                        old_byte & ~(~0 << old_bits);
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
        node_t *next = i < LIST_LEN(inits) - 1 ? fields[i + 1] : NULL;
        size_t end;
        if (next)
            end = FIELD_OFFSET(next);
        else
            end = TYPE_SIZE(ty);
        if (end - offset)
            emit_zero(end - offset);
    }
}

static void emit_initializer(node_t * init)
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

static void emit_data(node_t * n)
{
    node_t *sym = DECL_SYM(n);
    node_t *ty = SYM_TYPE(sym);
    const char *label = SYM_X_LABEL(sym);
    if (SYM_SCLASS(sym) != STATIC)
        emit(".globl %s", label);
    emit(".data");
    if (TYPE_ALIGN(ty) > 1)
        emit(".align %d", TYPE_ALIGN(ty));
    emit_noindent("%s:", label);
    emit_initializer(DECL_BODY(n));
}

static void emit_bss(node_t * n)
{
    node_t *sym = DECL_SYM(n);
    node_t *ty = SYM_TYPE(sym);
    if (SYM_SCLASS(sym) == STATIC)
        emit(".lcomm %s,%llu,%d",
             SYM_X_LABEL(sym),
             TYPE_SIZE(ty),
             TYPE_ALIGN(ty));
    else
        emit(".comm  %s,%llu,%d",
             SYM_X_LABEL(sym),
             TYPE_SIZE(ty),
             TYPE_ALIGN(ty));
}

static void gen_decl(node_t *n)
{
    cc_assert(isdecl(n));
    
    if (DECL_BODY(n))
        emit_data(n);
    else
        emit_bss(n);
}
