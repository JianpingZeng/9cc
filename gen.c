#include "cc.h"
#include "sys.h"
#include <stdint.h>

static FILE *outfp;
static struct dict *compound_lits;
static struct dict *string_lits;
static struct dict *float_lits;

static void emit_initializer(node_t * t);
static void emit_stmt(node_t * n);
static void emit_expr(node_t * n);
static const char *func_ret_label;

enum {
    OP_PUSH,
    OP_POP,
    OP_ADD,
    OP_MOV,
    OP_SUB,
};
struct opnames {
    const char *names[6];
} op_names[] = {
    {.names = {"pushb", "pushw", "pushl", "pushq"}},
    {.names = {"popb", "popw", "popl", "popq"}},
    {.names = {"addb", "addw", "addl", "addq", "addss", "addsd"}},
    {.names = {"movb", "movw", "movl", "movq", "movss", "movsd"}},
    {.names = {"subb", "subw", "subl", "subq", "subss", "subsd"}},
};

static unsigned log2(unsigned i)
{
    unsigned ret = 0;
    unsigned j = i;
    if (j == 0)
        return 0;
    while ((j & 0x01) == 0) {
        j >>= 1;
        ret++;
    }
    j >>= 1;
    if (j)
        derror("'%u' is not an legal input", i);
    return ret;
}

static const char *get_op_name(int op, node_t *ty)
{
    struct opnames op_name = op_names[op];
    int index = -1;
    if (isint(ty) || isptr(ty)) {
        size_t size = TYPE_SIZE(ty);
        index = log2(size);
    } else if (isfloat(ty)) {
        switch (TYPE_KIND(ty)) {
        case FLOAT:
            index = 4;
            break;
        case DOUBLE:
        case LONG + DOUBLE:
            index = 5;
            break;
        }
    }
    cc_assert(index >= 0);
    return op_name.names[index];
}

void emit(const char *fmt, ...)
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

static const char *glabel(const char *label)
{
    if (opts.fleading_underscore)
        return format("_%s", label);
    else
        return label;
}

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
        label = SYM_X(EXPR_SYM(n)).label;
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
    const char *label = glabel(SYM_X(sym).label);
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
        emit(".lcomm %s,%llu,%d", glabel(SYM_X(sym).label), TYPE_SIZE(ty),
             TYPE_ALIGN(ty));
    else
        emit(".comm  %s,%llu,%d", glabel(SYM_X(sym).label), TYPE_SIZE(ty),
             TYPE_ALIGN(ty));
}

static void emit_op2(int op, node_t *ty, struct operand *src, struct operand *dst)
{
    const char *op_name = get_op_name(op, ty);
    size_t size = TYPE_SIZE(ty);
    const char *src_name = get_operand_name(src, size);
    const char *dst_name = get_operand_name(dst, size);
    emit("%s %s, %s", op_name, src_name, dst_name);
}

static void emit_bop_reg(int op, struct reg * (*use_reg) (void), node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);
    node_t *r = EXPR_OPERAND(n, 1);

    emit_expr(l);
    emit_expr(r);

    struct operand *laddr = EXPR_X(l).addr;
    struct operand *raddr = EXPR_X(r).addr;

    if (laddr->kind == OPERAND_REGISTER) {
        emit_op2(op, AST_TYPE(n), raddr, laddr);
        free_operand(EXPR_X(r).addr);
        EXPR_X(n).addr = laddr;
    } else {
        struct reg *reg = use_reg();
        struct operand *addr = make_register_operand(reg);
        emit_op2(OP_MOV, AST_TYPE(l), laddr, addr);
        emit_op2(op, AST_TYPE(n), raddr, addr);
        free_operand(EXPR_X(r).addr);
        free_operand(EXPR_X(l).addr);
        EXPR_X(n).addr = addr;
    }
}

static void emit_bop_plus_minus(node_t *n, int op)
{
    node_t *l = EXPR_OPERAND(n, 0);
    node_t *r = EXPR_OPERAND(n, 1);
    int op_kind = op == '+' ? OP_ADD : OP_SUB;
    
    if (isint(AST_TYPE(l)) && isint(AST_TYPE(r))) {
        // int + int
        emit_bop_reg(op_kind, use_int_reg, n);
    } else if (isfloat(AST_TYPE(l)) && isfloat(AST_TYPE(r))) {
        // float + float
        emit_bop_reg(op_kind, use_float_reg, n);
    } else {
        // pointer + int
    }
}

static void emit_bop(node_t *n)
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
        break;
    case '+':
    case '-':
        emit_bop_plus_minus(n, op);
        break;
    case AND:
    case OR:
        break;
    default:
        cc_assert(0);
    }
}

static void emit_uop(node_t *n)
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
    default:
        cc_assert(0);
    }
}

static void emit_compound_literal(node_t *n)
{
    
}

static void emit_string_literal(node_t *n)
{
    node_t *sym = EXPR_SYM(n);
    const char *name = SYM_NAME(sym);
    const char *label = emit_string_literal_label(name);
    EXPR_X(n).addr = make_memory_operand(format("$%s", label));
}

static void emit_float_literal(node_t *n)
{
    node_t *sym = EXPR_SYM(n);
    node_t *ty = SYM_TYPE(sym);
    union value v = SYM_VALUE(sym);
    switch (TYPE_KIND(ty)) {
    case FLOAT:
    {
        float f = VALUE_D(v);
        const char *name = format("$%u", *(uint32_t *) &f);
        EXPR_X(n).addr = make_literal_operand(name);
    }
    break;
    case DOUBLE:
    case LONG + DOUBLE:
    {
        double d = VALUE_D(v);
        const char *name = format("$%llu", *(uint64_t *) &d);
        EXPR_X(n).addr = make_literal_operand(name);
    }
    break;
    default:
        cc_assert(0);
    }
}

static void emit_integer_literal(node_t *n)
{
    const char *name = format("$%llu", ILITERAL_VALUE(n));
    EXPR_X(n).addr = make_literal_operand(name);
}

static void emit_ref_expr(node_t *n)
{
    node_t *sym = EXPR_SYM(n);
    node_t *ty = SYM_TYPE(sym);
    if (isfunc(ty) || has_static_extent(sym)) {
        const char *name = glabel(SYM_X(sym).label);
        EXPR_X(n).addr = make_memory_operand(name);
    } else {
        const char *name = format("-%ld(%%rbp)", SYM_X(sym).loff);
        EXPR_X(n).addr = make_memory_operand(name);
    }
}

static const char *stack_arg(size_t off)
{
    if (off == 0)
        return "(%rsp)";
    else
        return format("%lld(%%rsp)", off);
}

static void set_funcall_context(void)
{
    
}

static void restore_funcall_context(void)
{
    
}

static void emit_funcall(node_t *n)
{
    node_t *node = EXPR_OPERAND(n, 0);
    node_t **args = EXPR_ARGS(n);

    emit_expr(node);

    set_funcall_context();

    size_t off = 0;
    for (int i = 0; i < LIST_LEN(args); i++) {
        node_t *arg = args[i];
        node_t *ty = AST_TYPE(arg);
        size_t typesize = ROUNDUP(TYPE_SIZE(ty), 8);

        emit_expr(arg);
        
        if (isint(ty) || isptr(ty)) {
            struct reg *reg = get_iarg_reg();
            if (reg) {
                EXPR_X(arg).arg = make_register_operand(reg);
            } else {
                EXPR_X(arg).arg = make_memory_operand(stack_arg(off));
                off += typesize;
            }
        } else if (isfloat(ty)) {
            struct reg *reg = get_farg_reg();
            if (reg) {
                EXPR_X(arg).arg = make_register_operand(reg);
            } else {
                EXPR_X(arg).arg = make_memory_operand(stack_arg(off));
                off += typesize;
            }
        } else if (isstruct(ty) || isunion(ty)) {
            EXPR_X(arg).arg = make_memory_operand(stack_arg(off));
            off += typesize;
        } else {
            die("unknown argument type: %s", type2s(ty));
        }
    }

    for (int i = LIST_LEN(args) - 1; i >= 0; i--) {
        node_t *arg = args[i];
        emit_op2(OP_MOV, AST_TYPE(arg), EXPR_X(arg).addr, EXPR_X(arg).arg);
        free_operand(EXPR_X(arg).arg);
    }
    
    emit("callq %s", EXPR_X(node).addr);
    restore_funcall_context();
}

static void narrow_int(node_t *dty, node_t *l)
{
    
}

static void widden_int(node_t *dty, node_t *l)
{
    static const char *movs[][4] = {
        {NULL, "movsbw", "movsbl", "movsbq"},
        {NULL, NULL, "movswl", "movwq"},
        {NULL, NULL, NULL, "movl"}
    };
    node_t *sty = AST_TYPE(l);
    size_t dsize = TYPE_SIZE(dty);
    size_t ssize = TYPE_SIZE(sty);

    int index1 = log2(ssize);
    int index2 = log2(dsize);

    cc_assert(index1 >= 0 && index1 < 4);
    cc_assert(index2 >= 0 && index2 < 4);

    const char *mov = movs[index1][index2];
    
    cc_assert(mov);

    const char *src = get_operand_name(EXPR_X(l).addr, ssize);
    const char *dst;

    if (EXPR_X(l).addr->kind != OPERAND_REGISTER) {
        struct operand *addr = make_register_operand(use_int_reg());
        EXPR_X(l).addr = addr;
    }
    dst = get_operand_name(EXPR_X(l).addr, dsize);

    emit("%s %s, %s", mov, src, dst);
}

static void int2int(node_t *dty, node_t *l)
{
    node_t *sty = AST_TYPE(l);
    size_t dsize = TYPE_SIZE(dty);
    size_t ssize = TYPE_SIZE(sty);
    if (dsize < ssize) {
        // narrow
        narrow_int(dty, l);
    } else if (dsize > ssize) {
        // widden
        widden_int(dty, l);
    } else {
        // equal size
        if (TYPE_OP(sty) == INT && TYPE_OP(dty) == UNSIGNED) {
            // signed => unsigned
        } else if (TYPE_OP(sty) == UNSIGNED && TYPE_OP(dty) == INT) {
            // unsigned => signed
        } else {
            dlog("'%s' => '%s' ignored", type2s(sty), type2s(dty));
        }
    }
}

static void int2float(node_t *dty, node_t *l)
{
    
}

static void float2int(node_t *dty, node_t *l)
{
    
}

static void float2float(node_t *dty, node_t *l)
{
    node_t *sty = AST_TYPE(l);
    switch (TYPE_KIND(sty)) {
    case FLOAT:
        if (TYPE_KIND(dty) == DOUBLE) {
            // float => double
        } else if (TYPE_KIND(dty) == LONG + DOUBLE) {
            // float => long + double
        }
        break;
    case DOUBLE:
        if (TYPE_KIND(dty) == FLOAT) {
            // double => float
            struct reg *reg = use_float_reg();
            emit("movsd %s, %s", EXPR_X(l).addr, reg->r64);
            emit("cvtpd2ps %s, %s", reg->r64, reg->r64);
        } else if (TYPE_KIND(dty) == LONG + DOUBLE) {
            // double => long double
            // the same now
        }
        break;
    case LONG + DOUBLE:
        if (TYPE_KIND(dty) == FLOAT) {
            // long double => float
        } else if (TYPE_KIND(dty) == DOUBLE) {
            // long double => double
            // the same now
        }
        break;
    default:
        cc_assert(0);
    }
}

static void arith2arith(node_t *dty, node_t *l)
{
    node_t *sty = AST_TYPE(l);
    if (isint(dty) && isint(sty))
        // int => int
        int2int(dty, l);
    else if (isfloat(dty) && isint(sty))
        // int => float
        int2float(dty, l);
    else if (isint(dty) && isfloat(sty))
        // float => int
        float2int(dty, l);
    else if (isfloat(dty) && isfloat(sty))
        // float => float
        float2float(dty, l);
    else
        die("'%s' => '%s'", type2s(sty), type2s(dty));
}

static void ptr2arith(node_t *dty, node_t *l)
{
    
}

static void ptr2ptr(node_t *dty, node_t *l)
{
    
}

static void arith2ptr(node_t *dty, node_t *l)
{
    
}

static void func2ptr(node_t *dty, node_t *l)
{
    
}

static void array2ptr(node_t *dty, node_t *l)
{
    
}

static void emit_conv(node_t *n)
{
    node_t *dty = AST_TYPE(n);
    node_t *l = EXPR_OPERAND(n, 0);
    node_t *sty = AST_TYPE(l);
    
    emit_expr(l);
    
    if (isarith(dty)) {
        if (isarith(sty))
            arith2arith(dty, l);
        else if (isptr(sty))
            ptr2arith(dty, l);
        else
            cc_assert(0);
    } else if (isptr(dty)) {
        if (isptr(sty))
            ptr2ptr(dty, l);
        else if (isarith(sty))
            arith2ptr(dty, l);
        else if (isfunc(sty))
            func2ptr(dty, l);
        else if (isarray(sty))
            array2ptr(dty, l);
        else
            cc_assert(0);
    } else {
        
    }
    EXPR_X(n).addr = EXPR_X(l).addr;
}

static void emit_cond(node_t *n)
{
    
}

static void emit_member(node_t *n)
{
    
}

static void emit_subscript(node_t *n)
{
    
}

static void emit_expr(node_t * n)
{
    switch (AST_ID(n)) {
    case BINARY_OPERATOR:
        emit_bop(n);
        break;
    case UNARY_OPERATOR:
        emit_uop(n);
        break;
    case PAREN_EXPR:
        emit_expr(EXPR_OPERAND(n, 0));
        break;
    case COND_EXPR:
        emit_cond(n);
        break;
    case MEMBER_EXPR:
        emit_member(n);
        break;
    case SUBSCRIPT_EXPR:
        emit_subscript(n);
        break;
    case CAST_EXPR:
    case CONV_EXPR:
        emit_conv(n);
        break;
    case CALL_EXPR:
        emit_funcall(n);
        break;
    case REF_EXPR:
        if (EXPR_OP(n) == ENUM)
            emit_integer_literal(n);
        else
            emit_ref_expr(n);
        break;
    case INTEGER_LITERAL:
        emit_integer_literal(n);
        break;
    case FLOAT_LITERAL:
        emit_float_literal(n);
        break;
    case STRING_LITERAL:
        emit_string_literal(n);
        break;
    case COMPOUND_LITERAL:
        emit_compound_literal(n);
        break;
    case INITS_EXPR:
    case VINIT_EXPR:
    default:
        die("unknown node (%s)'%s'", nname(n), node2s(n));
        break;
    }
}

static void emit_decl_init(node_t * init, size_t offset)
{

}

static void emit_decl(node_t * n)
{
    if (DECL_BODY(n))
        emit_decl_init(DECL_BODY(n), SYM_X(DECL_SYM(n)).loff);
}

static void emit_compound(node_t * n)
{
    
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

static void emit_if(node_t * n)
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

static void emit_stmt(node_t * n)
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
            emit_expr(GEN_OPERAND(n));
            emit_jmp(func_ret_label);
            break;
        case AST_LABEL:
            emit_label(GEN_LABEL(n));
            break;
        case AST_JUMP:
            emit_jmp(GEN_LABEL(n));
            break;
        default:
            // null statement
            cc_assert(AST_ID(n) == NULL_STMT);
            break;
        }
    }
}

static size_t calc_stack_size(node_t *n)
{
    size_t sub = 0;
    for (int i = 0; i < LIST_LEN(DECL_X(n).lvars); i++) {
        node_t *lvar = DECL_X(n).lvars[i];
        node_t *sym = DECL_SYM(lvar);
        size_t offset = sub + TYPE_SIZE(SYM_TYPE(sym));
        SYM_X(sym).loff = offset;
        sub = ROUNDUP(offset, 8);
    }
    sub += DECL_X(n).extra_stack_size;
    return sub;
}

static void emit_funcdef(node_t * n)
{
    node_t *sym = DECL_SYM(n);
    const char *label = glabel(SYM_X(sym).label);
    if (SYM_SCLASS(sym) != STATIC)
        emit(".globl %s", label);
    size_t sub = calc_stack_size(n);
    func_ret_label = gen_label();
    emit_noindent("%s:", label);
    emit("pushq %%rbp");
    emit("movq %%rsp, %%rbp");
    if (sub)
        emit("subq $%lld, %%rbp", sub);
    emit_stmt(DECL_BODY(n));
    emit_label(func_ret_label);
    if (sub)
        emit("leave");
    else
        emit("popq %%rbp");
    emit("ret");
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
    float_lits = dict_new();
    init_regs();
}

void gen(node_t * tree, FILE * fp)
{
    cc_assert(errors == 0 && fp);
    gen_init(fp);
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
    emit_compound_literals();
    emit_string_literals();
    emit(".ident \"mcc: %d.%d\"", MAJOR(version), MINOR(version));
}
