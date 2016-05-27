#include "cc.h"

/**
 *  Intermediate Representation
 *
 *  three address ir
 */

static const char *rops[] = {
#define _rop(a, b)  b,
#include "rop.def"
};

static void emit_stmt(node_t *n);
static void emit_expr(node_t *n);
static void emit_bool_expr(node_t *n);
static void emit_bop_bool(node_t *n);
static void emit_bss(node_t *decl);
static void emit_data(node_t *decl);
static void emit_funcdef_section(node_t *decl);
static const char *get_string_literal_label(const char *name);
static void emit_assign(node_t *ty, struct operand *l, node_t *r, long offset, node_t *bfield, bool sty);
static void emit_member_nonbitfield(node_t *n, node_t *field);
static struct vector * filter_global(struct vector *v);
static void emit_bitfield_basic(node_t *ty, struct operand *l, struct operand *r,
                                long offset, node_t *bfield, bool sty);
static struct operand * emit_conv_tac(int op, struct operand *l,
                                      int from_opsize,
                                      int to_opsize);

static struct tac *func_tac_head;
static struct tac *func_tac_tail;
static struct vector *extra_lvars;
static const char *func_end_label;
static struct table *tmps;
static struct table *labels;
static struct externals *exts;
static const char *fall = (const char *)&fall;
static const char *__continue;
static const char *__break;

#define SET_LOOP_CONTEXT(con, brk)              \
    const char *saved_continue = __continue;    \
    const char *saved_break = __break;          \
    __continue = con;                           \
    __break = brk

#define RESTORE_LOOP_CONTEXT()                  \
    __continue = saved_continue;                \
    __break = saved_break

#define SET_SWITCH_CONTEXT(brk)                 \
    const char *saved_break = __break;          \
    __break = brk

#define RESTORE_SWITCH_CONTEXT()                \
    __break = saved_break

#define BREAK_CONTEXT     (__break)
#define CONTINUE_CONTEXT  (__continue)

static int ops[] = {
    Zero, Byte, Word, -1, Long, -1, -1, -1, Quad
};

const char *rop2s(int op)
{
    assert(op >= IR_NONE && op < IR_END);
    return rops[op];
}

static void emit_tac(struct tac *tac)
{
    if (!func_tac_head)
        func_tac_head = tac;
    if (!func_tac_tail) {
        func_tac_tail = tac;
    } else {
        tac->prev = func_tac_tail;
        func_tac_tail->next = tac;
        func_tac_tail = tac;
    }
}

static bool isgref(node_t *sym)
{
    return has_static_extent(sym) ||
        SYM_SCOPE(sym) == CONSTANT ||
        isfunc(SYM_TYPE(sym));
}

static struct operand * copy_operand(struct operand *operand)
{
    struct operand *ret = (struct operand *)alloc_operand();
    *ret = *operand;
    return ret;
}

static struct operand * make_sym_operand(node_t *sym)
{
    struct operand *operand = (struct operand *)alloc_operand();
    operand->sym = sym;
    return operand;
}

static node_t * make_named_sym(const char *name, struct table **table, int scope)
{
    node_t *sym = lookup(name, *table);
    if (!sym)
        sym = install(name, table, scope);
    return sym;
}

node_t * make_label_sym(const char *name)
{
    node_t *sym = make_named_sym(name, &labels, GLOBAL);
    SYM_X_KIND(sym) = SYM_KIND_LABEL;
    return sym;
}

static node_t * make_tmp_sym(void)
{
    node_t *sym = make_named_sym(gen_tmpname_r(), &tmps, GLOBAL);
    SYM_X_KIND(sym) = SYM_KIND_TMP;
    return sym;
}

static struct operand * make_named_operand(const char *name, struct table **table, int scope)
{
    node_t *sym = make_named_sym(name, table, scope);
    return make_sym_operand(sym);
}

static struct operand * make_tmp_operand(void)
{
    node_t *sym = make_tmp_sym();
    return make_sym_operand(sym);
}

static struct operand * make_label_operand(const char *label)
{
    node_t *sym = make_label_sym(label);
    return make_sym_operand(sym);
}

static struct operand * make_int_operand(long long i)
{
    struct operand *operand = make_named_operand(strd(i), &constants, CONSTANT);
    SYM_VALUE_I(operand->sym) = i;
    SYM_X_KIND(operand->sym) = SYM_KIND_IMM;
    return operand;
}

static struct operand * make_unsigned_operand(unsigned long long u)
{
    struct operand *operand = make_named_operand(stru(u), &constants, CONSTANT);
    SYM_VALUE_U(operand->sym) = u;
    SYM_X_KIND(operand->sym) = SYM_KIND_IMM;
    return operand;
}

static struct operand * make_operand_one(void)
{
    static struct operand *operand_one;
    if (!operand_one)
        operand_one = make_int_operand(1);
    return operand_one;
}

static struct operand * make_operand_zero(void)
{
    static struct operand *operand_zero;
    if (!operand_zero)
        operand_zero = make_int_operand(0);
    return operand_zero;
}

static struct tac * make_tac(int op,
                             struct operand *l, struct operand *r,
                             struct operand *result,
                             int opsize)
{
    struct tac *tac = (struct tac *)alloc_tac();
    tac->op = op;
    tac->operands[1] = l;
    tac->operands[2] = r;
    tac->operands[0] = result;
    tac->opsize = opsize;
    return tac;
}

static struct tac * make_tac_r(int op,
                               struct operand *l, struct operand *r,
                               int opsize)
{
    return make_tac(op, l, r, make_tmp_operand(), opsize);
}

bool is_tmp_operand(struct operand *operand)
{
    return operand->op == IR_NONE && SYM_X_KIND(operand->sym) == SYM_KIND_TMP;
}

bool is_imm_operand(struct operand *operand)
{
    return operand->op == IR_NONE && SYM_X_KIND(operand->sym) == SYM_KIND_IMM;
}

bool is_mem_operand(struct operand *operand)
{
    switch (operand->op) {
    case IR_SUBSCRIPT:
    case IR_INDIRECTION:
        return true;
    case IR_NONE:
        return SYM_X_KIND(operand->sym) == SYM_KIND_GREF ||
            SYM_X_KIND(operand->sym) == SYM_KIND_LREF;
    default:
        assert(0);
    }
}

bool is_direct_mem_operand(struct operand *operand)
{
    return operand->op == IR_NONE && is_mem_operand(operand);
}

static struct tac * make_assign_tac(int op,
                                    struct operand *l, struct operand *r,
                                    int opsize)
{
    if (is_mem_operand(l) && is_mem_operand(r)) {
        struct tac *tac = make_tac_r(op, r, NULL, opsize);
        emit_tac(tac);
        return make_tac(op, tac->operands[0], NULL, l, opsize);
    } else {
        return make_tac(op, r, NULL, l, opsize);
    }
}

static struct operand * make_address_operand(struct operand *l)
{
    switch (l->op) {
    case IR_NONE:
    case IR_SUBSCRIPT:
        {
            struct tac *tac = make_tac_r(IR_ADDRESS, l, NULL, ops[Quad]);
            emit_tac(tac);
            return tac->operands[0];
        }
        break;
    case IR_INDIRECTION:
        {
            struct operand *result = make_sym_operand(l->sym);
            return result;
        }
        break;
    default:
        assert(0);
    }
}

static struct operand * make_indirection_operand1(struct operand *l)
{
    assert(SYM_X_KIND(l->sym) == SYM_KIND_TMP);
    struct operand *operand = make_sym_operand(l->sym);
    operand->op = IR_INDIRECTION;
    return operand;
}

static struct operand * make_indirection_operand(struct operand *l)
{
    if (l->op == IR_NONE && SYM_X_KIND(l->sym) == SYM_KIND_TMP) {
        return make_indirection_operand1(l);
    } else {
        struct tac *tac = make_assign_tac(IR_ASSIGNI,
                                          make_tmp_operand(),
                                          l,
                                          ops[Quad]);
        emit_tac(tac);
        return make_indirection_operand1(tac->operands[0]);
    }
}

static bool is_block_storage(node_t *sym)
{
    if (SYM_X_KIND(sym) == SYM_KIND_GREF ||
        SYM_X_KIND(sym) == SYM_KIND_LREF) {
        node_t *ty = SYM_TYPE(sym);
        return isrecord(ty) || isarray(ty);
    } else {
        return false;
    }
}

static bool canbe_subscript_base(node_t *sym)
{
    switch (SYM_X_KIND(sym)) {
    case SYM_KIND_GREF:
    case SYM_KIND_LREF:
        return is_block_storage(sym);
    case SYM_KIND_TMP:
        return true;
    default:
        return false;
    }
}

static struct operand * make_subscript_operand2(struct operand *l,
                                                struct operand *index,
                                                size_t step,
                                                long disp)
{
    assert(l->op == IR_NONE);
    assert(index->op == IR_NONE);
    
    struct operand *operand;
    if (canbe_subscript_base(l->sym)) {
        operand = make_sym_operand(l->sym);
        operand->op = IR_SUBSCRIPT;
    } else {
        struct tac *tac = make_assign_tac(IR_ASSIGNI,
                                          make_tmp_operand(),
                                          l,
                                          ops[Quad]);
        emit_tac(tac);
        operand = make_sym_operand(tac->operands[0]->sym);
        operand->op = IR_SUBSCRIPT;
    }
    
    if (SYM_X_KIND(index->sym) == SYM_KIND_IMM) {
        // disp(base)
        long d = SYM_VALUE_I(index->sym) * step + disp;
        operand->disp = d;
    } else if (step == Byte || step == Word || step == Long || step == Quad) {
        // disp(base,index,scale)

        // NOTE: index _MUST_ be a tmp operand.
        if (SYM_X_KIND(index->sym) == SYM_KIND_TMP) {
            operand->index = index->sym;
        } else {
            // cast to Quad
            node_t *sty = SYM_TYPE(index->sym);
            node_t *dty = longtype;
            if (TYPE_SIZE(sty) == TYPE_SIZE(dty)) {
                struct tac *tac = make_assign_tac(IR_ASSIGNI,
                                                  make_tmp_operand(),
                                                  index,
                                                  ops[Quad]);
                emit_tac(tac);
                operand->index = tac->operands[0]->sym;
            } else {
                int op = TYPE_OP(sty) == UNSIGNED ? IR_CONV_UI_SI : IR_CONV_SI_SI;
                struct operand *result = emit_conv_tac(op,
                                                       index,
                                                       ops[TYPE_SIZE(sty)],
                                                       ops[TYPE_SIZE(dty)]);
                operand->index = result->sym;
            }
        }
        operand->scale = step;
        operand->disp = disp;
    } else {
        // direct
        struct tac *tac1 = make_tac_r(IR_IMULI,
                                      index,
                                      make_int_operand(step),
                                      ops[Quad]);
        emit_tac(tac1);

        struct operand *addr;
        if (is_block_storage(l->sym))
            addr = make_address_operand(l);
        else
            addr = l;

        struct tac *tac2 = make_tac_r(IR_ADDI,
                                      addr,
                                      tac1->operands[0],
                                      ops[Quad]);
        emit_tac(tac2);
        operand->sym = tac2->operands[0]->sym;
        operand->disp = disp;
    }

    return operand;
}

static struct operand * make_subscript_operand1(struct operand *l,
                                                struct operand *index,
                                                size_t step,
                                                long disp)
{
    assert(l->op == IR_NONE);
    
    switch (index->op) {
    case IR_NONE:
        return make_subscript_operand2(l, index, step, disp);
    case IR_SUBSCRIPT:
        {
            struct tac *tac = make_assign_tac(IR_ASSIGNI,
                                              make_tmp_operand(),
                                              index,
                                              ops[Quad]);
            emit_tac(tac);
            return make_subscript_operand2(l, tac->operands[0], step, disp);
        }
        break;
    case IR_INDIRECTION:
        {
            struct tac *tac = make_assign_tac(IR_ASSIGNI,
                                              make_tmp_operand(),
                                              index,
                                              ops[Quad]);
            emit_tac(tac);
            return make_subscript_operand2(l, tac->operands[0], step, disp);
        }
        break;
    default:
        assert(0);
    }
}

static struct operand * make_subscript_operand(struct operand *l,
                                               struct operand *index,
                                               size_t step)
{
    switch (l->op) {
    case IR_NONE:
        return make_subscript_operand1(l, index, step, 0);
    case IR_SUBSCRIPT:
        if (l->index) {
            struct tac *tac = make_assign_tac(IR_ASSIGNI, make_tmp_operand(), l, ops[Quad]);
            emit_tac(tac);
            return make_subscript_operand1(tac->operands[0], index, step, 0);
        } else {
            return make_subscript_operand1(make_sym_operand(l->sym), index, step, l->disp);
        }
        break;
    case IR_INDIRECTION:
        {
            struct tac *tac = make_assign_tac(IR_ASSIGNI, make_tmp_operand(), l, ops[Quad]);
            emit_tac(tac);
            return make_subscript_operand1(tac->operands[0], index, step, 0);
        }
        break;
    default:
        assert(0);
    }
}

static struct operand * make_offset_operand(struct operand *l, long offset)
{
    assert(canbe_subscript_base(l->sym));
    struct operand *operand = copy_operand(l);
    operand->disp += offset;
    return operand;
}

static struct operand * make_member_operand(struct operand *l, long offset)
{
    switch (l->op) {
    case IR_NONE:
    case IR_INDIRECTION:
        {
            struct operand *operand = make_sym_operand(l->sym);
            operand->op = IR_SUBSCRIPT;
            operand->disp = offset;
            return operand;
        }
        break;
    case IR_SUBSCRIPT:
        return make_offset_operand(l, offset);
    default:
        assert(0);
    }
}

static void emit_simple_if(int op, bool sign,
                           struct operand *operand,
                           const char *label, int opsize)
{
    struct tac *tac = make_tac(op, operand, NULL, make_label_operand(label), opsize);
    tac->sign = sign;
    emit_tac(tac);
}

static void emit_rel_if(int op,
                        int relop,
                        bool sign,
                        struct operand *rel_l, struct operand *rel_r,
                        const char *label,
                        int opsize)
{
    struct tac *tac = make_tac(op, rel_l, rel_r, make_label_operand(label), opsize);
    tac->relop = relop;
    tac->sign = sign;
    emit_tac(tac);
}

static void emit_label(const char *label)
{
    //NOTE: maybe duplicate, but don't care...
    struct operand *operand = make_label_operand(label);
    struct tac *tac = make_tac(IR_LABEL, NULL, NULL, operand, Zero);
    emit_tac(tac);
}

static void emit_goto(const char *label)
{
    struct operand *operand = make_label_operand(label);
    struct tac *tac = make_tac(IR_GOTO, NULL, NULL, operand, Zero);
    emit_tac(tac);
}

static void emit_param(struct operand *operand)
{
    struct tac *tac = make_tac(IR_PARAM, operand, NULL, NULL, Zero);
    emit_tac(tac);
}

static struct tac * make_call_tac(struct operand *l, struct operand *result, node_t *call)
{
    struct tac *tac = make_tac(IR_CALL, l, NULL, result, Quad);
    tac->call = call;
    return tac;
}

static struct tac * make_conv_tac(int op, struct operand *l,
                                  int from_opszie, int to_opsize)
{
    struct tac *tac = make_tac_r(op, l, NULL, Zero);
    tac->from_opsize = from_opszie;
    tac->to_opsize = to_opsize;
    return tac;
}

static struct operand * emit_conv_tac(int op, struct operand *l,
                                      int from_opsize,
                                      int to_opsize)
{
    struct tac *tac = make_conv_tac(op, l, from_opsize, to_opsize);
    emit_tac(tac);
    return tac->operands[0];
}

static void emit_decl(node_t *decl)
{
    node_t *sym = DECL_SYM(decl);
    node_t *init = DECL_BODY(decl);
    if (!isvardecl(decl))
        return;
    else if (SYM_SCLASS(sym) == EXTERN ||
             SYM_SCLASS(sym) == STATIC)
        return;
    else if (!init)
        return;
    
    struct operand *l = make_sym_operand(sym);
    // set sym x kind
    SYM_X_KIND(sym) = SYM_KIND_LREF;
    emit_assign(SYM_TYPE(sym), l, init, 0, NULL, false);
}

static void emit_decls(struct vector *decls)
{
    for (int i = 0; i < vec_len(decls); i++) {
        node_t *decl = vec_at(decls, i);
        emit_decl(decl);
    }
}

// int
static void emit_uop_bitwise_not(node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);

    emit_expr(l);
    struct tac *tac = make_tac_r(IR_NOT,
                                 EXPR_X_ADDR(l), NULL,
                                 ops[TYPE_SIZE(AST_TYPE(n))]);
    emit_tac(tac);
    EXPR_X_ADDR(n) = tac->operands[0];
}

// int
static void emit_uop_sizeof(node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);
    node_t *ty = istype(l) ? l : AST_TYPE(l);
    EXPR_X_ADDR(n) = make_unsigned_operand(TYPE_SIZE(ty));
}

// arith
static void emit_uop_minus(node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);
    node_t *ty = AST_TYPE(n);
    int op = isint(ty) ? IR_MINUSI : IR_MINUSF;

    emit_expr(l);

    struct tac *tac = make_tac_r(op,
                                 EXPR_X_ADDR(l), NULL,
                                 ops[TYPE_SIZE(ty)]);
    emit_tac(tac);
    EXPR_X_ADDR(n) = tac->operands[0];
}

// arith
static void emit_uop_plus(node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);
    
    emit_expr(l);
    EXPR_X_ADDR(n) = EXPR_X_ADDR(l);
}

// ptr
static void emit_uop_indirection(node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);

    emit_expr(l);

    if (isfunc(AST_TYPE(n)))
        EXPR_X_ADDR(n) = EXPR_X_ADDR(l);
    else
        EXPR_X_ADDR(n) = make_indirection_operand(EXPR_X_ADDR(l));
}

// lvalue
static void emit_uop_address(node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);

    emit_expr(l);

    if (isfunc(AST_TYPE(l)))
        EXPR_X_ADDR(n) = EXPR_X_ADDR(l);
    else
        EXPR_X_ADDR(n) = make_address_operand(EXPR_X_ADDR(l));
}

// ptr + int
static struct operand * emit_ptr_int(int op,
                                     struct operand *l,
                                     struct operand *index,
                                     size_t step,
                                     int opsize)
{
    struct operand *distance;
    if (SYM_X_KIND(index->sym) == SYM_KIND_IMM) {
        size_t i = SYM_VALUE_U(index->sym) * step;
        distance = make_unsigned_operand(i);
    } else {
        struct tac *tac;
        int sup = log2i(step);
        if (sup == -1) {
            // using MUL
            tac = make_tac_r(IR_MULI, index, make_unsigned_operand(step), opsize);
            emit_tac(tac);
            distance = tac->operands[0];
        } else if (sup > 0) {
            // using SHIFT
            tac = make_tac_r(IR_LSHIFT, index, make_unsigned_operand(sup), opsize);
            emit_tac(tac);
            distance = tac->operands[0];
        } else {
            distance = index;
        }
    }
    
    struct tac *tac = make_tac_r(op, l, distance, opsize);
    emit_tac(tac);
    return tac->operands[0];
}

// scalar
static void emit_uop_increment(node_t *n)
{
    bool prefix = EXPR_PREFIX(n);
    node_t *l = EXPR_OPERAND(n, 0);
    node_t *ty = AST_TYPE(n);
    int opsize = ops[TYPE_SIZE(ty)];
    int assignop = isfloat(ty) ? IR_ASSIGNF : IR_ASSIGNI;
    int rop;
    if (EXPR_OP(n) == INCR) {
        if (isfloat(ty))
            rop = IR_ADDF;
        else
            rop = IR_ADDI;
    } else {
        if (isfloat(ty))
            rop = IR_SUBF;
        else
            rop = IR_SUBI;
    }

    emit_expr(l);

    if (!prefix) {
        //NOTE: assign to a tmp operand
        struct operand *tmp = make_tmp_operand();
        struct tac *tac = make_assign_tac(assignop, tmp, EXPR_X_ADDR(l), opsize);
        emit_tac(tac);
        EXPR_X_ADDR(n) = tmp;
    }

    if (isptr(ty)) {
        struct operand *tmp = emit_ptr_int(rop,
                                           EXPR_X_ADDR(l),
                                           make_operand_one(),
                                           TYPE_SIZE(rtype(ty)),
                                           opsize);
        struct tac *tac = make_assign_tac(assignop, EXPR_X_ADDR(l), tmp, opsize);
        emit_tac(tac);
    } else {
        struct tac *tac = make_tac_r(rop,
                                     EXPR_X_ADDR(l),
                                     make_operand_one(),
                                     opsize);
        emit_tac(tac);
        struct tac *tac2 = make_assign_tac(assignop,
                                           EXPR_X_ADDR(l),
                                           tac->operands[0],
                                           opsize);
        emit_tac(tac2);
    }

    if (prefix)
        EXPR_X_ADDR(n) = EXPR_X_ADDR(l);
}

// scalar
static void emit_uop_logic_not(node_t *n)
{
    emit_bop_bool(n);
}

static void emit_uop(node_t *n)
{
    switch (EXPR_OP(n)) {
    case INCR:
    case DECR:
        emit_uop_increment(n);
        break;
    case '*':
        emit_uop_indirection(n);
        break;
    case '&':
        emit_uop_address(n);
        break;
    case '+':
        emit_uop_plus(n);
        break;
    case '-':
        emit_uop_minus(n);
        break;
    case '~':
        emit_uop_bitwise_not(n);
        break;
    case '!':
        emit_uop_logic_not(n);
        break;
    case SIZEOF:
        emit_uop_sizeof(n);
        break;
    default:
        assert(0);
    }
}

static node_t * fieldof(node_t *n)
{
    if (AST_ID(n) != MEMBER_EXPR)
        return NULL;
    const char *name = AST_NAME(n);
    node_t *l = EXPR_OPERAND(n, 0);
    node_t *ty = AST_TYPE(l);
    if (isptr(ty))
        ty = rtype(ty);
    assert(isrecord(ty));
    node_t *field = find_field(ty, name);
    return field;
}

static void do_emit_zeros(node_t *ty, struct operand *l, long *offset,
                          size_t *bytes, unsigned size)
{
    assert(size <= 8);

    while (*bytes >= size) {
        int op = isfloat(ty) ? IR_ASSIGNF : IR_ASSIGNI;
        int opsize = ops[size];
        struct operand *r1 = make_operand_zero();
        struct operand *l1 = make_offset_operand(l, *offset);
        struct tac *tac = make_assign_tac(op, l1, r1, opsize);
        emit_tac(tac);
        *bytes -= size;
        *offset += size;
    }
}

static void emit_zeros(node_t *ty, struct operand *l, long offset, size_t bytes)
{
    do_emit_zeros(ty, l, &offset, &bytes, 8);
    do_emit_zeros(ty, l, &offset, &bytes, 4);
    do_emit_zeros(ty, l, &offset, &bytes, 2);
    do_emit_zeros(ty, l, &offset, &bytes, 1);
    assert(bytes == 0);
}

static void do_emit_bytes(struct operand *l, long *loffset,
                          struct operand *r, long *roffset,
                          size_t *bytes, unsigned size)
{
    assert(size <= 8);

    while (*bytes >= size) {
        int op = IR_ASSIGNI;
        int opsize = ops[size];
        struct operand *l1 = make_offset_operand(l, *loffset);
        struct operand *r1 = make_offset_operand(r, *roffset);
        struct tac *tac = make_assign_tac(op, l1, r1, opsize);
        emit_tac(tac);
        *bytes -= size;
        *loffset += size;
        *roffset += size;
    }
}

static void emit_bytes(struct operand *l, long offset, struct operand *r, size_t bytes)
{
    long roffset = 0;
    do_emit_bytes(l, &offset, r, &roffset, &bytes, 8);
    do_emit_bytes(l, &offset, r, &roffset, &bytes, 4);
    do_emit_bytes(l, &offset, r, &roffset, &bytes, 2);
    do_emit_bytes(l, &offset, r, &roffset, &bytes, 1);
    assert(bytes == 0);
}

static void emit_inits(node_t *ty, struct operand *l, node_t *r, long offset, bool sty)
{
    assert(AST_ID(r) == INITS_EXPR);

    if (isstruct(ty) || isunion(ty)) {
        struct vector *inits = EXPR_INITS(r);
        struct vector *fields = TYPE_FIELDS(ty);
        for (int i = 0; i < vec_len(inits); i++) {
            node_t *init = vec_at(inits, i);
            node_t *field = vec_at(fields, i);
            node_t *rty = FIELD_TYPE(field);
            long off = offset + FIELD_OFFSET(field);
            if (FIELD_ISBIT(field)) {
                if (AST_ID(init) == VINIT_EXPR)
                    emit_bitfield_basic(rty, l, make_operand_zero(), off, field, sty);
                else
                    emit_assign(rty, l, init, off, field, sty);
            } else {
                if (AST_ID(init) == VINIT_EXPR)
                    emit_zeros(rty, l, off, TYPE_SIZE(rty));
                else
                    emit_assign(rty, l, init, off, NULL, sty);
            }
        }
        if (vec_len(inits) < vec_len(fields)) {
            node_t *field = vec_at(fields, vec_len(inits));
            long off = FIELD_OFFSET(field);
            size_t bytes = TYPE_SIZE(ty) - off;
            // as integer
            emit_zeros(inttype, l, off, bytes);
        }
    } else if (isarray(ty)) {
        node_t *rty = rtype(ty);
        struct vector *inits = EXPR_INITS(r);
        for (int i = 0; i < vec_len(inits); i++) {
            node_t *init = vec_at(inits, i);
            long off = offset + i * TYPE_SIZE(rty);
            if (AST_ID(init) == VINIT_EXPR)
                emit_zeros(rty, l, off, TYPE_SIZE(rty));
            else
                emit_assign(rty, l, init, off, NULL, sty);
        }
        if (vec_len(inits) < TYPE_LEN(ty)) {
            long off = offset + vec_len(inits) * TYPE_SIZE(rty);
            size_t bytes = TYPE_SIZE(ty) - off;
            emit_zeros(rty, l, off, bytes);
        }
    } else {
        assert(0);
    }
}

static void emit_scalar_basic(node_t *ty, int opsize,
                              struct operand *l, struct operand *r,
                              long offset, bool sty)
{
    int op = isfloat(ty) ? IR_ASSIGNF : IR_ASSIGNI;
    if (offset || sty) {
        struct operand *x = make_offset_operand(l, offset);
        struct tac *tac = make_assign_tac(op, x, r, opsize);
        emit_tac(tac);
    } else {
        struct tac *tac = make_assign_tac(op, l, r, opsize);
        emit_tac(tac);
    }
}

static void emit_scalar(node_t *ty, struct operand *l, node_t *r, long offset, bool sty)
{
    emit_expr(r);
    emit_scalar_basic(ty, ops[TYPE_SIZE(ty)], l, EXPR_X_ADDR(r), offset, sty);
}

static int get_bfield_opsize(node_t *bfield)
{
    int boff = FIELD_BITOFF(bfield);
    int bsize = FIELD_BITSIZE(bfield);
    int bytes = BYTES(boff + bsize);
    if (bytes == 1)
        return Byte;
    else if (bytes == 2)
        return Word;
    else if (bytes <= 4)
        return Long;
    else
        return Quad;
}

static struct operand * get_bfield_mask1(node_t *bfield)
{
    int bsize = FIELD_BITSIZE(bfield);
    int opsize = get_bfield_opsize(bfield);
    switch (opsize) {
    case Byte:
        {
            unsigned char mask1 = (1 << bsize) - 1;
            return make_unsigned_operand(mask1);
        }
        break;
    case Word:
        {
            unsigned short mask1 = (1 << bsize) - 1;
            return make_unsigned_operand(mask1);
        }
        break;
    case Long:
        {
            unsigned int mask1 = (1 << bsize) - 1;
            return make_unsigned_operand(mask1);
        }
        break;
    case Quad:
        {
            unsigned long mask1 = (1 << bsize) - 1;
            return make_unsigned_operand(mask1);
        }
        break;
    default:
        assert(0);
    }
}

static struct operand * get_bfield_mask2(node_t *bfield)
{
    int boff = FIELD_BITOFF(bfield);
    int bsize = FIELD_BITSIZE(bfield);
    int opsize = get_bfield_opsize(bfield);
    switch (opsize) {
    case Byte:
        {
            unsigned char mask1 = (1 << bsize) - 1;
            unsigned char mask2 = ~(mask1 << boff);
            return make_unsigned_operand(mask2);
        }
        break;
    case Word:
        {
            unsigned short mask1 = (1 << bsize) - 1;
            unsigned short mask2 = ~(mask1 << boff);
            return make_unsigned_operand(mask2);
        }
        break;
    case Long:
        {
            unsigned int mask1 = (1 << bsize) - 1;
            unsigned int mask2 = ~(mask1 << boff);
            return make_unsigned_operand(mask2);
        }
        break;
    case Quad:
        {
            unsigned long mask1 = (1 << bsize) - 1;
            unsigned long mask2 = ~(mask1 << boff);
            return make_unsigned_operand(mask2);
        }
        break;
    default:
        assert(0);
    }
}

static void emit_bitfield_basic(node_t *ty, struct operand *l, struct operand *r,
                                long offset, node_t *bfield, bool sty)
{
    int boff = FIELD_BITOFF(bfield);
    int opsize = ops[get_bfield_opsize(bfield)];
    struct operand *mask1_operand = get_bfield_mask1(bfield);
    struct operand *mask2_operand = get_bfield_mask2(bfield);

    // &
    struct tac *tac1 = make_tac_r(IR_AND, r, mask1_operand, opsize);
    emit_tac(tac1);
    // <<
    struct operand *operand2;
    if (boff) {
        struct operand *boff_operand = make_unsigned_operand(boff);
        struct tac *tac2 = make_tac_r(IR_LSHIFT, tac1->operands[0], boff_operand, opsize);
        emit_tac(tac2);
        operand2 = tac2->operands[0];
    } else {
        operand2 = tac1->operands[0];
    }
    // &
    struct operand *field_operand = make_offset_operand(l, offset);
    struct tac *tac3 = make_tac_r(IR_AND, field_operand, mask2_operand, opsize);
    emit_tac(tac3);
    // |
    struct tac *tac4 = make_tac_r(IR_OR, operand2, tac3->operands[0], opsize);
    emit_tac(tac4);
    // assign
    emit_scalar_basic(ty, opsize, l, tac4->operands[0], offset, sty);
}

static void emit_bitfield(node_t *ty, struct operand *l, node_t *r,
                          long offset, node_t *bfield, bool sty)
{
    emit_expr(r);
    emit_bitfield_basic(ty, l, EXPR_X_ADDR(r), offset, bfield, sty);
}

/*
  ty - type of left node
  l - left operand
  r - right node (_NOT_ evaluated)
  offset - offset to assign
  bfield - not NULL only if left node is a bitfield
  sty - when offset == 0, it means whether offset is zero or not exist.
 */
static void emit_assign(node_t *ty, struct operand *l, node_t *r,
                        long offset, node_t *bfield, bool sty)
{
    assert(ty);
    
    if (isstruct(ty) || isunion(ty)) {
        if (AST_ID(r) == INITS_EXPR) {
            emit_inits(ty, l, r, offset, true);
        } else {
            emit_expr(r);
            emit_bytes(l, offset, EXPR_X_ADDR(r), TYPE_SIZE(ty));
        }
    } else if (isarray(ty)) {
        if (AST_ID(r) == INITS_EXPR) {
            emit_inits(ty, l, r, offset, true);
        } else if (AST_ID(r) == STRING_LITERAL) {
            emit_expr(r);
            emit_bytes(l, offset, EXPR_X_ADDR(r), TYPE_SIZE(ty));
        } else {
            assert(0);
        }
    } else {
        if (bfield)
            emit_bitfield(ty, l, r, offset, bfield, true);
        else
            emit_scalar(ty, l, r, offset, sty);
    }
}

static void emit_bop_assign(node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);
    node_t *r = EXPR_OPERAND(n, 1);
    
    node_t *field = fieldof(l);
    if (field && FIELD_ISBIT(field)) {
        // if it's a bit-field
        emit_member_nonbitfield(l, field);
        emit_assign(AST_TYPE(l), EXPR_X_ADDR(l), r, 0, field, false);
    } else {
        emit_expr(l);
        emit_assign(AST_TYPE(l), EXPR_X_ADDR(l), r, 0, NULL, false);
    }
    
    EXPR_X_ADDR(n) = EXPR_X_ADDR(l);
}

static void emit_bop_comma(node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);
    node_t *r = EXPR_OPERAND(n, 1);
    emit_expr(l);
    emit_expr(r);
    EXPR_X_ADDR(n) = EXPR_X_ADDR(r);
}

static void emit_bop_bool(node_t *n)
{
    int opsize = ops[TYPE_SIZE(AST_TYPE(n))];
    EXPR_X_TRUE(n) = fall;
    EXPR_X_FALSE(n) = gen_label();
    const char *label = gen_label();
    struct operand *result = make_tmp_operand();
    emit_bool_expr(n);
    // true
    emit_tac(make_assign_tac(IR_ASSIGNI, result, make_operand_one(), opsize));
    emit_goto(label);
    // false
    emit_label(EXPR_X_FALSE(n));
    emit_tac(make_assign_tac(IR_ASSIGNI, result, make_operand_zero(), opsize));
    // out
    emit_label(label);
    EXPR_X_ADDR(n) = result;
}

static int bop2rop(int op, node_t *ty)
{
    switch (op) {
        // scalar
    case '+':
        return isfloat(ty) ? IR_ADDF : IR_ADDI;
    case '-':
        return isfloat(ty) ? IR_SUBF : IR_SUBI;
        // arith
    case '*':
        if (isfloat(ty))
            return IR_MULF;
        else if (TYPE_OP(ty) == UNSIGNED)
            return IR_MULI;
        else
            return IR_IMULI;
    case '/':
        if (isfloat(ty))
            return IR_DIVF;
        else if (TYPE_OP(ty) == UNSIGNED)
            return IR_DIVI;
        else
            return IR_IDIVI;
        // int
    case '%':
        if (TYPE_OP(ty) == UNSIGNED)
            return IR_MOD;
        else
            return IR_IMOD;
    case '|':
        return IR_OR;
    case '&':
        return IR_AND;
    case '^':
        return IR_XOR;
    case LSHIFT:
        if (TYPE_OP(ty) == UNSIGNED)
            return IR_LSHIFT;
        else
            return IR_ILSHIFT;
    case RSHIFT:
        if (TYPE_OP(ty) == UNSIGNED)
            return IR_RSHIFT;
        else
            return IR_IRSHIFT;
    default:
        assert(0);
    }
}

// arith op arith
static void emit_bop_arith(node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);
    node_t *r = EXPR_OPERAND(n, 1);
    node_t *ty = AST_TYPE(n);
    int op = bop2rop(EXPR_OP(n), ty);
    int opsize = ops[TYPE_SIZE(ty)];

    emit_expr(l);
    emit_expr(r);

    struct tac *tac = make_tac_r(op, EXPR_X_ADDR(l), EXPR_X_ADDR(r), opsize);
    emit_tac(tac);
    EXPR_X_ADDR(n) = tac->operands[0];
}

// arith + arith
// ptr + int
// int + ptr
//
// arith - arith
// ptr - int
static void emit_bop_plus_minus(node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);
    node_t *r = EXPR_OPERAND(n, 1);

    if (isarith(AST_TYPE(l)) && isarith(AST_TYPE(r))) {
        emit_bop_arith(n);
    } else {
        emit_expr(l);
        emit_expr(r);

        node_t *ptr = isptr(AST_TYPE(l)) ? l : r;
        node_t *i = isint(AST_TYPE(l)) ? l : r;
        node_t *rty = rtype(AST_TYPE(ptr));
        int op = EXPR_OP(n) == '+' ? IR_ADDI : IR_SUBI;
        EXPR_X_ADDR(n) = emit_ptr_int(op,
                                      EXPR_X_ADDR(ptr),
                                      EXPR_X_ADDR(i),
                                      TYPE_SIZE(rty),
                                      ops[TYPE_SIZE(AST_TYPE(ptr))]);
    }
}

static void emit_bop(node_t *n)
{    
    switch (EXPR_OP(n)) {
    case '=':
        emit_bop_assign(n);
        break;
    case ',':
        emit_bop_comma(n);
        break;
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
        emit_bop_arith(n);
        break;
    case '+':
    case '-':
        emit_bop_plus_minus(n);
        break;
        // scalar
    case '<':
    case '>':
    case GEQ:
    case LEQ:
    case EQ:
    case NEQ:
    case AND:
    case OR:
        emit_bop_bool(n);
        break;
    default:
        assert(0);
    }
}

static struct operand * make_extra_decl(node_t *ty)
{
    assert(isrecord(ty));
    
    if (!extra_lvars)
        extra_lvars = vec_new();

    node_t *sym = gen_tmp_sym();
    SYM_TYPE(sym) = ty;
    // set scope as LOCAL
    SYM_SCOPE(sym) = LOCAL;

    node_t *decl = ast_decl(VAR_DECL);
    DECL_SYM(decl) = sym;
    vec_push(extra_lvars, decl);
    
    struct operand *operand = make_sym_operand(sym);
    SYM_X_KIND(sym) = SYM_KIND_LREF;
    return operand;
}

// scalar ? type : type
static void emit_cond(node_t *n)
{
    node_t *cond = EXPR_COND(n);
    node_t *then = EXPR_THEN(n);
    node_t *els = EXPR_ELSE(n);

    EXPR_X_TRUE(cond) = fall;
    EXPR_X_FALSE(cond) = gen_label();
    const char *label = gen_label();
    if (isvoid(AST_TYPE(n))) {
        emit_bool_expr(cond);
        // true
        emit_goto(label);
        // false
        emit_label(EXPR_X_FALSE(cond));
        // out
        emit_label(label);
    } else {
        struct operand *result;
        if (isrecord(AST_TYPE(n)))
            result = make_extra_decl(AST_TYPE(n));
        else
            result = make_tmp_operand();
        emit_bool_expr(cond);
        // true
        emit_assign(AST_TYPE(n), result, then, 0, NULL, false);
        emit_goto(label);
        // false
        emit_label(EXPR_X_FALSE(cond));
        emit_assign(AST_TYPE(n), result, els, 0, NULL, false);
        // out
        emit_label(label);
        EXPR_X_ADDR(n) = result;
    }
}

// s.a
// s->a
static void emit_member_nonbitfield(node_t *n, node_t *field)
{
    node_t *l = EXPR_OPERAND(n, 0);
    emit_expr(l);

    struct operand *addr = EXPR_X_ADDR(l);

    // if l is ptr, change the base address.
    if (isptr(AST_TYPE(l))) {
        if (!is_tmp_operand(addr)) {
            struct tac *tac = make_assign_tac(IR_ASSIGNI,
                                              make_tmp_operand(),
                                              EXPR_X_ADDR(l),
                                              ops[Quad]);
            emit_tac(tac);
            addr = tac->operands[0];
        }
    }
    EXPR_X_ADDR(n) = make_member_operand(addr, FIELD_OFFSET(field));
}

static void emit_member_bitfield(node_t *n, node_t *field)
{
    int boff = FIELD_BITOFF(field);
    int size = get_bfield_opsize(field);
    struct operand *mask1_operand = get_bfield_mask1(field);
    struct operand *result = EXPR_X_ADDR(n);
    if (boff) {
        struct tac *tac = make_tac_r(IR_RSHIFT,
                                     result,
                                     make_unsigned_operand(boff),
                                     ops[size]);
        emit_tac(tac);
        result = tac->operands[0];
    }
    struct tac *tac = make_tac_r(IR_AND,
                                 result,
                                 mask1_operand,
                                 ops[size]);
    emit_tac(tac);

    // extend: type conv
    node_t *ty = FIELD_TYPE(field);
    if (size != TYPE_SIZE(ty)) {
        int rop = TYPE_OP(ty) == UNSIGNED ? IR_CONV_UI_UI : IR_CONV_SI_SI;
        tac = make_conv_tac(rop,
                            tac->operands[0],
                            ops[size],
                            ops[TYPE_SIZE(ty)]);
        emit_tac(tac);
    }
    
    EXPR_X_ADDR(n) = tac->operands[0];
}

static void emit_member(node_t *n)
{
    node_t *field = fieldof(n);
    emit_member_nonbitfield(n, field);

    // if it's a bit-field
    if (FIELD_ISBIT(field))
        emit_member_bitfield(n, field);
}

static void emit_subscript(node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);
    node_t *r = EXPR_OPERAND(n, 1);

    emit_expr(l);
    emit_expr(r);

    node_t *ptr = isptr(AST_TYPE(l)) ? l : r;
    node_t *i = ptr == l ? r : l;
    node_t *rty = rtype(AST_TYPE(ptr));
    struct operand *addr = EXPR_X_ADDR(ptr);
    EXPR_X_ADDR(n) = make_subscript_operand(addr, EXPR_X_ADDR(i), TYPE_SIZE(rty));
}

static void emit_call(node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);
    struct vector *args = EXPR_ARGS(n);
    int len = vec_len(args);
    node_t *rty = AST_TYPE(n);

    emit_expr(l);

    for (size_t i = 0; i < len; i++) {
        node_t *arg = vec_at(args, i);
        emit_expr(arg);
    }

    struct operand *call_operand = EXPR_X_ADDR(l);
    if (call_operand->op == IR_NONE &&
        SYM_X_KIND(call_operand->sym) == SYM_KIND_GREF &&
        isptr(SYM_TYPE(call_operand->sym))) {
        struct tac *tac = make_assign_tac(IR_ASSIGNI,
                                          make_tmp_operand(),
                                          call_operand,
                                          ops[Quad]);
        emit_tac(tac);
        call_operand = tac->operands[0];
    }

    // parameters
    for (size_t i = 0; i < len; i++) {
        node_t *arg = vec_at(args, i);
        emit_param(EXPR_X_ADDR(arg));
    }
    
    if (isvoid(rty)) {
        struct tac *tac = make_call_tac(call_operand, NULL, n);
        emit_tac(tac);
    } else {
        struct tac *tac = make_call_tac(call_operand, make_tmp_operand(), n);
        emit_tac(tac);
        EXPR_X_ADDR(n) = tac->operands[0];
    }
}

static void int2int(node_t *sty, node_t *dty, node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);
    int op;

    if (TYPE_SIZE(dty) == TYPE_SIZE(sty)) {
        EXPR_X_ADDR(n) = EXPR_X_ADDR(l);
    } else {
        if (TYPE_OP(dty) == UNSIGNED && TYPE_OP(sty) == UNSIGNED)
            op = IR_CONV_UI_UI;
        else if (TYPE_OP(dty) == UNSIGNED && TYPE_OP(sty) == INT)
            op = IR_CONV_SI_UI;
        else if (TYPE_OP(dty) == INT && TYPE_OP(sty) == INT)
            op = IR_CONV_SI_SI;
        else
            op = IR_CONV_UI_SI;

        EXPR_X_ADDR(n) = emit_conv_tac(op,
                                       EXPR_X_ADDR(l),
                                       ops[TYPE_SIZE(sty)],
                                       ops[TYPE_SIZE(dty)]);
    }
}

static void int2float(node_t *sty, node_t *dty, node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);
    int op = TYPE_OP(sty) == UNSIGNED ? IR_CONV_UI_F : IR_CONV_SI_F;

    EXPR_X_ADDR(n) = emit_conv_tac(op,
                                   EXPR_X_ADDR(l),
                                   ops[TYPE_SIZE(sty)],
                                   ops[TYPE_SIZE(dty)]);
}

static void float2int(node_t *sty, node_t *dty, node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);
    int op = TYPE_OP(dty) == UNSIGNED ? IR_CONV_F_UI : IR_CONV_F_SI;

    EXPR_X_ADDR(n) = emit_conv_tac(op,
                                   EXPR_X_ADDR(l),
                                   ops[TYPE_SIZE(sty)],
                                   ops[TYPE_SIZE(dty)]);
}

static void float2float(node_t *sty, node_t *dty, node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);
    if (TYPE_SIZE(dty) == TYPE_SIZE(sty))
        EXPR_X_ADDR(n) = EXPR_X_ADDR(l);
    else
        EXPR_X_ADDR(n) = emit_conv_tac(IR_CONV_FF,
                                       EXPR_X_ADDR(l),
                                       ops[TYPE_SIZE(sty)],
                                       ops[TYPE_SIZE(dty)]);
}

static void arith2arith(node_t *sty, node_t *dty, node_t *n)
{
    if (eqarith(sty, dty)) {
        node_t *l = EXPR_OPERAND(n, 0);
        EXPR_X_ADDR(n) = EXPR_X_ADDR(l);
    } else {
        if (isint(sty) && isint(dty))
            int2int(sty, dty, n);
        else if (isint(sty) && isfloat(dty))
            int2float(sty, dty, n);
        else if (isfloat(sty) && isint(dty))
            float2int(sty, dty, n);
        else if (isfloat(sty) && isfloat(dty))
            float2float(sty, dty, n);
        else
            assert(0);
    }
}

static void ptr2arith(node_t *sty, node_t *dty, node_t *n)
{
    assert(isint(dty));

    arith2arith(unsignedlongtype, dty, n);
}

static void arith2ptr(node_t *sty, node_t *dty, node_t *n)
{
    assert(isint(sty));

    arith2arith(sty, unsignedlongtype, n);
}

static void ptr2ptr(node_t *sty, node_t *dty, node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);
    EXPR_X_ADDR(n) = EXPR_X_ADDR(l);
}

//@ function to pointer decay
static void func2ptr(node_t *sty, node_t *dty, node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);
    EXPR_X_ADDR(n) = EXPR_X_ADDR(l);
}

//@ array to pointer decay
static void array2ptr(node_t *sty, node_t *dty, node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);
    EXPR_X_ADDR(n) = make_address_operand(EXPR_X_ADDR(l));
}

static void emit_conv(node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);
    node_t *dty = AST_TYPE(n);
    node_t *sty = AST_TYPE(l);

    emit_expr(l);

    if (isarith(dty)) {
        if (isarith(sty))
            arith2arith(sty, dty, n);
        else if (isptr(sty))
            ptr2arith(sty, dty, n);
        else
            assert(0);
    } else if (isptr(dty)) {
        if (isptr(sty))
            ptr2ptr(sty, dty, n);
        else if (isarith(sty))
            arith2ptr(sty, dty, n);
        else if (isfunc(sty))
            func2ptr(sty, dty, n);
        else if (isarray(sty))
            array2ptr(sty, dty, n);
        else
            assert(0);
    } else if (isvoid(dty)) {
        EXPR_X_ADDR(n) = NULL;
    } else {
        // nothing
        EXPR_X_ADDR(n) = EXPR_X_ADDR(l);
    }
}

static void emit_paren(node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);
    emit_expr(l);
    EXPR_X_ADDR(n) = EXPR_X_ADDR(l);
}

static void emit_integer_literal(node_t *n)
{
    node_t *sym = EXPR_SYM(n);
    SYM_X_LABEL(sym) = stru(SYM_VALUE_U(sym));
    SYM_X_KIND(sym) = SYM_KIND_IMM;
    EXPR_X_ADDR(n) = make_sym_operand(sym);
}

static const char *get_float_label(const char *name)
{
    const char *label = map_get(exts->floats, name);
    if (!label) {
        label = gen_sliteral_label();
        map_put(exts->floats, name, (void *)label);
    }
    return label;
}

static void emit_float_literal(node_t *n)
{
    node_t *sym = EXPR_SYM(n);
    const char *label = get_float_label(SYM_NAME(sym));
    SYM_X_LABEL(sym) = label;
    SYM_X_KIND(sym) = SYM_KIND_GREF;
    EXPR_X_ADDR(n) = make_sym_operand(sym);
}

static void emit_string_literal(node_t *n)
{
    node_t *sym = EXPR_SYM(n);
    const char *label = get_string_literal_label(SYM_NAME(sym));
    SYM_X_LABEL(sym) = label;
    SYM_X_KIND(sym) = SYM_KIND_GREF;
    EXPR_X_ADDR(n) = make_sym_operand(sym);
}

static void emit_compound_literal(node_t *n)
{
    node_t *sym = EXPR_SYM(n);
    SYM_X_KIND(sym) = SYM_KIND_LREF;
    EXPR_X_ADDR(n) = make_sym_operand(sym);

    node_t *l = EXPR_OPERAND(n, 0);
    emit_assign(AST_TYPE(n), EXPR_X_ADDR(n), l, 0, NULL, false);
}

static void emit_ref(node_t *n)
{
    node_t *sym = EXPR_SYM(n);
    SYM_X_KIND(sym) = isgref(sym) ? SYM_KIND_GREF : SYM_KIND_LREF;
    EXPR_X_ADDR(n) = make_sym_operand(sym);
}

static void emit_expr(node_t *n)
{
    assert(isexpr(n));

    switch (AST_ID(n)) {
    case BINARY_OPERATOR:
        emit_bop(n);
        break;
    case UNARY_OPERATOR:
        emit_uop(n);
        break;
    case PAREN_EXPR:
        emit_paren(n);
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
        emit_call(n);
        break;
    case REF_EXPR:
        if (EXPR_OP(n) == ENUM)
            emit_integer_literal(n);
        else
            emit_ref(n);
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
        assert(0);
    }
}

static void emit_logic_and(node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);
    node_t *r = EXPR_OPERAND(n, 1);
    if (EXPR_X_FALSE(n) == fall) {
        EXPR_X_TRUE(l) = fall;
        EXPR_X_FALSE(l) = gen_label();
        EXPR_X_TRUE(r) = EXPR_X_TRUE(n);
        EXPR_X_FALSE(r) = EXPR_X_FALSE(n);

        emit_bool_expr(l);
        emit_bool_expr(r);
        emit_label(EXPR_X_FALSE(l));
    } else {
        EXPR_X_TRUE(l) = fall;
        EXPR_X_FALSE(l) = EXPR_X_FALSE(n);
        EXPR_X_TRUE(r) = EXPR_X_TRUE(n);
        EXPR_X_FALSE(r) = EXPR_X_FALSE(n);

        emit_bool_expr(l);
        emit_bool_expr(r);
    }
}

static void emit_logic_or(node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);
    node_t *r = EXPR_OPERAND(n, 1);
    if (EXPR_X_TRUE(n) == fall) {
        EXPR_X_TRUE(l) = gen_label();
        EXPR_X_FALSE(l) = fall;
        EXPR_X_TRUE(r) = EXPR_X_TRUE(n);
        EXPR_X_FALSE(r) = EXPR_X_FALSE(n);

        emit_bool_expr(l);
        emit_bool_expr(r);
        emit_label(EXPR_X_TRUE(l));
    } else {
        EXPR_X_TRUE(l) = EXPR_X_TRUE(n);
        EXPR_X_FALSE(l) = fall;
        EXPR_X_TRUE(r) = EXPR_X_TRUE(n);
        EXPR_X_FALSE(r) = EXPR_X_FALSE(n);

        emit_bool_expr(l);
        emit_bool_expr(r);
    }
}

static bool isrelop(int op)
{
    switch (op) {
    case '>':
    case '<':
    case GEQ:
    case LEQ:
    case NEQ:
    case EQ:
        return true;
    default:
        return false;
    }
}

static void emit_rel_expr(node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);
    node_t *r = EXPR_OPERAND(n, 1);
    int relop = EXPR_OP(n);
    node_t *ty = AST_TYPE(n);
    int opsize = ops[TYPE_SIZE(ty)];
    bool floating = isfloat(AST_TYPE(l));
    bool sign = TYPE_OP(AST_TYPE(l)) == INT;

    emit_expr(l);
    emit_expr(r);

    if (EXPR_X_TRUE(n) != fall && EXPR_X_FALSE(n) != fall) {
        
        int op = floating ? IR_IF_F : IR_IF_I;
        emit_rel_if(op,
                    relop,
                    sign,
                    EXPR_X_ADDR(l), EXPR_X_ADDR(r),
                    EXPR_X_TRUE(n),
                    opsize);
        emit_goto(EXPR_X_FALSE(n));
        
    } else if (EXPR_X_TRUE(n) != fall) {

        int op = floating ? IR_IF_F : IR_IF_I;
        emit_rel_if(op,
                    relop,
                    sign,
                    EXPR_X_ADDR(l), EXPR_X_ADDR(r),
                    EXPR_X_TRUE(n),
                    opsize);
        
    } else if (EXPR_X_FALSE(n) != fall) {

        int op = floating ? IR_IF_FALSE_F : IR_IF_FALSE_I;
        emit_rel_if(op,
                    relop,
                    sign,
                    EXPR_X_ADDR(l), EXPR_X_ADDR(r),
                    EXPR_X_FALSE(n),
                    opsize);
    } else {
        // both fall
    }
}

static void emit_bool_expr(node_t *n)
{
    if (AST_ID(n) == BINARY_OPERATOR && EXPR_OP(n) == AND) {
        emit_logic_and(n);
    } else if (AST_ID(n) == BINARY_OPERATOR && EXPR_OP(n) == OR) {
        emit_logic_or(n);
    } else if (AST_ID(n) == BINARY_OPERATOR && isrelop(EXPR_OP(n))) {
        emit_rel_expr(n);
    } else if (AST_ID(n) == UNARY_OPERATOR && EXPR_OP(n) == '!') {
        node_t *l = EXPR_OPERAND(n, 0);
        EXPR_X_TRUE(l) = EXPR_X_FALSE(n);
        EXPR_X_FALSE(l) = EXPR_X_TRUE(n);
        emit_bool_expr(l);
    } else {
        emit_expr(n);

        node_t *ty = AST_TYPE(n);
        int opsize = ops[TYPE_SIZE(ty)];
        struct operand *test = EXPR_X_ADDR(n);
        bool sign = TYPE_OP(ty) == INT;
        
        if (EXPR_X_TRUE(n) != fall && EXPR_X_FALSE(n) != fall) {
            
            int op = isfloat(ty) ? IR_IF_F : IR_IF_I;
            emit_simple_if(op, sign, test, EXPR_X_TRUE(n), opsize);
            emit_goto(EXPR_X_FALSE(n));
            
        } else if (EXPR_X_TRUE(n) != fall) {
            
            int op = isfloat(ty) ? IR_IF_F : IR_IF_I;
            emit_simple_if(op, sign, test, EXPR_X_TRUE(n), opsize);
            
        } else if (EXPR_X_FALSE(n) != fall) {
            
            int op = isfloat(ty) ? IR_IF_FALSE_F : IR_IF_FALSE_I;
            emit_simple_if(op, sign, test, EXPR_X_FALSE(n), opsize);
            
        } else {
            // both fall: do nothing
        }
    }
}

static void emit_compound_stmt(node_t *stmt)
{
    struct vector *blks = STMT_BLKS(stmt);
    for (int i = 0; i < vec_len(blks); i++) {
        node_t *node = vec_at(blks, i);
        if (isdecl(node)) {
            emit_decl(node);
        } else if (isstmt(node)) {
            STMT_X_NEXT(node) = gen_label();
            emit_stmt(node);
        } else {
            assert(0);
        }
    }
}

static void emit_if_stmt(node_t *stmt)
{
    node_t *cond = STMT_COND(stmt);
    node_t *then = STMT_THEN(stmt);
    node_t *els = STMT_ELSE(stmt);

    EXPR_X_TRUE(cond) = fall;
    STMT_X_NEXT(then) = STMT_X_NEXT(stmt);

    if (els) {
        EXPR_X_FALSE(cond) = gen_label();
        STMT_X_NEXT(els) = STMT_X_NEXT(stmt);
        emit_bool_expr(cond);
        emit_stmt(then);
        emit_goto(STMT_X_NEXT(stmt));
        emit_label(EXPR_X_FALSE(cond));
        emit_stmt(els);
    } else {
        EXPR_X_FALSE(cond) = STMT_X_NEXT(stmt);
        emit_bool_expr(cond);
        emit_stmt(then);
    }
    emit_label(STMT_X_NEXT(stmt));
}

static void emit_while_stmt(node_t *stmt)
{
    const char *beg = gen_label();
    node_t *cond = STMT_WHILE_COND(stmt);
    node_t *body = STMT_WHILE_BODY(stmt);

    EXPR_X_TRUE(cond) = fall;
    EXPR_X_FALSE(cond) = STMT_X_NEXT(stmt);
    STMT_X_NEXT(body) = STMT_X_NEXT(stmt);

    emit_label(beg);
    emit_bool_expr(cond);

    SET_LOOP_CONTEXT(beg, STMT_X_NEXT(stmt));
    emit_stmt(body);
    RESTORE_LOOP_CONTEXT();

    emit_goto(beg);
    emit_label(STMT_X_NEXT(stmt));
}

static void emit_do_while_stmt(node_t *stmt)
{
    const char *beg = gen_label();
    node_t *cond = STMT_WHILE_COND(stmt);
    node_t *body = STMT_WHILE_BODY(stmt);

    EXPR_X_TRUE(cond) = beg;
    EXPR_X_FALSE(cond) = fall;
    STMT_X_NEXT(body) = STMT_X_NEXT(stmt); 

    emit_label(beg);

    SET_LOOP_CONTEXT(beg, STMT_X_NEXT(stmt));
    emit_stmt(body);
    RESTORE_LOOP_CONTEXT();

    emit_bool_expr(cond);
    emit_label(STMT_X_NEXT(stmt));
}

static void emit_for_stmt(node_t *stmt)
{
    struct vector *decl = STMT_FOR_DECL(stmt);
    node_t *init = STMT_FOR_INIT(stmt);
    node_t *cond = STMT_FOR_COND(stmt);
    node_t *ctrl = STMT_FOR_CTRL(stmt);
    node_t *body = STMT_FOR_BODY(stmt);

    const char *beg = gen_label();
    const char *mid = gen_label();
    
    if (decl)
        emit_decls(decl);
    else if (init)
        emit_expr(init);

    emit_label(beg);

    if (cond) {
        EXPR_X_TRUE(cond) = fall;
        EXPR_X_FALSE(cond) = STMT_X_NEXT(stmt);
        emit_bool_expr(cond);
    }

    SET_LOOP_CONTEXT(mid, STMT_X_NEXT(stmt));
    
    STMT_X_NEXT(body) = mid;
    emit_stmt(body);

    RESTORE_LOOP_CONTEXT();

    emit_label(mid);
    
    if (ctrl)
        emit_expr(ctrl);
    
    emit_goto(beg);
    emit_label(STMT_X_NEXT(stmt));
}

static void emit_switch_jmp(node_t *cond, node_t *case_stmt)
{
    node_t *ty = AST_TYPE(cond);
    bool sign = TYPE_OP(ty) == INT;
    struct operand *cond_operand = EXPR_X_ADDR(cond);
    struct operand *case_operand = make_int_operand(STMT_CASE_INDEX(case_stmt));
    STMT_X_LABEL(case_stmt) = gen_label();

    emit_rel_if(IR_IF_I,
                EQ,
                sign,
                cond_operand, case_operand,
                STMT_X_LABEL(case_stmt),
                ops[TYPE_SIZE(ty)]);
}

static void emit_switch_stmt(node_t *stmt)
{
    node_t *expr = STMT_SWITCH_EXPR(stmt);
    node_t *body = STMT_SWITCH_BODY(stmt);
    struct vector *cases = STMT_SWITCH_CASES(stmt);

    emit_expr(expr);
    
    for (int i = 0; i < vec_len(cases); i++) {
        node_t *case_stmt = vec_at(cases, i);
        emit_switch_jmp(expr, case_stmt);
    }

    node_t *default_stmt = STMT_SWITCH_DEFAULT(stmt);
    if (default_stmt) {
        const char *label = gen_label();
        STMT_X_LABEL(default_stmt) = label;
        emit_goto(label);
    } else {
        emit_goto(STMT_X_NEXT(stmt));
    }

    // set body next
    STMT_X_NEXT(body) = STMT_X_NEXT(stmt);

    SET_SWITCH_CONTEXT(STMT_X_NEXT(stmt));
    emit_stmt(body);
    RESTORE_SWITCH_CONTEXT();
    emit_label(STMT_X_NEXT(stmt));
}

static void emit_case_stmt(node_t *stmt)
{
    node_t *body = STMT_CASE_BODY(stmt);
    emit_label(STMT_X_LABEL(stmt));
    // set body next
    STMT_X_NEXT(body) = STMT_X_NEXT(stmt);
    emit_stmt(body);
}

static void emit_default_stmt(node_t *stmt)
{
    // the same as case_stmt
    emit_case_stmt(stmt);
}

static void emit_label_stmt(node_t *stmt)
{
    node_t *body = STMT_LABEL_BODY(stmt);
    // only emit the label refed.
    if (STMT_LABEL_REFS(stmt))
        emit_label(STMT_X_LABEL(stmt));
    //set body next
    STMT_X_NEXT(body) = STMT_X_NEXT(stmt);
    emit_stmt(body);
}

static void emit_goto_stmt(node_t *stmt)
{
    emit_goto(STMT_X_LABEL(stmt));
}

static void emit_break_stmt(node_t *stmt)
{
    emit_goto(BREAK_CONTEXT);
}

static void emit_continue_stmt(node_t *stmt)
{
    emit_goto(CONTINUE_CONTEXT);
}

// return expr(opt)
static void emit_return_stmt(node_t *stmt)
{
    node_t *n = STMT_RETURN_EXPR(stmt);
    struct operand *result = make_label_operand(func_end_label);
    
    if (!n || isnullstmt(n)) {
        struct tac *tac = make_tac(IR_RETURNI, NULL, NULL, result, ops[Zero]);
        emit_tac(tac);
    } else {
        node_t *ty = AST_TYPE(n);
        int op = isfloat(ty) ? IR_RETURNF : IR_RETURNI;
        emit_expr(n);
        // may be void
        if (EXPR_X_ADDR(n)) {
            struct tac *tac = make_tac(op, EXPR_X_ADDR(n), NULL, result, ops[Zero]);
            emit_tac(tac);
        } else {
            struct tac *tac = make_tac(IR_RETURNI, NULL, NULL, result, ops[Zero]);
            emit_tac(tac);
        }
    }
}

static void emit_expr_stmt(node_t *stmt)
{
    node_t *expr = STMT_EXPR_BODY(stmt);
    emit_expr(expr);
}

static void emit_stmt(node_t *stmt)
{
    switch (AST_ID(stmt)) {
    case COMPOUND_STMT:
        emit_compound_stmt(stmt);
        break;
    case IF_STMT:
        emit_if_stmt(stmt);
        break;
    case WHILE_STMT:
        emit_while_stmt(stmt);
        break;
    case DO_WHILE_STMT:
        emit_do_while_stmt(stmt);
        break;
    case FOR_STMT:
        emit_for_stmt(stmt);
        break;
    case SWITCH_STMT:
        emit_switch_stmt(stmt);
        break;
    case CASE_STMT:
        emit_case_stmt(stmt);
        break;
    case DEFAULT_STMT:
        emit_default_stmt(stmt);
        break;
    case LABEL_STMT:
        emit_label_stmt(stmt);
        break;
    case GOTO_STMT:
        emit_goto_stmt(stmt);
        break;
    case BREAK_STMT:
        emit_break_stmt(stmt);
        break;
    case CONTINUE_STMT:
        emit_continue_stmt(stmt);
        break;
    case RETURN_STMT:
        emit_return_stmt(stmt);
        break;
    case EXPR_STMT:
        emit_expr_stmt(stmt);
        break;
    case NULL_STMT:
        // do nothing
        break;
    default:
        assert(0);
    }
}

static void emit_function(node_t *decl)
{
    node_t *stmt = DECL_BODY(decl);

    func_tac_head = NULL;
    func_tac_tail = NULL;
    extra_lvars = NULL;

    STMT_X_NEXT(stmt) = func_end_label = gen_label();
    emit_stmt(stmt);
    construct_basic_blocks(decl, func_tac_head);
    DECL_X_HEAD(decl) = func_tac_head;
    // add extra local vars
    if (extra_lvars) {
        struct vector *v = DECL_X_LVARS(decl);
        vec_add(v, extra_lvars);
    }
    emit_funcdef_section(decl);
}

static void emit_globalvar(node_t *n)
{
    assert(isdecl(n));
    
    if (DECL_BODY(n))
        emit_data(n);
    else
        emit_bss(n);
}

static void ir_init(void)
{
    tmps = new_table(NULL, GLOBAL);
    labels = new_table(NULL, GLOBAL);
    exts = zmalloc(sizeof(struct externals));
    exts->sections = vec_new();
    exts->strings = map_new();
    exts->compounds = map_new();
    exts->floats = map_new();
}

struct externals * ir(node_t *tree)
{
    assert(istudecl(tree) && errors == 0);

    ir_init();
    struct vector *v = filter_global(DECL_EXTS(tree));
    
    for (int i = 0; i < vec_len(v); i++) {
        node_t *decl = vec_at(v, i);
        if (isfuncdef(decl))
            emit_function(decl);
        else if (isvardecl(decl))
            emit_globalvar(decl);
    }

    return exts;
}

static const char *glabel(const char *label)
{
    if (opts.fleading_underscore)
        return format("_%s", label);
    else
        return label;
}

static struct vector * filter_global(struct vector *v)
{
    struct vector *r = vec_new();
    struct map *map = map_newf(nocmp);
    for (int i = 0; i < vec_len(v); i++) {
        node_t *decl = vec_at(v, i);
        node_t *sym = DECL_SYM(decl);

        SYM_X_LABEL(sym) = glabel(SYM_NAME(sym));

        // skip unused symbols
        if (SYM_SCLASS(sym) == STATIC && SYM_REFS(sym) == 0) {
            // but warning only when top file
            if (is_original_file(AST_SRC(sym).file)) {
                if (isfuncdef(decl))
                    warningf(AST_SRC(sym), "unused function '%s'", SYM_NAME(sym));
                else if (isvardecl(decl))
                    warningf(AST_SRC(sym), "unused variable '%s'", SYM_NAME(sym));
            }
            
            continue;
        }
        
        if (isfuncdef(decl)) {
            vec_push(r, decl);
            vec_add(r, DECL_X_SVARS(decl));
        } else if (isvardecl(decl)) {
            node_t *sym = DECL_SYM(decl);
            if (SYM_SCLASS(sym) == EXTERN)
                continue;
            node_t *decl1 = map_get(map, sym);
            if (decl1) {
                if (DECL_BODY(decl))
                    DECL_BODY(decl1) = DECL_BODY(decl);
            } else {
                vec_push(r, decl);
                map_put(map, sym, decl);
            }
        }
    }
    map_free(map);
    return r;
}

node_t * reduce(node_t *expr)
{
    return expr;
}

//
// decl
//
static struct vector *__xvalues;

#define SET_SECTION_CONTEXT()                   \
    struct vector *__saved_xvalues = __xvalues; \
    __xvalues = vec_new()

#define RESTORE_SECTION_CONTEXT()               \
    __xvalues = __saved_xvalues

#define XVALUES   (__xvalues)

static void emit_initializer(node_t *init);

static struct xvalue * alloc_xvalue(void)
{
    return zmalloc(sizeof(struct xvalue));
}

static struct section * alloc_section(void)
{
    return zmalloc(sizeof(struct section));
}

static void emit_xvalue(int size, const char *name)
{
    struct xvalue *value = alloc_xvalue();
    value->size = size;
    value->name = name;
    vec_push(XVALUES, value);
}

static void emit_zero(size_t bytes)
{
    emit_xvalue(Zero, format("%llu", bytes));
}

static void emit_section(struct section *data)
{
    vec_push(exts->sections, data);
}

static void emit_funcdef_section(node_t *decl)
{
    node_t *sym = DECL_SYM(decl);
    struct section *section = alloc_section();
    section->id = SECTION_TEXT;
    section->global = SYM_SCLASS(sym) == STATIC ? false : true;
    section->label = SYM_X_LABEL(sym);
    section->u.decl = decl;
    emit_section(section);
}

static const char *get_string_literal_label(const char *name)
{
    const char *label = map_get(exts->strings, name);
    if (!label) {
        label = gen_sliteral_label();
        map_put(exts->strings, name, (void *)label);
    }
    return label;
}

static struct section *emit_compound_literal_label(const char *label, node_t *init)
{
    node_t *ty = AST_TYPE(init);
    
    struct section *section = alloc_section();
    section->id = SECTION_DATA;
    section->label = label;
    section->size = TYPE_SIZE(ty);
    section->align = TYPE_ALIGN(ty);

    SET_SECTION_CONTEXT();

    emit_initializer(init);

    section->u.xvalues = XVALUES;

    RESTORE_SECTION_CONTEXT();

    return section;
}

static const char *get_compound_literal_label(node_t *n)
{
    assert(AST_ID(n) == INITS_EXPR);
    
    const char *label = gen_compound_label();
    struct section *section = emit_compound_literal_label(label, n);
    map_put(exts->compounds, label, section);
    return label;
}

static const char *get_ptr_label(node_t *n)
{
    switch (AST_ID(n)) {
    case STRING_LITERAL:
        return get_string_literal_label(SYM_NAME(EXPR_SYM(n)));
    case REF_EXPR:
        return SYM_X_LABEL(EXPR_SYM(n));
    case BINARY_OPERATOR:
        return get_ptr_label(EXPR_OPERAND(n, 0));
    case UNARY_OPERATOR:
        assert(EXPR_OP(n) == '&');
        return get_ptr_label(EXPR_OPERAND(n, 0));
    case INITS_EXPR:
        return get_compound_literal_label(n);
    default:
        assert(0);
    }
}

static void emit_struct_initializer(node_t *n)
{
    assert(AST_ID(n) == INITS_EXPR);
    node_t *ty = AST_TYPE(n);
    struct vector *fields = TYPE_FIELDS(ty);
    struct vector *inits = EXPR_INITS(n);
    for (int i = 0; i < vec_len(inits); i++) {
        node_t *init = vec_at(inits, i);
        node_t *field = vec_at(fields, i);
        size_t offset = FIELD_OFFSET(field);
        if (FIELD_ISBIT(field)) {
            int old_bits = 0;
            unsigned long long old_byte = 0;
            for (; i < vec_len(inits); i++) {
                node_t *next;
                if (i < vec_len(inits) - 1)
                    next = vec_at(fields, i+1);
                else
                    next = NULL;
                field = vec_at(fields, i);
                init = vec_at(inits, i);
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
                    emit_xvalue(Byte, format("%d", val));
                    offset += 1;
                }
                old_bits += bits;
                old_byte += byte;
            }
            if (old_bits) {
                unsigned char r = old_byte & ~(~0 << old_bits);
                emit_xvalue(Byte, format("%d", r));
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
        node_t *next;
        if (i < vec_len(inits) - 1)
            next = vec_at(fields, i+1);
        else
            next = NULL;
        size_t end;
        if (next)
            end = FIELD_OFFSET(next);
        else
            end = TYPE_SIZE(ty);
        if (end - offset)
            emit_zero(end - offset);
    }
}

static void emit_array_initializer(node_t *n)
{
    if (issliteral(n)) {
        const char *label = get_ptr_label(n);
        emit_xvalue(Quad, label);
    } else {
        assert(AST_ID(n) == INITS_EXPR);
        int i;
        for (i = 0; i < vec_len(EXPR_INITS(n)); i++) {
            node_t *init = vec_at(EXPR_INITS(n), i);
            if (AST_ID(init) == VINIT_EXPR)
                emit_zero(TYPE_SIZE(rtype(AST_TYPE(n))));
            else
                emit_initializer(init);
        }
        int left = TYPE_LEN(AST_TYPE(n)) - i;
        if (left > 0)
            emit_zero(left * TYPE_SIZE(rtype(AST_TYPE(n))));
    }
}

static void emit_address_initializer(node_t *init)
{
    node_t *ty = AST_TYPE(init);
    if (isiliteral(init)) {
        emit_xvalue(Quad, format("%llu", ILITERAL_VALUE(init)));
    } else {
        const char *label = get_ptr_label(init);
        if (AST_ID(init) == BINARY_OPERATOR) {
            node_t *r = EXPR_OPERAND(init, 1);
            int op = EXPR_OP(init);
            size_t size = TYPE_SIZE(rtype(ty));
            if (op == '+') {
                if (TYPE_OP(AST_TYPE(r)) == INT) {
                    long long i = ILITERAL_VALUE(r);
                    if (i < 0)
                        emit_xvalue(Quad,
                                    format("%s%lld", label, i * size));
                    else
                        emit_xvalue(Quad,
                                    format("%s+%lld", label, i * size));
                } else {
                    emit_xvalue(Quad,
                                format("%s+%llu", label, ILITERAL_VALUE(r) * size));
                }
            } else {
                if (TYPE_OP(AST_TYPE(r)) == INT) {
                    long long i = ILITERAL_VALUE(r);
                    if (i < 0)
                        emit_xvalue(Quad,
                                    format("%s+%lld", label, -i * size));
                    else
                        emit_xvalue(Quad,
                                    format("%s-%lld", label, i * size));
                } else {
                    emit_xvalue(Quad,
                                format("%s-%llu", label, ILITERAL_VALUE(r) * size));
                }
            }
        } else {
            emit_xvalue(Quad, label);
        }
    }
}

static void emit_arith_initializer(node_t *init)
{
    node_t *ty = AST_TYPE(init);
    switch (TYPE_KIND(ty)) {
    case _BOOL:
    case CHAR:
        emit_xvalue(Byte, format("%d", ILITERAL_VALUE(init)));
        break;
    case SHORT:
        emit_xvalue(Word, format("%d", ILITERAL_VALUE(init)));
        break;
    case INT:
    case UNSIGNED:
        emit_xvalue(Long, format("%d", ILITERAL_VALUE(init)));
        break;
    case LONG:
    case LONG+LONG:
        emit_xvalue(Quad, format("%llu", ILITERAL_VALUE(init)));
        break;
    case FLOAT:
        {
            float f = FLITERAL_VALUE(init);
            emit_xvalue(Long, format("%u", *(uint32_t *)&f));
        }
        break;
    case DOUBLE:
    case LONG+DOUBLE:
        {
            double d = FLITERAL_VALUE(init);
            emit_xvalue(Quad, format("%llu", *(uint64_t *)&d));
        }
        break;
    default:
        die("unknown type '%s'", type2s(ty));
        break;
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
    else if (isrecord(ty))
        emit_struct_initializer(init);
    else
        die("unexpected initializer type: %s", type2s(ty));
}

static void set_section_basic(struct section *section, node_t *decl)
{
    node_t *sym = DECL_SYM(decl);
    node_t *ty = SYM_TYPE(sym);
    
    section->global = SYM_SCLASS(sym) == STATIC ? false : true;
    section->label = SYM_X_LABEL(sym);
    section->size = TYPE_SIZE(ty);
    section->align = TYPE_ALIGN(ty);
}

static void emit_bss(node_t *decl)
{
    struct section *section = alloc_section();
    section->id = SECTION_BSS;
    set_section_basic(section, decl);
    emit_section(section);
}

static void emit_data(node_t *decl)
{
    struct section *section = alloc_section();
    section->id = SECTION_DATA;
    set_section_basic(section, decl);
    
    // enter context
    SET_SECTION_CONTEXT();

    emit_initializer(DECL_BODY(decl));
    emit_section(section);

    section->u.xvalues = XVALUES;
    
    // exit context
    RESTORE_SECTION_CONTEXT();
}
