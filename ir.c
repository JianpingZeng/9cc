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
static void emit_funcdef_gdata(node_t *decl);
static const char *get_string_literal_label(const char *name);
static void emit_bop_ptr_int(unsigned op, node_t *ptr, node_t *i, node_t *n);
static void emit_assign(node_t *ty, struct operand *l, node_t *r);

static struct vector *func_tacs;
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
    cc_assert(op >= IR_NONE && op < IR_END);
    return rops[op];
}

static void emit_tac(struct tac *tac)
{
    vec_push(func_tacs, tac);
}

static struct operand * make_sym_operand(node_t *sym)
{
    struct operand *operand = zmalloc(sizeof(struct operand));
    operand->sym = sym;
    return operand;
}

static struct operand * make_named_operand(const char *name, struct table **table, int scope)
{
    node_t *sym = lookup(name, *table);
    if (!sym)
        sym = install(name, table, scope);
    return make_sym_operand(sym);
}

static struct operand * make_tmp_operand(void)
{
    struct operand *operand = make_named_operand(gen_tmpname_r(), &tmps, GLOBAL);
    SYM_X_KIND(operand->sym) = SYM_KIND_TMP;
    return operand;
}

static struct operand * make_label_operand(const char *label)
{
    struct operand *operand = make_named_operand(label, &labels, GLOBAL);
    SYM_X_KIND(operand->sym) = SYM_KIND_LABEL;
    return operand;
}

static struct operand * make_int_operand(long long i)
{
    struct operand *operand = make_named_operand(strd(i), &constants, CONSTANT);
    SYM_VALUE_I(operand->sym) = i;
    SYM_X_KIND(operand->sym) = SYM_KIND_LITERAL;
    return operand;
}

static struct operand * make_unsigned_operand(unsigned long long u)
{
    struct operand *operand = make_named_operand(stru(u), &constants, CONSTANT);
    SYM_VALUE_U(operand->sym) = u;
    SYM_X_KIND(operand->sym) = SYM_KIND_LITERAL;
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

static struct operand * make_subscript_operand(struct operand *array, struct operand *index)
{
    struct operand *operand = make_sym_operand(array->sym);
    operand->index = index->sym;
    operand->op = IR_SUBSCRIPT;
    return operand;
}

static struct operand * make_indirection_operand(node_t *sym)
{
    struct operand *operand = make_sym_operand(sym);
    operand->op = IR_INDIRECTION;
    return operand;
}

static struct operand * make_address_operand(node_t *sym)
{
    struct operand *operand = make_sym_operand(sym);
    operand->op = IR_ADDRESS;
    return operand;
}

static struct tac * make_tac(unsigned op,
                             struct operand *l, struct operand *r,
                             struct operand *result,
                             unsigned opsize)
{
    struct tac *tac = zmalloc(sizeof(struct tac));
    tac->op = op;
    tac->args[0] = l;
    tac->args[1] = r;
    tac->result = result;
    tac->opsize = opsize;
    return tac;
}

static struct tac * make_tac_r(unsigned op,
                               struct operand *l, struct operand *r,
                               unsigned opsize)
{
    return make_tac(op, l, r, make_tmp_operand(), opsize);
}

static struct tac * make_tac_nor(unsigned op,
                                 struct operand *l, struct operand *r,
                                 unsigned opsize)
{
    return make_tac(op, l, r, NULL, opsize);
}

static struct tac * make_assign_tac(unsigned op,
                                    struct operand *l, struct operand *r,
                                    unsigned opsize)
{
    return make_tac(op, r, NULL, l, opsize);
}

static void emit_simple_if(unsigned op, struct operand *operand,
                           const char *label, unsigned opsize)
{
    struct tac *tac = make_tac(op, operand, NULL, make_label_operand(label), opsize);
    emit_tac(tac);
}

static void emit_rel_if(unsigned op,
                        unsigned relop,
                        struct operand *rel_l, struct operand *rel_r,
                        const char *label,
                        unsigned opsize)
{
    struct tac *tac = make_tac(op, rel_l, rel_r, make_label_operand(label), opsize);
    tac->relop = relop;
    emit_tac(tac);
}

static void emit_label(const char *label)
{
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

static struct tac * make_call_tac(struct operand *l, int args)
{
    struct tac *tac = make_tac(IR_CALL, l, NULL, NULL, Quad);
    tac->relop = args;
    return tac;
}

static struct tac * make_conv_tac(unsigned op, struct operand *l,
                                  unsigned from_opszie, unsigned to_opsize)
{
    struct tac *tac = make_tac_r(op, l, NULL, Zero);
    tac->from_opsize = from_opszie;
    tac->to_opsize = to_opsize;
    return tac;
}

static struct operand * emit_conv_tac(int op, struct operand *l,
                                      unsigned from_opsize,
                                      unsigned to_opsize)
{
    struct tac *tac = make_conv_tac(op, l, from_opsize, to_opsize);
    emit_tac(tac);
    return tac->result;
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
    emit_expr(init);
    emit_assign(SYM_TYPE(sym), l, init);
}

static void emit_decls(node_t **decls)
{
    for (int i = 0; i < LIST_LEN(decls); i++)
        emit_decl(decls[i]);
}

static int uop2rop(int op)
{
    switch (op) {
    case '~':
        return IR_NOT;
    case '&':
        return IR_ADDRESS;
    default:
        cc_assert(0);
    }
}

// int
static void emit_uop_bitwise_not(node_t *n)
{
    int op = EXPR_OP(n);
    node_t *l = EXPR_OPERAND(n, 0);

    emit_expr(l);
    struct tac *tac = make_tac_r(uop2rop(op),
                                 EXPR_X_ADDR(l), NULL,
                                 ops[TYPE_SIZE(AST_TYPE(n))]);
    emit_tac(tac);
    EXPR_X_ADDR(n) = tac->result;
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
    unsigned op = isint(ty) ? IR_MINUSI : IR_MINUSF;

    emit_expr(l);

    struct tac *tac = make_tac_r(op,
                                 EXPR_X_ADDR(l), NULL,
                                 ops[TYPE_SIZE(ty)]);
    emit_tac(tac);
    EXPR_X_ADDR(n) = tac->result;
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

    EXPR_X_ADDR(n) = make_indirection_operand(EXPR_X_ADDR(l)->sym);
}

// lvalue
static void emit_uop_address(node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);

    emit_expr(l);

    EXPR_X_ADDR(n) = make_address_operand(EXPR_X_ADDR(l)->sym);
}

// ptr + int
static struct operand * emit_ptr_int(unsigned op,
                                     struct operand *l,
                                     struct operand *result,
                                     struct operand *index,
                                     size_t step,
                                     unsigned opsize)
{
    struct tac *tac = make_tac_r(IR_MULI, index, make_unsigned_operand(step), opsize);
    emit_tac(tac);
    struct tac *tac2 = make_tac(op, l, tac->result, result, opsize);
    emit_tac(tac2);
    return tac2->result;
}

// scalar
static void emit_uop_increment(node_t *n, int op)
{
    bool prefix = EXPR_PREFIX(n);
    node_t *l = EXPR_OPERAND(n, 0);
    node_t *ty = AST_TYPE(n);
    unsigned opsize = ops[TYPE_SIZE(ty)];
    int rop;
    if (op == INCR) {
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
    
    if (prefix) {
        if (isptr(ty)) {
            EXPR_X_ADDR(n) = emit_ptr_int(rop,
                                          EXPR_X_ADDR(l),
                                          EXPR_X_ADDR(l),
                                          make_operand_one(),
                                          TYPE_SIZE(rtype(ty)),
                                          opsize);
        } else {
            struct tac *tac = make_tac(rop,
                                       EXPR_X_ADDR(l),
                                       make_operand_one(),
                                       EXPR_X_ADDR(l),
                                       opsize);
            emit_tac(tac);
            EXPR_X_ADDR(n) = EXPR_X_ADDR(l);
        }
    } else {
        struct operand *tmp = make_tmp_operand();
        emit_tac(make_assign_tac(IR_ASSIGNI, tmp, EXPR_X_ADDR(l), opsize));
        if (isptr(ty)) {
            emit_ptr_int(rop,
                         EXPR_X_ADDR(l),
                         EXPR_X_ADDR(l),
                         make_operand_one(),
                         TYPE_SIZE(rtype(ty)),
                         opsize);
        } else {
            
            struct tac *tac = make_tac(rop,
                                       EXPR_X_ADDR(l),
                                       make_operand_one(),
                                       EXPR_X_ADDR(l),
                                       opsize);
            emit_tac(tac);
        }
        EXPR_X_ADDR(n) = tmp;
    }
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
        emit_uop_increment(n, INCR);
        break;
    case DECR:
        emit_uop_increment(n, DECR);
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
        cc_assert(0);
    }
}

static unsigned bop2rop(int op, node_t *ty)
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
        return IR_MOD;
    case '|':
        return IR_OR;
    case '&':
        return IR_AND;
    case '^':
        return IR_XOR;
    case LSHIFT:
        return IR_LSHIFT;
    case RSHIFT:
        return IR_RSHIFT;
    default:
        cc_assert(0);
    }
}

static void emit_assign_struct(node_t *ty, struct operand *l, node_t *r)
{
    // TODO: 
}

static void emit_assign_array(node_t *ty, struct operand *l, node_t *r)
{
    // TODO: 
}

static void emit_assign_scalar(node_t *ty, struct operand *l, node_t *r)
{
    // TODO:
    unsigned op = isfloat(ty) ? IR_ASSIGNF : IR_ASSIGNI;
    unsigned opsize = ops[TYPE_SIZE(ty)];
    struct tac *tac = make_assign_tac(op, l, EXPR_X_ADDR(r), opsize);
    emit_tac(tac);
}

static void emit_assign(node_t *ty, struct operand *l, node_t *r)
{
    cc_assert(ty);
    
    if (isstruct(ty) || isunion(ty))
        emit_assign_struct(ty, l, r);
    else if (isarray(ty))
        emit_assign_array(ty, l, r);
    else
        emit_assign_scalar(ty, l, r);
}

static void emit_bop_assign(node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);
    node_t *r = EXPR_OPERAND(n, 1);

    emit_expr(l);
    emit_expr(r);
    emit_assign(AST_TYPE(l), EXPR_X_ADDR(l), r);
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
    unsigned opsize = ops[TYPE_SIZE(AST_TYPE(n))];
    EXPR_X_TRUE(n) = fall;
    EXPR_X_FALSE(n) = gen_label();
    const char *label = gen_label();
    struct operand *result = make_tmp_operand();
    emit_bool_expr(n);
    // true
    emit_tac(make_assign_tac(IR_ASSIGNI, result, make_operand_one(), opsize));
    emit_goto(label);
    emit_label(EXPR_X_FALSE(n));
    // false
    emit_tac(make_assign_tac(IR_ASSIGNI, result, make_operand_zero(), opsize));
    emit_label(label);
    EXPR_X_ADDR(n) = result;
}

// arith op arith
static void emit_bop_arith(node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);
    node_t *r = EXPR_OPERAND(n, 1);
    node_t *ty = AST_TYPE(n);
    unsigned op = bop2rop(EXPR_OP(n), ty);
    unsigned opsize = ops[TYPE_SIZE(ty)];

    emit_expr(l);
    emit_expr(r);

    struct tac *tac = make_tac_r(op, EXPR_X_ADDR(l), EXPR_X_ADDR(r), opsize);
    emit_tac(tac);
    EXPR_X_ADDR(n) = tac->result;
}

static void emit_bop_ptr_int(unsigned op, node_t *ptr, node_t *index, node_t *n)
{
    node_t *rty = rtype(AST_TYPE(ptr));
    
    EXPR_X_ADDR(n) =  emit_ptr_int(op,
                                   EXPR_X_ADDR(ptr),
                                   make_tmp_operand(),
                                   EXPR_X_ADDR(index),
                                   TYPE_SIZE(rty),
                                   ops[TYPE_SIZE(ptr)]);
}

// arith + arith
// ptr + int
// int + ptr
static void emit_bop_plus(node_t *n)
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
        emit_bop_ptr_int(IR_ADDI, ptr, i, n);
    }
}

// arith - arith
// ptr - int
static void emit_bop_minus(node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);
    node_t *r = EXPR_OPERAND(n, 1);

    if (isarith(AST_TYPE(l)) && isarith(AST_TYPE(r))) {
        emit_bop_arith(n);
    } else {
        emit_expr(l);
        emit_expr(r);

        emit_bop_ptr_int(IR_SUBI, l, r, n);
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
        emit_bop_plus(n);
        break;
    case '-':
        emit_bop_minus(n);
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
        cc_assert(0);
    }
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
    struct operand *result = make_tmp_operand();
    emit_bool_expr(cond);
    emit_expr(then);
    emit_assign(AST_TYPE(n), result, then);
    emit_goto(label);
    emit_label(EXPR_X_FALSE(cond));
    emit_expr(els);
    emit_assign(AST_TYPE(n), result, els);
    emit_label(label);
    EXPR_X_ADDR(n) = result;
}

static void emit_member(node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);
    const char *name = AST_NAME(n);
    node_t *ty;

    if (isrecord(AST_TYPE(l)))
        ty = AST_TYPE(l);
    else
        ty = rtype(AST_TYPE(l));

    node_t *field = find_field(ty, name);

    emit_expr(l);
    struct operand *index = make_unsigned_operand(FIELD_OFFSET(field));
    EXPR_X_ADDR(n) = make_subscript_operand(EXPR_X_ADDR(l), index);
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

    if (addr->op == IR_SUBSCRIPT) {
        struct operand *size = make_unsigned_operand(TYPE_SIZE(rty));
        struct tac *tac = make_tac_r(IR_IMULI, EXPR_X_ADDR(i), size, Quad);
        emit_tac(tac);
        tac = make_tac_r(IR_ADDI, tac->result, make_sym_operand(addr->index), Quad);
        emit_tac(tac);
        EXPR_X_ADDR(n) = make_subscript_operand(addr, tac->result);
    } else {
        struct operand *size = make_unsigned_operand(TYPE_SIZE(rty));
        struct tac *tac = make_tac_r(IR_IMULI, EXPR_X_ADDR(i), size, Quad);
        emit_tac(tac);
        EXPR_X_ADDR(n) = make_subscript_operand(addr, tac->result);
    }
}

static void emit_inits(node_t *n)
{
    cc_assert(AST_ID(n) == INITS_EXPR);

    node_t **inits = EXPR_INITS(n);
}

static void int2int(node_t *dty, node_t *sty, node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);
    unsigned op;
    
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

static void int2float(node_t *dty, node_t *sty, node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);
    unsigned op = TYPE_OP(sty) == UNSIGNED ? IR_CONV_UI_F : IR_CONV_SI_F;

    EXPR_X_ADDR(n) = emit_conv_tac(op,
                                   EXPR_X_ADDR(l),
                                   ops[TYPE_SIZE(sty)],
                                   ops[TYPE_SIZE(dty)]);
}

static void float2int(node_t *dty, node_t *sty, node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);
    unsigned op = TYPE_OP(dty) == UNSIGNED ? IR_CONV_F_UI : IR_CONV_F_SI;

    EXPR_X_ADDR(n) = emit_conv_tac(op,
                                   EXPR_X_ADDR(l),
                                   ops[TYPE_SIZE(sty)],
                                   ops[TYPE_SIZE(dty)]);
}

static void float2float(node_t *dty, node_t *sty, node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);
    EXPR_X_ADDR(n) = emit_conv_tac(IR_CONV_FF,
                                   EXPR_X_ADDR(l),
                                   ops[TYPE_SIZE(sty)],
                                   ops[TYPE_SIZE(dty)]);
}

static void arith2arith(node_t *dty, node_t *sty, node_t *n)
{
    if (eqarith(sty, dty)) {
        node_t *l = EXPR_OPERAND(n, 0);
        EXPR_X_ADDR(n) = EXPR_X_ADDR(l);
    } else {
        if (isint(sty) && isint(dty))
            int2int(dty, sty, n);
        else if (isint(sty) && isfloat(dty))
            int2float(dty, sty, n);
        else if (isfloat(sty) && isint(dty))
            float2int(dty, sty, n);
        else if (isfloat(sty) && isfloat(dty))
            float2float(dty, sty, n);
        else
            cc_assert(0);
    }
}

static void ptr2arith(node_t *dty, node_t *sty, node_t *n)
{
    cc_assert(isint(dty));

    arith2arith(dty, unsignedlongtype, n);
}

static void arith2ptr(node_t *dty, node_t *sty, node_t *n)
{
    cc_assert(isint(sty));

    arith2arith(unsignedlongtype, sty, n);
}

static inline void ptr2ptr(node_t *dty, node_t *sty, node_t *n)
{
   node_t *l = EXPR_OPERAND(n, 0);
   EXPR_X_ADDR(n) = EXPR_X_ADDR(l);
}

static inline void func2ptr(node_t *dty, node_t *sty, node_t *n)
{
   node_t *l = EXPR_OPERAND(n, 0);
   EXPR_X_ADDR(n) = EXPR_X_ADDR(l);
}

static inline void array2ptr(node_t *dty, node_t *sty, node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);
    EXPR_X_ADDR(n) = make_address_operand(EXPR_X_ADDR(l)->sym);
}

static void emit_conv(node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);
    node_t *dty = AST_TYPE(n);
    node_t *sty = AST_TYPE(l);

    emit_expr(l);

    if (isarith(dty)) {
        if (isarith(sty))
            arith2arith(dty, sty, n);
        else if (isptr(sty))
            ptr2arith(dty, sty, n);
        else
            cc_assert(0);
    } else if (isptr(dty)) {
        if (isptr(sty))
            ptr2ptr(dty, sty, n);
        else if (isarith(sty))
            arith2ptr(dty, sty, n);
        else if (isfunc(sty))
            func2ptr(dty, sty, n);
        else if (isarray(sty))
            array2ptr(dty, sty, n);
        else
            cc_assert(0);
    } else {
        // nothing
        EXPR_X_ADDR(n) = EXPR_X_ADDR(l);
    }
}

static void emit_call(node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);
    node_t **args = EXPR_ARGS(n);
    int len = LIST_LEN(args);
    node_t *ty = AST_TYPE(n);

    emit_expr(l);

    for (int i = 0; i < len; i++) {
        node_t *arg = args[i];
        emit_expr(arg);
    }

    // in reverse order
    for (int i = len - 1; i >= 0; i--) {
        node_t *arg = args[i];
        emit_param(EXPR_X_ADDR(arg));
    }

    if (isvoid(ty)) {
        struct tac *tac = make_call_tac(EXPR_X_ADDR(l), len);
        emit_tac(tac);
    } else {
        struct tac *tac = make_call_tac(EXPR_X_ADDR(l), len);
        tac->result = make_tmp_operand();
        emit_tac(tac);
        EXPR_X_ADDR(n) = tac->result;
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
    SYM_X_KIND(sym) = SYM_KIND_LITERAL;
    EXPR_X_ADDR(n) = make_sym_operand(sym);
}

static const char *get_float_label(const char *name)
{
    const char *label = dict_get(exts->floats, name);
    if (!label) {
        label = gen_sliteral_label();
        dict_put(exts->floats, name, (void *)label);
    }
    return label;
}

static void emit_float_literal(node_t *n)
{
    node_t *sym = EXPR_SYM(n);
    const char *label = get_float_label(SYM_NAME(sym));
    SYM_X_LABEL(sym) = label;
    SYM_X_KIND(sym) = SYM_KIND_LITERAL;
    EXPR_X_ADDR(n) = make_indirection_operand(sym);
}

static void emit_string_literal(node_t *n)
{
    node_t *sym = EXPR_SYM(n);
    const char *label = get_string_literal_label(SYM_NAME(sym));
    SYM_X_LABEL(sym) = label;
    SYM_X_KIND(sym) = SYM_KIND_REF;
    EXPR_X_ADDR(n) = make_sym_operand(sym);
}

static void emit_compound_literal(node_t *n)
{
    node_t *sym = EXPR_SYM(n);
    SYM_X_KIND(sym) = SYM_KIND_REF;
    EXPR_X_ADDR(n) = make_sym_operand(sym);
    
    node_t *l = EXPR_OPERAND(n, 0);
    emit_expr(l);
    emit_assign(AST_TYPE(n), EXPR_X_ADDR(n), l);
}

static void emit_ref(node_t *n)
{
    node_t *sym = EXPR_SYM(n);
    SYM_X_KIND(sym) = SYM_KIND_REF;
    EXPR_X_ADDR(n) = make_sym_operand(sym);
}

static void emit_expr(node_t *n)
{
    cc_assert(isexpr(n));

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
        emit_inits(n);
        break;
    case VINIT_EXPR:
        // do nothing
        break;
    default:
        cc_assert(0);
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
    unsigned opsize = ops[TYPE_SIZE(ty)];

    emit_expr(l);
    emit_expr(r);

    if (EXPR_X_TRUE(n) != fall && EXPR_X_FALSE(n) != fall) {
        
        unsigned op = isfloat(ty) ? IR_IF_F : IR_IF_I;
        emit_rel_if(op,
                    relop, EXPR_X_ADDR(l), EXPR_X_ADDR(r),
                    EXPR_X_TRUE(n),
                    opsize);
        emit_goto(EXPR_X_FALSE(n));
        
    } else if (EXPR_X_TRUE(n) != fall) {

        unsigned op = isfloat(ty) ? IR_IF_F : IR_IF_I;
        emit_rel_if(op,
                    relop, EXPR_X_ADDR(l), EXPR_X_ADDR(r),
                    EXPR_X_TRUE(n),
                    opsize);
        
    } else if (EXPR_X_FALSE(n) != fall) {

        unsigned op = isfloat(ty) ? IR_IF_FALSE_F : IR_IF_FALSE_I;
        emit_rel_if(op,
                    relop, EXPR_X_ADDR(l), EXPR_X_ADDR(r),
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
        unsigned opsize = ops[TYPE_SIZE(ty)];
        struct operand *test = EXPR_X_ADDR(n);
        
        if (EXPR_X_TRUE(n) != fall && EXPR_X_FALSE(n) != fall) {
            
            unsigned op = isfloat(ty) ? IR_IF_F : IR_IF_I;
            emit_simple_if(op, test, EXPR_X_TRUE(n), opsize);
            emit_goto(EXPR_X_FALSE(n));
            
        } else if (EXPR_X_TRUE(n) != fall) {
            
            unsigned op = isfloat(ty) ? IR_IF_F : IR_IF_I;
            emit_simple_if(op, test, EXPR_X_TRUE(n), opsize);
            
        } else if (EXPR_X_FALSE(n) != fall) {
            
            unsigned op = isfloat(ty) ? IR_IF_FALSE_F : IR_IF_FALSE_I;
            emit_simple_if(op, test, EXPR_X_FALSE(n), opsize);
            
        } else {
            // both fall: do nothing
        }
    }
}

static void emit_compound_stmt(node_t *stmt)
{
    node_t **blks = STMT_BLKS(stmt);
    for (int i = 0; i < LIST_LEN(blks); i++) {
        node_t *node = blks[i];
        if (isdecl(node)) {
            emit_decl(node);
        } else if (isstmt(node)) {
            STMT_X_NEXT(node) = gen_label();
            emit_stmt(node);
        } else if (isexpr(node)) {
            emit_expr(node);
        } else {
            cc_assert(0);
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
    node_t **decl = STMT_FOR_DECL(stmt);
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

    SET_LOOP_CONTEXT(beg, STMT_X_NEXT(stmt));
    
    STMT_X_NEXT(body) = STMT_X_NEXT(stmt);
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
    struct operand *cond_operand = EXPR_X_ADDR(cond);
    struct operand *case_operand = make_int_operand(STMT_CASE_INDEX(case_stmt));
    STMT_X_LABEL(case_stmt) = gen_label();

    emit_rel_if(IR_IF_I,
                EQ, cond_operand, case_operand,
                STMT_X_LABEL(case_stmt),
                ops[TYPE_SIZE(ty)]);
}

static void emit_switch_stmt(node_t *stmt)
{
    node_t *expr = STMT_SWITCH_EXPR(stmt);
    node_t *body = STMT_SWITCH_BODY(stmt);
    node_t **cases = STMT_SWITCH_CASES(stmt);

    emit_expr(expr);
    
    for (int i = 0; i < LIST_LEN(cases); i++) {
        node_t *case_stmt = cases[i];
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

    SET_SWITCH_CONTEXT(STMT_X_NEXT(stmt));
    emit_stmt(body);
    RESTORE_SWITCH_CONTEXT();
    emit_label(STMT_X_NEXT(stmt));
}

static void emit_case_stmt(node_t *stmt)
{
    node_t *body = STMT_CASE_BODY(stmt);
    emit_label(STMT_X_LABEL(stmt));
    emit_stmt(body);
}

static void emit_default_stmt(node_t *stmt)
{
    // the same as case_stmt
    emit_case_stmt(stmt);
}

static void emit_label_stmt(node_t *stmt)
{
    emit_label(STMT_X_LABEL(stmt));
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

static void emit_return_stmt(node_t *stmt)
{
    node_t *n = STMT_RETURN_EXPR(stmt);
    node_t *ty = AST_TYPE(n);
    unsigned op = isfloat(ty) ? IR_RETURNF : IR_RETURNI;
    
    emit_expr(n);
    
    struct tac *tac = make_tac_nor(op,
                                   EXPR_X_ADDR(n),
                                   NULL,
                                   ops[TYPE_SIZE(ty)]);
    emit_tac(tac);
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
    case NULL_STMT:
        // do nothing
        break;
    default:
        emit_expr(stmt);
        break;
    }
}

static void emit_function(node_t *decl)
{
    node_t *stmt = DECL_BODY(decl);

    func_tacs = vec_new();

    STMT_X_NEXT(stmt) = gen_label();
    emit_stmt(stmt);
    DECL_X_TACS(decl) = func_tacs;
    emit_funcdef_gdata(decl);
}

static void emit_globalvar(node_t *n)
{
    cc_assert(isdecl(n));
    
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
    exts->gdatas = vec_new();
    exts->strings = dict_new();
    exts->compounds = dict_new();
    exts->floats = dict_new();
}

static const char *glabel(const char *label)
{
    if (opts.fleading_underscore)
        return format("_%s", label);
    else
        return label;
}

struct externals * ir(node_t *tree)
{
    cc_assert(istudecl(tree) && errors == 0);

    ir_init();
    for (int i = 0; i < LIST_LEN(DECL_EXTS(tree)); i++) {
        node_t *decl = DECL_EXTS(tree)[i];
        node_t *sym = DECL_SYM(decl);

        SYM_X_LABEL(sym) = glabel(SYM_NAME(sym));
        
        if (isfuncdef(decl))
            emit_function(decl);
        else if (isvardecl(decl))
            emit_globalvar(decl);
    }

    return exts;
}

node_t * reduce(node_t *expr)
{
    return expr;
}

//
// decl
//
static struct vector *__xvalues;

#define SET_GDATA_CONTEXT()                     \
    struct vector *__saved_xvalues = __xvalues; \
    __xvalues = vec_new()

#define RESTORE_GDATA_CONTEXT()                 \
    vec_free(__xvalues);                        \
    __xvalues = __saved_xvalues

#define XVALUES   (__xvalues)

static void emit_initializer(node_t *init);

static inline struct xvalue * alloc_xvalue(void)
{
    return zmalloc(sizeof(struct xvalue));
}

static inline gdata_t * alloc_gdata(void)
{
    return zmalloc(sizeof(gdata_t));
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

static void emit_gdata(gdata_t *data)
{
    vec_push(exts->gdatas, data);
}

static void emit_funcdef_gdata(node_t *decl)
{
    node_t *sym = DECL_SYM(decl);
    gdata_t *gdata = alloc_gdata();
    GDATA_ID(gdata) = GDATA_TEXT;
    GDATA_GLOBAL(gdata) = SYM_SCLASS(sym) == STATIC ? false : true;
    GDATA_LABEL(gdata) = SYM_X_LABEL(sym);
    GDATA_TEXT_DECL(gdata) = decl;
    emit_gdata(gdata);
}

static const char *get_string_literal_label(const char *name)
{
    const char *label = dict_get(exts->strings, name);
    if (!label) {
        label = gen_sliteral_label();
        dict_put(exts->strings, name, (void *)label);
    }
    return label;
}

static gdata_t *emit_compound_literal_label(const char *label, node_t *init)
{
    node_t *ty = AST_TYPE(init);
    
    gdata_t *gdata = alloc_gdata();
    GDATA_ID(gdata) = GDATA_DATA;
    GDATA_LABEL(gdata) = label;
    GDATA_SIZE(gdata) = TYPE_SIZE(ty);
    GDATA_ALIGN(gdata) = TYPE_ALIGN(ty);

    SET_GDATA_CONTEXT();

    emit_initializer(init);

    GDATA_DATA_XVALUES(gdata) = (struct xvalue **)vtoa(XVALUES);

    RESTORE_GDATA_CONTEXT();

    return gdata;
}

static const char *get_compound_literal_label(node_t *n)
{
    cc_assert(AST_ID(n) == INITS_EXPR);
    
    const char *label = gen_compound_label();
    gdata_t *gdata = emit_compound_literal_label(label, n);
    dict_put(exts->compounds, label, gdata);
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
        cc_assert(EXPR_OP(n) == '&');
        return get_ptr_label(EXPR_OPERAND(n, 0));
    case INITS_EXPR:
        return get_compound_literal_label(n);
    default:
        cc_assert(0);
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
                node_t *next = i < LIST_LEN(inits) - 1 ? fields[i + 1] : NULL;
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

static void emit_array_initializer(node_t *n)
{
    if (issliteral(n)) {
        const char *label = get_ptr_label(n);
        emit_xvalue(Quad, label);
    } else {
        cc_assert(AST_ID(n) == INITS_EXPR);
        int i;
        for (i = 0; i < LIST_LEN(EXPR_INITS(n)); i++)
            emit_initializer(EXPR_INITS(n)[i]);
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

static void set_gdata_basic(gdata_t *gdata, node_t *decl)
{
    node_t *sym = DECL_SYM(decl);
    node_t *ty = SYM_TYPE(sym);
    
    GDATA_GLOBAL(gdata) = SYM_SCLASS(sym) == STATIC ? false : true;
    GDATA_LABEL(gdata) = SYM_X_LABEL(sym);
    GDATA_SIZE(gdata) = TYPE_SIZE(ty);
    GDATA_ALIGN(gdata) = TYPE_ALIGN(ty);
}

static void emit_bss(node_t *decl)
{
    gdata_t *gdata = alloc_gdata();
    GDATA_ID(gdata) = GDATA_BSS;
    set_gdata_basic(gdata, decl);
    emit_gdata(gdata);
}

static void emit_data(node_t *decl)
{
    gdata_t *gdata = alloc_gdata();
    GDATA_ID(gdata) = GDATA_DATA;
    set_gdata_basic(gdata, decl);
    
    // enter context
    SET_GDATA_CONTEXT();

    emit_initializer(DECL_BODY(decl));
    emit_gdata(gdata);

    GDATA_DATA_XVALUES(gdata) = (struct xvalue **)vtoa(XVALUES);
    
    // exit context
    RESTORE_GDATA_CONTEXT();
}
