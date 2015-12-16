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

static struct vector *func_irs;
static struct vector *staticvars;
static struct vector *localvars;
static struct table *tmps;
static struct table *labels;
static const char *fall = (const char *)&fall;
static const char *__continue;
static const char *__break;
static struct operand *true_operand;
static struct operand *false_operand;

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

const char *rop2s(int op)
{
    cc_assert(op >= IR_NONE && op < IR_END);
    return rops[op];
}

static void emit_ir(struct ir *ir)
{
    vec_push(func_irs, ir);
}

static struct operand * new_operand(void)
{
    return zmalloc(sizeof(struct operand));
}

static struct operand * make_sym_operand(node_t *sym)
{
    struct operand *operand = new_operand();
    operand->sym = sym;
    return operand;
}

static struct operand * make_name_operand(const char *name, struct table **table, int scope)
{
    node_t *sym = lookup(name, *table);
    if (!sym)
        sym = install(name, table, scope);
    return make_sym_operand(sym);
}

static struct operand * make_tmp_operand(void)
{
    return make_name_operand(gen_tmpname_r(), &tmps, GLOBAL);
}

static struct operand * make_label_operand(const char *label)
{
    return make_name_operand(label, &labels, GLOBAL);
}

static struct operand * make_integer_operand(const char *name)
{
    return make_name_operand(name, &constants, CONSTANT);
}

static struct operand * make_int_operand(long long i)
{
    return make_integer_operand(strd(i));
}

static struct operand * make_unsigned_operand(unsigned long long u)
{
    return make_integer_operand(stru(u));
}

static struct operand * make_literal_operand(node_t *sym)
{
    struct operand *operand = make_sym_operand(sym);
    return operand;
}

static struct ir * new_ir(int op, struct operand *l, struct operand *r, struct operand *result)
{
    struct ir *ir = zmalloc(sizeof(struct ir));
    ir->op = op;
    ir->args[0] = l;
    ir->args[1] = r;
    ir->result = result;
    return ir;
}

static struct ir * make_ir_r(int op, struct operand *l, struct operand *r)
{
    return new_ir(op, l, r, make_tmp_operand());
}

static struct ir * make_ir_nor(int op, struct operand *l, struct operand *r)
{
    return new_ir(op, l, r, NULL);
}

static struct ir * make_conv_ir(int op, node_t *dty, struct operand *l)
{
    struct ir *ir = make_ir_r(op, l, NULL);
    AST_TYPE(ir->result->sym) = dty;
    return ir;
}

static struct ir * make_assign_ir(struct operand *l, struct operand *r)
{
    struct ir *ir = new_ir(IR_ASSIGN, r, NULL, l);
    return ir;
}

static void emit_assin_ir(struct operand *l, struct operand *r)
{
    struct ir *ir = make_assign_ir(l, r);
    emit_ir(ir);
}

static struct ir * emit_conv_ir(int op, node_t *dty, struct operand *l)
{
    struct ir *ir = make_conv_ir(op, dty, l);
    emit_ir(ir);
    return ir;
}

static void emit_simple_if(int op, struct operand *operand, const char *label)
{
    struct ir *ir = new_ir(op, operand, NULL, make_label_operand(label));
    emit_ir(ir);
}

static void emit_rel_if(int op, int relop,
                        struct operand *rel_l, struct operand *rel_r,
                        const char *label)
{
    struct ir *ir = new_ir(op, rel_l, rel_r, make_label_operand(label));
    ir->relop = relop;
    emit_ir(ir);
}

static void emit_label(const char *label)
{
    struct operand *operand = make_label_operand(label);
    struct ir *ir = new_ir(IR_LABEL, NULL, NULL, operand);
    emit_ir(ir);
}

static void emit_goto(const char *label)
{
    struct operand *operand = make_label_operand(label);
    struct ir *ir = new_ir(IR_GOTO, NULL, NULL, operand);
    emit_ir(ir);
}

static void do_emit_local_decl(node_t *decl)
{
    // TODO: 
}

static void emit_local_decl(node_t *decl)
{
    cc_assert(isdecl(decl));
    
    node_t *sym = DECL_SYM(decl);
    int sclass = SYM_SCLASS(sym);

    if (!isvardecl(decl))
        return;
    
    if (SYM_REFS(sym) == 0) {
        if (!SYM_PREDEFINE(sym))
            warningf(AST_SRC(sym),
                     "unused variable '%s'", SYM_NAME(sym));
    } else {
        if (sclass == EXTERN) {
            // do nothing
        } else if (sclass == STATIC) {
            SYM_X_LABEL(sym) = gen_static_label();
            vec_push(staticvars, decl);
            do_emit_local_decl(decl);
        } else {
            vec_push(localvars, decl);
            do_emit_local_decl(decl);
        }
    }
}

static void emit_local_decls(node_t **decls)
{
    for (int i = 0; i < LIST_LEN(decls); i++)
        emit_local_decl(decls[i]);
}

static int bop2rop(int op)
{
    switch (op) {
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
    case '*':
        return IR_MUL;
    case '/':
        return IR_DIV;
    default:
        cc_assert(0);
    }
}

static void emit_assign(node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);
    node_t *r = EXPR_OPERAND(n, 1);

    emit_expr(l);
    emit_expr(r);
    emit_assin_ir(EXPR_X_ADDR(l), EXPR_X_ADDR(r));
    EXPR_X_ADDR(n) = EXPR_X_ADDR(l);
}

static void emit_bool_expr_e(node_t *n)
{
    EXPR_X_TRUE(n) = fall;
    EXPR_X_FALSE(n) = gen_label();
    const char *label = gen_label();
    struct operand *result = make_tmp_operand();
    emit_bool_expr(n);
    // true
    emit_assin_ir(result, true_operand);
    emit_goto(label);
    emit_label(EXPR_X_FALSE(n));
    // false
    emit_assin_ir(result, false_operand);
    emit_label(label);
    EXPR_X_ADDR(n) = result;
}

static void emit_bop(node_t *n)
{    
    switch (EXPR_OP(n)) {
    case '=':
        emit_assign(n);
        break;
    case ',':
        {
            node_t *l = EXPR_OPERAND(n, 0);
            node_t *r = EXPR_OPERAND(n, 1);
            emit_expr(l);
            emit_expr(r);
            EXPR_X_ADDR(n) = EXPR_X_ADDR(r);
        }
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
        {
            int op = EXPR_OP(n);
            node_t *l = EXPR_OPERAND(n, 0);
            node_t *r = EXPR_OPERAND(n, 1);
            struct operand *result1;
            struct operand *result2;
            struct ir *ir;

            emit_expr(l);
            emit_expr(r);
            result1 = EXPR_X_ADDR(l);
            result2 = EXPR_X_ADDR(r);
            ir = make_ir_r(bop2rop(op), result1, result2);
            emit_ir(ir);
            EXPR_X_ADDR(n) = ir->result;
        }
        break;
    case '+':
    case '-':
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
        emit_bool_expr_e(n);
        break;
    default:
        cc_assert(0);
    }
}

static void emit_uop(node_t *n)
{
    switch (EXPR_OP(n)) {
    case INCR:
    case DECR:
        break;
    case '*':
    case '&':
        break;
    case '+':
        emit_expr(EXPR_OPERAND(n, 0));
        EXPR_X_ADDR(n) = EXPR_X_ADDR(EXPR_OPERAND(n, 0));
        break;
    case '-':
        break;
    case '~':
        break;
    case '!':
        emit_bool_expr_e(n);
        break;
    case SIZEOF:
        {
            node_t *l = EXPR_OPERAND(n, 0);
            node_t *ty = istype(l) ? l : AST_TYPE(l);
            size_t size = TYPE_SIZE(ty);
            struct operand *operand = make_unsigned_operand(size);
            EXPR_X_ADDR(n) = operand;
        }
        break;
    default:
        cc_assert(0);
    }
}

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
    emit_assin_ir(result, EXPR_X_ADDR(then));
    emit_goto(label);
    emit_label(EXPR_X_FALSE(cond));
    emit_expr(els);
    emit_assin_ir(result, EXPR_X_ADDR(els));
    emit_label(label);
    EXPR_X_ADDR(n) = result;
}

static void emit_member(node_t *n)
{
}

static void emit_subscript(node_t *n)
{
}

static struct operand * arith2arith(node_t *dty, struct operand *l)
{
    node_t *sty = SYM_TYPE(l->sym);

    if (eqarith(sty, dty))
        return l;

    if (isint(sty) && isint(dty)) {
        struct ir *ir = emit_conv_ir(IR_CONV_II, dty, l);
        return ir->result;
    } else if (isint(sty) && isfloat(dty)) {
        struct ir *ir = emit_conv_ir(IR_CONV_IF, dty, l);
        return ir->result;
    } else if (isfloat(sty) && isint(dty)) {
        struct ir *ir = emit_conv_ir(IR_CONV_FI, dty, l);
        return ir->result;
    } else if (isfloat(sty) && isfloat(dty)) {
        struct ir *ir = emit_conv_ir(IR_CONV_FF, dty, l);
        return ir->result;
    } else {
        cc_assert(0);
    }
}

static struct operand * ptr2arith(node_t *dty, struct operand *l)
{
    cc_assert(isint(dty));
    struct ir *ir = emit_conv_ir(IR_CONV_PI, dty, l);
    return ir->result;
}

static struct operand * ptr2ptr(node_t *dty, struct operand *l)
{
    struct ir *ir = emit_conv_ir(IR_CONV_PP, dty, l);
    return ir->result;
}

static struct operand * arith2ptr(node_t *dty, struct operand *l)
{
    cc_assert(isint(SYM_TYPE(l->sym)));
    struct ir *ir = emit_conv_ir(IR_CONV_IP, dty, l);
    return ir->result;
}

static struct operand * func2ptr(node_t *dty, struct operand *l)
{
    struct ir *ir = emit_conv_ir(IR_CONV_FP, dty, l);
    return ir->result;
}

static struct operand * array2ptr(node_t *dty, struct operand *l)
{
    struct ir *ir = emit_conv_ir(IR_CONV_AP, dty, l);
    return ir->result;
}

static void emit_conv(node_t *n)
{
    node_t *dty = AST_TYPE(n);
    node_t *l = EXPR_OPERAND(n, 0);
    node_t *sty = AST_TYPE(l);

    emit_expr(l);

    if (isarith(dty)) {
        if (isarith(sty))
            EXPR_X_ADDR(n) = arith2arith(dty, EXPR_X_ADDR(l));
        else if (isptr(sty))
            EXPR_X_ADDR(n) = ptr2arith(dty, EXPR_X_ADDR(l));
        else
            cc_assert(0);
    } else if (isptr(dty)) {
        if (isptr(sty))
            EXPR_X_ADDR(n) = ptr2ptr(dty, EXPR_X_ADDR(l));
        else if (isarith(sty))
            EXPR_X_ADDR(n) = arith2ptr(dty, EXPR_X_ADDR(l));
        else if (isfunc(sty))
            EXPR_X_ADDR(n) = func2ptr(dty, EXPR_X_ADDR(l));
        else if (isarray(sty))
            EXPR_X_ADDR(n) = array2ptr(dty, EXPR_X_ADDR(l));
        else
            cc_assert(0);
    } else {
        // nothing
        EXPR_X_ADDR(n) = EXPR_X_ADDR(l);
    }
}

static void emit_funcall(node_t *n)
{
}

static void emit_ref_expr(node_t *n)
{
    EXPR_X_ADDR(n) = make_sym_operand(EXPR_SYM(n));
}

static void emit_integer_literal(node_t *n)
{
    EXPR_X_ADDR(n) = make_literal_operand(EXPR_SYM(n));
}

static void emit_float_literal(node_t *n)
{
    EXPR_X_ADDR(n) = make_literal_operand(EXPR_SYM(n));
}

static void emit_string_literal(node_t *n)
{
    EXPR_X_ADDR(n) = make_literal_operand(EXPR_SYM(n));
}

static void emit_compound_literal(node_t *n)
{
    EXPR_X_ADDR(n) = make_literal_operand(EXPR_SYM(n));
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
        emit_expr(EXPR_OPERAND(n, 0));
        EXPR_X_ADDR(n) = EXPR_X_ADDR(EXPR_OPERAND(n, 0));
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
    int op = EXPR_OP(n);

    emit_expr(l);
    emit_expr(r);

    if (EXPR_X_TRUE(n) != fall && EXPR_X_FALSE(n) != fall) {
        emit_rel_if(IR_IF,
                    op, EXPR_X_ADDR(l), EXPR_X_ADDR(r),
                    EXPR_X_TRUE(n));
        emit_goto(EXPR_X_FALSE(n));
    } else if (EXPR_X_TRUE(n) != fall) {
        emit_rel_if(IR_IF,
                    op, EXPR_X_ADDR(l), EXPR_X_ADDR(r),
                    EXPR_X_TRUE(n));
    } else if (EXPR_X_FALSE(n) != fall) {
        emit_rel_if(IR_IF_FALSE,
                    op, EXPR_X_ADDR(l), EXPR_X_ADDR(r),
                    EXPR_X_FALSE(n));
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
        struct operand *test = EXPR_X_ADDR(n);
        
        if (EXPR_X_TRUE(n) != fall && EXPR_X_FALSE(n) != fall) {
            emit_simple_if(IR_IF, test, EXPR_X_TRUE(n));
            emit_goto(EXPR_X_FALSE(n));
        } else if (EXPR_X_TRUE(n) != fall) {
            emit_simple_if(IR_IF, test, EXPR_X_TRUE(n));
        } else if (EXPR_X_FALSE(n) != fall) {
            emit_simple_if(IR_IF_FALSE, test, EXPR_X_FALSE(n));
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
            emit_local_decl(node);
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
        emit_local_decls(decl);
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

static void emit_switch_jmp(struct operand *l, node_t *case_stmt)
{
    struct operand *case_operand;

    case_operand = make_int_operand(STMT_CASE_INDEX(case_stmt));
    STMT_X_LABEL(case_stmt) = gen_label();
    emit_rel_if(IR_IF,
                EQ, l, case_operand,
                STMT_X_LABEL(case_stmt));
}

static void emit_switch_stmt(node_t *stmt)
{
    node_t *expr = STMT_SWITCH_EXPR(stmt);
    node_t *body = STMT_SWITCH_BODY(stmt);
    node_t **cases = STMT_SWITCH_CASES(stmt);

    emit_expr(expr);
    
    for (int i = 0; i < LIST_LEN(cases); i++) {
        node_t *case_stmt = cases[i];
        emit_switch_jmp(EXPR_X_ADDR(expr), case_stmt);
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
    emit_expr(n);
    struct ir *ir = make_ir_nor(IR_RETURN, EXPR_X_ADDR(n), NULL);
    emit_ir(ir);
}

static void emit_null_stmt(node_t *stmt)
{
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
        emit_null_stmt(stmt);
        break;
    default:
        emit_expr(stmt);
        break;
    }
}

static void emit_function(node_t *decl)
{
    node_t *stmt = DECL_BODY(decl);

    func_irs = vec_new();
    staticvars = vec_new();
    localvars = vec_new();

    STMT_X_NEXT(stmt) = gen_label();
    emit_stmt(stmt);
    DECL_X_IRS(decl) = func_irs;
    DECL_X_LVARS(decl) = (node_t **)vtoa(localvars);
    DECL_X_SVARS(decl) = (node_t **)vtoa(staticvars);
}

static void emit_globalvar(node_t *decl)
{

}

static void ir_init(void)
{
    tmps = new_table(NULL, GLOBAL);
    labels = new_table(NULL, GLOBAL);
    true_operand = make_int_operand(1);
    false_operand = make_int_operand(0);
}

static const char *glabel(const char *label)
{
    if (opts.fleading_underscore)
        return format("_%s", label);
    else
        return label;
}

node_t * ir(node_t *tree)
{
    cc_assert(istudecl(tree) && errors == 0);

    struct vector *v = vec_new();
    ir_init();
    for (int i = 0; i < LIST_LEN(DECL_EXTS(tree)); i++) {
        node_t *decl = DECL_EXTS(tree)[i];
        node_t *sym = DECL_SYM(decl);

        // skip unused symbols
        if (SYM_SCLASS(sym) == STATIC && SYM_REFS(sym) == 0) {
            if (isfuncdef(decl))
                warningf(AST_SRC(sym), "unused function '%s'", SYM_NAME(sym));
            else if (isvardecl(decl))
                warningf(AST_SRC(sym), "unused variable '%s'", SYM_NAME(sym));
            continue;
        }
        
        if (isfuncdef(decl)) {
            emit_function(decl);
            vec_push(v, decl);
            vec_add_array(v, (void **)DECL_X_SVARS(decl));
        } else if (isvardecl(decl)) {
            emit_globalvar(decl);
            vec_push(v, decl);
        }

        SYM_X_LABEL(sym) = glabel(SYM_NAME(sym));
    }

    DECL_EXTS(tree) = (node_t **)vtoa(v);
    return tree;
}

node_t * reduce(node_t *expr)
{
    return expr;
}
