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
static const char *func_ret_label;
static unsigned func_ret_refs;
static struct table *tmps;
static struct table *labels;
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

#define BREAK_CONTEXT     (__break)
#define CONTINUE_CONTEXT  (__continue)

static void set_func_context()
{
    func_irs = vec_new();
    func_ret_label = gen_label();
    func_ret_refs = 0;
}

static void restore_func_context()
{

}

static const char *rop2s(int op)
{
    cc_assert(op >= IR_NONE && op < IR_END);
    return rops[op];
}

static void emit_ir(ir_t *ir)
{
    vec_push(func_irs, ir);
}

static struct operand * new_operand(void)
{
    return zmalloc(sizeof(struct operand));
}

static struct operand * make_tmp_operand(void)
{
    struct operand *operand = new_operand();
    const char *name = gen_tmpname_r();
    operand->sym = install(name, &tmps, GLOBAL);
    return operand;
}

static struct operand * make_label_operand(const char *label)
{
    struct operand *operand = new_operand();
    operand->sym = install(label, &labels, GLOBAL);
    return operand;
}

static ir_t * new_ir(int op, struct operand *l, struct operand *r, bool result)
{
    ir_t *ir = zmalloc(sizeof(ir_t));
    IR_OP(ir) = op;
    IR_ARG(ir, 0) = l;
    IR_ARG(ir, 1) = r;
    if (result)
        IR_RESULT(ir) = make_tmp_operand();
    return ir;
}

static ir_t * make_ir(int op, struct operand *l, struct operand *r)
{
    return new_ir(op, l, r, true);
}

static ir_t * make_ir_nor(int op, struct operand *l, struct operand *r)
{
    return new_ir(op, l, r, false);
}

static ir_t * make_conv_ir(int op, node_t *dty, struct operand *l)
{
    ir_t *ir = new_ir(op, l, NULL, true);
    AST_TYPE(IR_RESULT(ir)->sym) = dty;
    return ir;
}

static ir_t * emit_conv_ir(int op, node_t *dty, struct operand *l)
{
    ir_t *ir = make_conv_ir(op, dty, l);
    emit_ir(ir);
    return ir;
}

static void emit_if(struct operand *operand, const char *label)
{
    
}

static void emit_iffalse(struct operand *operand, const char *label)
{
    
}

static void emit_decl(node_t *n)
{
}

static void emit_decls(node_t **n)
{
    
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

static void emit_bop(node_t *n)
{
    int op = EXPR_OP(n);
    node_t *l = EXPR_OPERAND(n, 0);
    node_t *r = EXPR_OPERAND(n, 1);
    switch (op) {
    case '=':
    case ',':
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

            struct operand *result1;
            struct operand *result2;
            ir_t *ir;

            emit_expr(l);
            emit_expr(r);
            result1 = EXPR_X_ADDR(l);
            result2 = EXPR_X_ADDR(r);
            ir = make_ir(bop2rop(op), result1, result2);
            emit_ir(ir);
            EXPR_X_ADDR(n) = IR_RESULT(ir);
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

static void emit_cond(node_t *n)
{
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
        ir_t *ir = emit_conv_ir(IR_CONV_II, dty, l);
        return IR_RESULT(ir);
    } else if (isint(sty) && isfloat(dty)) {
        ir_t *ir = emit_conv_ir(IR_CONV_IF, dty, l);
        return IR_RESULT(ir);
    } else if (isfloat(sty) && isint(dty)) {
        ir_t *ir = emit_conv_ir(IR_CONV_FI, dty, l);
        return IR_RESULT(ir);
    } else if (isfloat(sty) && isfloat(dty)) {
        ir_t *ir = emit_conv_ir(IR_CONV_FF, dty, l);
        return IR_RESULT(ir);
    } else {
        cc_assert(0);
    }
}

static struct operand * ptr2arith(node_t *dty, struct operand *l)
{
    cc_assert(isint(dty));
    ir_t *ir = emit_conv_ir(IR_CONV_PI, dty, l);
    return IR_RESULT(ir);
}

static struct operand * ptr2ptr(node_t *dty, struct operand *l)
{
    ir_t *ir = emit_conv_ir(IR_CONV_PP, dty, l);
    return IR_RESULT(ir);
}

static struct operand * arith2ptr(node_t *dty, struct operand *l)
{
    cc_assert(isint(SYM_TYPE(l->sym)));
    ir_t *ir = emit_conv_ir(IR_CONV_IP, dty, l);
    return IR_RESULT(ir);
}

static struct operand * func2ptr(node_t *dty, struct operand *l)
{
    ir_t *ir = emit_conv_ir(IR_CONV_FP, dty, l);
    return IR_RESULT(ir);
}

static struct operand * array2ptr(node_t *dty, struct operand *l)
{
    ir_t *ir = emit_conv_ir(IR_CONV_AP, dty, l);
    return IR_RESULT(ir);
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
    node_t *sym = EXPR_SYM(n);
    struct operand *operand = new_operand();
    operand->sym = sym;
    EXPR_X_ADDR(n) = operand;
}

static void emit_integer_literal(node_t *n)
{
    node_t *sym = EXPR_SYM(n);
    struct operand *operand = new_operand();
    operand->sym = sym;
    EXPR_X_ADDR(n) = operand;
}

static void emit_float_literal(node_t *n)
{
    node_t *sym = EXPR_SYM(n);
    struct operand *operand = new_operand();
    operand->sym = sym;
    EXPR_X_ADDR(n) = operand;
}

static void emit_string_literal(node_t *n)
{
    node_t *sym = EXPR_SYM(n);
    struct operand *operand = new_operand();
    operand->sym = sym;
    EXPR_X_ADDR(n) = operand;
}

static void emit_compound_literal(node_t *n)
{
    node_t *sym = EXPR_SYM(n);
    struct operand *operand = new_operand();
    operand->sym = sym;
    EXPR_X_ADDR(n) = operand;
}

static void emit_expr(node_t *n)
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
        cc_assert(0);
    }
}

static void emit_label(const char *label)
{
    struct operand *operand = make_label_operand(label);
    ir_t *ir = make_ir_nor(IR_LABEL, operand, NULL);
    emit_ir(ir);
}

static void emit_goto(const char *label)
{
    struct operand *operand = make_label_operand(label);
    ir_t *ir = make_ir_nor(IR_GOTO, operand, NULL);
    emit_ir(ir);
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

static void emit_rel_expr(node_t *n)
{
    node_t *l = EXPR_OPERAND(n, 0);
    node_t *r = EXPR_OPERAND(n, 1);
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
            emit_if(test, EXPR_X_TRUE(n));
            emit_goto(EXPR_X_FALSE(n));
        } else if (EXPR_X_TRUE(n) != fall) {
            emit_if(test, EXPR_X_TRUE(n));
        } else if (EXPR_X_FALSE(n) != fall) {
            emit_iffalse(test, EXPR_X_FALSE(n));
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

static void emit_switch_stmt(node_t *stmt)
{

}

static void emit_case_stmt(node_t *stmt)
{

}

static void emit_default_stmt(node_t *stmt)
{

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
    node_t *expr = STMT_RETURN_EXPR(stmt);
    emit_expr(expr);
    // TODO: mov return value
    emit_goto(func_ret_label);
    func_ret_refs++;
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
        cc_assert(isexpr(stmt));
        emit_expr(stmt);
        break;
    }
}

static void emit_function(node_t *decl)
{
    node_t *stmt = DECL_BODY(decl);

    set_func_context();

    STMT_X_NEXT(stmt) = func_ret_label;
    emit_stmt(stmt);
    if (func_ret_refs > 0)
        emit_label(func_ret_label);
    DECL_X_IRS(decl) = func_irs;

    restore_func_context();
}

static void emit_globalvar(node_t *decl)
{

}

static void emit_init(void)
{
    tmps = new_table(NULL, GLOBAL);
    labels = new_table(NULL, GLOBAL);
}

node_t * ir(node_t *tree)
{
    cc_assert(istudecl(tree) && errors == 0);

    emit_init();
    for (int i = 0; i < LIST_LEN(DECL_EXTS(tree)); i++) {
        node_t *decl = DECL_EXTS(tree)[i];
        if (isfuncdef(decl))
            emit_function(decl);
        else if (isvardecl(decl))
            emit_globalvar(decl);
    }

    return tree;
}

static void print_ir(ir_t *ir)
{
    switch (IR_OP(ir)) {
    case IR_NONE:
        break;
    case IR_LABEL:
        println("%s:", SYM_NAME(IR_ARG(ir, 0)->sym));
        break;
    case IR_GOTO:
        break;
    case IR_IF:
    case IR_IF_FALSE:
        break;
    case IR_ADD:
    case IR_MINUS:
    case IR_DIV:
    case IR_MUL:
    case IR_MOD:
        println("%s = %s %s %s",
                SYM_NAME(IR_RESULT(ir)->sym),
                SYM_NAME(IR_ARG(ir, 0)->sym),
                rop2s(IR_OP(ir)),
                SYM_NAME(IR_ARG(ir, 1)->sym));
        break;
    case IR_U_MINUS:
        break;
    case IR_CONV_II:
    case IR_CONV_IF:
    case IR_CONV_FI:
    case IR_CONV_FF:
        println("%s = (%s=>%s) %s",
                SYM_NAME(IR_RESULT(ir)->sym),
                TYPE_NAME(SYM_TYPE(IR_ARG(ir, 0)->sym)),
                TYPE_NAME(SYM_TYPE(IR_RESULT(ir)->sym)),
                SYM_NAME(IR_ARG(ir, 0)->sym));
        break;
    case IR_CONV_IP:
    case IR_CONV_PI:
    case IR_CONV_PP:
    case IR_CONV_FP:
    case IR_CONV_AP:
        println("%s = (%s) %s",
                SYM_NAME(IR_RESULT(ir)->sym),
                rop2s(IR_OP(ir)),
                SYM_NAME(IR_ARG(ir, 0)->sym));
        break;
    default:
        die("unexpected rop %d", IR_OP(ir));
    }
}

void print_irs(struct vector *irs)
{
    println("IRS: %lld", vec_len(irs));
    for (int i = 0; i < vec_len(irs); i++) {
        ir_t *ir = vec_at(irs, i);
        print_ir(ir);
    }
}

node_t * reduce(node_t *expr)
{
    return expr;
}
