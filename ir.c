#include "cc.h"

/**
 *  Intermediate Representation
 *
 *  three address code
 */

static const char *rops[] = {
#define _rop(a, b)  b,
#include "rop.def"    
};

static void ir_stmt(node_t *n);
static struct operand * ir_expr(node_t *n);

static struct vector *func_codes;
static const char *func_ret_label;
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

static const char *rop2s(int op)
{
    cc_assert(op >= IR_NONE && op < IR_END);
    return rops[op];
}

static void emit_code(struct code *code)
{
    vec_push(func_codes, ir);
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

static struct code * new_code(int op, struct operand *l, struct operand *r, bool result)
{
    struct code *code = zmalloc(sizeof(struct code));
    code->op = op;
    code->arg1 = l;
    code->arg2 = r;
    if (result)
        code->result = make_tmp_operand();
    return code;
}

static struct code * make_code(int op, struct operand *l, struct operand *r)
{
    return new_code(op, l, r, true);
}

static struct code * make_code_nor(int op, struct operand *l, struct operand *r)
{
    return new_code(op, l, r, false);
}

static struct code * make_conv_code(int op, node_t *dty, struct operand *l)
{
    struct code *code = new_code(op, l, NULL, true);
    AST_TYPE(code->result->sym) = dty;
    return code;
}

static struct code * emit_conv_code(int op, node_t *dty, struct operand *l)
{
    struct code *code = make_conv_code(op, dty, l);
    emit_code(code);
    return code;
}

static void set_func_context(void)
{
    func_codes = vec_new();
    func_ret_label = gen_label();
}

static void restore_func_context(void)
{
    func_codes = NULL;
    func_ret_label = NULL;
}

static void ir_decl(node_t *n)
{
}

static struct operand * ir_bop(node_t *n)
{
    int op = EXPR_OP(n);
    node_t *l = EXPR_OPERAND(n, 0);
    node_t *r = EXPR_OPERAND(n, 1);
    switch (op) {
    case '=':
    case ',':
        return NULL;
        // int
    case '%':
    case '|':
    case '&':
    case '^':
    case LSHIFT:
    case RSHIFT:
        return NULL;
        // arith
    case '*':
    case '/':
        return NULL;
        // scalar
    case '<':
    case '>':
    case GEQ:
    case LEQ:
    case EQ:
    case NEQ:
        return NULL;
    case '+':
    case '-':
        {
            struct operand *result1;
            struct operand *result2;
            result1 = ir_expr(l);
            result2 = ir_expr(r);
            struct code *code = make_code(IR_ADD, result1, result2);
            emit_code(code);
            return code->result;
        }
        break;
    case AND:
    case OR:
        return NULL;
    default:
        cc_assert(0);
    }
}

static struct operand * ir_uop(node_t *n)
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
        return NULL;
    default:
        cc_assert(0);
    }
}

static struct operand * ir_cond(node_t *n)
{
    return NULL;
}

static struct operand * ir_member(node_t *n)
{
    return NULL;
}

static struct operand * ir_subscript(node_t *n)
{
    return NULL;
}

static struct operand * arith2arith(node_t *dty, struct operand *l)
{
    node_t *sty = SYM_TYPE(l->sym);

    if (eqarith(sty, dty))
        return l;

    if (isint(sty) && isint(dty)) {
        struct code *code = emit_conv_code(IR_CONV_II, dty, l);
        return code->result;
    } else if (isint(sty) && isfloat(dty)) {
        struct code *code = emit_conv_code(IR_CONV_IF, dty, l);
        return code->result;
    } else if (isfloat(sty) && isint(dty)) {
        struct code *code = emit_conv_code(IR_CONV_FI, dty, l);
        return code->result;
    } else if (isfloat(sty) && isfloat(dty)) {
        struct code *code = emit_conv_code(IR_CONV_FF, dty, l);
        return code->result;
    } else {
        cc_assert(0);
    }
}

static struct operand * ptr2arith(node_t *dty, struct operand *l)
{
    cc_assert(isint(dty));
    struct code *code = emit_conv_code(IR_CONV_PI, dty, l);
    return code->result;
}

static struct operand * ptr2ptr(node_t *dty, struct operand *l)
{
    struct code *code = emit_conv_code(IR_CONV_PP, dty, l);
    return code->result;
}

static struct operand * arith2ptr(node_t *dty, struct operand *l)
{
    cc_assert(isint(SYM_TYPE(l->sym)));
    struct code *code = emit_conv_code(IR_CONV_IP, dty, l);
    return code->result;
}

static struct operand * func2ptr(node_t *dty, struct operand *l)
{
    struct code *code = emit_conv_code(IR_CONV_FP, dty, l);
    return code->result;
}

static struct operand * array2ptr(node_t *dty, struct operand *l)
{
    struct code *code = emit_conv_code(IR_CONV_AP, dty, l);
    return code->result;
}

static struct operand * ir_conv(node_t *n)
{
    node_t *dty = AST_TYPE(n);
    node_t *l = EXPR_OPERAND(n, 0);
    node_t *sty = AST_TYPE(l);

    struct operand *operand = ir_expr(l);
    
    if (isarith(dty)) {
        if (isarith(sty))
            return arith2arith(dty, operand);
        else if (isptr(sty))
            return ptr2arith(dty, operand);
        else
            cc_assert(0);
    } else if (isptr(dty)) {
        if (isptr(sty))
            return ptr2ptr(dty, operand);
        else if (isarith(sty))
            return arith2ptr(dty, operand);
        else if (isfunc(sty))
            return func2ptr(dty, operand);
        else if (isarray(sty))
            return array2ptr(dty, operand);
        else
            cc_assert(0);
    } else {
        return operand;
    }
}

static struct operand * ir_funcall(node_t *n)
{
    return NULL;
}

static struct operand * ir_ref_expr(node_t *n)
{
    node_t *sym = EXPR_SYM(n);
    struct operand *operand = new_operand();
    operand->sym = sym;
    return operand;
}

static struct operand * ir_integer_literal(node_t *n)
{
    node_t *sym = EXPR_SYM(n);
    struct operand *operand = new_operand();
    operand->sym = sym;
    return operand;
}

static struct operand * ir_float_literal(node_t *n)
{
    node_t *sym = EXPR_SYM(n);
    struct operand *operand = new_operand();
    operand->sym = sym;
    return operand;
}

static struct operand * ir_string_literal(node_t *n)
{
    node_t *sym = EXPR_SYM(n);
    struct operand *operand = new_operand();
    operand->sym = sym;
    return operand;
}

static struct operand * ir_compound_literal(node_t *n)
{
    node_t *sym = EXPR_SYM(n);
    struct operand *operand = new_operand();
    operand->sym = sym;
    return operand;
}

static struct operand * ir_expr(node_t *n)
{
    switch (AST_ID(n)) {
    case BINARY_OPERATOR:
        return ir_bop(n);
    case UNARY_OPERATOR:
        return ir_uop(n);
    case PAREN_EXPR:
        return ir_expr(EXPR_OPERAND(n, 0));
    case COND_EXPR:
        return ir_cond(n);
    case MEMBER_EXPR:
        return ir_member(n);
    case SUBSCRIPT_EXPR:
        return ir_subscript(n);
    case CAST_EXPR:
    case CONV_EXPR:
        return ir_conv(n);
    case CALL_EXPR:
        return ir_funcall(n);
    case REF_EXPR:
        if (EXPR_OP(n) == ENUM)
            return ir_integer_literal(n);
        else
            return ir_ref_expr(n);
    case INTEGER_LITERAL:
        return ir_integer_literal(n);
    case FLOAT_LITERAL:
        return ir_float_literal(n);
    case STRING_LITERAL:
        return ir_string_literal(n);
    case COMPOUND_LITERAL:
        return ir_compound_literal(n);
    case INITS_EXPR:
    case VINIT_EXPR:
    default:
        cc_assert(0);
    }
}

static void ir_label(const char *label)
{
    struct operand *operand = make_label_operand(label);
    struct code *code = make_code_nor(IR_LABEL, operand, NULL);
    emit_code(code);
}

static void ir_goto(const char *label)
{
    struct operand *operand = make_label_operand(label);
    struct code *code = make_code_nor(IR_GOTO, operand, NULL);
    emit_code(code);
}

static void ir_bool_expr(node_t *expr)
{
    
}

static void ir_compound_stmt(node_t *stmt)
{
    node_t **blks = STMT_BLKS(stmt);
    for (int i = 0; i < LIST_LEN(blks); i++) {
        node_t *node = blks[i];
        if (isdecl(node))
            ir_decl(node);
        else if (isstmt(node))
            ir_stmt(node);
        else if (isexpr(node))
            ir_expr(node);
        else
            cc_assert(0);
    }
}

static void ir_if_stmt(node_t *stmt)
{
    node_t *cond = STMT_COND(stmt);
    node_t *then = STMT_THEN(stmt);
    node_t *els = STMT_ELSE(stmt);

    STMT_X_NEXT(stmt) = gen_label();
    EXPR_X_TRUE(cond) = fall;
    if (els) {
        EXPR_X_FALSE(cond) = gen_label();
        STMT_X_NEXT(then) = STMT_X_NEXT(stmt);
        STMT_X_NEXT(els) = STMT_X_NEXT(stmt);
        ir_bool_expr(cond);
        ir_stmt(then);
        ir_goto(STMT_X_NEXT(stmt));
        ir_label(EXPR_X_FALSE(cond));
        ir_stmt(els);
    } else {
        EXPR_X_FALSE(cond) = STMT_X_NEXT(stmt);
        STMT_X_NEXT(then) = STMT_X_NEXT(stmt);
        ir_bool_expr(cond);
        ir_stmt(then);
    }
    ir_label(STMT_X_NEXT(stmt));
}

static void ir_while_stmt(node_t *stmt)
{
    const char *beg = gen_label();
    const char *end = gen_label();
    node_t *cond = STMT_WHILE_COND(stmt);
    node_t *body = STMT_WHILE_BODY(stmt);

    STMT_X_NEXT(stmt) = end;
    EXPR_X_TRUE(cond) = fall;
    EXPR_X_FALSE(cond) = STMT_X_NEXT(stmt);

    ir_label(beg);
    ir_bool_expr(cond);
            
    SET_LOOP_CONTEXT(beg, end);
    ir_stmt(body);
    RESTORE_LOOP_CONTEXT();

    ir_goto(beg);
    ir_label(STMT_X_NEXT(stmt));
}

static void ir_do_while_stmt(node_t *stmt)
{
    const char *beg = gen_label();
    const char *end = gen_label();
    node_t *cond = STMT_WHILE_COND(stmt);
    node_t *body = STMT_WHILE_BODY(stmt);

    STMT_X_NEXT(stmt) = end;
    EXPR_X_TRUE(cond) = beg;
    EXPR_X_FALSE(cond) = STMT_X_NEXT(stmt);

    ir_label(beg);

    SET_LOOP_CONTEXT(beg, end);
    ir_stmt(body);
    RESTORE_LOOP_CONTEXT();
}

static void ir_for_stmt(node_t *stmt)
{
    
}

static void ir_switch_stmt(node_t *stmt)
{
    
}

static void ir_case_stmt(node_t *stmt)
{
    
}

static void ir_default_stmt(node_t *stmt)
{
    
}

static void ir_label_stmt(node_t *stmt)
{
    
}

static void ir_goto_stmt(node_t *stmt)
{
    
}

static void ir_break_stmt(node_t *stmt)
{
    
}

static void ir_continue_stmt(node_t *stmt)
{
    
}

static void ir_return_stmt(node_t *stmt)
{
    
}

static void ir_null_stmt(node_t *stmt)
{
    
}

static void ir_stmt(node_t *stmt)
{
    switch (AST_ID(stmt)) {
    case COMPOUND_STMT:
        ir_compound_stmt(stmt);
        break;
    case IF_STMT:
        ir_if_stmt(stmt);
        break;
    case WHILE_STMT:
        ir_while_stmt(stmt);
        break;
    case DO_WHILE_STMT:
        ir_do_while_stmt(stmt);
        break;
    case FOR_STMT:
        ir_for_stmt(stmt);
        break;
    case SWITCH_STMT:
        ir_switch_stmt(stmt);
        break;
    case CASE_STMT:
        ir_case_stmt(stmt);
        break;
    case DEFAULT_STMT:
        ir_default_stmt(stmt);
        break;
    case LABEL_STMT:
        ir_label_stmt(stmt);
        break;
    case GOTO_STMT:
        ir_goto_stmt(stmt);
        break;
    case BREAK_STMT:
        ir_break_stmt(stmt);
        break;
    case CONTINUE_STMT:
        ir_continue_stmt(stmt);
        break;
    case RETURN_STMT:
        ir_return_stmt(stmt);
        break;
    case NULL_STMT:
        ir_null_stmt(stmt);
        break;
    default:
        cc_assert(0);
    }
}

static void ir_function(node_t *decl)
{
    node_t *stmt = DECL_BODY(decl);

    set_func_context();
    
    ir_stmt(stmt);
    DECL_X_CODES(decl) = func_codes;

    restore_func_context();
}

static void ir_globalvar(node_t *decl)
{
    
}

static void ir_init(void)
{
    tmps = new_table(NULL, GLOBAL);
    labels = new_table(NULL, GLOBAL);
}

node_t * ir(node_t *tree)
{
    cc_assert(istudecl(tree) && errors == 0);

    ir_init();
    for (int i = 0; i < LIST_LEN(DECL_EXTS(tree)); i++) {
        node_t *decl = DECL_EXTS(tree)[i];
        if (isfuncdef(decl))
            ir_function(decl);
        else if (isvardecl(decl))
            ir_globalvar(decl);
    }
    
    return tree;
}

static void print_code(struct code *code)
{
    switch (code->op) {
    case IR_NONE:
        break;
    case IR_LABEL:
        println("%s:", SYM_NAME(code->arg1->sym));
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
                SYM_NAME(code->result->sym),
                SYM_NAME(code->arg1->sym),
                rop2s(code->op),
                SYM_NAME(code->arg2->sym));
        break;
    case IR_U_MINUS:
        break;
    case IR_CONV_II:
    case IR_CONV_IF:
    case IR_CONV_FI:
    case IR_CONV_FF:
        println("%s = (%s=>%s) %s",
                SYM_NAME(code->result->sym),
                TYPE_NAME(SYM_TYPE(code->arg1->sym)),
                TYPE_NAME(SYM_TYPE(code->result->sym)),
                SYM_NAME(code->arg1->sym));
        break;
    case IR_CONV_IP:
    case IR_CONV_PI:
    case IR_CONV_PP:
    case IR_CONV_FP:
    case IR_CONV_AP:
        println("%s = (%s) %s",
                SYM_NAME(code->result->sym),
                rop2s(code->op),
                SYM_NAME(code->arg1->sym));
        break;
    default:
        die("unexpected rop %d", code->op);
    }
}

void print_codes(struct vector *codes)
{
    println("IRS: %lld", vec_len(codes));
    for (int i = 0; i < vec_len(codes); i++) {
        struct code *code = vec_at(codes, i);
        print_code(code);
    }
}

node_t * reduce(node_t *expr)
{
    return expr;
}
