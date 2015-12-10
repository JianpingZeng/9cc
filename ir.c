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

static struct vector *func_irs;
static const char *func_ret_label;
static struct table *tmps;

static const char *rop2s(int op)
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

static struct operand * make_tmp_operand(void)
{
    struct operand *operand = new_operand();
    const char *name = gen_tmpname_r();
    operand->sym = install(name, &tmps, GLOBAL);
    return operand;
}

static struct ir * make_label_operand(const char *label)
{
    
}

static struct ir * new_ir(int op, struct operand *l, struct operand *r)
{
    struct ir *ir = zmalloc(sizeof(struct ir));
    ir->op = op;
    ir->arg1 = l;
    ir->arg2 = r;
    ir->result = make_tmp_operand();
    return ir;
}

static struct ir * make_ir(int op, struct operand *l, struct operand *r)
{
    return new_ir(op, l, r);
}

static struct ir * make_label_ir(const char *label)
{
    
}

static struct ir * make_jmp_ir(int jop, const char *label)
{
    
}

static void set_func_context(void)
{
    func_irs = vec_new();
    func_ret_label = gen_label();
}

static void restore_func_context(void)
{
    func_irs = NULL;
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
        break;
        // int
    case '%':
    case '|':
    case '&':
    case '^':
    case LSHIFT:
    case RSHIFT:
        break;
        // arith
    case '*':
        break;
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
        {
            struct operand *result1;
            struct operand *result2;
            result1 = ir_expr(l);
            result2 = ir_expr(r);
            struct ir *ir = make_ir(IR_ADD, result1, result2);
            emit_ir(ir);
            return ir->result;
        }
        break;
    case AND:
    case OR:
        break;
    default:
        cc_assert(0);
    }
}

static void ir_uop(node_t *n)
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

static struct operand * ir_cond(node_t *n)
{
    
}

static struct operand * ir_member(node_t *n)
{
    
}

static struct operand * ir_subscript(node_t *n)
{
    
}

static struct operand * ir_conv(node_t *n)
{
    node_t *dty = AST_TYPE(n);
    node_t *l = EXPR_OPERAND(n, 0);
    node_t *sty = AST_TYPE(l);

    struct operand *operand = ir_expr(l);
    if (isarith(dty)) {
        
    } else if (isptr(dty)) {
        
    } else {
        return operand;
    }
}

static struct operand * ir_funcall(node_t *n)
{
    
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

static void ir_jmp(const char *label)
{
    emit_ir(make_jmp_ir(IR_JMP, label));
}

static void ir_if_jmp(int jop, const char *label)
{
    emit_ir(make_jmp_ir(jop, label));
}

static void ir_label(const char *label)
{
    emit_ir(make_label_ir(label));
}

static void ir_return(node_t *n)
{
    ir_expr(GEN_OPERAND(n));
    ir_jmp(func_ret_label);
}

static int ir_jop(node_t *expr)
{
    int nop = nrelop(expr);
    switch (nop) {
    case '<':
        return IR_JGE;
    case '>':
        return IR_JLE;
    case GEQ:
        return IR_JL;
    case LEQ:
        return IR_JG;
    case EQ:
        return IR_JNE;
    case NEQ:
        return IR_JE;
    default:
        return -1;
    }
}

static int ir_je(node_t *expr)
{
    return IR_JE;
}

static int ir_if_cond(node_t *cond)
{
    if (AST_ID(cond) == BINARY_OPERATOR) {
        switch (EXPR_OP(cond)) {
        case '<':
            break;
        case '>':
        case GEQ:
        case LEQ:
        case EQ:
        case NEQ:
            break;
        default:
            return ir_je(cond);
        }
    } else if (AST_ID(cond) == UNARY_OPERATOR) {
        if (EXPR_OP(cond) == '!') {
            int op = IR_JNE;
            int *node = EXPR_OPERAND(cond, 0);
            ir_expr(node);
            
        } else {
            return ir_je(cond);
        }
    } else {
        return ir_je(cond);
    }
}

static void ir_if(node_t *n)
{
    int jop = ir_if_cond(GEN_OPERAND(n));
    const char *label = gen_label();

    ir_if_jmp(jop, label);

    if (GEN_THEN(n))
        ir_stmt(GEN_THEN(n));
    if (GEN_ELSE(n)) {
        const char *end = gen_label();
        ir_jmp(end);
        ir_label(label);
        ir_stmt(GEN_ELSE(n));
        ir_label(end);
    } else {
        ir_label(label);
    }
}

static void ir_compound(node_t *n)
{
    for (int i = 0; i < LIST_LEN(GEN_LIST(n)); i++)
        ir_stmt(GEN_LIST(n)[i]);
}

static void ir_gen(node_t *n)
{
    switch (AST_ID(n)) {
    case AST_IF:
        ir_if(n);
        break;
    case AST_COMPOUND:
        ir_compound(n);
        break;
    case AST_RETURN:
        ir_return(n);
        break;
    case AST_LABEL:
        ir_label(GEN_LABEL(n));
        break;
    case AST_JUMP:
        ir_jmp(GEN_LABEL(n));
        break;
    case NULL_STMT:
        break;
    default:
        die("unexpected node '%s'", nname(n));
    }
}

static void ir_stmt(node_t *n)
{
    if (isexpr(n))
        ir_expr(n);
    else if (isdecl(n))
        ir_decl(n);
    else
        ir_gen(n);
}

static void ir_function(node_t *decl)
{
    node_t *stmt = DECL_BODY(decl);

    set_func_context();
    
    ir_stmt(stmt);
    DECL_X_IRS(decl) = func_irs;

    restore_func_context();
}

static void ir_globalvar(node_t *decl)
{
    
}

static void ir_init(void)
{
    tmps = new_table(NULL, GLOBAL);
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

void print_irs(struct vector *irs)
{
    println("IRS: %lld", vec_len(irs));
    for (int i = 0; i < vec_len(irs); i++) {
        struct ir *ir = vec_at(irs, i);
        println("%s = %p %s %p",
                ir->result->name,
                ir->arg1,
                rop2s(ir->op),
                ir->arg2);
    }
}
