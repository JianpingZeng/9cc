#include "cc.h"

/**
 *  Intermediate Representation
 *
 *  three address code
 */

static void ir_stmt(node_t *n);

static struct vector *func_irs;
static const char *func_ret_label;

static struct ir * new_ir(int op, struct operand *l, struct operand *r)
{
    struct ir *ir = zmalloc(sizeof(struct ir));
    ir->op = op;
    ir->l = l;
    ir->r = r;
    return ir;
}

static void ir_emit(struct ir *ir)
{
    vec_push(func_irs, ir);
}

static struct ir * make_label_operand(const char *label)
{
    
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

static void ir_bop(node_t *n)
{
    
}

static void ir_uop(node_t *n)
{
    
}

static void ir_cond(node_t *n)
{
    
}

static void ir_member(node_t *n)
{
    
}

static void ir_subscript(node_t *n)
{
    
}

static void ir_conv(node_t *n)
{
    
}

static void ir_funcall(node_t *n)
{
    
}

static void ir_ref_expr(node_t *n)
{
    
}

static void ir_integer_literal(node_t *n)
{
    
}

static void ir_float_literal(node_t *n)
{
    
}

static void ir_string_literal(node_t *n)
{
    
}

static void ir_compound_literal(node_t *n)
{
    
}

static void ir_expr(node_t *n)
{
    switch (AST_ID(n)) {
    case BINARY_OPERATOR:
        ir_bop(n);
        break;
    case UNARY_OPERATOR:
        ir_uop(n);
        break;
    case PAREN_EXPR:
        ir_expr(EXPR_OPERAND(n, 0));
        break;
    case COND_EXPR:
        ir_cond(n);
        break;
    case MEMBER_EXPR:
        ir_member(n);
        break;
    case SUBSCRIPT_EXPR:
        ir_subscript(n);
        break;
    case CAST_EXPR:
    case CONV_EXPR:
        ir_conv(n);
        break;
    case CALL_EXPR:
        ir_funcall(n);
        break;
    case REF_EXPR:
        if (EXPR_OP(n) == ENUM)
            ir_integer_literal(n);
        else
            ir_ref_expr(n);
        break;
    case INTEGER_LITERAL:
        ir_integer_literal(n);
        break;
    case FLOAT_LITERAL:
        ir_float_literal(n);
        break;
    case STRING_LITERAL:
        ir_string_literal(n);
        break;
    case COMPOUND_LITERAL:
        ir_compound_literal(n);
        break;
    case INITS_EXPR:
    case VINIT_EXPR:
    default:
        die("unexpected node '%s'", nname(n));
    }
}

static void ir_jmp(const char *label)
{
    ir_emit(make_jmp_ir(IR_JMP, label));
}

static void ir_if_jmp(int jop, const char *label)
{
    ir_emit(make_jmp_ir(jop, label));
}

static void ir_label(const char *label)
{
    ir_emit(make_label_ir(label));
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
    DECL_X_IRS(decl) = firs;

    restore_func_context();
}

static void ir_globalvar(node_t *decl)
{
    
}

node_t * ir(node_t *tree)
{
    cc_assert(istudecl(tree) && errors == 0);

    for (int i = 0; i < LIST_LEN(DECL_EXTS(tree)); i++) {
        node_t *decl = DECL_EXTS(tree)[i];
        if (isfuncdef(decl))
            ir_function(decl);
        else if (isvardecl(decl))
            ir_globalvar(decl);
    }
    
    return tree;
}
