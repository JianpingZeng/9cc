#include "cc.h"

static struct expr * cast_expr();

static inline int is_assign_op(int t)
{
    return t == '=' ||
    t == MULEQ ||
    t == ADDEQ ||
    t == MINUSEQ ||
    t == DIVEQ ||
    t == MODEQ ||
    t == XOREQ ||
    t == BANDEQ ||
    t == BOREQ ||
    t == LSHIFTEQ ||
    t == RSHIFTEQ;
}

static struct expr * typename_expr()
{
    struct expr *expr;
    struct type *type;
    
    expect('(');
    type = typename();
    expect(')');
    expr = expr_node(CAST_EXPR, 0, NULL, NULL);
    expr->node.symbol = anonymous_symbol(&identifiers, SCOPE);
    expr->node.symbol->type = type;
    
    return expr;
}

static struct expr ** argument_expr_list()
{
    struct expr **args = NULL;
    
    if (firstexpr(token)) {
        struct vector *v = new_vector();
        for (;;) {
            vec_push(v, NODE(assign_expr()));
            if (token->id == ',')
                expect(',');
            else
                break;
        }
        args = (struct expr **)vtoa(v);
    } else if (token->id != ')') {
        error("expect assignment expression");
    }
    
    return args;
}

static struct expr * postfix_expr1(struct expr *ret)
{
    int t;
    
    for (;token->id == '[' || token->id == '(' || token->id == '.'
         || token->id == DEREF || token->id == INCR || token->id == DECR;) {
        switch (token->id) {
            case '[':
                t = token->id;
                expect('[');
                ret = expr_node(INDEX_EXPR, t, ret, expression());
                expect(']');
                break;
            case '(':
                t = token->id;
                expect('(');
                ret = expr_node(CALL_EXPR, 0, ret, NULL);
                ret->u.args = argument_expr_list();
                expect(')');
                break;
            case '.':
            case DEREF:
            {
                t = token->id;
                expect(t);
                if (token->id == ID) {
                    //TODO
                    
                } else {
                    error("expect identifier");
                }
                ret = expr_node(MEMBER_EXPR, t, ret, expr_node(REF_EXPR, ID, NULL, NULL));
                expect(ID);
            }
                break;
            case INCR:
            case DECR:
                t = token->id;
                expect(token->id);
                ret = expr_node(UNARY_EXPR, t, ret, NULL);
                break;
            default:
                assert(0);
        }
    }
    
    return ret;
}

static struct expr * postfix_expr()
{
    int t;
    struct symbol *sym;
    struct expr *ret;
    
    switch (token->id) {
        case ID:
        {
            t = token->id;
            sym = lookup_symbol(token->name, identifiers);
            if (sym)
                sym->refs++;
            else
                error("use of undeclared symbol '%s'", token->name);
            expect(t);
            ret = expr_node(REF_EXPR, ID, NULL, NULL);
            ret->node.symbol = sym;
        }
            break;
        case ICONSTANT:
        case FCONSTANT:
        {
            t = token->id;
            sym = lookup_symbol(token->name, constants);
            if (!sym) {
                sym = install_symbol(token->name, &constants, CONSTANT);
                sym->value = token->v.u;
                sym->type = token->v.type;
            }
            expect(t);
            ret = expr_node(t == ICONSTANT ? INTEGER_LITERAL : FLOAT_LITERAL, t, NULL, NULL);
            ret->node.symbol = sym;
        }
            break;
        case SCONSTANT:
        {
            t = token->id;
            sym = lookup_symbol(token->name, constants);
            if (!sym) {
                sym = install_symbol(token->name, &constants, CONSTANT);
                sym->type = token->v.type;
            }
            expect(t);
            ret = expr_node(STRING_LITERAL, t, NULL, NULL);
            ret->node.symbol = sym;
        }
            break;
        case '(':
        {
            struct token *ahead = lookahead();
            if (istypename(ahead)) {
                ret = typename_expr();
                KID0(ret) = NODE(initializer_list());
            } else {
                expect('(');
                ret = expr_node(PAREN_EXPR, 0, expression(), NULL);
                expect(')');
            }
        }
            break;
        default:
            ret = NULL;
            error("invalid postfix expression at '%s'", token->name);
            break;
    }
    
    return postfix_expr1(ret);
}

static struct expr * unary_expr()
{
    struct expr * uexpr;
    int t;
    struct token *ahead;
    
    switch (token->id) {
        case INCR:
        case DECR:
            t = token->id;
            expect(t);
            uexpr = expr_node(UNARY_EXPR, t, unary_expr(), NULL);
            uexpr->u.prefix = 1;
            break;
        case '&':
        case '*':
        case '+':
        case '-':
        case '~':
        case '!':
            t = token->id;
            expect(t);
            uexpr = expr_node(UNARY_EXPR, t, cast_expr(), NULL);
            break;
        case SIZEOF:
            t = token->id;
            expect(token->id);
            ahead = lookahead();
            if (token->id == '(' && istypename(ahead)) {
                struct expr *texpr = typename_expr();
                if (token->id == '{') {
                    KID0(texpr) = NODE(initializer_list());
                    texpr = postfix_expr1(texpr);
                }
                uexpr = expr_node(UNARY_EXPR, t, texpr, NULL);
            } else {
                uexpr = expr_node(UNARY_EXPR, t, unary_expr(), NULL);
            }
            break;
        default:
            uexpr = postfix_expr();
            break;
    }
    
    return uexpr;
}

static struct expr * cast_expr()
{
    struct expr * cast1;
    struct token * ahead = lookahead();
    
    if (token->id == '(' && istypename(ahead)) {
        cast1 = typename_expr();
        if (token->id == '{') {
            KID0(cast1) = NODE(initializer_list());
            cast1 = postfix_expr1(cast1);
        } else {
            KID0(cast1) = (struct node*)cast_expr();
        }
    } else {
        cast1 = unary_expr();
    }
    
    return cast1;
}

static struct expr * multiple_expr()
{
    struct expr * mulp1;
    
    mulp1 = cast_expr();
    while (token->id == '*' || token->id == '/' || token->id == '%') {
        int t = token->id;
        expect(token->id);
        mulp1 = expr_node(BINARY_EXPR, t, mulp1, cast_expr());
    }
    
    return mulp1;
}

static struct expr * additive_expr()
{
    struct expr * add1;
    
    add1 = multiple_expr();
    while (token->id == '+' || token->id == '-') {
        int t = token->id;
        expect(token->id);
        add1 = expr_node(BINARY_EXPR, t, add1, multiple_expr());
    }
    
    return add1;
}

static struct expr * shift_expr()
{
    struct expr * shift1;
    
    shift1 = additive_expr();
    while (token->id == LSHIFT || token->id == RSHIFT) {
        int t = token->id;
        expect(token->id);
        shift1 = expr_node(BINARY_EXPR, t, shift1, additive_expr());
    }
    
    return shift1;
}

static struct expr * relation_expr()
{
    struct expr * rel;
    
    rel = shift_expr();
    while (token->id == '<' || token->id == '>' || token->id == LEQ || token->id == GEQ) {
        int t = token->id;
        expect(token->id);
        rel = expr_node(BINARY_EXPR, t, rel, shift_expr());
    }
    
    return rel;
}

static struct expr * equality_expr()
{
    struct expr * equl;
    
    equl = relation_expr();
    while (token->id == EQ || token->id == NEQ) {
        int t = token->id;
        expect(token->id);
        equl = expr_node(BINARY_EXPR, t, equl, relation_expr());
    }
    
    return equl;
}

static struct expr * and_expr()
{
    struct expr * and1;
    
    and1 = equality_expr();
    while (token->id == '&') {
        expect('&');
        and1 = expr_node(BINARY_EXPR, '&', and1, equality_expr());
    }
    
    return and1;
}

static struct expr * exclusive_or()
{
    struct expr * eor;
    
    eor = and_expr();
    while (token->id == '^') {
        expect('^');
        eor = expr_node(BINARY_EXPR, '^', eor, and_expr());
    }
    
    return eor;
}

static struct expr * inclusive_or()
{
    struct expr * ior;
    
    ior = exclusive_or();
    while (token->id == '|') {
        expect('|');
        ior = expr_node(BINARY_EXPR, '|', ior, exclusive_or());
    }
    
    return ior;
}

static struct expr * logic_and()
{
    struct expr * and1;
    
    and1 = inclusive_or();
    while (token->id == AND) {
        expect(AND);
        and1 = expr_node(BINARY_EXPR, AND, and1, inclusive_or());
    }
    
    return and1;
}

static struct expr * cond_expr(struct expr *e)
{
    struct expr * ret;
    int t;
    
    ret = e ? e : cast_expr();
    for (;;) {
        switch (token->id) {
            case '*':
            case '/':
            case '%':
                t = token->id;
                expect(token->id);
                ret = expr_node(BINARY_EXPR, t, ret, cast_expr());
                break;
            case '+':
            case '-':
                t = token->id;
                expect(token->id);
                ret = expr_node(BINARY_EXPR, t, ret, multiple_expr());
                break;
            case LSHIFT:
            case RSHIFT:
                t = token->id;
                expect(token->id);
                ret = expr_node(BINARY_EXPR, t, ret, additive_expr());
                break;
            case '>':
            case '<':
            case LEQ:
            case GEQ:
                t = token->id;
                expect(token->id);
                ret = expr_node(BINARY_EXPR, t, ret, shift_expr());
                break;
            case EQ:
            case NEQ:
                t = token->id;
                expect(token->id);
                ret = expr_node(BINARY_EXPR, t, ret, relation_expr());
                break;
            case '&':
                t = token->id;
                expect(token->id);
                ret = expr_node(BINARY_EXPR, t, ret, equality_expr());
                break;
            case '^':
                t = token->id;
                expect(token->id);
                ret = expr_node(BINARY_EXPR, t, ret, and_expr());
                break;
            case '|':
                t = token->id;
                expect(token->id);
                ret = expr_node(BINARY_EXPR, t, ret, exclusive_or());
                break;
            case AND:
                t = token->id;
                expect(token->id);
                ret = expr_node(BINARY_EXPR, t, ret, inclusive_or());
                break;
            case OR:
                t = token->id;
                expect(token->id);
                ret = expr_node(BINARY_EXPR, t, ret, logic_and());
                break;
            default:
                if (token->id == '?') {
                    struct expr *o, *e, *c;
                    o = ret;
                    expect('?');
                    e = expression();
                    expect(':');
                    c = cond_expr(NULL);
                    ret = expr_node(COND_EXPR, '?', NULL, NULL);
                    ret->u.cond.o = o;
                    ret->u.cond.e = e;
                    ret->u.cond.c = c;
                }
                return ret;
        }
    }
}

struct expr * assign_expr()
{
    struct expr *assign1;
    struct token *ahead = lookahead();
    
    if (token->id == '(' && istypename(ahead)) {
        // type-name
        struct expr *texpr = typename_expr();
        if (token->id == '{') {
            // unary
            KID0(texpr) = NODE(initializer_list());
            texpr = postfix_expr1(texpr);
            if (is_assign_op(token->id)) {
                int t = token->id;
                expect(token->id);
                assign1 = expr_node(BINARY_EXPR, t, texpr, assign_expr());
            } else {
                assign1 = cond_expr(texpr);
            }
        } else {
            // cast
            KID0(texpr) = (struct node*)cast_expr();
            assign1 = cond_expr(texpr);
        }
    } else {
        struct expr * uexpr = unary_expr();
        if (is_assign_op(token->id)) {
            int t = token->id;
            expect(token->id);
            assign1 = expr_node(BINARY_EXPR, t, uexpr, assign_expr());
        } else {
            assign1 = cond_expr(uexpr);
        }
    }
    
    return assign1;
}

struct expr * expression()
{
    struct expr *expr;
    
    expr = assign_expr();
    while (token->id == ',') {
        expect(',');
        expr = expr_node(BINARY_EXPR, ',', expr, assign_expr());
    }
    
    return expr;
}

// TODO: cast initializer list
static int isconstexpr(struct node *expr)
{
    if (!expr || !isexpr(expr))
        return 0;
    
    switch (expr->id) {
        case BINARY_EXPR:
            return isconstexpr(KID0(expr)) && isconstexpr(KID1(expr));
            
        case UNARY_EXPR:
        case CAST_EXPR:
            return isconstexpr(KID0(expr));
            
        case INITS_EXPR:
            //TODO
            return 0;
            
        case INTEGER_LITERAL:
        case FLOAT_LITERAL:
        case STRING_LITERAL:
            return 1;
            
        case REF_EXPR:
            return 0;
            
        case CALL_EXPR:
            return 0;
            
        default:
            assert(0);
    }
}

//TODO
static int eval(struct expr *expr)
{
    if (!expr)
        return 0;
    
    assert(isconstexpr(NODE(expr)));
    
    return 0;
}

struct expr * constant_expr()
{
    struct source src = source;
    struct expr *expr = cond_expr(NULL);
    if (isconstexpr(NODE(expr))) {
        return expr;
    } else {
        errorf(src, "expect constant expression");
        return NULL;
    }
}

// TODO
int intexpr()
{
    struct expr *expr;
    int val;
    
    expr = constant_expr();
    val = eval(expr);
    
    return val;
}
