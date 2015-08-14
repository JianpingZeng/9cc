#include "cc.h"

static struct expr * cast_expr();
static struct type * reduce(struct expr *expr);
static void ensure_assignable(struct expr *asign);
static int islvalue(struct expr *expr);
static struct expr * cond_expr();
static struct expr * cond_expr1(struct expr *o);

static inline int is_assign_op(int t)
{
    return t == '=' ||
    t == MULEQ || t == ADDEQ || t == MINUSEQ || t == DIVEQ ||
    t == MODEQ ||
    t == XOREQ ||
    t == BANDEQ || t == BOREQ ||
    t == LSHIFTEQ || t == RSHIFTEQ;
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
                    struct type *lty = reduce(ret);
                    struct type *basety = t == DEREF ? lty->type : lty;
                    if (isstruct(basety) || isunion(basety)) {
                        int i;
                        for (i=0; i < ARRAY_SIZE(basety->u.s.fields); i++) {
                            struct field *f = basety->u.s.fields[i];
                            if (f->name && !strcmp(f->name, token->name))
                                break;
                        }
                        if (i >= ARRAY_SIZE(basety->u.s.fields))
                            error("no member named '%s' in '%s'", token->name, basety->name);
                    } else {
                        error("member reference base type '%s' is not a struct or union", basety->name);
                    }
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
            if (t == '*') {
                struct type *p = reduce(KID0(uexpr));
                if (!ispointer(p))
                    error("indirection requires pointer operand ('%s' invalid)", p->name);
            } else if (t == '&') {
                struct type *p = reduce(KID0(uexpr));
                if (!islvalue(KID0(uexpr)))
                    error("cannot take the address of an rvalue of type '%s'", p->name);
            }
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

static struct expr * logic_or()
{
    struct expr * or1;
    
    or1 = logic_and();
    while (token->id == OR) {
        expect(OR);
        or1 = expr_node(BINARY_EXPR, OR, or1, logic_and());
    }
    
    return or1;
}

static struct expr * cond_expr1(struct expr *o)
{
    struct expr *ret, *e, *c;
    expect('?');
    
    e = expression();
    expect(':');
    c = cond_expr();
    
    ret = expr_node(COND_EXPR, '?', NULL, NULL);
    ret->u.cond.o = o;
    ret->u.cond.e = e;
    ret->u.cond.c = c;
    
    return ret;
}

static struct expr * cond_expr()
{
    struct expr * or1 = logic_or();
    if (token->id == '?')
        return cond_expr1(or1);
    return or1;
}

struct expr * assign_expr()
{
    struct expr *or1 = logic_or();
    if (token->id == '?')
        return cond_expr1(or1);
    if (is_assign_op(token->id)) {
        int t = token->id;
        expect(token->id);
        or1 = expr_node(BINARY_EXPR, t, or1, assign_expr());
        ensure_assignable(or1);
    }
    return or1;
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

//TODO
static int eval(struct expr *expr, int *error)
{
    if (!expr || (error && *error))
        return 0;
    
    assert(isexpr(expr));
    
    struct expr *l = (struct expr *)KID0(expr);
    struct expr *r = (struct expr *)KID1(expr);
#define L eval(l, error)
#define R eval(r, error)
    
    switch (NODE(expr)->id) {
        case BINARY_EXPR:
        {
            switch (expr->op) {
                case ',': return L , R;
                case '+': return L + R;
                case '-': return L - R;
                case '*': return L * R;
                case '/': return L / R;
                case '%': return L % R;
                case LSHIFT: return L << R;
                case RSHIFT: return L >> R;
                case '>': return L > R;
                case '<': return L < R;
                case GEQ: return L >= R;
                case LEQ: return L <= R;
                case EQ: return L == R;
                case NEQ: return L != R;
                case AND: return L && R;
                case OR: return L || R;
                case '^': return L ^ R;
                case '|': return L | R;
                case '&': return L & R;
                    
                case '=':
                case MULEQ:case ADDEQ:case MINUSEQ:case DIVEQ:
                case MODEQ:case XOREQ:case BANDEQ:case BOREQ:
                case LSHIFTEQ:case RSHIFTEQ:
                    if (error)
                        *error = 1;
                    return 0;
                    
                default:
                    assert(0);
            }
        }
            
        case UNARY_EXPR:
        {
            switch (expr->op) {
                case '&':
                case '*':
                case INCR:
                case DECR:
                    if (error)
                        *error = 1;
                    return 0;
                case '+': return +L;
                case '-': return -L;
                case '~': return ~L;
                case '!': return !L;
                case SIZEOF:
                    
                    
                default:
                    assert(0);
            }
        }
            
        case CAST_EXPR:
            //TODO
            return 0;
            
        case INDEX_EXPR:
            //TODO
            return 0;
            
        case MEMBER_EXPR:
            //TODO
            return 0;
            
        case COND_EXPR:
        {
            struct expr *o = expr->u.cond.o;
            struct expr *e = expr->u.cond.e;
            struct expr *c = expr->u.cond.c;
            int ret0, ret1, ret2, err;
            err = 0;
            ret0 = eval(o, &err);
            if (err) goto end;
            err = 0;
            ret1 = eval(e, &err);
            if (err) goto end;
            if (ret0) {
                return ret1;
            } else {
                err = 0;
                ret2 = eval(c, &err);
                if (err) goto end;
                return ret2;
            }
        end:
            if (error)
                *error = 1;
            return 0;
        }
            
        case PAREN_EXPR:
            return L;
            
        case INITS_EXPR:
        {
            if (expr->u.args)
                return eval(expr->u.args[0], error);
            else
                return 0;
        }
            
        case INTEGER_LITERAL:
	    {
            struct symbol *sym = expr->node.symbol;
            union value v = sym->value;
            if (sym->type->op == INT)
                return v.i;
            else if (sym->type->op == UNSIGNED)
                return v.u;
            else
                assert(0);
	    }
	    
        case FLOAT_LITERAL:
        {
            struct symbol *sym = expr->node.symbol;
            union value v = sym->value;
            if (sym->type == floattype || sym->type == doubletype)
                return v.d;
            else if (sym->type == longdoubletype)
                return v.ld;
            else
                assert(0);
        }
	    
        case STRING_LITERAL:
            return (int) expr->node.symbol->name;
            
        case REF_EXPR:
            if (error)
                *error = 1;
            return 0;
            
        case CALL_EXPR:
            if (error)
                *error = 1;
            return 0;
            
        default:
            assert(0);
    }
}

static struct expr * eval_intexpr(int *value)
{
    struct source src = source;
    struct expr *expr = cond_expr();
    int error = 0;
    int val = eval(expr, &error);
    if (value)
        *value = val;
    if (error == 0) {
        return expr;
    } else {
        errorf(src, "expect constant expression");
        return NULL;
    }
}

struct expr * constant_expr()
{
    return eval_intexpr(NULL);
}

int intexpr()
{
    int val = 0;
    (void) eval_intexpr(&val);
    return val;
}

// TODO
static struct type * reduce(struct expr *expr)
{
    return NULL;
}

// TODO
static int islvalue(struct expr *expr)
{
    
}

// TODO
static void ensure_assignable(struct expr *asign)
{
    struct expr *l = (struct expr *)KID0(asign);
    struct expr *r = (struct expr *)KID1(asign);
    
    if (l == NULL || r == NULL)
        error("assign expression invalid");
    
    assert(isexpr(l) && isexpr(r));
    
    
    
}
