#include <assert.h>
#include "cc.h"

static struct expr *cast_expr(void);
static struct expr *cond_expr1(struct expr *);
static struct expr *cond_expr(void);
static struct expr *unary_expr(void);

#define is_assign_op(op)    ((op == '=') || (op >= MULEQ && op <= RSHIFTEQ))

static struct expr *compound_literal(struct type * ty)
{
    struct expr *inits;
    struct source src = source;

    inits = initializer_list(ty);
    
    return actions.compound_literal(ty, inits, src);
}

static struct type *cast_type(void)
{
    struct type *ty;

    expect('(');
    ty = typename();
    expect(')');

    return ty;
}

/// primary-expression:
///   identifier
///   constant
///   string-literal
///   '(' expression ')'
///
static struct expr *primary_expr(void)
{
    int t = token->id;
    struct expr *ret = NULL;
    
    switch (t) {
    case ID:
        ret = actions.id(token);
        expect(t);
        break;
    case ICONSTANT:
        ret = actions.iconst(token);
        expect(t);
        break;
    case FCONSTANT:
        ret = actions.fconst(token);
        expect(t);
        break;
    case SCONSTANT:
        ret = actions.sconst(token);
        expect(t);
        break;
    case '(':
        if (first_typename(lookahead())) {
            struct type *ty = cast_type();
            ret = compound_literal(ty);
        } else {
            struct source src = source;
            struct expr *e;
            
            expect('(');
            e = expression();
            expect(')');

            ret = actions.paren(e, src);
        }
        break;
    default:
        error("invalid postfix expression at '%s'", tok2s(token));
        break;
    }

    return ret;
}

/// argument-expression-list:
///   assignment-expression
///   argument-expression-list ',' assignment-expression
///
static struct expr **argument_expr_list(void)
{
    struct vector *v = NULL;

    if (first_expr(token)) {
        v = vec_new();
        for (;;) {
            vec_push_safe(v, assign_expr());
            if (token->id != ',')
                break;
            expect(',');
        }
    } else if (token->id != ')') {
        error("expect assignment expression");
    }

    return vtoa(v, PERM);
}

static struct expr *postfix_expr1(struct expr *ret)
{
    for (; token->id == '[' || token->id == '(' || token->id == '.' ||
             token->id == DEREF || token->id == INCR || token->id == DECR;) {
        switch (token->id) {
        case '[':
            {
                struct source src = source;
                struct expr *index;

                expect('[');
                index = expression();
                expect(']');

                ret = actions.subscript(ret, index, src);
            }
            break;
        case '(':
            {
                struct source src = source;
                struct expr **args;

                expect('(');
                args = argument_expr_list();
                expect(')');

                ret = actions.funcall(ret, args, src);
            }
            break;
        case '.':
        case DEREF:
            {
                int t = token->id;
                struct source src = source;
                const char *name = NULL;

                expect(t);
                if (token->id == ID)
                    name = TOK_ID_STR(token);
                expect(ID);

                ret = actions.direction(ret, t, name, src);
            }
            break;
        case INCR:
        case DECR:
            {
                int t = token->id;
                struct source src = source;

                expect(t);

                ret = actions.post_increment(ret, t, src);
            }
            break;
        default:
            assert(0);
        }
    }

    return ret;
}

/// postfix-expression:
///   primary-expression
///   postfix-expression '[' expression ']'
///   postfix-expression '(' argument-expression-list[opt] ')'
///   postfix-expression '.' identifier
///   postfix-expression '->' identifier
///   postfix-expression '++'
///   postfix-expression '--'
///   '(' type-name ')' '{' initializer-list '}'
///   '(' type-name ')' '{' initializer-list ',' '}'
///
static struct expr *postfix_expr(void)
{
    struct expr *expr = primary_expr();

    return postfix_expr1(expr);
}

/// unary-expression:
///   postfix-expression
///   '++' unary-expression
///   '--' unary-expression
///   unary-operator cast-expression
///   'sizeof' unary-expression
///   'sizeof' '(' type-name ')'
///
/// unary-operator:
///   '&' '*' '+' '-' '~' '!'
///
static struct expr *unary_expr(void)
{
    int t = token->id;
    struct source src = source;
    
    switch (t) {
    case INCR:
    case DECR:
        expect(t);
        return actions.pre_increment(t, unary_expr(), src);
    case '+':
    case '-':
        expect(t);
        return actions.minus_plus(t, cast_expr(), src);
    case '~':
        expect(t);
        return actions.bitwise_not(cast_expr(), src);
    case '!':
        expect(t);
        return actions.logical_not(cast_expr(), src);
    case '&':
        expect(t);
        return actions.address(cast_expr(), src);
    case '*':
        expect(t);
        return actions.indirection(cast_expr(), src);
    case SIZEOF:
        {
            struct token *ahead;
            struct expr *n = NULL;
            struct type *ty = NULL;

            expect(t);
            ahead = lookahead();
            if (token->id == '(' && first_typename(ahead)) {
                ty = cast_type();
                if (token->id == '{') {
                    struct expr *node = compound_literal(ty);
                    n = postfix_expr1(node);
                }
            } else {
                n = unary_expr();
            }
            return actions.sizeofop(ty, n, src);
        }
    default:
        return postfix_expr();
    }
}

/// cast-expression:
///   unary-expression
///   '(' type-name ')' cast-expression
///
static struct expr *cast_expr(void)
{
    struct token *ahead = lookahead();
    struct source src = source;

    if (token->id == '(' && first_typename(ahead)) {
        struct type *ty = cast_type();
        if (token->id == '{') {
            struct expr *node = compound_literal(ty);
            return postfix_expr1(node);
        }

        struct expr *cast = cast_expr();
        return actions.castop(ty, cast, src);
    }
    return unary_expr();
}

/// multiplicative-expression:
///   cast-expression
///   multiplicative-expression '*' cast-expression
///   multiplicative-expression '/' cast-expression
///   multiplicative-expression '%' cast-expression
///
static struct expr *multiple_expr(void)
{
    struct expr *mulp1;

    mulp1 = cast_expr();
    while (token->id == '*' || token->id == '/' || token->id == '%') {
        int t = token->id;
        struct source src = source;
        expect(t);
        mulp1 = actions.bop(t, mulp1, cast_expr(), src);
    }

    return mulp1;
}

/// additive-expression:
///   multiplicative-expression
///   additive-expression '+' multiplicative-expression
///   additive-expression '-' multiplicative-expression
///
static struct expr *additive_expr(void)
{
    struct expr *add1;

    add1 = multiple_expr();
    while (token->id == '+' || token->id == '-') {
        int t = token->id;
        struct source src = source;
        expect(t);
        add1 = actions.bop(t, add1, multiple_expr(), src);
    }

    return add1;
}

/// shift-expression:
///   additive-expression
///   shift-expression '<<' additive-expression
///   shift-expression '>>' additive-expression
///
static struct expr *shift_expr(void)
{
    struct expr *shift1;

    shift1 = additive_expr();
    while (token->id == LSHIFT || token->id == RSHIFT) {
        int t = token->id;
        struct source src = source;
        expect(t);
        shift1 = actions.bop(t, shift1, additive_expr(), src);
    }

    return shift1;
}

/// relational-expression:
///   shift-expression
///   relational-expression '<' shift-expression
///   relational-expression '>' shift-expression
///   relational-expression '<=' shift-expression
///   relational-expression '>=' shift-expression
///
static struct expr *relation_expr(void)
{
    struct expr *rel;

    rel = shift_expr();
    while (token->id == '<' || token->id == '>' || token->id == LEQ || token->id == GEQ) {
        int t = token->id;
        struct source src = source;
        expect(t);
        rel = actions.bop(t, rel, shift_expr(), src);
    }

    return rel;
}

/// equality-expression:
///   relational-expression
///   equality-expression '==' relational-expression
///   equality-expression '!=' relational-expression
///
static struct expr *equality_expr(void)
{
    struct expr *equl;

    equl = relation_expr();
    while (token->id == EQL || token->id == NEQ) {
        int t = token->id;
        struct source src = source;
        expect(t);
        equl = actions.bop(t, equl, relation_expr(), src);
    }

    return equl;
}

/// AND-expression:
///   equality-expression
///   AND-expression '&' equality-expression
///
static struct expr *and_expr(void)
{
    struct expr *and1;

    and1 = equality_expr();
    while (token->id == '&') {
        struct source src = source;
        expect('&');
        and1 = actions.bop('&', and1, equality_expr(), src);
    }

    return and1;
}

/// exclusive-OR-expression:
///   AND-expression
///   exclusive-OR-expression '^' AND-expression
///
static struct expr *exclusive_or(void)
{
    struct expr *eor;

    eor = and_expr();
    while (token->id == '^') {
        struct source src = source;
        expect('^');
        eor = actions.bop('^', eor, and_expr(), src);
    }

    return eor;
}

/// inclusive-OR-expression:
///   exclusive-OR-expression
///   inclusive-OR-expression '|' exclusive-OR-expression
///
static struct expr *inclusive_or(void)
{
    struct expr *ior;

    ior = exclusive_or();
    while (token->id == '|') {
        struct source src = source;
        expect('|');
        ior = actions.bop('|', ior, exclusive_or(), src);
    }

    return ior;
}

/// logical-AND-expression:
///   inclusive-OR-expression
///   logical-AND-expression '&&' inclusive-OR-expression
///
static struct expr *logic_and(void)
{
    struct expr *and1;

    and1 = inclusive_or();
    while (token->id == ANDAND) {
        struct source src = source;
        expect(ANDAND);
        and1 = actions.logicop(ANDAND, and1, inclusive_or(), src);
    }

    return and1;
}

/// logical-OR-expression:
///   logical-AND-expression
///   logical-OR-expression '||' logical-AND-expression
///
static struct expr *logic_or(void)
{
    struct expr *or1;

    or1 = logic_and();
    while (token->id == OROR) {
        struct source src = source;
        expect(OROR);
        or1 = actions.logicop(OROR, or1, logic_and(), src);
    }

    return or1;
}

static struct expr *cond_expr1(struct expr *cond)
{
    struct expr *then, *els;
    struct source src = source;

    expect('?');
    then = expression();
    expect(':');
    els = cond_expr();

    return actions.condop(cond, then, els, src);
}

/// conditional-expression:
///   logical-OR-expression
///   logical-OR-expression '?' expression ':' conditional-expression
///
static struct expr *cond_expr(void)
{
    struct expr *or1 = logic_or();
    if (token->id == '?')
        return cond_expr1(or1);
    return or1;
}

/// assignment-expression:
///   conditional-expression
///   unary-expression assignment-operator assignment-expression
///
/// assignment-operator:
///   '=' '*=' '/=' '%=' '+=' '-=' '<<=' '>>=' '&=' '^=' '|='
///
struct expr *assign_expr(void)
{
    struct expr *or1 = logic_or();
    if (token->id == '?')
        return cond_expr1(or1);
    if (is_assign_op(token->id)) {
        struct source src = source;
        int t = token->id;
        expect(t);
        return actions.assignop(t, or1, assign_expr(), src);
    }
    return or1;
}

/// expression:
///   assignment-expression
///   expression ',' assignment-expression
///
struct expr *expression(void)
{
    struct expr *assign1;

    assign1 = assign_expr();
    while (token->id == ',') {
        struct source src = source;
        expect(',');
        assign1 = actions.commaop(assign1, assign_expr(), src);
    }
    return assign1;
}

long intexpr1(struct type *ty)
{
    struct source src = source;
    return actions.intexpr(cond_expr(), ty, src);
}

long intexpr(void)
{
    return intexpr1(NULL);
}

// for expression in conditional statement
struct expr *bool_expr(void)
{
    struct source src = source;
    return actions.bool_expr(expression(), src);
}

// for expression in switch statement
struct expr *switch_expr(void)
{
    struct source src = source;
    return actions.switch_expr(expression(), src);
}
