#include <assert.h>
#include "lex.h"
#include "internal.h"

typedef long cpp_num;
static cpp_num expr(void);
static cpp_num cond(void);
static cpp_num cast(void);

static void skip_pair(int p1, int p2)
{
    int nests = 0;

    expect(p1);
    for (;;) {
        if (token->id == p1) {
            nests++;
        } else if (token->id == p2) {
            if (nests == 0)
                break;
            nests--;
        }
        gettok();
    }
    expect(p2);
}

static inline void typename(void)
{
    skip_pair('(', ')');
}

static inline void initializer_list(void)
{
    skip_pair('{', '}');
}

static inline void args_list(void)
{
    skip_pair('(', ')');
}

/// primary-expression:
///   identifier
///   constant
///   string-literal
///   '(' expression ')'
///
static cpp_num primary(void)
{
    int t = token->id;
    cpp_num num;
    
    switch (t) {
    case ICONSTANT:
        num = token->u.lit.v.i;
        expect(t);
        break;

    case FCONSTANT:
        cpp_error("floating constant in preprocessor expression");
        num = 0;
        expect(t);
        break;

    case '(':
        expect('(');
        num = expr();
        expect(')');
        break;

    case ID:
    case SCONSTANT:
    default:
        cpp_error("token '%s' is not valid in preprocessor expression", tok2s(token));
        num = 0;
        expect(t);
        break;
    }

    return num;
}

static cpp_num postfix1(cpp_num num)
{
    int t = token->id;
    for (; t == '[' || t == '(' || t == '.' || t == DEREF || t == INCR || t == DECR;) {
        switch (t) {
        case '[':
            expect('[');
            expr();
            expect(']');
            break;
        case '(':
            expect('(');
            args_list();
            expect(')');
            break;
        case '.':
        case DEREF:
            expect(t);
            expect(ID);
            break;
        case INCR:
        case DECR:
            expect(t);
            break;
        default:
            assert(0);
        }
    }
    return num;
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
static cpp_num postfix(void)
{
    cpp_num num;

    num = primary();
    return postfix1(num);
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
static cpp_num unary(void)
{
    int t = token->id;
    switch (t) {
    case INCR:
    case DECR:
        cpp_error("increment/decrement is not valid in preprocessor expression");
        expect(t);
        unary();
        return 0;
    case '+':
        expect(t);
        return cast();
    case '-':
        expect(t);
        return - cast();
    case '~':
        expect(t);
        return ~ cast();
    case '!':
        expect(t);
        return ! cast();
    case '&':
    case '*':
        cpp_error("address/indirection is not valid in preprocessor expression");
        expect(t);
        cast();
        return 0;
    case SIZEOF:
        cpp_error("sizeof is not valid in preprocessor expression");
        expect(t);
        if (token->id == '(') {
            typename();
            if (token->id == '{') {
                initializer_list();
                postfix1(0);
            }
        } else {
            unary();
        }
        return 0;
    default:
        return postfix();
    }
}

/// cast-expression:
///   unary-expression
///   '(' type-name ')' cast-expression
///
static cpp_num cast(void)
{
    return unary();
}

/// multiplicative-expression:
///   cast-expression
///   multiplicative-expression '*' cast-expression
///   multiplicative-expression '/' cast-expression
///   multiplicative-expression '%' cast-expression
///
static cpp_num multiple(void)
{
    cpp_num num;

    num = cast();
    while (token->id == '*' || token->id == '/' || token->id == '%') {
        int t = token->id;
        expect(t);
        if (t == '*') {
            num *= cast();
        } else if (t == '/') {
            cpp_num num2 = cast();
            if (num2)
                num /= num2;
            else
                cpp_error("division by zero");
        } else {
            cpp_num num2 = cast();
            if (num2)
                num %= num2;
            else
                cpp_error("division by zero");
        }
    }

    return num;
}

/// additive-expression:
///   multiplicative-expression
///   additive-expression '+' multiplicative-expression
///   additive-expression '-' multiplicative-expression
///
static cpp_num additive(void)
{
    cpp_num num;

    num = multiple();
    while (token->id == '+' || token->id == '-') {
        int t = token->id;
        expect(t);
        if (t == '+')
            num += multiple();
        else
            num -= multiple();
    }

    return num;
}

/// shift-expression:
///   additive-expression
///   shift-expression '<<' additive-expression
///   shift-expression '>>' additive-expression
///
static cpp_num shift(void)
{
    cpp_num num;

    num = additive();
    while (token->id == LSHIFT || token->id == RSHIFT) {
        int t = token->id;
        expect(t);
        if (t == LSHIFT)
            num <<= additive();
        else
            num >>= additive();
    }

    return num;
}

/// relational-expression:
///   shift-expression
///   relational-expression '<' shift-expression
///   relational-expression '>' shift-expression
///   relational-expression '<=' shift-expression
///   relational-expression '>=' shift-expression
///
static cpp_num relational(void)
{
    cpp_num num;

    num = shift();
    while (token->id == '<' || token->id == '>' || token->id == LEQ || token->id == GEQ) {
        int t = token->id;
        expect(t);
        if (t == '<')
            num = num < shift();
        else if (t == '>')
            num = num > shift();
        else if (t == LEQ)
            num = num <= shift();
        else
            num = num >= shift();
    }

    return num;
}

/// equality-expression:
///   relational-expression
///   equality-expression '==' relational-expression
///   euqality-expression '!=' relational-expression
///
static cpp_num equality(void)
{
    cpp_num num;

    num = relational();
    while (token->id == EQL || token->id == NEQ) {
        int t = token->id;
        expect(t);
        if (t == EQL)
            num = num == relational();
        else
            num = num != relational();
    }

    return num;
}

/// AND-expression:
///   equality-expression
///   AND-expression '&' equality-expression
///
static cpp_num and(void)
{
    cpp_num num;

    num = equality();
    while (token->id == '&') {
        expect('&');
        num &= equality();
    }

    return num;
}

/// exclusive-OR-expression:
///   AND-expression
///   exclusive-OR-expression '^' AND-expression
///
static cpp_num exclusive_or(void)
{
    cpp_num num;

    num = and();
    while (token->id == '^') {
        expect('^');
        num ^= and();
    }

    return num;
}

/// inclusive-OR-expression:
///   exclusive-OR-expression
///   inclusive-OR-expression '|' exclusive-OR-expression
///
static cpp_num inclusive_or(void)
{
    cpp_num num;

    num = exclusive_or();
    while (token->id == '|') {
        expect('|');
        num |= exclusive_or();
    }

    return num;
}

/// logical-AND-expression:
///   inclusive-OR-expression
///   logical-AND-expression '&&' inclusive-OR-expression
///
static cpp_num logical_and(void)
{
    cpp_num num;

    num = inclusive_or();
    while (token->id == ANDAND) {
        expect(ANDAND);
        cpp_num num2 = inclusive_or();
        num = num && num2;
    }

    return num;
}

/// logical-OR-expression:
///   logical-AND-expression
///   logical-OR-expression '||' logical-AND-expression
///
static cpp_num logical_or(void)
{
    cpp_num num;

    num = logical_and();
    while (token->id == OROR) {
        expect(OROR);
        cpp_num num2 = logical_and();
        num = num || num2;
    }

    return num;
}

static cpp_num cond1(cpp_num num)
{
    cpp_num then, els;

    expect('?');
    then = expr();
    expect(':');
    els = cond();

    return num ? then : els;
}

/// conditional-expression:
///   logical-OR-expression
///   logical-OR-expression '?' expression ':' conditional-expression
///
static cpp_num cond(void)
{
    cpp_num num;

    num = logical_or();
    if (token->id == '?')
        return cond1(num);

    return num;
}

/// assignment-expression:
///   conditional-expression
///   unary-expression assignment-operator assignment-expression
///
/// assignment-operator:
///   '=' '*=' '/=' '%=' '+=' '-=' '<<=' '>>=' '&=' '^=' '|='
///
static cpp_num assign(void)
{
    cpp_num num;

    num = logical_or();
    if (token->id == '?')
        return cond1(num);
    if (is_assign_tok(token)) {
        cpp_error("assignment is not allowed in preprocessor expression");
        expect(token->id);
        num = assign();
    }

    return num;
}

/// expression:
///   assignment-expression
///   expression ',' assignment-expression
///
static cpp_num expr(void)
{
    cpp_num num;

    num = assign();
    while (token->id == ',') {
        expect(',');
        num = assign();
    }

    return num;
}

/// constant-expression:
///   conditional-expression
///
bool eval_cpp_const_expr(void)
{
    gettok();

    bool b = cond();
    if (token->id != EOI)
        cpp_error("missing binary operator before token '%s'", tok2s(token));

    return b;
}
