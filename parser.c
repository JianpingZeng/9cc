#include <assert.h>
#include "cc.h"

// topmost expression that can be simplified
static struct tree *parse_expr0(void);
static struct tree *parse_expr(void);
static struct tree *parse_assign_expr(void);
static long parse_intexpr1(struct type *);
static long parse_intexpr(void);
// for expression in conditional statement
static struct tree *parse_expr_in_cond(void);
// for expression in switch statement
static struct tree *parse_expr_in_switch(void);
static struct tree *parse_initializer_list(struct type *);
static void parse_decls(decl_fp dcl);
static struct type *parse_typename(void);
#define next_token_of(func)  (func(lookahead()))

/*=================================================================*
 *                        expression                               *
 *=================================================================*/

// binary op precedence
enum {
    PREC_NONE,
    PREC_LOGICOR,
    PREC_LOGICAND,
    PREC_BOR,
    PREC_XOR,
    PREC_BAND,
    PREC_EQ,
    PREC_REL,
    PREC_SHIFT,
    PREC_ADD,
    PREC_MUL,
    NUM_PRECS
};

static struct tree *parse_cast_expr(void);
static struct tree *parse_cond_expr1(struct tree *);
static struct tree *parse_cond_expr(void);
static struct tree *parse_unary_expr(void);

static struct tree *parse_compound_literal(struct type *ty)
{
    struct source src = source;
    struct tree *inits = parse_initializer_list(ty);
    
    return actions.compound_literal(ty, inits, src);
}

static struct type *parse_cast_type(void)
{
    struct type *ty;

    expect('(');
    ty = parse_typename();
    match(')', skip_to_bracket);

    return ty;
}

/*
 primary-expression:
   identifier
   constant
   string-literal
   '(' expression ')'
*/
static struct tree *parse_primary_expr(void)
{
    struct tree *ret = NULL;
 
    switch (token->id) {
    case ID:
        ret = actions.id(token);
        gettok();
        break;
    case ICONSTANT:
        ret = actions.iconst(token);
        gettok();
        break;
    case FCONSTANT:
        ret = actions.fconst(token);
        gettok();
        break;
    case SCONSTANT:
        ret = actions.sconst(token);
        gettok();
        break;
    case '(':
        if (next_token_of(first_typename)) {
            struct type *ty = parse_cast_type();
            ret = parse_compound_literal(ty);
        } else {
            struct source src = source;
            struct tree *e;
            
            gettok();
            e = parse_expr();
            match(')', skip_to_bracket);
            ret = actions.paren(e, src);
        }
        break;
    default:
        error("invalid postfix expression at '%t'", token);
        break;
    }

    return ret;
}

/*
 argument-expression-list:
   assignment-expression
   argument-expression-list ',' assignment-expression
*/
static struct tree **parse_argument_expr_list(void)
{
    struct list *l = NULL;

    if (first_expr(token)) {
        for (;;) {
            struct tree *assign = parse_assign_expr();
            if (assign)
                l = list_append(l, assign);

            if (token_is_not(','))
                break;
            gettok();
        }
    } else if (token_is_not(')')) {
        error("expect assignment expression");
    }

    return ltoa(&l, FUNC);
}

static struct tree *parse_postfix_expr1(struct tree *ret)
{
    for (; token_is('[') || token_is('(') || token_is('.') ||
             token_is(DEREF) || token_is(INCR) || token_is(DECR);) {
        switch (token->id) {
        case '[':
            {
                struct source src = source;
                struct tree *index;

                gettok();
                index = parse_expr();
                match(']', skip_to_squarebracket);
                ret = actions.subscript(ret, index, src);
            }
            break;
        case '(':
            {
                struct source src = source;
                struct tree **args;

                gettok();
                args = parse_argument_expr_list();
                match(')', skip_to_bracket);
                ret = actions.funcall(ret, args, src);
            }
            break;
        case '.':
        case DEREF:
            {
                int t = token->id;
                struct source src = source;
                const char *name = NULL;

                gettok();
                if (token_is(ID))
                    name = TOK_ID_STR(token);
                expect(ID);
                ret = actions.direction(t, name, ret, src);
            }
            break;
        case INCR:
        case DECR:
            {
                int t = token->id;
                struct source src = source;

                gettok();
                ret = actions.post_increment(t, ret, src);
            }
            break;
        default:
            CC_UNAVAILABLE();
        }
    }

    return ret;
}

/*
 postfix-expression:
   primary-expression
   postfix-expression '[' expression ']'
   postfix-expression '(' argument-expression-list[opt] ')'
   postfix-expression '.' identifier
   postfix-expression '->' identifier
   postfix-expression '++'
   postfix-expression '--'
   '(' type-name ')' '{' initializer-list '}'
   '(' type-name ')' '{' initializer-list ',' '}'
*/
static struct tree *parse_postfix_expr(void)
{
    struct tree *expr = parse_primary_expr();

    return parse_postfix_expr1(expr);
}

/*
 unary-expression:
   postfix-expression
   '++' unary-expression
   '--' unary-expression
   unary-operator cast-expression
   'sizeof' unary-expression
   'sizeof' '(' type-name ')'

 unary-operator:
   '&' '*' '+' '-' '~' '!'
*/
static struct tree *parse_unary_expr(void)
{
    int t = token->id;
    struct source src = source;
    
    switch (t) {
    case INCR:
    case DECR:
        gettok();
        return actions.pre_increment(t, parse_unary_expr(), src);
    case '+':
    case '-':
        gettok();
        return actions.minus_plus(t, parse_cast_expr(), src);
    case '~':
        gettok();
        return actions.bitwise_not(parse_cast_expr(), src);
    case '!':
        gettok();
        return actions.logical_not(parse_cast_expr(), src);
    case '&':
        gettok();
        return actions.address(parse_cast_expr(), src);
    case '*':
        gettok();
        return actions.indirection(parse_cast_expr(), src);
    case SIZEOF:
        {
            struct tree *n = NULL;
            struct type *ty = NULL;

            gettok();
            if (token_is('(') && next_token_of(first_typename)) {
                ty = parse_cast_type();
                if (token_is('{')) {
                    struct tree *expr = parse_compound_literal(ty);
                    n = parse_postfix_expr1(expr);
                }
            } else {
                n = parse_unary_expr();
            }
            return actions.sizeofop(ty, n, src);
        }
    default:
        return parse_postfix_expr();
    }
}

/*
 cast-expression:
   unary-expression
   '(' type-name ')' cast-expression
*/
static struct tree *parse_cast_expr(void)
{
    struct source src = source;

    if (token_is('(') && next_token_of(first_typename)) {
        struct type *ty = parse_cast_type();

        if (token_is('{')) {
            struct tree *expr = parse_compound_literal(ty);
            return parse_postfix_expr1(expr);
        }

        struct tree *cast = parse_cast_expr();
        return actions.cast(ty, cast, src);
    }
    return parse_unary_expr();
}

/*
 multiplicative-expression:
   cast-expression
   multiplicative-expression '*' cast-expression
   multiplicative-expression '/' cast-expression
   multiplicative-expression '%' cast-expression

 additive-expression:
   multiplicative-expression
   additive-expression '+' multiplicative-expression
   additive-expression '-' multiplicative-expression

 shift-expression:
   additive-expression
   shift-expression '<<' additive-expression
   shift-expression '>>' additive-expression

 relational-expression:
   shift-expression
   relational-expression '<' shift-expression
   relational-expression '>' shift-expression
   relational-expression '<=' shift-expression
   relational-expression '>=' shift-expression

 equality-expression:
   relational-expression
   equality-expression '==' relational-expression
   equality-expression '!=' relational-expression

 AND-expression:
   equality-expression
   AND-expression '&' equality-expression

 exclusive-OR-expression:
   AND-expression
   exclusive-OR-expression '^' AND-expression

 inclusive-OR-expression:
   exclusive-OR-expression
   inclusive-OR-expression '|' exclusive-OR-expression

 logical-AND-expression:
   inclusive-OR-expression
   logical-AND-expression '&&' inclusive-OR-expression

 logical-OR-expression:
   logical-AND-expression
   logical-OR-expression '||' logical-AND-expression
 */
static struct tree *parse_binary_expr(void)
{
    // Parse a binary expression using precedence.
    struct {
        struct tree *expr;
        int prec;
        int op;
        struct source src;
    } stack[NUM_PRECS];
    
    int sp;                     // stack pointer

    // pop stack[sp] and stack[sp-1]
#define POP()                                                   \
    do {                                                        \
        stack[sp-1].expr = actions.bop(stack[sp].op,            \
                                       stack[sp-1].expr,        \
                                       stack[sp].expr,          \
                                       stack[sp].src);          \
        sp--;                                                   \
    } while (0)
    
    // init stack
    stack[0].src = source;
    stack[0].expr = parse_cast_expr();
    stack[0].prec = PREC_NONE;
    sp = 0;
    
    while (1) {
        int prec;
        int t;
        struct source src;

        switch (token->id) {
        case '*':
        case '%':
        case '/':
            prec = PREC_MUL;
            break;
        case '+':
        case '-':
            prec = PREC_ADD;
            break;
        case LSHIFT:
        case RSHIFT:
            prec = PREC_SHIFT;
            break;
        case '<':
        case '>':
        case GEQ:
        case LEQ:
            prec = PREC_REL;
            break;
        case NEQ:
        case EQL:
            prec = PREC_EQ;
            break;
        case '&':
            prec = PREC_BAND;
            break;
        case '^':
            prec = PREC_XOR;
            break;
        case '|':
            prec = PREC_BOR;
            break;
        case ANDAND:
            prec = PREC_LOGICAND;
            break;
        case OROR:
            prec = PREC_LOGICOR;
            break;
        default:
            goto out;
        }

        while (prec <= stack[sp].prec)
            POP();

        t = token->id;
        src = source;
        gettok();
        // push
        sp++;
        stack[sp].expr = parse_cast_expr();
        stack[sp].prec = prec;
        stack[sp].op = t;
        stack[sp].src = src;
    }

 out:
    while (sp > 0)
        POP();
    
    return stack[0].expr;
#undef POP
}

static struct tree *parse_cond_expr1(struct tree *cond)
{
    struct tree *then, *els;
    struct source src = source;

    expect('?');
    then = parse_expr();
    expect(':');
    els = parse_cond_expr();

    return actions.cond(cond, then, els, src);
}

/*
 conditional-expression:
   logical-OR-expression
   logical-OR-expression '?' expression ':' conditional-expression
*/
static struct tree *parse_cond_expr(void)
{
    struct tree *or1 = parse_binary_expr();

    if (token_is('?'))
        return parse_cond_expr1(or1);
    return or1;
}

/*
 assignment-expression:
   conditional-expression
   unary-expression assignment-operator assignment-expression

 assignment-operator:
   '=' '*=' '/=' '%=' '+=' '-=' '<<=' '>>=' '&=' '^=' '|='
*/
static struct tree *parse_assign_expr(void)
{
    struct tree *or1 = parse_binary_expr();

    if (token_is('?'))
        return parse_cond_expr1(or1);
    if (is_assign_tok(token)) {
        struct source src = source;
        int t = token->id;
        gettok();
        return actions.assign(t, or1, parse_assign_expr(), src);
    }
    return or1;
}

/*
 expression:
   assignment-expression
   expression ',' assignment-expression
*/
static struct tree *parse_expr(void)
{
    struct tree *assign1 = parse_assign_expr();

    while (token_is(',')) {
        struct source src = source;
        gettok();
        assign1 = actions.comma(assign1, parse_assign_expr(), src);
    }
    return assign1;
}

static struct tree *parse_expr0(void)
{
    return root(parse_expr());
}

static long parse_intexpr1(struct type *ty)
{
    struct source src = source;
    return actions.intexpr(parse_cond_expr(), ty, src);
}

static long parse_intexpr(void)
{
    return parse_intexpr1(NULL);
}

// for expression in conditional statement
static struct tree *parse_expr_in_cond(void)
{
    struct source src = source;
    return actions.bool_expr(parse_expr(), src);
}

// for expression in switch statement
static struct tree *parse_expr_in_switch(void)
{
    struct source src = source;
    return actions.switch_expr(parse_expr(), src);
}

/*=================================================================*
 *                        statement                                *
 *=================================================================*/
static void statement(int cnt, int brk, struct swtch *swtch);

/*
 expression-statement:
   expression[opt] ';'
*/
static void expr_stmt(void)
{    
    if (token_is(';')) {
        // do nothing
    } else if (first_expr(token)) {
        struct tree *e = parse_expr0();
        if (e) actions.gen(e);
    } else {
        error("missing statement before '%t'", token);
    }
    
    expect(';');
}

/*
 The entire **if** statement forms its own block scope, as do the
 substatements even if they are not compound statements. This serves
 to restrict the scope of objects and types that might be created
 as a side effect of using compound literal or type names.
*/
/*
 selection-statement:
   'if' '(' expression ')' statement
   'if' '(' expression ')' statement 'else' statement
*/
static void if_stmt(int lab, int cnt, int brk, struct swtch *swtch)
{
    struct tree *cond;

    enter_scope();

    expect(IF);
    expect('(');
    cond = parse_expr_in_cond();
    match(')', skip_to_bracket);

    actions.branch(cond, 0, lab);
    
    enter_scope();
    statement(cnt, brk, swtch);
    exit_scope();

    if (token_is(ELSE)) {
        gettok();
        actions.jump(lab+1);
        actions.label(lab);
        enter_scope();
        statement(cnt, brk, swtch);
        exit_scope();
        actions.label(lab+1);
    } else {
        actions.label(lab);
    }

    exit_scope();
}

/*
 Each iterative statement(do/while/for) forms its own block scope,
 as do the substatements even if they are not compound statements.
*/
/*
 iteration-statement:
   'while' '(' expression ')' statement
*/
static void while_stmt(int lab, struct swtch *swtch)
{
    struct tree *cond;

    enter_scope();

    expect(WHILE);
    expect('(');
    cond = parse_expr_in_cond();
    match(')', skip_to_bracket);

    actions.jump(lab+1);
    actions.label(lab);
    statement(lab+1, lab+2, swtch);
    actions.label(lab+1);
    actions.branch(cond, lab, 0);
    actions.label(lab+2);

    exit_scope();
}

/*
 iteration-statement:
   'do' statement 'while' '(' expression ')' ';'
*/
static void do_while_stmt(int lab, struct swtch *swtch)
{
    struct tree *cond;

    enter_scope();

    expect(DO);
    actions.label(lab);
    statement(lab+1, lab+2, swtch);
    expect(WHILE);
    expect('(');
    cond = parse_expr_in_cond();
    match(')', skip_to_bracket);
    expect(';');
    actions.label(lab+1);
    actions.branch(cond, lab, 0);
    actions.label(lab+2);

    exit_scope();
}

/*
 iteration-statement:
   'for' '(' expression[opt] ';' expression[opt] ';' expression[opt] ')' statement
   'for' '(' declaration expression[opt] ';' expression[opt] ')' statement
*/
static void for_stmt(int lab, struct swtch *swtch)
{
    struct tree *init = NULL;
    struct tree *cond = NULL;
    struct tree *ctrl = NULL;
    
    enter_scope();

    expect(FOR);
    expect('(');

    if (token_is(';')) {
        gettok();
    } else {
        if (first_decl(token)) {
            // declaration
            parse_decls(actions.localdecl);
        } else {
            // expression
            init = parse_expr0();
            expect(';');
        }
    }

    if (token_is_not(';'))
        cond = parse_expr_in_cond();

    expect(';');

    if (token_is_not(')'))
        ctrl = parse_expr0();

    match(')', skip_to_bracket);

    if (init)
        actions.gen(init);
    actions.jump(lab+3);
    actions.label(lab);
    statement(lab+1, lab+2, swtch);
    actions.label(lab+1);
    if (ctrl)
        actions.gen(ctrl);
    actions.label(lab+3);
    actions.branch(cond, lab, 0);
    actions.label(lab+2);
    
    exit_scope();
}

/*
 Switch Statements Notes:

 1. The control expression is subject to the usual unary convresion.

 2. When comparing the control expression and the **case** expressions,
    the **case** expressions are converted to the type of the control
    expression (after the usual unary conversion).
*/
/*
 selection-statement:
   'switch' '(' expression ')' statement
*/
static void switch_stmt(int lab, int cnt)
{
    struct tree *expr;
    struct source src = source;
    struct swtch *swtch = NEWS0(struct swtch, FUNC);

    expect(SWITCH);
    expect('(');
    expr = parse_expr_in_switch();
    match(')', skip_to_bracket);

    swtch->src = src;
    swtch->type = expr ? expr->type : inttype;

    actions.jump(lab);
    statement(cnt, lab+1, swtch);
    actions.jump(lab+1);
    actions.label(lab);
    
    // gen switch code
    for (struct cse *cs = swtch->cases; cs; cs = cs->link) {
        struct tree *e = actions.bop(EQL,
                                     expr, cnsti(cs->value, longtype),
                                     src);
        actions.branch(e, cs->label, 0);
    }

    if (swtch->defalt)
        actions.jump(swtch->defalt->label);
    
    actions.label(lab+1);
}

/*
 labeled-statement:
   'case' constant-expression ':' statement
*/
static void case_stmt(int lab, int cnt, int brk, struct swtch *swtch)
{
    struct source src = source;
    long value;

    expect(CASE);
    value = parse_intexpr1(swtch ? swtch->type : inttype);
    expect(':');

    if (swtch) {
        struct cse *cse = NEWS0(struct cse, FUNC);
        cse->src = src;
        cse->value = value;
        cse->label = lab;

        check_case_duplicates(cse, swtch);

        // link
        cse->link = swtch->cases;
        swtch->cases = cse;

        actions.label(lab);
    } else {
        error_at(src, "'case' statement not in switch statement");
    }

    // always parse even if not in a switch statement
    statement(cnt, brk, swtch);
}

/*
 labeled-statement:
   'default' ':' statement
*/
static void default_stmt(int lab, int cnt, int brk, struct swtch *swtch)
{
    struct source src = source;

    expect(DEFAULT);
    expect(':');

    // print before parsing statement
    if (swtch) {
        if (swtch->defalt)
            error_at(src,
                     "multiple default labels in one switch, previous case defined here: %S",
                     swtch->defalt->src);

        // new default case
        struct cse *defalt = NEWS0(struct cse, FUNC);
        defalt->src = src;
        defalt->label = lab;
        // set
        swtch->defalt = defalt;

        actions.label(lab);
    } else {
        error_at(src, "'default' statement not in switch statement");
    }
    
    statement(cnt, brk, swtch);
}

/*
 labled-statement:
   identifier ':' statement 
*/
static void label_stmt(int lab, int cnt, int brk, struct swtch *swtch)
{
    struct source src = source;
    const char *id = NULL;

    if (token_is(ID))
        id = TOK_ID_STR(token);
    expect(ID);
    expect(':');

    // install label before parsing body
    if (id) {
        struct symbol *sym = lookup(id, func.labels);
        if (!sym) {
            sym = install(id, &func.labels, LOCAL, FUNC);
            sym->defined = true;
            sym->x.label = lab;
        } else if (!sym->defined) {
            sym->defined = true;
        } else {
            error_at(src,
                     "redefinition of label '%s', previous label defined here: %S",
                     sym->name, sym->src);
        }

        actions.label(sym->x.label);
    }

    statement(cnt, brk, swtch);
}

/*
 jump-statement:
   'goto' identifier ';'
*/
static void goto_stmt(int lab)
{
    struct source src = source;
    const char *id = NULL;
    
    expect(GOTO);
    if (token_is(ID))
        id = TOK_ID_STR(token);
    expect(ID);
    expect(';');

    // lookup
    if (id) {
        struct symbol *sym = lookup(id, func.labels);
        if (!sym) {
            sym = install(id, &func.labels, LOCAL, FUNC);
            sym->x.label = lab;
        }

        if (!sym->defined)
            mark_goto(id, src);

        actions.jump(sym->x.label);
    }
}

/*
 jump-statement:
   'break' ';'
*/
static void break_stmt(int brk)
{
    struct source src = source;
    
    expect(BREAK);
    expect(';');

    if (brk)
        actions.jump(brk);
    else
        error_at(src, "'break' statement not in loop or switch statement");
}

/*
 jump-statement:
   'continue' ';'
*/
static void continue_stmt(int cnt)
{
    struct source src = source;
    
    expect(CONTINUE);
    expect(';');

    if (cnt)
        actions.jump(cnt);
    else
        error_at(src, "'continue' statement not in loop statement");
}

/*
 jump-statement:
   'return' expression[opt] ';'
*/
static void return_stmt(void)
{
    struct source src = source;
    struct tree *e = NULL;
    bool isnull = false;

    expect(RETURN);
    if (token_is(';'))
        isnull = true;
    else
        e = parse_expr();

    expect(';');
    actions.ret(e, isnull, src);
}

/*
 statement:
   labeled-statement
   compound-statement
   expression-statement
   selection-statement
   iteration-statement
   jump-statement
*/
static void statement(int cnt, int brk, struct swtch *swtch)
{
    switch (token->id) {
    case '{':
        compound_stmt(NULL, cnt, brk, swtch);
        break;
    case IF:
        if_stmt(genlabel(2), cnt, brk, swtch);
        break;
    case SWITCH:
        switch_stmt(genlabel(2), cnt);
        break;
    case WHILE:
        while_stmt(genlabel(3), swtch);
        break;
    case DO:
        do_while_stmt(genlabel(3), swtch);
        break;
    case FOR:
        for_stmt(genlabel(4), swtch);
        break;
    case GOTO:
        goto_stmt(genlabel(1));
        break;
    case CONTINUE:
        continue_stmt(cnt);
        break;
    case BREAK:
        break_stmt(brk);
        break;
    case RETURN:
        return_stmt();
        break;
    case CASE:
        case_stmt(genlabel(1), cnt, brk, swtch);
        break;
    case DEFAULT:
        default_stmt(genlabel(1), cnt, brk, swtch);
        break;
    case ID:
        if (next_token_is(':')) {
            label_stmt(genlabel(1), cnt, brk, swtch);
            break;
        }
        // fall through
    default:
        expr_stmt();
        break;
    }
}

/*
 compound-statement:
   '{' block-item-list[opt] '}'

 block-item-list:
   block-item
   block-item-list block-item

 block-item:
   declaration
   statement
*/
void compound_stmt(void (*cb) (void), int cnt, int brk, struct swtch *swtch)
{
    expect('{');
    enter_scope();

    if (cb) cb();

    while (first_decl(token) || first_expr(token) || first_stmt(token)) {
        if (first_decl(token))
            // declaration
            parse_decls(actions.localdecl);
        else
            // statement
            statement(cnt, brk, swtch);
    }

    match('}', skip_to_brace);
    exit_scope();
}

/*=================================================================*
 *                        initialization                           *
 *=================================================================*/

static void parse_initializer_list1(struct desig *, struct init **);

static struct desig *parse_designator(struct desig *desig)
{
    struct list *list = NULL;
    
    assert(token_is('.') || token_is('['));
    
    do {
        if (token_is('.')) {
            gettok();
            struct source src = source;
            // only create list item when desig != NULL
            if (desig) {
                if (token_is(ID))
                    list = list_append(list,
                                       new_desig_name(TOK_ID_STR(token),
                                                      src));
                else
                    // set desig to NULL
                    desig = NULL;
            }
            expect(ID);
        } else {
            expect('[');
            struct source src = source;
            long index = parse_intexpr();
            match(']', skip_to_squarebracket);
            // only create list item when desig != NULL
            if (desig)
                list = list_append(list, new_desig_index(index, src));
        }
    } while (token_is('.') || token_is('['));

    expect('=');

    return desig ? actions.designator(desig, ltoa(&list, FUNC)) : NULL;
}

static void parse_initializer1(struct desig **pdesig, struct init **pinit)
{
    if (token_is('{')) {
        // begin a new root designator
        struct desig *desig = *pdesig;
        struct desig *d;

        if (desig->kind == DESIG_NONE) {
            d = desig;
            d->braces++;
        } else {
            d = new_desig(DESIG_NONE);
            d->type = desig->type;
            d->offset = desig->offset;
            d->src = desig->src;
            d->all = desig;         // all link
        }
        
        parse_initializer_list1(d, pinit);
    } else {
        actions.element_init(pdesig, parse_assign_expr(), pinit);
    }
}

static void parse_initializer_list1(struct desig *desig,
                                    struct init **pinit)
{
    struct desig *d = desig;
    int next = 0;
    
    expect('{');

    if (token_is('}')) {
        actions.element_init(&desig, zinit(desig->type), pinit);
    } else {
        while (1) {
            if (token_is('.') || token_is('['))
                d = parse_designator(desig);
            else
                d = next_designator(d, next++);

            parse_initializer1(&d, pinit);

            if (token_is_not(','))
                break;

            expect(',');

            if (token_is('}'))
                break;
        }
    }

    match('}', skip_to_brace);
}

/*
 initializer:
       assignment-expression
       '{' initializer-list '}'
       '{' initializer-list ',' '}'
 [GNU] '{' '}'
*/
static struct tree *parse_initializer(struct type *ty)
{
    if (token_is('{'))
        return parse_initializer_list(ty);
    else
        return parse_assign_expr();
}

/*
 initializer-list:
   designation[opt] initializer
   initializer-list ',' designation[opt] initializer

 designation:
   designator-list '='

 designator-list:
   designator
   designator-list designator

 designator:
   '[' constant-expression ']'
   '.' identifier
*/
static struct tree *parse_initializer_list(struct type *ty)
{
    if (ty) {
        struct desig desig = {
            .kind = DESIG_NONE,
            .type = ty,
            .offset = 0,
            .src = source
        };
        struct init *ilist = NULL;

        parse_initializer_list1(&desig, &ilist);        

        return actions.initializer_list(ty, ilist);
    } else {
        parse_initializer_list1(NULL, NULL);
        return NULL;
    }
}

/*=================================================================*
 *                        declaration                              *
 *=================================================================*/

// 0 means both
enum { DIRECT = 1, ABSTRACT };
static void parse_param_declarator(struct type **, struct token **);
static struct type *parse_tag_decl(void);

static void attach_type(struct type **typelist, struct type *type)
{
    if (*typelist) {
        struct type *tp = *typelist;
        while (tp && tp->type)
            tp = tp->type;
        
        tp->type = type;
    } else {
        *typelist = type;
    }
}

static void prepend_type(struct type **typelist, struct type *type)
{
    attach_type(&type, *typelist);
    *typelist = type;
}

static void exit_params(struct symbol *params[])
{
    assert(params);
    if (params[0] && !params[0]->defined)
        error_at(params[0]->src,
                 "a parameter list without types is only allowed "
                 "in a function definition");

    if (cscope > PARAM)
        exit_scope();

    exit_scope();
}

/*
 declaration-specifier:
   storage-class-specifier declaration-specifiers[opt]
   type-specifier          declaration-specifiers[opt]
   type-qualifier          declaration-specifiers[opt]
   function-specifier      declaration-specifiers[opt]

 storage-class-specifier:
   'auto'
   'extern'
   'register'
   'static'
   'typedef'

 type-qualifier:
   'const'
   'volatile'
   'restrict'

 function-specifier:
   'inline'

 type-specifier:
   'void'
   'char'
   'short'
   'int'
   'long'
   'float'
   'double'
   'signed'
   'unsigned'
   '_Bool'
   '_Complex'
   '_Imaginary'
   enum-specifier
   struct-or-union-specifier
   typedef-name

 typedef-name:
   identifier
*/
static struct type *parse_specifiers(int *sclass, int *fspec)
{
    int cls, sign, size, type;
    int cons, vol, res, inl;
    struct type *basety;
    int ci;                        // _Complex, _Imaginary

    basety = NULL;
    cls = sign = size = type = 0;
    cons = vol = res = inl = 0;
    ci = 0;
    if (sclass == NULL)
        cls = AUTO;

    for (;;) {
        int *p, t = token->id;
        struct token *tok = token;
        switch (t) {
        case AUTO:
        case EXTERN:
        case REGISTER:
        case STATIC:
        case TYPEDEF:
            p = &cls;
            gettok();
            break;

        case CONST:
            p = &cons;
            gettok();
            break;

        case VOLATILE:
            p = &vol;
            gettok();
            break;

        case RESTRICT:
            p = &res;
            gettok();
            break;

        case INLINE:
            p = &inl;
            gettok();
            break;

        case ENUM:
        case STRUCT:
        case UNION:
            p = &type;
            basety = parse_tag_decl();
            break;

        case LONG:
            if (size == LONG) {
                t = LONG + LONG;
                size = 0;        // clear
            }
            // fall through
        case SHORT:
            p = &size;
            gettok();
            break;

        case FLOAT:
            p = &type;
            basety = floattype;
            gettok();
            break;

        case DOUBLE:
            p = &type;
            basety = doubletype;
            gettok();
            break;

        case VOID:
            p = &type;
            basety = voidtype;
            gettok();
            break;

        case CHAR:
            p = &type;
            basety = chartype;
            gettok();
            break;

        case INT:
            p = &type;
            basety = inttype;
            gettok();
            break;

        case _BOOL:
            p = &type;
            basety = booltype;
            gettok();
            break;

        case SIGNED:
        case UNSIGNED:
            p = &sign;
            gettok();
            break;

        case _COMPLEX:
        case _IMAGINARY:
            p = &ci;
            gettok();
            break;

        case ID:
            {
                const char *id = TOK_ID_STR(token);
                struct symbol *sym = lookup(id, identifiers);
                if (sym && sym->sclass == TYPEDEF) {
                    use(sym);
                    basety = sym->type;
                    p = &type;
                    gettok();
                } else {
                    p = NULL;
                }
            }
            break;

        default:
            p = NULL;
            break;
        }

        if (p == NULL)
            break;

        if (*p != 0) {
            if (p == &cls) {
                if (sclass)
                    error_at(tok->src,
                             "duplicate storage class '%t'", tok);
                else
                    error_at(tok->src,
                             "type name does not allow storage class "
                             "to be specified");
            } else if (p == &inl) {
                if (fspec)
                    warning_at(tok->src,
                               "duplicate '%t' declaration specifier",
                               tok);
                else
                    error_at(tok->src, "function specifier not allowed");
            } else if (p == &cons || p == &res || p == &vol) {
                warning_at(tok->src,
                           "duplicate '%t' declaration specifier",
                           tok);
            } else if (p == &ci) {
                error_at(tok->src,
                         "duplicate _Complex/_Imaginary specifier '%t'",
                         tok);
            } else if (p == &sign) {
                error_at(tok->src,
                         "duplicate signed/unsigned speficier '%t'", tok);
            } else if (p == &type || p == &size) {
                error_at(tok->src, "duplicate type specifier '%t'", tok);
            } else {
                CC_UNAVAILABLE();
            }
        }

        *p = t;
    }

    // default is int
    if (type == 0) {
        if (sign == 0 && size == 0)
            error("missing type specifier");
        type = INT;
        basety = inttype;
    }
    // type check
    if ((size == SHORT && type != INT) ||
        (size == LONG + LONG && type != INT) ||
        (size == LONG && type != INT && type != DOUBLE)) {
        if (size == LONG + LONG)
            error("%s %s %s is invalid",
                  id2s(size/2), id2s(size/2), id2s(type));
        else
            error("%s %s is invalid", id2s(size), id2s(type));
    } else if (sign && type != INT && type != CHAR) {
        error("'%s' cannot be signed or unsigned", id2s(type));
    } else if (ci && type != DOUBLE && type != FLOAT) {
        error("'%s' cannot be %s", id2s(type), id2s(ci));
    }

    if (type == CHAR && sign)
        basety = sign == UNSIGNED ? unsignedchartype : signedchartype;
    else if (size == SHORT)
        basety = sign == UNSIGNED ? unsignedshorttype : shorttype;
    else if (type == INT && size == LONG)
        basety = sign == UNSIGNED ? unsignedlongtype : longtype;
    else if (size == LONG + LONG)
        basety = sign == UNSIGNED ? unsignedlonglongtype : longlongtype;
    else if (type == DOUBLE && size == LONG)
        basety = longdoubletype;
    else if (sign == UNSIGNED)
        basety = unsignedinttype;

    // qulifier
    if (cons)
        basety = qual(CONST, basety);
    if (vol)
        basety = qual(VOLATILE, basety);
    if (res)
        basety = qual(RESTRICT, basety);

    if (sclass)
        *sclass = cls;
    if (fspec)
        *fspec = inl;

    return basety;
}

/*
 parameter-type-list:
   parameter-list
   parameter-list ',' '...'

 parameter-list:
   parameter-declaration
   parameter-list parameter-declaration

 parameter-declaration:
   declaration-specifier declarator
   declaration-specifier abstract-declarator[opt]
*/
static struct symbol **parse_prototype(struct type *ftype)
{
    struct list *list = NULL;

    while (1) {
        struct type *basety = NULL;
        int sclass, fspec;
        struct type *ty = NULL;
        struct token *id = NULL;
        struct symbol *sym;
        struct source src = source;

        basety = parse_specifiers(&sclass, &fspec);
        parse_param_declarator(&ty, &id);
        attach_type(&ty, basety);

        sym = actions.paramdecl(id ? TOK_ID_STR(id) : NULL,
                                ty, sclass, fspec, NULL,
                                id ? id->src : src);
        list = list_append(list, sym);

        if (token_is_not(','))
            break;

        gettok();
        if (token_is(ELLIPSIS)) {
            TYPE_VARG(ftype) = 1;
            gettok();
            break;
        }
    }

    // check
    return actions.prototype(ftype, ltoa(&list, FUNC));
}

/*
 identifier-list:
   identifier
   identifier-list ',' identifier
*/
static struct symbol **parse_oldstyle(struct type *ftype)
{
    struct list *params = NULL;

    while (1) {
        if (token_is(ID)) {
            const char *name = TOK_ID_STR(token);
            struct symbol *sym;

            sym = actions.paramdecl(name, inttype, 0, 0, NULL, token->src);
            sym->defined = false;
            params = list_append(params, sym);
        }
        expect(ID);
        if (token_is_not(','))
            break;
        gettok();
    }

    return ltoa(&params, FUNC);
}

static struct symbol **parse_parameters(struct type *ftype)
{
    struct symbol **params;

    if (first_decl(token)) {
        // prototype
        int i;
        struct type **proto;

        params = parse_prototype(ftype);
        proto = newarray(sizeof(struct type *), length(params) + 1, PERM);
        for (i = 0; params[i]; i++)
            proto[i] = params[i]->type;

        proto[i] = NULL;
        TYPE_PROTO(ftype) = proto;
        TYPE_OLDSTYLE(ftype) = 0;
    } else if (token_is(ID)) {
        // oldstyle
        params = parse_oldstyle(ftype);
        TYPE_OLDSTYLE(ftype) = 1;
    } else if (token_is(')')) {
        params = vtoa(NULL, FUNC);
        TYPE_OLDSTYLE(ftype) = 1;
    } else {
        params = vtoa(NULL, FUNC);
        TYPE_OLDSTYLE(ftype) = 1;
        if (token_is(ELLIPSIS))
            error("ISO C requires a named parameter before '...'");
        else
            error("expect parameter declarator at '%t'", token);
        gettok();
    }

    return params;
}

static void parse_array_qualifiers(struct type *atype)
{
    int cons, vol, res;
    int *p;
    cons = vol = res = 0;
    while (token->kind == CONST) {
        int t = token->id;
        struct source src = source;
        switch (t) {
        case CONST:
            p = &cons;
            gettok();
            break;

        case VOLATILE:
            p = &vol;
            gettok();
            break;

        case RESTRICT:
            p = &res;
            gettok();
            break;

        default:
            CC_UNAVAILABLE();
        }

        if (*p != 0)
            warning_at(src, "duplicate type qualifier '%s'", id2s(*p));

        *p = t;
    }

    if (cons)
        TYPE_A_CONST(atype) = 1;
    if (vol)
        TYPE_A_VOLATILE(atype) = 1;
    if (res)
        TYPE_A_RESTRICT(atype) = 1;
}

static struct type *parse_array(int abstract)
{
    struct type *atype = array_type(NULL);

    //NOTE: '*' is in `first_expr`
    switch (abstract) {
    case ABSTRACT:
        if (token_is('*') && next_token_is(']')) {
            gettok();
            TYPE_A_STAR(atype) = 1;
        } else if (first_expr(token)) {
            struct source src = source;
            actions.array_index(atype, parse_assign_expr(), src);
        }
        break;

    case DIRECT:
    case 0:
        if (token_is(STATIC)) {
            gettok();
            TYPE_A_STATIC(atype) = 1;
            parse_array_qualifiers(atype);
            struct source src = source;
            actions.array_index(atype, parse_assign_expr(), src);
        } else {
            parse_array_qualifiers(atype);
            if (token_is(STATIC)) {
                gettok();
                TYPE_A_STATIC(atype) = 1;
                struct source src = source;
                actions.array_index(atype, parse_assign_expr(), src);
            } else if (token_is('*') && next_token_is(']')) {
                gettok();
                TYPE_A_STAR(atype) = 1;
            } else if (first_expr(token)) {
                struct source src = source;
                actions.array_index(atype, parse_assign_expr(), src);
            }
        }
        break;

    default:
        CC_UNAVAILABLE();
    }

    return atype;
}

static struct type *parse_func_or_array(int abstract,
                                        struct symbol ***params)
{
    struct type *ty = NULL;

    for (; token_is('(') || token_is('[');) {
        if (token_is('[')) {
            struct type *atype;
            gettok();
            atype = parse_array(abstract);
            match(']', skip_to_squarebracket);
            attach_type(&ty, atype);
        } else {
            struct symbol **args;
            struct type *ftype = func_type(NULL);
            gettok();
            /*
              To make it easy to distinguish between
              'paramaters in parameter' and
              'compound statement of function definition',
              they both may be at scope LOCAL (aka PARAM+1),
              so enter scope again to make things easy.
             */
            enter_scope();
            if (cscope > PARAM)
                enter_scope();
            args = parse_parameters(ftype);
            if (params && *params == NULL)
                *params = args;
            else
                exit_params(args);
            match(')', skip_to_bracket);
            attach_type(&ty, ftype);
        }
    }

    return ty;
}

/*
 pointer:
   '*' type-qualifier-list[opt]
   '*' type-qualifier-list[opt] pointer

 type-qualifier-list:
   type-qualifier
   type-qualifier-list type-qualifier
*/
static struct type *parse_ptr(void)
{
    struct type *ret = NULL;
    int con, vol, res, type;

    assert(token_is('*'));

    while (1) {
        int *p, t = token->id;
        switch (token->id) {
        case CONST:
            p = &con;
            break;

        case VOLATILE:
            p = &vol;
            break;

        case RESTRICT:
            p = &res;
            break;

        case '*':
            {
                struct type *pty = ptr_type(NULL);
                con = vol = res = type = 0;
                p = &type;
                prepend_type(&ret, pty);
            }
            break;

        default:
            p = NULL;
            break;
        }

        if (p == NULL)
            break;

        if (*p != 0)
            warning("duplicate type qulifier '%t'", token);

        *p = t;

        if (t == CONST || t == VOLATILE || t == RESTRICT)
            ret = qual(t, ret);

        gettok();
    }

    return ret;
}

/*
 abstract-declarator:
   pointer
   pointer[opt] direct-abstract-declarator

 direct-abstract-declarator:
   '(' abstract-declarator ')'
   direct-abstract-declarator[opt] '[' assignment-expression[opt] ']'
   direct-abstract-declarator[opt] '[' '*' ']'
   direct-abstract-declarator[opt] '(' parameter-type-list[opt] ')'
*/
static void parse_abstract_declarator(struct type **ty)
{
    assert(ty);

    if (token_is('*') || token_is('(') || token_is('[')) {
        if (token_is('*')) {
            struct type *pty = parse_ptr();
            prepend_type(ty, pty);
        }

        if (token_is('(')) {
            if (next_token_of(first_decl)) {
                struct type *faty = parse_func_or_array(ABSTRACT, NULL);
                prepend_type(ty, faty);
            } else {
                struct type *type1 = *ty;
                struct type *rtype = NULL;
                expect('(');
                parse_abstract_declarator(&rtype);
                match(')', skip_to_bracket);
                if (token_is('[') || token_is('(')) {
                    struct type *faty = parse_func_or_array(ABSTRACT, NULL);
                    attach_type(&faty, type1);
                    attach_type(&rtype, faty);
                } else {
                    attach_type(&rtype, type1);
                }
                *ty = rtype;
            }
        } else if (token_is('[')) {
            struct type *faty = parse_func_or_array(ABSTRACT, NULL);
            prepend_type(ty, faty);
        }
    } else {
        error("expect '(', '[' or '*'");
    }
}

/*
 declarator:
   pointer[opt] direct-declarator

 direct-declarator:
   identifier
   '(' declarator ')'
   direct-declarator '[' type-qualifier-list[opt] assignment-expression[opt] ']'
   direct-declarator '[' 'static' type-qualifier-list[opt] assignment-expression ']'
   direct-declarator '[' type-qualifier-list 'static' assignment-expression ']'
   direct-declarator '[' type-qualifier-list[opt] '*' ']'
   direct-declarator '(' parameter-type-list ')'
   direct-declarator '(' identifier-list[opt] ')'
*/
static void parse_declarator(struct type **ty,
                             struct token **id,
                             struct symbol ***params)
{
    assert(ty && id);

    if (token_is('*')) {
        struct type *pty = parse_ptr();
        prepend_type(ty, pty);
    }

    if (token_is(ID)) {
        *id = token;
        gettok();
        if (token_is('[') || token_is('(')) {
            struct type *faty = parse_func_or_array(DIRECT, params);
            prepend_type(ty, faty);
        }
    } else if (token_is('(')) {
        struct type *type1 = *ty;
        struct type *rtype = NULL;
        gettok();
        parse_declarator(&rtype, id, params);
        match(')', skip_to_bracket);
        if (token_is('[') || token_is('(')) {
            struct type *faty = parse_func_or_array(DIRECT, params);
            attach_type(&faty, type1);
            attach_type(&rtype, faty);
        } else {
            attach_type(&rtype, type1);
        }
        *ty = rtype;
    } else {
        error("expect identifier or '('");
    }
}

// Both 'abstract-declarator' and 'direct-declarator' are allowed.
static void parse_param_declarator(struct type **ty, struct token **id)
{
    assert(ty && id);
    
    if (token_is('*')) {
        struct type *pty = parse_ptr();
        prepend_type(ty, pty);
    }

    if (token_is('(')) {
        if (next_token_is(')') || next_token_of(first_decl)) {
            struct type *faty = parse_func_or_array(0, NULL);
            prepend_type(ty, faty);
        } else {
            struct type *type1 = *ty;
            struct type *rtype = NULL;
            expect('(');
            parse_param_declarator(&rtype, id);
            match(')', skip_to_bracket);
            if (token_is('(') || token_is('[')) {
                struct type *faty = parse_func_or_array(0, NULL);
                attach_type(&faty, type1);
                attach_type(&rtype, faty);
            } else {
                attach_type(&rtype, type1);
            }
            *ty = rtype;
        }
    } else if (token_is('[')) {
        struct type *faty = parse_func_or_array(0, NULL);
        prepend_type(ty, faty);
    } else if (token_is(ID)) {
        *id = token;
        gettok();
        if (token_is('[') || token_is('(')) {
            struct type *faty = parse_func_or_array(0, NULL);
            prepend_type(ty, faty);
        }
    }
}

/*
 enumerator-list:
   enumerator
   enumerator-list ',' enumerator

 enumerator:
   enumeration-constant
   enumeration-constant '=' constant-expression

 enumeration-constant:
   identifier
*/
static void parse_enum_body(struct symbol *sym)
{
    int val = 0;
    struct list *list = NULL;
    
    if (token_is_not(ID))
        error("expect identifier");

    while (token_is(ID)) {
        const char *id = TOK_ID_STR(token);
        struct source src = source;
        gettok();
        if (token_is('=')) {
            gettok();
            val = parse_intexpr();
        }
        struct symbol *p = actions.enum_id(id, val++, sym, src);
        list = list_append(list, p);
        if (token_is_not(','))
            break;
        gettok();
    }

    actions.enumdecl(sym, ltoa(&list, PERM));
}

/*
 struct-declaration-list:
   struct-declaration
   struct-declaration-list struct-declaration

 struct-declaration:
       specifier-qualifier-list struct-declarator-list ';'
 [C11] specifier-qualifier-list struct-declarator-list[opt] ';'

 specifier-qualifier-list:
   type-specifier specifier-qualifier-list[opt]
   type-qualifier specifier-qualifier-list[opt]

 struct-declarator-list:
   struct-declarator
   struct-declarator-list ',' struct-declarator

 struct-declarator:
   declarator
   declarator[opt] ':' constant-expression
*/
static void parse_struct_body(struct symbol *sym)
{    
    while (first_decl(token)) {
        struct type *basety = parse_specifiers(NULL, NULL);

        while (1) {
            struct field *field = alloc_field();
            if (token_is(':')) {
                field->src = source;
                gettok();
                field->bitsize = parse_intexpr();
                field->isbit = true;
                field->type = basety;
                // link
                actions.direct_field(sym, field);
            } else if (token_is(';') &&
                       isrecord(basety) &&
                       TYPE_TSYM(basety)->anonymous) {
                //C11: anonymous struct/union
                field->type = basety;
                field->src = source;
                actions.indirect_field(sym, field);
                goto next;
            } else {
                struct type *ty = NULL;
                struct token *id = NULL;
                parse_declarator(&ty, &id, NULL);
                attach_type(&ty, basety);
                if (token_is(':')) {
                    gettok();
                    field->bitsize = parse_intexpr();
                    field->isbit = true;
                }
                field->type = ty;
                if (id) {
                    field->name = TOK_ID_STR(id);
                    field->src = id->src;
                    // link
                    actions.direct_field(sym, field);
                }
            }

            if (token_is_not(','))
                break;
            gettok();
        }
    next:
        match(';', skip_to_decl);
    }

    actions.recorddecl(sym);
}

/*
 enum-specifier:
   'enum' identifier[opt] '{' enumerator-list '}'
   'enum' identifier[opt] '{' enumerator-list ',' '}'
   'enum' identifier

 struct-or-union-specifier:
       struct-or-union identifier[opt] '{' struct-declaration-list '}'
       struct-or-union identifier
 [GNU] struct-or-union identifier[opt] '{' '}'

 struct-or-union:
   'struct'
   'union'
*/
static struct type *parse_tag_decl(void)
{
    int t = token->id;
    const char *id = NULL;
    struct symbol *sym;
    struct source src = source;

    gettok();
    if (token_is(ID)) {
        id = TOK_ID_STR(token);
        gettok();
    }
    if (token_is('{')) {
        gettok();
        sym = tag_symbol(t, id, src);
        if (t == ENUM)
            parse_enum_body(sym);
        else
            parse_struct_body(sym);
        match('}', skip_to_brace);
    } else if (id) {
        sym = lookup(id, tags);
        if (sym) {
            if (is_current_scope(sym) && TYPE_OP(sym->type) != t)
                error_at(src, "use of '%s' with tag type that does "
                         "not match previous declaration '%T' at %S",
                         id2s(t), sym->type, sym->src);
        } else {
            sym = tag_symbol(t, id, src);
        }
    } else {
        error("expected identifier or '{'");
        sym = tag_symbol(t, NULL, src);
    }

    return sym->type;
}

/*
 external-declaration:
   declaration
   function-definition

 declaration:
   declaration-specifier init-declarator-list[opt] ';'

 function-definition:
   declaration-specifier declarator declaration-list[opt] compound-statement

 init-declarator-list:
   init-declarator
   init-declarator-list ',' init-declarator

 init-declarator:
   declarator
   declarator '=' initializer
*/
static void parse_decls(decl_fp dcl)
{
    struct type *basety;
    int sclass, fspec;
    int level = cscope;

    basety = parse_specifiers(&sclass, &fspec);
    if (token_is(ID) || token_is('*') || token_is('(')) {
        struct token *id = NULL;
        struct type *ty = NULL;
        struct symbol **params = NULL;        // for functioness
        struct source src = source;

        // declarator
        if (level == GLOBAL)
            parse_declarator(&ty, &id, &params);
        else
            parse_declarator(&ty, &id, NULL);
        attach_type(&ty, basety);

        if (level == GLOBAL && params) {
            if (isfunc(ty) &&
                (token_is('{') ||
                 (first_decl(token) && TYPE_OLDSTYLE(ty)))) {
                if (TYPE_OLDSTYLE(ty)) {
                    exit_scope();
                    // start with a new table
                    enter_scope();
                    assert(cscope == PARAM);
                    /// declaration-list:
                    ///   declaration
                    ///   declaration-list declaration
                    ///
                    while (first_decl(token))
                        parse_decls(actions.paramdecl);
                }

                funcdef(id ? TOK_ID_STR(id) : NULL,
                        ty, sclass, fspec, params,
                        id ? id->src : src);
                return;
            } else {
                exit_params(params);
            }
        }

        while (1) {
            if (id) {
                const char *name = TOK_ID_STR(id);
                struct tree *init = NULL;

                if (token_is('=')) {
                    if (level == PARAM) {
                        error("C does not support default arguments");
                        gettok();
                        parse_initializer(NULL);
                    } else if (sclass == TYPEDEF) {
                        error("illegal initializer "
                              "(only variable can be initialized)");
                        gettok();
                        parse_initializer(NULL);
                    } else {
                        gettok();
                        init = parse_initializer(ty);
                    }
                }

                if (sclass == TYPEDEF)
                    actions.typedefdecl(name, ty, fspec, level, id->src);
                else
                    dcl(name, ty, sclass, fspec, init, id->src);
            }

            if (token_is_not(','))
                break;

            gettok();
            id = NULL;
            ty = NULL;
            // declarator
            parse_declarator(&ty, &id, NULL);
            attach_type(&ty, basety);
        }
    } else if (isenum(basety) || isstruct(basety) || isunion(basety)) {
        // struct/union/enum
        actions.tagdecl(basety, sclass, fspec, source);
    } else {
        error("invalid token '%t' in declaration", token);
    }
    match(';', skip_to_decl);
}

/*
 type-name:
   specifier-qualifier-list abstract-declarator[opt]
*/
static struct type *parse_typename(void)
{
    struct type *basety;
    struct type *ty = NULL;

    basety = parse_specifiers(NULL, NULL);
    if (token_is('*') || token_is('(') || token_is('['))
        parse_abstract_declarator(&ty);

    attach_type(&ty, basety);

    return ty;
}

/*
 translation-unit:
   external-declaration
   translation-unit external-declaration
*/
void translation_unit(void)
{
    for (gettok(); token_is_not(EOI);) {
        if (first_decl(token)) {
            assert(cscope == GLOBAL);
            parse_decls(actions.globaldecl);
            deallocate(FUNC);
        } else {
            if (token_is(';')) {
                // empty declaration
                gettok();
            } else {
                error("expect declaration");
                skip_to_decl();
            }
        }
    }

    actions.finalize();
}
