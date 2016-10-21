#include <assert.h>
#include "cc.h"

static struct expr *expression0(void); // topmost expression that can be simplified
static struct expr *expression(void);
static struct expr *assign_expr(void);
static long intexpr1(struct type *);
static long intexpr(void);
static struct expr *bool_expr(void); // for expression in conditional statement
static struct expr *switch_expr(void); // for expression in switch statement
static struct expr *initializer_list(struct type *);
static void declaration(void);
static struct type *typename(void);

/*=================================================================*
 *                        expression                               *
 *=================================================================*/

static struct expr *cast_expr(void);
static struct expr *cond_expr1(struct expr *);
static struct expr *cond_expr(void);
static struct expr *unary_expr(void);

static struct expr *compound_literal(struct type *ty)
{
    struct source src = source;
    struct expr *inits = initializer_list(ty);
    
    return actions.compound_literal(ty, inits, src);
}

static struct type *cast_type(void)
{
    struct type *ty;

    expect('(');
    ty = typename();
    match(')', skip_to_bracket);

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
    struct expr *ret = NULL;
 
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
        if (first_typename(lookahead())) {
            struct type *ty = cast_type();
            ret = compound_literal(ty);
        } else {
            struct source src = source;
            struct expr *e;
            
            gettok();
            e = expression();
            match(')', skip_to_bracket);

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
    struct list *l = NULL;

    if (first_expr(token)) {
        for (;;) {
            struct expr *assign = assign_expr();
            if (assign)
                l = list_append(l, assign);

            if (token->id != ',')
                break;
            gettok();
        }
    } else if (token->id != ')') {
        error("expect assignment expression");
    }

    return ltoa(&l, FUNC);
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

                gettok();
                index = expression();
                match(']', skip_to_squarebracket);

                ret = actions.subscript(ret, index, src);
            }
            break;
        case '(':
            {
                struct source src = source;
                struct expr **args;

                gettok();
                args = argument_expr_list();
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

                gettok();
                ret = actions.post_increment(ret, t, src);
            }
            break;
        default:
            CC_UNAVAILABLE
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
        gettok();
        return actions.pre_increment(t, unary_expr(), src);
    case '+':
    case '-':
        gettok();
        return actions.minus_plus(t, cast_expr(), src);
    case '~':
        gettok();
        return actions.bitwise_not(cast_expr(), src);
    case '!':
        gettok();
        return actions.logical_not(cast_expr(), src);
    case '&':
        gettok();
        return actions.address(cast_expr(), src);
    case '*':
        gettok();
        return actions.indirection(cast_expr(), src);
    case SIZEOF:
        {
            struct token *ahead;
            struct expr *n = NULL;
            struct type *ty = NULL;

            gettok();
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
        gettok();
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
        gettok();
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
        gettok();
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
        gettok();
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
        gettok();
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
        gettok();
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
        gettok();
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
        gettok();
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
        gettok();
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
        gettok();
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
static struct expr *assign_expr(void)
{
    struct expr *or1 = logic_or();
    if (token->id == '?')
        return cond_expr1(or1);
    if (is_assign_tok(token)) {
        struct source src = source;
        int t = token->id;
        gettok();
        return actions.assignop(t, or1, assign_expr(), src);
    }
    return or1;
}

/// expression:
///   assignment-expression
///   expression ',' assignment-expression
///
static struct expr *expression(void)
{
    struct expr *assign1;

    assign1 = assign_expr();
    while (token->id == ',') {
        struct source src = source;
        gettok();
        assign1 = actions.commaop(assign1, assign_expr(), src);
    }
    return assign1;
}

static struct expr *expression0(void)
{
    return reduce(expression());
}

static long intexpr1(struct type *ty)
{
    struct source src = source;
    return actions.intexpr(cond_expr(), ty, src);
}

static long intexpr(void)
{
    return intexpr1(NULL);
}

// for expression in conditional statement
static struct expr *bool_expr(void)
{
    struct source src = source;
    return actions.bool_expr(expression(), src);
}

// for expression in switch statement
static struct expr *switch_expr(void)
{
    struct source src = source;
    return actions.switch_expr(expression(), src);
}

/*=================================================================*
 *                        statement                                *
 *=================================================================*/
static void statement(int cnt, int brk, struct swtch *swtch);

/// expression-statement:
///   expression[opt] ';'
///
static void expr_stmt(void)
{    
    if (token->id == ';') {
        // do nothing
    } else if (first_expr(token)) {
        struct expr *e = expression0();
        if (e) actions.gen(e);
    } else {
        error("missing statement before '%s'", tok2s(token));
    }
    
    expect(';');
}

/**
 * The entire **if** statement forms its own block scope, as do the
 * substatements even if they are not compound statements. This serves
 * to restrict the scope of objects and types that might be created
 * as a side effect of using compound literal or type names.
 */
/// selection-statement:
///   'if' '(' expression ')' statement
///   'if' '(' expression ')' statement 'else' statement
///
static void if_stmt(int lab, int cnt, int brk, struct swtch *swtch)
{
    struct expr *cond;

    enter_scope();

    expect(IF);
    expect('(');
    cond = bool_expr();
    match(')', skip_to_bracket);

    actions.branch(cond, 0, lab);
    
    enter_scope();
    statement(cnt, brk, swtch);
    exit_scope();

    if (token->id == ELSE) {
        expect(ELSE);
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

/**
 * Each iterative statement(do/while/for) forms its own block scope,
 * as do the substatements even if they are not compound statements.
 */
/// iteration-statement:
///   'while' '(' expression ')' statement
///
static void while_stmt(int lab, struct swtch *swtch)
{
    struct expr *cond;

    enter_scope();

    expect(WHILE);
    expect('(');
    cond = bool_expr();
    match(')', skip_to_bracket);

    actions.jump(lab+1);
    actions.label(lab);
    statement(lab+1, lab+2, swtch);
    actions.label(lab+1);
    actions.branch(cond, lab, 0);
    actions.label(lab+2);

    exit_scope();
}

/// iteration-statement:
///   'do' statement 'while' '(' expression ')' ';'
///
static void do_while_stmt(int lab, struct swtch *swtch)
{
    struct expr *cond;

    enter_scope();

    expect(DO);
    actions.label(lab);
    statement(lab+1, lab+2, swtch);
    expect(WHILE);
    expect('(');
    cond = bool_expr();
    match(')', skip_to_bracket);
    expect(';');
    actions.label(lab+1);
    actions.branch(cond, lab, 0);
    actions.label(lab+2);

    exit_scope();
}

/// iteration-statement:
///   'for' '(' expression[opt] ';' expression[opt] ';' expression[opt] ')' statement
///   'for' '(' declaration expression[opt] ';' expression[opt] ')' statement
///
static void for_stmt(int lab, struct swtch *swtch)
{
    struct expr *init = NULL;
    struct expr *cond = NULL;
    struct expr *ctrl = NULL;
    
    enter_scope();

    expect(FOR);
    expect('(');

    if (token->id == ';') {
        expect(';');
    } else {
        if (first_decl(token)) {
            // declaration
            declaration();
        } else {
            // expression
            init = expression0();
            expect(';');
        }
    }

    if (token->id != ';')
        cond = bool_expr();

    expect(';');

    if (token->id != ')')
        ctrl = expression0();

    match(')', skip_to_bracket);

    actions.gen(init);
    actions.jump(lab+3);
    actions.label(lab);
    statement(lab+1, lab+2, swtch);
    actions.label(lab+1);
    actions.gen(ctrl);
    actions.label(lab+3);
    actions.branch(cond, lab, 0);
    actions.label(lab+2);
    
    exit_scope();
}

/**
 * Switch Statements Notes:
 *
 * 1. The control expression is subject to the usual unary convresion.
 *
 * 2. When comparing the control expression and the **case** expressions,
 *    the **case** expressions are converted to the type of the control
 *    expression (after the usual unary conversion).
 */
/// selection-statement:
///   'switch' '(' expression ')' statement
///
static void switch_stmt(int lab, int cnt)
{
    struct expr *expr, *n, *tmp;
    struct symbol *sym;
    struct source src = source;
    struct swtch *swtch = NEWS0(struct swtch, FUNC);

    expect(SWITCH);
    expect('(');
    expr = switch_expr();
    match(')', skip_to_bracket);

    swtch->src = src;
    swtch->type = expr ? expr->type : inttype;
        
    // make a tmp var
    sym = mktmp(gen_tmpname(), swtch->type, REGISTER);
    n = assign(sym, expr);
    tmp = n->kids[0];

    actions.gen(n);
    actions.jump(lab);
    statement(cnt, lab+1, swtch);
    actions.jump(lab+1);
    actions.label(lab);
    
    // gen switch code
    for (struct cse *cs = swtch->cases; cs; cs = cs->link) {
        struct expr *e = actions.bop(EQL, tmp, cnsti(cs->value, longtype), src);
        actions.branch(e, cs->label, 0);
    }

    if (swtch->defalt)
        actions.jump(swtch->defalt->label);
    
    actions.label(lab+1);
}

/// labeled-statement:
///   'case' constant-expression ':' statement
///
static void case_stmt(int lab, int cnt, int brk, struct swtch *swtch)
{
    struct source src = source;
    long value;

    expect(CASE);
    value = intexpr1(swtch ? swtch->type : inttype);
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

/// labeled-statement:
///   'default' ':' statement
///
static void default_stmt(int lab, int cnt, int brk, struct swtch *swtch)
{
    struct source src = source;

    expect(DEFAULT);
    expect(':');

    // print before parsing statement
    if (swtch) {
        if (swtch->defalt)
            error_at(src,
                     "multiple default labels in one switch, previous case defined here:%s:%u:%u",
                     swtch->defalt->src.file,
                     swtch->defalt->src.line,
                     swtch->defalt->src.column);

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

/// labled-statement:
///   identifier ':' statement
///
static void label_stmt(int lab, int cnt, int brk, struct swtch *swtch)
{
    struct source src = source;
    const char *id = NULL;

    if (token->id == ID)
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
                     "redefinition of label '%s', previous label defined here:%s:%u:%u",
                     sym->src.file, sym->src.line, sym->src.column);
        }

        actions.label(sym->x.label);
    }

    statement(cnt, brk, swtch);
}

/// jump-statement:
///   'goto' identifier ';'
///
static void goto_stmt(int lab)
{
    struct source src = source;
    const char *id = NULL;
    
    expect(GOTO);
    if (token->id == ID)
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

/// jump-statement:
///   'break' ';'
///
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

/// jump-statement:
///   'continue' ';'
///
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

/// jump-statement:
///   'return' expression[opt] ';'
///
static void return_stmt(void)
{
    struct source src = source;
    struct expr *e = NULL;
    bool isnull = false;

    expect(RETURN);
    if (token->id == ';')
        isnull = true;
    else
        e = expression();

    expect(';');
    actions.ret(e, isnull, src);
}

/// statement:
///   labeled-statement
///   compound-statement
///   expression-statement
///   selection-statement
///   iteration-statement
///   jump-statement
///
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
        if (lookahead()->id == ':') {
            label_stmt(genlabel(1), cnt, brk, swtch);
            break;
        }
        // go through

    default:
        expr_stmt();
        break;
    }
}

/// compound-statement:
///   '{' block-item-list[opt] '}'
///
/// block-item-list:
///   block-item
///   block-item-list block-item
///
/// block-item:
///   declaration
///   statement
///
void compound_stmt(void (*cb) (void), int cnt, int brk, struct swtch *swtch)
{
    expect('{');
    enter_scope();

    if (cb) cb();

    while (first_decl(token) || first_expr(token) || first_stmt(token)) {
        if (first_decl(token))
            // declaration
            declaration();
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

static void parse_initializer_list(struct desig *, struct init **);

static struct desig *parse_designator(struct desig *desig)
{
    struct list *list = NULL;
    
    assert(token->id == '.' || token->id == '[');
    
    do {
        if (token->id == '.') {
            expect('.');
            struct source src = source;
            // only create list item when desig != NULL
            if (desig) {
                if (token->id == ID)
                    list = list_append(list, new_desig_name(TOK_ID_STR(token), src));
                else
                    // set desig to NULL
                    desig = NULL;
            }
            expect(ID);
        } else {
            expect('[');
            struct source src = source;
            long index = intexpr();
            match(']', skip_to_squarebracket);
            // only create list item when desig != NULL
            if (desig)
                list = list_append(list, new_desig_index(index, src));
        }
    } while (token->id == '.' || token->id == '[');

    expect('=');

    return desig ? actions.designator(desig, ltoa(&list, FUNC)) : NULL;
}

static void parse_initializer(struct desig **pdesig, struct init **pinit)
{
    if (token->id == '{') {
        // begin a new root designator
        struct desig *desig = *pdesig;
        struct desig *d;

        if (desig->id == DESIG_NONE) {
            d = desig;
            d->braces++;
        } else {
            d = new_desig(DESIG_NONE);
            d->type = desig->type;
            d->offset = desig->offset;
            d->src = desig->src;
            d->all = desig;         // all link
        }
        
        parse_initializer_list(d, pinit);
    } else {
        actions.element_init(pdesig, assign_expr(), pinit);
    }
}

static void parse_initializer_list(struct desig *desig, struct init **pinit)
{
    struct desig *d = desig;
    
    expect('{');

    if (token->id == '}') {
        actions.element_init(&desig, zinit(desig->type), pinit);
    } else {
        while (1) {
            if (token->id == '.' || token->id == '[')
                d = parse_designator(desig);
            else
                d = next_designator(d);

            parse_initializer(&d, pinit);

            if (token->id != ',')
                break;

            expect(',');

            if (token->id == '}')
                break;
        }
    }

    match('}', skip_to_brace);
}

/// initializer:
///       assignment-expression
///       '{' initializer-list '}'
///       '{' initializer-list ',' '}'
/// [GNU] '{' '}'
///
struct expr *initializer(struct type *ty)
{
    if (token->id == '{')
        return initializer_list(ty);
    else
        return assign_expr();
}

/// initializer-list:
///   designation[opt] initializer
///   initializer-list ',' designation[opt] initializer
///
/// designation:
///   designator-list '='
///
/// designator-list:
///   designator
///   designator-list designator
///
/// designator:
///   '[' constant-expression ']'
///   '.' identifier
///
static struct expr *initializer_list(struct type *ty)
{
    if (ty) {
        struct desig desig = {.id = DESIG_NONE, .type = ty, .offset = 0, .src = source};
        struct init *init = NULL;

        parse_initializer_list(&desig, &init);        

        return actions.initializer_list(ty, init);
    } else {
        parse_initializer_list(NULL, NULL);
        return NULL;
    }
}

/*=================================================================*
 *                        declaration                              *
 *=================================================================*/

// 0 means both
enum { DIRECT = 1, ABSTRACT };

static void param_declarator(struct type **, struct token **);
static struct type *tag_decl(void);

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
                 "a parameter list without types is only allowed in a function definition");

    if (cscope > PARAM)
        exit_scope();

    exit_scope();
}

/// declaration-specifier:
///   storage-class-specifier declaration-specifiers[opt]
///   type-specifier          declaration-specifiers[opt]
///   type-qualifier          declaration-specifiers[opt]
///   function-specifier      declaration-specifiers[opt]
///
/// storage-class-specifier:
///   'auto'
///   'extern'
///   'register'
///   'static'
///   'typedef'
///
/// type-qualifier:
///   'const'
///   'volatile'
///   'restrict'
///
/// function-specifier:
///   'inline'
///
/// type-specifier:
///   'void'
///   'char'
///   'short'
///   'int'
///   'long'
///   'float'
///   'double'
///   'signed'
///   'unsigned'
///   '_Bool'
///   '_Complex'
///   '_Imaginary'
///   enum-specifier
///   struct-or-union-specifier
///   typedef-name
///
/// typedef-name:
///   identifier
///
static struct type *specifiers(int *sclass, int *fspec)
{
    int cls, sign, size, type;
    int cons, vol, res, inl;
    struct type *basety, *tydefty;
    int ci;                        // _Complex, _Imaginary

    basety = tydefty = NULL;
    cls = sign = size = type = 0;
    cons = vol = res = inl = 0;
    ci = 0;
    if (sclass == NULL)
        cls = AUTO;

    for (;;) {
        int *p, t = token->id;
        struct token *tok = token;
        switch (token->id) {
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
            basety = tag_decl();
            break;

        case LONG:
            if (size == LONG) {
                t = LONG + LONG;
                size = 0;        // clear
            }
            // go through
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
                struct symbol *sym = lookup_typedef(TOK_ID_STR(token));
                if (sym) {
                    use(sym);
                    tydefty = sym->type;
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
                             "duplicate storage class '%s'",
                             tok2s(tok));
                else
                    error_at(tok->src,
                             "type name does not allow storage class to be specified",
                             tok2s(tok));
            } else if (p == &inl) {
                if (fspec)
                    warning_at(tok->src,
                               "duplicate '%s' declaration specifier",
                               tok2s(tok));
                else
                    error_at(tok->src, "function specifier not allowed");
            } else if (p == &cons || p == &res || p == &vol) {
                warning_at(tok->src,
                           "duplicate '%s' declaration specifier",
                           tok2s(tok));
            } else if (p == &ci) {
                error_at(tok->src,
                         "duplicate _Complex/_Imaginary specifier '%s'",
                         tok2s(tok));
            } else if (p == &sign) {
                error_at(tok->src,
                         "duplicate signed/unsigned speficier '%s'",
                         tok2s(tok));
            } else if (p == &type || p == &size) {
                error_at(tok->src,
                         "duplicate type specifier '%s'",
                         tok2s(tok));
            } else {
                CC_UNAVAILABLE
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
                  id2s(size / 2), id2s(size / 2), id2s(type));
        else
            error("%s %s is invalid", id2s(size), id2s(type));
    } else if (sign && type != INT && type != CHAR) {
        error("'%s' cannot be signed or unsigned", id2s(type));
    } else if (ci && type != DOUBLE && type != FLOAT) {
        error("'%s' cannot be %s", id2s(type), id2s(ci));
    }

    if (type == ID)
        basety = tydefty;
    else if (type == CHAR && sign)
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

/// parameter-type-list:
///   parameter-list
///   parameter-list ',' '...'
///
/// parameter-list:
///   parameter-declaration
///   parameter-list parameter-declaration
///
/// parameter-declaration:
///   declaration-specifier declarator
///   declaration-specifier abstract-declarator[opt]
///
static struct symbol **prototype(struct type *ftype)
{
    struct list *list = NULL;

    while (1) {
        struct type *basety = NULL;
        int sclass, fspec;
        struct type *ty = NULL;
        struct token *id = NULL;
        struct symbol *sym;
        struct source src = source;

        basety = specifiers(&sclass, &fspec);
        param_declarator(&ty, &id);
        attach_type(&ty, basety);

        sym = actions.paramdecl(id ? TOK_ID_STR(id) : NULL,
                                ty, sclass, fspec, id ? id->src : src);
        list = list_append(list, sym);

        if (token->id != ',')
            break;

        gettok();
        if (token->id == ELLIPSIS) {
            TYPE_VARG(ftype) = 1;
            gettok();
            break;
        }
    }

    // check
    return actions.prototype(ftype, ltoa(&list, FUNC));
}

/// identifier-list:
///   identifier
///   identifier-list ',' identifier
///
static struct symbol **oldstyle(struct type *ftype)
{
    struct list *params = NULL;

    while (1) {
        if (token->id == ID) {
            struct symbol *sym = actions.paramdecl(TOK_ID_STR(token), inttype, 0, 0, token->src);
            sym->defined = false;
            params = list_append(params, sym);
        }
        expect(ID);
        if (token->id != ',')
            break;
        gettok();
    }

    return ltoa(&params, FUNC);
}

static struct symbol **parameters(struct type *ftype)
{
    struct symbol **params;

    if (first_decl(token)) {
        // prototype
        int i;
        struct type **proto;

        params = prototype(ftype);
        proto = newarray(sizeof(struct type *), length(params) + 1, PERM);
        for (i = 0; params[i]; i++)
            proto[i] = params[i]->type;

        proto[i] = NULL;
        TYPE_PROTO(ftype) = proto;
        TYPE_OLDSTYLE(ftype) = 0;
    } else if (token->id == ID) {
        // oldstyle
        params = oldstyle(ftype);
        TYPE_OLDSTYLE(ftype) = 1;
    } else if (token->id == ')') {
        params = vtoa(NULL, FUNC);
        TYPE_OLDSTYLE(ftype) = 1;
    } else {
        params = vtoa(NULL, FUNC);
        TYPE_OLDSTYLE(ftype) = 1;

        if (token->id == ELLIPSIS)
            error("ISO C requires a named parameter before '...'");
        else
            error("expect parameter declarator at '%s'", tok2s(token));
        gettok();
    }

    return params;
}

static void array_qualifiers(struct type *atype)
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
            CC_UNAVAILABLE
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

static struct type *arrays(int abstract)
{
    struct type *atype = array_type(NULL);

    //NOTE: '*' is in `first_expr`
    switch (abstract) {
    case ABSTRACT:
        if (token->id == '*' && lookahead()->id == ']') {
            gettok();
            TYPE_A_STAR(atype) = 1;
        } else if (first_expr(token)) {
            struct source src = source;
            actions.array_index(atype, assign_expr(), src);
        }
        break;

    case DIRECT:
    case 0:
        if (token->id == STATIC) {
            gettok();
            TYPE_A_STATIC(atype) = 1;
            array_qualifiers(atype);
            struct source src = source;
            actions.array_index(atype, assign_expr(), src);
        } else {
            array_qualifiers(atype);
            if (token->id == STATIC) {
                gettok();
                TYPE_A_STATIC(atype) = 1;
                struct source src = source;
                actions.array_index(atype, assign_expr(), src);
            } else if (token->id == '*' && lookahead()->id == ']') {
                gettok();
                TYPE_A_STAR(atype) = 1;
            } else if (first_expr(token)) {
                struct source src = source;
                actions.array_index(atype, assign_expr(), src);
            }
        }
        break;

    default:
        CC_UNAVAILABLE
    }

    return atype;
}

static struct type *func_or_array(int abstract, struct symbol ***params)
{
    struct type *ty = NULL;

    for (; token->id == '(' || token->id == '[';) {
        if (token->id == '[') {
            struct type *atype;
            gettok();
            atype = arrays(abstract);
            match(']', skip_to_squarebracket);
            attach_type(&ty, atype);
        } else {
            struct symbol **args;
            struct type *ftype = func_type(NULL);
            gettok();
            /**
             * To make it easy to distinguish between 'paramaters in parameter'
             * and 'compound statement of function definition', they both may be
             * at scope LOCAL (aka PARAM+1), so enter scope again to make things
             * easy.
             */
            enter_scope();
            if (cscope > PARAM)
                enter_scope();
            args = parameters(ftype);
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

/// pointer:
///   '*' type-qualifier-list[opt]
///   '*' type-qualifier-list[opt] pointer
///
/// type-qualifier-list:
///   type-qualifier
///   type-qualifier-list type-qualifier
///
static struct type *ptr_decl(void)
{
    struct type *ret = NULL;
    int con, vol, res, type;

    assert(token->id == '*');

    for (;;) {
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
            warning("duplicate type qulifier '%s'", tok2s(token));

        *p = t;

        if (t == CONST || t == VOLATILE || t == RESTRICT)
            ret = qual(t, ret);

        gettok();
    }

    return ret;
}

/// abstract-declarator:
///   pointer
///   pointer[opt] direct-abstract-declarator
///
/// direct-abstract-declarator:
///   '(' abstract-declarator ')'
///   direct-abstract-declarator[opt] '[' assignment-expression[opt] ']'
///   direct-abstract-declarator[opt] '[' '*' ']'
///   direct-abstract-declarator[opt] '(' parameter-type-list[opt] ')'
///
static void abstract_declarator(struct type **ty)
{
    assert(ty);

    if (token->id == '*' || token->id == '(' || token->id == '[') {
        if (token->id == '*') {
            struct type *pty = ptr_decl();
            prepend_type(ty, pty);
        }

        if (token->id == '(') {
            if (first_decl(lookahead())) {
                struct type *faty = func_or_array(ABSTRACT, NULL);
                prepend_type(ty, faty);
            } else {
                struct type *type1 = *ty;
                struct type *rtype = NULL;
                expect('(');
                abstract_declarator(&rtype);
                match(')', skip_to_bracket);
                if (token->id == '[' || token->id == '(') {
                    struct type *faty = func_or_array(ABSTRACT, NULL);
                    attach_type(&faty, type1);
                    attach_type(&rtype, faty);
                } else {
                    attach_type(&rtype, type1);
                }
                *ty = rtype;
            }
        } else if (token->id == '[') {
            struct type *faty = func_or_array(ABSTRACT, NULL);
            prepend_type(ty, faty);
        }
    } else {
        error("expect '(', '[' or '*'");
    }
}

/// declarator:
///   pointer[opt] direct-declarator
///
/// direct-declarator:
///   identifier
///   '(' declarator ')'
///   direct-declarator '[' type-qualifier-list[opt] assignment-expression[opt] ']'
///   direct-declarator '[' 'static' type-qualifier-list[opt] assignment-expression ']'
///   direct-declarator '[' type-qualifier-list 'static' assignment-expression ']'
///   direct-declarator '[' type-qualifier-list[opt] '*' ']'
///   direct-declarator '(' parameter-type-list ')'
///   direct-declarator '(' identifier-list[opt] ')'
///
static void declarator(struct type **ty, struct token **id, struct symbol ***params)
{
    assert(ty && id);

    if (token->id == '*') {
        struct type *pty = ptr_decl();
        prepend_type(ty, pty);
    }

    if (token->id == ID) {
        *id = token;
        gettok();
        if (token->id == '[' || token->id == '(') {
            struct type *faty = func_or_array(DIRECT, params);
            prepend_type(ty, faty);
        }
    } else if (token->id == '(') {
        struct type *type1 = *ty;
        struct type *rtype = NULL;
        gettok();
        declarator(&rtype, id, params);
        match(')', skip_to_bracket);
        if (token->id == '[' || token->id == '(') {
            struct type *faty = func_or_array(DIRECT, params);
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
static void param_declarator(struct type **ty, struct token **id)
{
    assert(ty && id);
    
    if (token->id == '*') {
        struct type *pty = ptr_decl();
        prepend_type(ty, pty);
    }

    if (token->id == '(') {
        struct token *ahead = lookahead();
        if (ahead->id == ')' || first_decl(ahead)) {
            struct type *faty = func_or_array(0, NULL);
            prepend_type(ty, faty);
        } else {
            struct type *type1 = *ty;
            struct type *rtype = NULL;
            expect('(');
            param_declarator(&rtype, id);
            match(')', skip_to_bracket);
            if (token->id == '(' || token->id == '[') {
                struct type *faty = func_or_array(0, NULL);
                attach_type(&faty, type1);
                attach_type(&rtype, faty);
            } else {
                attach_type(&rtype, type1);
            }
            *ty = rtype;
        }
    } else if (token->id == '[') {
        struct type *faty = func_or_array(0, NULL);
        prepend_type(ty, faty);
    } else if (token->id == ID) {
        *id = token;
        gettok();
        if (token->id == '[' || token->id == '(') {
            struct type *faty = func_or_array(0, NULL);
            prepend_type(ty, faty);
        }
    }
}

/// enumerator-list:
///   enumerator
///   enumerator-list ',' enumerator
///
/// enumerator:
///   enumeration-constant
///   enumeration-constant '=' constant-expression
///
/// enumeration-constant:
///   identifier
///
static void enum_body(struct symbol *sym)
{
    int val = 0;
    struct list *list = NULL;
    
    if (token->id != ID)
        error("expect identifier");

    while (token->id == ID) {
        const char *name = TOK_ID_STR(token);
        struct source src = source;
        gettok();
        if (token->id == '=') {
            gettok();
            val = intexpr();
        }
        struct symbol *p = actions.enum_id(name, val++, sym, src);
        list = list_append(list, p);
        if (token->id != ',')
            break;
        gettok();
    }

    actions.enumdecl(sym, ltoa(&list, PERM));
}

static void bitfield(struct field *field)
{
    field->src = source;
    expect(':');
    field->bitsize = intexpr();
    field->isbit = true;
}

/// struct-declaration-list:
///   struct-declaration
///   struct-declaration-list struct-declaration
///
/// struct-declaration:
///       specifier-qualifier-list struct-declarator-list ';'
/// [C11] specifier-qualifier-list struct-declarator-list[opt] ';'
///
/// specifier-qualifier-list:
///   type-specifier specifier-qualifier-list[opt]
///   type-qualifier specifier-qualifier-list[opt]
///
/// struct-declarator-list:
///   struct-declarator
///   struct-declarator-list ',' struct-declarator
///
/// struct-declarator:
///   declarator
///   declarator[opt] ':' constant-expression
///
static void struct_body(struct symbol *sym)
{    
    while (first_decl(token)) {
        struct type *basety = specifiers(NULL, NULL);

        while (1) {
            struct field *field = alloc_field();
            if (token->id == ':') {
                bitfield(field);
                field->type = basety;
            } else if (token->id == ';' &&
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
                declarator(&ty, &id, NULL);
                attach_type(&ty, basety);
                if (token->id == ':')
                    bitfield(field);
                field->type = ty;
                if (id) {
                    field->name = TOK_ID_STR(id);
                    field->src = id->src;
                }
            }

            // link
            actions.direct_field(sym, field);

            if (token->id != ',')
                break;
            gettok();
        }
    next:
        match(';', skip_to_decl);
    }

    actions.recorddecl(sym);
}

/// enum-specifier:
///   'enum' identifier[opt] '{' enumerator-list '}'
///   'enum' identifier[opt] '{' enumerator-list ',' '}'
///   'enum' identifier
///
/// struct-or-union-specifier:
///       struct-or-union identifier[opt] '{' struct-declaration-list '}'
///       struct-or-union identifier
/// [GNU] struct-or-union identifier[opt] '{' '}'
///
/// struct-or-union:
///   'struct'
///   'union'
///
static struct type *tag_decl(void)
{
    int t = token->id;
    const char *id = NULL;
    struct symbol *sym = NULL;
    struct source src = source;

    gettok();                   // consume `t`
    if (token->id == ID) {
        id = TOK_ID_STR(token);
        gettok();
    }
    if (token->id == '{') {
        gettok();
        sym = tag_symbol(t, id, src);
        if (t == ENUM)
            enum_body(sym);
        else
            struct_body(sym);
        match('}', skip_to_brace);
    } else if (id) {
        sym = lookup(id, tags);
        if (sym) {
            if (is_current_scope(sym) && TYPE_OP(sym->type) != t)
                error_at(src,
                         "use of '%s' with tag type that does not match previous declaration '%s' at %s:%u:%u",
                         id2s(t), type2s(sym->type), sym->src.file, sym->src.line, sym->src.column);
        } else {
            sym = tag_symbol(t, id, src);
        }
    } else {
        error("expected identifier or '{'");
        sym = tag_symbol(t, NULL, src);
    }

    return sym->type;
}

/// external-declaration:
///   declaration
///   function-definition
///
/// declaration:
///   declaration-specifier init-declarator-list[opt] ';'
///
/// function-definition:
///   declaration-specifier declarator declaration-list[opt] compound-statement
///
/// init-declarator-list:
///   init-declarator
///   init-declarator-list ',' init-declarator
///
/// init-declarator:
///   declarator
///   declarator '=' initializer
///
static void decls(struct symbol *(*dcl)(const char *, struct type *, int, int, struct source))
{
    struct type *basety;
    int sclass, fspec;
    int level = cscope;

    basety = specifiers(&sclass, &fspec);
    if (token->id == ID || token->id == '*' || token->id == '(') {
        struct token *id = NULL;
        struct type *ty = NULL;
        struct symbol **params = NULL;        // for functioness
        struct source src = source;

        // declarator
        if (level == GLOBAL)
            declarator(&ty, &id, &params);
        else
            declarator(&ty, &id, NULL);
        attach_type(&ty, basety);

        if (level == GLOBAL && params) {
            if (isfunc(ty) && (token->id == '{' ||
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
                        decls(actions.paramdecl);
                }

                actions.funcdef(id ? TOK_ID_STR(id) : NULL,
                                ty, sclass, fspec, params, id ? id->src : src);
                return;
            } else {
                exit_params(params);
            }
        }

        for (;;) {
            if (id) {
                if (sclass == TYPEDEF)
                    actions.typedefdecl(TOK_ID_STR(id), ty, fspec, level, id->src);
                else
                    dcl(TOK_ID_STR(id), ty, sclass, fspec, id->src);
            }

            if (token->id != ',')
                break;

            gettok();
            id = NULL;
            ty = NULL;
            // declarator
            declarator(&ty, &id, NULL);
            attach_type(&ty, basety);
        }
    } else if (isenum(basety) || isstruct(basety) || isunion(basety)) {
        // struct/union/enum
        if (isstruct(basety) || isunion(basety)) {
            // anonymous record (can't be referenced)
            if (TYPE_TSYM(basety)->anonymous)
                warning("declaration does not declare anything");
        }
    } else {
        error("invalid token '%s' in declaration", tok2s(token));
    }
    match(';', skip_to_decl);
}

static void declaration(void)
{
    assert(cscope >= LOCAL);
    decls(actions.localdecl);
}

/// type-name:
///   specifier-qualifier-list abstract-declarator[opt]
///
static struct type *typename(void)
{
    struct type *basety;
    struct type *ty = NULL;

    basety = specifiers(NULL, NULL);
    if (token->id == '*' || token->id == '(' || token->id == '[')
        abstract_declarator(&ty);

    attach_type(&ty, basety);

    return ty;
}

/// translation-unit:
///   external-declaration
///   translation-unit external-declaration
///
void translation_unit(void)
{
    for (gettok(); token->id != EOI;) {
        if (first_decl(token)) {
            assert(cscope == GLOBAL);
            decls(actions.globaldecl);
            deallocate(FUNC);
        } else {
            if (token->id == ';') {
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
