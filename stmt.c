#include "cc.h"

static void statement(int cnt, int brk, struct swtch *swtch);

/// expression-statement:
///   expression[opt] ';'
///
static void expr_stmt(void)
{    
    if (token->id == ';') {
        // do nothing
    } else if (first_expr(token)) {
        struct expr *e = expression();
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
    expect(')');

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
    expect(')');

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
    expect(')');
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
            init = expression();
            expect(';');
        }
    }

    if (token->id != ';')
        cond = bool_expr();

    expect(';');

    if (token->id != ')')
        ctrl = expression();

    expect(')');

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
    struct source src = source;
    struct swtch *swtch = NEWS0(struct swtch, FUNC);

    expect(SWITCH);
    expect('(');
    expr = switch_expr();
    expect(')');

    swtch->src = src;
    swtch->type = expr ? EXPR_TYPE(expr) : inttype;
        
    // make a tmp var
    n = assign(mktmpvar(swtch->type, REGISTER), expr);
    tmp = EXPR_OPERAND(n, 0);

    actions.gen(n);
    actions.jump(lab);
    statement(cnt, lab+1, swtch);
    actions.jump(lab+1);
    actions.label(lab);
    
    // gen switch code
    for (struct cse *cs = swtch->cases; cs; cs = cs->link) {
        struct expr *e = ast_bop(EQ, inttype, tmp, new_int_literal(cs->value));
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
        if (swtch->defalt) {
            struct source prev = swtch->defalt->src;
            error_at(src,
                     "multiple default labels in one switch, previous case defined here:%s:%u:%u",
                     prev.file, prev.line, prev.column);
        }

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
            struct source prev = sym->src;
            error_at(src,
                     "redefinition of label '%s', previous label defined here:%s:%u:%u",
                     prev.file, prev.line, prev.column);
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
    ensure_return(e, isnull, src);
    actions.ret(e);
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

    expect('}');
    exit_scope();
}
