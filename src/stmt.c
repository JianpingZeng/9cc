#include "cc.h"

static struct node * statement(struct node *context);
static struct node * _compound_stmt(struct node *context);

static struct node * expr_stmt()
{
    struct node *ret;
    
    if (token->id == ';') {
        ret = NULL;
    } else if (firstexpr(token)) {
        ret = ast_stmt(EXPR_STMT, expression(), NULL);
    } else {
        ret = NULL;
        error("missing statement before '%s'", token->name);
    }
    
    expect(';');
    
    return ret;
}

static struct node * if_stmt(struct node *context)
{
    struct node *ret;
    struct node *expr;
    struct node *stmt1;
    
    expect(IF);
    expect('(');
    expr = expression();
    expect(')');
    
    stmt1 = statement(context);
    ret = ast_stmt(IF_STMT, expr, stmt1);
    
    if (token->id == ELSE) {
        expect(ELSE);
        ret = ast_stmt(ELSE_STMT, ret, statement(context));
    }
    
    return ret;
}

static struct node * while_stmt(struct node *context)
{
    struct node *expr;
    struct node *ret;
    
    expect(WHILE);
    expect('(');
    expr = expression();
    expect(')');
    
    ret = ast_stmt(WHILE_STMT, expr, NULL);
    ret->u.s.up = context;
    RIGHT(ret) = statement(ret);
    ret->u.s.up = NULL;
    
    return ret;
}

static struct node * do_while_stmt(struct node *context)
{
    struct node *stmt;
    struct node *expr;
    struct node *ret;
    
    expect(DO);
    
    ret = ast_stmt(DO_WHILE_STMT, NULL, NULL);
    ret->u.s.up = context;
    stmt = statement(ret);
    expect(WHILE);
    expect('(');
    expr = expression();
    expect(')');
    expect(';');
    LEFT(ret) = stmt;
    RIGHT(ret) = expr;
    ret->u.s.up = NULL;
    
    return ret;
}

static struct node * for_stmt(struct node *context)
{
    struct node *ret = ast_stmt(FOR_STMT, NULL, NULL);
    
    expect(FOR);
    expect('(');
    
    enter_scope();
    
    if (token->id == ';') {
        expect(';');
    } else {
        if (firstdecl(token)) {
            // declaration
            ret->u.s.forstmt.decl = declaration();
        } else {
            // expression
            ret->u.s.forstmt.init = expression();
            expect(';');
        }
    }
    
    if (token->id != ';')
        ret->u.s.forstmt.cond = expression();
    
    expect(';');
    
    if (token->id != ')')
        ret->u.s.forstmt.ctrl = expression();
    
    expect(')');
    
    ret->u.s.up = context;
    LEFT(ret) = statement(ret);
    ret->u.s.up = NULL;
    
    exit_scope();
    
    return ret;
}

static struct node * switch_stmt(struct node *context)
{
    struct node *expr;
    struct node *ret;
    
    expect(SWITCH);
    expect('(');
    expr = expression();
    expect(')');
    
    ret = ast_stmt(SWITCH_STMT, expr, NULL);
    ret->u.s.up = context;
    RIGHT(ret) = statement(ret);
    ret->u.s.up = NULL;
    
    return ret;
}

static struct node * case_stmt(struct node *context)
{
    int in_sw = 0;
    int val;
    struct node *stmt;
    struct source src = source;
    
    expect(CASE);
    val = intexpr();
    expect(':');
    
    while (context) {
        if (is_switch_stmt(context)) {
            in_sw = 1;
            break;
        } else {
            context = context->u.s.up;
        }
    }
    
    // print before parsing statement
    if (!in_sw)
        errorf(src, "'case' statement is not in a switch statement.");
    
    // always parse even if not in a switch statement
    stmt = statement(context);
    
    if (!in_sw)
        return NULL;
    
    struct node *ret = ast_stmt(CASE_STMT, stmt, NULL);
    ret->u.s.casestmt.value = val;
    return ret;
}

static struct node * default_stmt(struct node *context)
{
    int in_sw = 0;
    struct node *stmt;
    struct source src = source;
    
    expect(DEFAULT);
    expect(':');
    
    while (context) {
        if (is_switch_stmt(context)) {
            in_sw = 1;
            break;
        } else {
            context = context->u.s.up;
        }
    }
    
    // print before parsing statement
    if (!in_sw)
        errorf(src, "'default' statement is not in a switch statement.");
    
    stmt = statement(context);
    
    if (!in_sw)
        return NULL;
    else
        return ast_stmt(DEFAULT_STMT, stmt, NULL);
}

static struct node * label_stmt(struct node *context)
{
    struct node *label;
    struct node *stmt;
    
    label = ast_expr(REF_EXPR, ID, NULL, NULL);
    expect(ID);
    expect(':');
    stmt = statement(context);
    
    return ast_stmt(LABEL_STMT, label, stmt);
}

static struct node * goto_stmt()
{
    struct node *expr = NULL;
    
    expect(GOTO);
    if (token->id == ID)
        expr = ast_expr(REF_EXPR, ID, NULL, NULL);
    expect(ID);
    expect(';');
    
    return ast_stmt(GOTO_STMT, expr, NULL);
}

static struct node * break_stmt(struct node *context)
{
    int in_iter_sw = 0;
    struct source src = source;
    struct node *ret;
    
    expect(BREAK);
    expect(';');
    
    while (context) {
        if (is_iteration_stmt(context) || is_switch_stmt(context)) {
            in_iter_sw = 1;
            break;
        } else {
            context = context->u.s.up;
        }
    }
    
    if (!in_iter_sw) {
        errorf(src, "'break' statement is not in a loop or switch statement.");
        return NULL;
    }
    
    ret = ast_stmt(BREAK_STMT, NULL, NULL);
    ret->u.s.up = context;
    
    return ret;
}

static struct node * continue_stmt(struct node *context)
{
    int in_iter = 0;
    struct node *ret;
    struct source src = source;
    
    expect(CONTINUE);
    expect(';');
    
    while (context) {
        if (is_iteration_stmt(context)) {
            in_iter = 1;
            break;
        } else {
            context = context->u.s.up;
        }
    }
    
    if (!in_iter) {
        errorf(src, "'continue' statement is not in a loop statement.");
        return NULL;
    }
    
    ret = ast_stmt(CONTINUE_STMT, NULL, NULL);
    ret->u.s.up = context;
    
    return ret;
}

static struct node * return_stmt()
{
    expect(RETURN);
    
    return ast_stmt(RETURN_STMT, expr_stmt(), NULL);;
}

static struct node * statement(struct node *context)
{
    switch (token->id) {
        case '{':       return _compound_stmt(context);
        case IF:        return if_stmt(context);
        case SWITCH:    return switch_stmt(context);
        case WHILE:     return while_stmt(context);
        case DO:        return do_while_stmt(context);
        case FOR:       return for_stmt(context);
        case GOTO:      return goto_stmt();
        case CONTINUE:  return continue_stmt(context);
        case BREAK:     return break_stmt(context);
        case RETURN:    return return_stmt();
        case CASE:      return case_stmt(context);
        case DEFAULT:   return default_stmt(context);
        case ID:
            if (lookahead()->id == ':')
                return label_stmt(context);
            // go through
        default:
            return expr_stmt();
    }
}

static struct node * _compound_stmt(struct node *context)
{
    struct node *ret = ast_stmt(COMPOUND_STMT, NULL, NULL);
    struct vector *v = new_vector();
    
    expect('{');
    enter_scope();
    
    while (firstdecl(token) || firstexpr(token) || firststmt(token)) {
        if (firstdecl(token))
            // declaration
            vec_add_from_array(v, (void **)declaration());
        else
            // statement
            vec_push(v, statement(context));
    }
    
    ret->u.s.compoundstmt.blks = (struct node **)vtoa(v);
    expect('}');
    exit_scope();
    
    return ret;
}

struct node * compound_stmt()
{
    return _compound_stmt(NULL);
}
