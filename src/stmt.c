#include "cc.h"

static struct stmt * statement(struct stmt *context);

static struct stmt * expr_stmt()
{
    struct stmt *ret;
    
    if (token->id == ';') {
        ret = NULL;
    } else if (firstexpr(token)) {
        ret = stmt_node(EXPR_STMT, NODE(expression()), NULL);
    } else {
        ret = NULL;
        error("missing statement before '%s'", token->name);
    }
    
    expect(';');
    
    return ret;
}

static struct stmt * if_stmt(struct stmt *context)
{
    struct stmt *ret;
    struct expr *expr;
    struct stmt *stmt1;
    
    expect(IF);
    expect('(');
    expr = expression();
    expect(')');
    
    stmt1 = statement(context);
    ret = stmt_node(IF_STMT, NODE(expr), NODE(stmt1));
    
    if (token->id == ELSE) {
        expect(ELSE);
        ret = stmt_node(ELSE_STMT, NODE(ret), NODE(statement(context)));
    }
    
    return ret;
}

static struct stmt * while_stmt(struct stmt *context)
{
    struct expr *expr;
    struct stmt *ret;
    
    expect(WHILE);
    expect('(');
    expr = expression();
    expect(')');
    
    ret = stmt_node(WHILE_STMT, NODE(expr), NULL);
    ret->up = context;
    ret->node.kids[1] = NODE(statement(ret));
    ret->up = NULL;
    
    return ret;
}

static struct stmt * do_while_stmt(struct stmt *context)
{
    struct stmt *stmt;
    struct expr *expr;
    struct stmt *ret;
    
    expect(DO);
    
    ret = stmt_node(DO_WHILE_STMT, NULL, NULL);
    ret->up = context;
    stmt = statement(ret);
    expect(WHILE);
    expect('(');
    expr = expression();
    expect(')');
    expect(';');
    ret->node.kids[0] = NODE(stmt);
    ret->node.kids[1] = NODE(expr);
    ret->up = NULL;
    
    return ret;
}

static struct stmt * for_stmt(struct stmt *context)
{
    struct stmt *ret = stmt_node(FOR_STMT, NULL, NULL);
    
    expect(FOR);
    expect('(');
    
    enter_scope();
    
    if (token->id == ';') {
        expect(';');
    } else {
        if (firstdecl(token)) {
            // declaration
            ret->u.forstmt.decl = declaration();
        } else {
            // expression
            ret->u.forstmt.init = expression();
            expect(';');
        }
    }
    
    if (token->id != ';')
        ret->u.forstmt.cond = expression();
    
    expect(';');
    
    if (token->id != ')')
        ret->u.forstmt.ctrl = expression();
    
    expect(')');
    
    ret->up = context;
    ret->node.kids[0] = NODE(statement(ret));
    ret->up = NULL;
    
    exit_scope();
    
    return ret;
}

static struct stmt * switch_stmt(struct stmt *context)
{
    struct expr *expr;
    struct stmt *ret;
    
    expect(SWITCH);
    expect('(');
    expr = expression();
    expect(')');
    
    ret = stmt_node(SWITCH_STMT, NODE(expr), NULL);
    ret->up = context;
    ret->node.kids[1] = NODE(statement(ret));
    ret->up = NULL;
    
    return ret;
}

static struct stmt * case_stmt(struct stmt *context)
{
    int in_sw = 0;
    struct expr *expr;
    struct stmt *stmt;
    struct source src = source;
    
    expect(CASE);
    expr = constant_expr();
    expect(':');
    
    while (context) {
        if (is_switch_stmt(context)) {
            in_sw = 1;
            break;
        } else {
            context = context->up;
        }
    }
    
    // print before parsing statement
    if (!in_sw)
        errorf(src, "'case' statement is not in a switch statement.");
    
    // always parse even if not in a switch statement
    stmt = statement(context);
    
    if (!in_sw)
        return NULL;
    else
        return stmt_node(CASE_STMT, NODE(expr), NODE(stmt));
}

static struct stmt * default_stmt(struct stmt *context)
{
    int in_sw = 0;
    struct stmt *stmt;
    struct source src = source;
    
    expect(DEFAULT);
    expect(':');
    
    while (context) {
        if (is_switch_stmt(context)) {
            in_sw = 1;
            break;
        } else {
            context = context->up;
        }
    }
    
    // print before parsing statement
    if (!in_sw)
        errorf(src, "'default' statement is not in a switch statement.");
    
    stmt = statement(context);
    
    if (!in_sw)
        return NULL;
    else
        return stmt_node(DEFAULT_STMT, NODE(stmt), NULL);
}

static struct stmt * label_stmt(struct stmt *context)
{
    struct node *label;
    struct stmt *stmt;
    
    label = NODE(expr_node(ADDR_EXPR, ID, NULL, NULL));
    expect(ID);
    expect(':');
    stmt = statement(context);
    
    return stmt_node(LABEL_STMT, label, NODE(stmt));
}

static struct stmt * goto_stmt()
{
    struct node *expr = NULL;
    
    expect(GOTO);
    if (token->id == ID)
        expr = NODE(expr_node(ADDR_EXPR, ID, NULL, NULL));
    expect(ID);
    expect(';');
    
    return stmt_node(GOTO_STMT, expr, NULL);
}

static struct stmt * break_stmt(struct stmt *context)
{
    int in_iter_sw = 0;
    struct source src = source;
    struct stmt *ret;
    
    expect(BREAK);
    expect(';');
    
    while (context) {
        if (is_iteration_stmt(context) || is_switch_stmt(context)) {
            in_iter_sw = 1;
            break;
        } else {
            context = context->up;
        }
    }
    
    if (!in_iter_sw) {
        errorf(src, "'break' statement is not in a loop or switch statement.");
        return NULL;
    }
    
    ret = stmt_node(BREAK_STMT, NULL, NULL);
    ret->up = context;
    
    return ret;
}

static struct stmt * continue_stmt(struct stmt *context)
{
    int in_iter = 0;
    struct stmt *ret;
    struct source src = source;
    
    expect(CONTINUE);
    expect(';');
    
    while (context) {
        if (is_iteration_stmt(context)) {
            in_iter = 1;
            break;
        } else {
            context = context->up;
        }
    }
    
    if (!in_iter) {
        errorf(src, "'continue' statement is not in a loop statement.");
        return NULL;
    }
    
    ret = stmt_node(CONTINUE_STMT, NULL, NULL);
    ret->up = context;
    
    return ret;
}

static struct stmt * return_stmt()
{
    expect(RETURN);
    
    return stmt_node(RETURN_STMT, NODE(expr_stmt()), NULL);;
}

static struct stmt * statement(struct stmt *context)
{
    switch (token->id) {
            // compound
        case '{':
            return compound_statement(context);
            // selection
        case IF:
            return if_stmt(context);
        case SWITCH:
            return switch_stmt(context);
            // iteration
        case WHILE:
            return while_stmt(context);
        case DO:
            return do_while_stmt(context);
        case FOR:
            return for_stmt(context);
            // jump
        case GOTO:
            return goto_stmt();
        case CONTINUE:
            return continue_stmt(context);
        case BREAK:
            return break_stmt(context);
        case RETURN:
            return return_stmt();
            // labeled
        case CASE:
            return case_stmt(context);
        case DEFAULT:
            return default_stmt(context);
        case ID:
            if (lookahead()->id == ':')
                return label_stmt(context);
            // go through
            // expression
        default:
            return expr_stmt();
    }
}

struct stmt * compound_statement(struct stmt *context)
{
    struct stmt *ret = stmt_node(COMPOUND_STMT, NULL, NULL);
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
    
    ret->u.compoundstmt.blks = (struct node **)vtoa(v);
    expect('}');
    exit_scope();
    
    return ret;
}
