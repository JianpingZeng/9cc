#include "cc.h"

static struct stmt * statement(struct stmt *context);

static struct stmt * expr_stmt()
{
    struct stmt *ret;

    if (token->id == ';') {
	ret = NULL;
    } else if (kind(token->id) & FIRST_EXPR) {
	ret = stmt_node(EXPR_STMT, NODE(expression()), NULL);	
    } else {
	ret = NULL;
	error("missing statement before '%k'", token);
    }
    
    match(';');

    return ret;
}

static struct stmt * if_stmt(struct stmt *context)
{
    struct stmt *ret;
    struct expr *expr;
    struct stmt *stmt1;
    
    match(IF);
    match('(');
    expr = expression();
    match(')');

    stmt1 = statement(context);
    ret = stmt_node(IF_STMT, NODE(expr), NODE(stmt1));
    
    if (token->id == ELSE) {
	match(ELSE);
	ret = stmt_node(ELSE_STMT, NODE(ret), NODE(statement(context)));
    }

    return ret;
}

static struct stmt * while_stmt(struct stmt *context)
{
    struct expr *expr;

    match(WHILE);
    match('(');
    expr = expression();
    match(')');

    return stmt_node(WHILE_STMT, NODE(expr), NODE(statement(context)));
}

static struct stmt * do_while_stmt(struct stmt *context)
{
    struct stmt *stmt;
    struct expr *expr;
    
    match(DO);
    stmt = statement(context);
    match(WHILE);
    match('(');
    expr = expression();
    match(')');
    match(';');

    return stmt_node(DO_WHILE_STMT, NODE(stmt), NODE(expr));
}

static struct stmt * for_stmt(struct stmt *context)
{
    struct node *node;
    struct node *expr;
    struct stmt *ret;

    match(FOR);
    match('(');

    if (token->id == ';')
	node = concat_node(NULL, NULL);
    else
	node = concat_node(NODE(expression()), NULL);

    expr = node;

    match(';');

    if (token->id == ';')
	node->kids[1] = concat_node(NULL, NULL);
    else
	node->kids[1] = concat_node(NODE(expression()), NULL);

    node = node->kids[1];

    match(';');

    if (token->id == ')')
	node->kids[1] = concat_node(NULL, NULL);
    else
	node->kids[1] = concat_node(NODE(expression()), NULL);
    
    match(')');

    ret = stmt_node(FOR_STMT, expr, NULL);
    ret->up = context;
    ret->node.kids[1] = NODE(statement(ret));

    return ret;
}

static struct stmt * switch_stmt(struct stmt *context)
{
    struct expr *expr;
    struct stmt *stmt;
    
    match(SWITCH);
    match('(');
    expr = expression();
    match(')');
    stmt = statement(context);
    
    return stmt_node(SWITCH_STMT, NODE(expr), NODE(stmt));
}

static struct stmt * case_stmt(struct stmt *context)
{
    struct expr *expr;
    struct stmt *stmt;
    
    match(CASE);
    expr = constant_expression();
    match(':');
    stmt = statement(context);
    
    return stmt_node(CASE_STMT, NODE(expr), NODE(stmt));
}

static struct stmt * default_stmt(struct stmt *context)
{
    match(DEFAULT);
    match(':');
    
    return stmt_node(DEFAULT_STMT, NODE(statement(context)), NULL);
}

static struct stmt * label_stmt(struct stmt *context)
{
    struct node *label;
    struct stmt *stmt;

    label = NODE(expr_node(ADDR_OP, ID, NULL, NULL));
    match(ID);
    match(':');
    stmt = statement(context);
    
    return stmt_node(LABEL_STMT, label, NODE(stmt));
}

static struct stmt * goto_stmt()
{
    struct node *expr = NULL;
    
    match(GOTO);
    if (token->id == ID)
	expr = NODE(expr_node(ADDR_OP, ID, NULL, NULL));
    match(ID);
    match(';');
    
    return stmt_node(GOTO_STMT, expr, NULL);
}

static struct stmt * break_stmt(struct stmt *context)
{
    match(BREAK);
    match(';');
    return stmt_node(BREAK_STMT, NULL, NULL);
}

static struct stmt * continue_stmt(struct stmt *context)
{
    int is_in_iteration = 0;
    struct stmt *ret;
    unsigned line = src.line;
    
    match(CONTINUE);
    match(';');

    while (context) {
	if (is_iteration_stmt(context)) {
	    is_in_iteration = 1;
	    break;
	} else {
	    context = context->up;
	}
    }

    if (!is_in_iteration)
	errorf(line, "'continue' statement is not in a loop statement.");
    
    ret = stmt_node(CONTINUE_STMT, NULL, NULL);
    ret->up = context;

    return ret;
}

static struct stmt * return_stmt()
{   
    match(RETURN);
    
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
    struct node *node = NULL;

    match('{');

    while (kind(token->id) & (FIRST_STMT|FIRST_EXPR|FIRST_DECL)) {
	struct node *item;
	struct node *node1;

	if ((token->id == ID && is_typedef_name(token->name)) ||
	    (token->id != ID && kind(token->id) & FIRST_DECL))
	    // declaration
	    item = NODE(declaration());
	else
	    // statement
	    item = NODE(statement(context));
	
	node1 = concat_node(item, NULL);
        if (node)
	    node->kids[1] = node1;
	else
	    ret->node.kids[0] = node1;

	node = node1;
    }

    match('}');

    return ret;
}
