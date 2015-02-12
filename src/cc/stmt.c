#include "cc.h"

static struct stmt * expr_stmt()
{
    struct stmt *ret;

    if (token->id == ';')
	ret = NULL;
    else
	ret = stmt_node(EXPR_STMT, NODE(expression()), NULL);
    
    match(';');

    return ret;
}

static struct stmt * if_stmt()
{
    struct stmt *ret;
    struct expr *expr;
    struct stmt *stmt1;
    
    match(IF);
    match('(');
    expr = expression();
    match(')');

    stmt1 = statement();
    ret = stmt_node(IF_STMT, NODE(expr), NODE(stmt1));
    
    if (token->id == ELSE) {
	match(ELSE);
	ret = stmt_node(ELSE_STMT, NODE(ret), NODE(statement()));
    }

    return ret;
}

static struct stmt * while_stmt()
{
    struct expr *expr;

    match(WHILE);
    match('(');
    expr = expression();
    match(')');

    return stmt_node(WHILE_STMT, NODE(expr), NODE(statement()));
}

static struct stmt * do_while_stmt()
{
    struct stmt *stmt;
    struct expr *expr;
    
    match(DO);
    stmt = statement();
    match(WHILE);
    match('(');
    expr = expression();
    match(')');
    match(';');

    return stmt_node(DO_WHILE_STMT, NODE(stmt), NODE(expr));
}

static struct stmt * for_stmt()
{
    return NULL;
}

static struct stmt * switch_stmt()
{
    return NULL;
}

static struct stmt * case_stmt()
{
    return NULL;
}

static struct stmt * default_stmt()
{
    return NULL;
}

static struct stmt * label_stmt()
{
    return NULL;
}

static struct stmt * goto_stmt()
{
    return NULL;
}

static struct stmt * break_stmt()
{
    return NULL;
}

static struct stmt * continue_stmt()
{
    return NULL;
}

static struct stmt * return_stmt()
{
    return NULL;
}

struct stmt * statement()
{
    switch (token->id) {
	// compound
    case '{':
	return compound_statement();
	// selection
    case IF:
	return if_stmt();
    case SWITCH:
	return switch_stmt();
	// iteration
    case WHILE:
	return while_stmt();
    case DO:
	return do_while_stmt();
    case FOR:
	return for_stmt();
	// jump
    case GOTO:
	return goto_stmt();
    case CONTINUE:
	return continue_stmt();
    case BREAK:
	return break_stmt();
    case RETURN:
	return return_stmt();
	// labeled
    case CASE:
	return case_stmt();
    case DEFAULT:
	return default_stmt();
    case ID:
        if (lookahead()->id == ':')
	    return label_stmt();
	// go through
	// expression
    default:
        return expr_stmt();
    }
}

struct stmt * compound_statement()
{
    struct stmt *ret = stmt_node(COMPOUND_STMT, NULL, NULL);
    struct node *node = NULL;

    match('{');

    // TODO
    while (kind(token->id) & (FIRST_STMT|FIRST_EXPR|FIRST_DECL)) {
	struct stmt *stmt;
	struct node *node1;
	
	stmt = statement();
	node1 = concat_node(NODE(stmt), NULL);
        if (node)
	    node->kids[1] = node1;
	else
	    ret->node.kids[0] = node1;

	node = node1;
    }

    match('}');

    return ret;
}
