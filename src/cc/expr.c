#include "cc.h"

static struct expr * cast_expr();

static inline int is_assign_op(int t)
{
    return t == '=' ||
	t == MULEQ ||
	t == ADDEQ ||
	t == MINUSEQ ||
	t == DIVEQ ||
	t == MODEQ ||
	t == XOREQ ||
	t == BANDEQ ||
	t == BOREQ ||
	t == LSHIFTEQ ||
	t == RSHIFTEQ;
}

static struct expr * typename_expr()
{
    struct expr *expr;
    struct type *type;

    match('(');
    type = typename();
    match(')');
    expr = expr_node(CAST_EXPR, 0, NULL, NULL);
    expr->node.symbol = anonymous_symbol(&identifiers, SCOPE);
    expr->node.symbol->type = type;

    return expr;
}

static struct expr * argument_expr_list()
{
    struct expr *ret = NULL;

    if (kind(token->id) & FIRST_ASSIGN_EXPR) {
	struct vector *v = new_vector();
	ret = expr_node(ARGS_EXPR, 0, NULL, NULL);
	for (;;) {
	    vector_push(v, NODE(assign_expression()));
	    if (token->id == ',')
		match(',');
	    else
		break;
	}
	ret->node.kids[0] = NODE(new_anode((struct node **)vector_to_array(v)));
    } else if (token->id != ')') {
	error("expect assignment expression");
    }

    return ret;
}

static struct expr * postfix_expr1(struct expr *ret)
{
    int t;
    
    for (;token->id == '[' || token->id == '(' || token->id == '.' || token->id == DEREF || token->id == INCR || token->id == DECR;) {
	switch (token->id) {
	case '[':
	    t = token->id;
	    match('[');
	    ret = expr_node(BINARY_EXPR, t, ret, expression());
	    match(']');
	    break;
	case '(':
	    t = token->id;
	    match('(');
	    ret = expr_node(CALL_EXPR, 0, ret, argument_expr_list());
	    match(')');
	    break;
	case '.':
	case DEREF:
	    t = token->id;
	    match(token->id);
	    ret = expr_node(BINARY_EXPR, t, ret, expr_node(ADDR_EXPR, ID, NULL, NULL));
	    match(ID);
	    break;
	case INCR:
	case DECR:
	    t = token->id;
	    match(token->id);
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
	    match(t);
	    ret = expr_node(ADDR_EXPR, ID, NULL, NULL);
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
	    match(t);
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
	    match(t);
	    ret = expr_node(STRING_LITERAL, t, NULL, NULL);
	    ret->node.symbol = sym;
	}
	break;
    case '(':
	{
	    struct token *ahead = lookahead();
	    if (istypename(ahead)) {
		ret = typename_expr();
		ret->node.kids[0] = NODE(initializer_list());
	    } else {
		match('(');
		ret = expression();
		match(')');
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
	match(t);
	uexpr = expr_node(UNARY_EXPR, t, unary_expr(), NULL);
	break;
    case '&':
    case '*':
    case '+':
    case '-':
    case '~':
    case '!':
	t = token->id;
	match(t);
	uexpr = expr_node(UNARY_EXPR, t, cast_expr(), NULL);
	break;
    case SIZEOF:  
	t = token->id;
	match(token->id);	
	ahead = lookahead();
	if (token->id == '(' && istypename(ahead)) {
	    struct expr *texpr = typename_expr();
	    if (token->id == '{') {
		texpr->node.kids[0] = NODE(initializer_list());
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
	// type-name
	cast1 = typename_expr();
	if (token->id == '{') {
	    cast1->node.kids[0] = NODE(initializer_list());
	    cast1 = postfix_expr1(cast1);
	} else {
	    cast1->node.kids[0] = (struct node*)cast_expr();
	}
    } else {
	// expression
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
	match(token->id);
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
	match(token->id);
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
	match(token->id);
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
	match(token->id);
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
	match(token->id);
	equl = expr_node(BINARY_EXPR, t, equl, relation_expr());
    }

    return equl;
}

static struct expr * and_expr()
{
    struct expr * and1;

    and1 = equality_expr();
    while (token->id == '&') {
	match('&');
	and1 = expr_node(BINARY_EXPR, '&', and1, equality_expr());
    }

    return and1;
}

static struct expr * exclusive_or()
{
    struct expr * eor;

    eor = and_expr();
    while (token->id == '^') {
	match('^');
	eor = expr_node(BINARY_EXPR, '^', eor, and_expr());
    }

    return eor;
}

static struct expr * inclusive_or()
{
    struct expr * ior;

    ior = exclusive_or();
    while (token->id == '|') {
	match('|');
	ior = expr_node(BINARY_EXPR, '|', ior, exclusive_or());
    }

    return ior;
}

static struct expr * logic_and()
{
    struct expr * and1;

    and1 = inclusive_or();
    while (token->id == AND) {
	match(AND);
	and1 = expr_node(BINARY_EXPR, AND, and1, inclusive_or());
    }

    return and1;
}

static struct expr * cond_expr(struct expr *e)
{
    struct expr * ret;
    int t;

    ret = e ? e : cast_expr();
    for (;;) {
	switch (token->id) {
	case '*':
	case '/':
	case '%':
	    t = token->id;
	    match(token->id);
	    ret = expr_node(BINARY_EXPR, t, ret, cast_expr());
	    break;
	case '+':
	case '-':
	    t = token->id;
	    match(token->id);
	    ret = expr_node(BINARY_EXPR, t, ret, multiple_expr());
	    break;
	case LSHIFT:
	case RSHIFT:
	    t = token->id;
	    match(token->id);
	    ret = expr_node(BINARY_EXPR, t, ret, additive_expr());
	    break;
	case '>':
	case '<':
	case LEQ:
	case GEQ:
	    t = token->id;
	    match(token->id);
	    ret = expr_node(BINARY_EXPR, t, ret, shift_expr());
	    break;
	case EQ:
	case NEQ:
	    t = token->id;
	    match(token->id);
	    ret = expr_node(BINARY_EXPR, t, ret, relation_expr());
	    break;
	case '&':
	    t = token->id;
	    match(token->id);
	    ret = expr_node(BINARY_EXPR, t, ret, equality_expr());
	    break;
	case '^':
	    t = token->id;
	    match(token->id);
	    ret = expr_node(BINARY_EXPR, t, ret, and_expr());
	    break;
	case '|':
	    t = token->id;
	    match(token->id);
	    ret = expr_node(BINARY_EXPR, t, ret, exclusive_or());
	    break;
	case AND:
	    t = token->id;
	    match(token->id);
	    ret = expr_node(BINARY_EXPR, t, ret, inclusive_or());
	    break;
	case OR:
	    t = token->id;
	    match(token->id);
	    ret = expr_node(BINARY_EXPR, t, ret, logic_and());
	    break;
	default:
	    if (token->id == '?') {
		struct expr *r, *e, *c;
		match('?');
		e = expression();
		match(':');
		c = cond_expr(NULL);
		r = expr_node(BINARY_EXPR, ':', e, c);
		ret = expr_node(BINARY_EXPR, '?', ret, r);
	    }
	    return ret;
	}
    }
}

struct expr * assign_expression()
{
    struct expr *assign1;
    struct token *ahead = lookahead();

    if (token->id == '(' && istypename(ahead)) {
	// type-name
	struct expr *texpr = typename_expr();
	if (token->id == '{') {
	    // unary
	    texpr->node.kids[0] = NODE(initializer_list());
	    texpr = postfix_expr1(texpr);
	    if (is_assign_op(token->id)) {
		int t = token->id;
		match(token->id);
		assign1 = expr_node(BINARY_EXPR, t, texpr, assign_expression());
	    } else {
		assign1 = cond_expr(texpr);
	    }
	} else {
	    // cast
	    texpr->node.kids[0] = (struct node*)cast_expr();
	    assign1 = cond_expr(texpr);
	}
    } else {
	struct expr * uexpr = unary_expr();
	if (is_assign_op(token->id)) {
	    int t = token->id;
	    match(token->id);
	    assign1 = expr_node(BINARY_EXPR, t, uexpr, assign_expression());
	} else {
	    assign1 = cond_expr(uexpr);
	}
    }
    
    return assign1;
}

struct expr * expression()
{
    struct expr *expr;

    expr = assign_expression();
    while (token->id == ',') {
	match(token->id);
	expr = expr_node(COMMA_EXPR, ',', expr, assign_expression());
    }

    return expr;
}

// TODO: cast initializer list
int is_constexpr(struct node *expr)
{
    if (!expr || !isexpr(expr))
	return 0;

    switch (expr->id) {
    case BINARY_EXPR:
    case COMMA_EXPR:
	return is_constexpr(expr->kids[0]) && is_constexpr(expr->kids[1]);
	
    case UNARY_EXPR:
    case CAST_EXPR:
	return is_constexpr(expr->kids[0]);

    case INTEGER_LITERAL:
    case FLOAT_LITERAL:
    case STRING_LITERAL:
	return 1;

    case ADDR_EXPR:
	return 0;

    case CALL_EXPR:
    case ARGS_EXPR:
	return 0;

    default:
	assert(0);
    }
}

struct expr * constant_expression()
{
    struct expr *expr = cond_expr(NULL);
    if (is_constexpr(NODE(expr)))
	return expr;
    else
	return NULL;
}

// TODO
int eval_constexpr(struct expr *expr, union value *value)
{
    value->i = 0;
    return 0;
}
