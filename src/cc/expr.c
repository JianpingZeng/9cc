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

// TODO
static struct expr * typename_expr()
{
    struct expr * type1;
    struct symbol *sym;

    match('(');
    sym = install_symbol(token->name, &identifiers, scopelevel());
    type1 = expr_node(CAST_OP, ID, NULL, NULL);
    type1->node.symbol = sym;
    // TODO
    if (token->id == ID || kind(token->id) & (TYPE_SPEC|TYPE_QUAL))
	match(token->id);
    match(')');
    return type1;
}

static struct expr * postfix_expr1(struct expr *ret)
{
    int t;
    
    for (;token->id == '[' || token->id == '(' || token->id == '.' || token->id == DEREF || token->id == INCR || token->id == DECR;) {
	switch (token->id) {
	case '[':
	    t = token->id;
	    match(token->id);
	    ret = expr_node(BINARY_OP, t, ret, expression());
	    match(']');
	    break;
	case '(':
	    t = token->id;
	    match(token->id);
	    ret = expr_node(BINARY_OP, t, ret, expression());
	    match(')');
	    break;
	case '.':
	case DEREF:
	    t = token->id;
	    match(token->id);
	    ret = expr_node(BINARY_OP, t, ret, expr_node(ADDR_OP, token->id, NULL, NULL));
	    match(ID);
	    break;
	case INCR:
	case DECR:
	    t = token->id;
	    match(token->id);
	    ret = expr_node(UNARY_OP, t, ret, NULL);
	    break;
	default:
	    assert(0);
	}
    }

    return ret;
}

// TODO
static struct expr * postfix_expr()
{
    int t;
    struct symbol *sym;
    struct expr *ret;
    
    switch (token->id) {
    case ID:
	{
	    t = token->id;
	    sym = install_symbol(token->name, &identifiers, scopelevel());
	    sym->src = source;
	    match(t);
	    ret = expr_node(ADDR_OP, t, NULL, NULL);
	    ret->node.symbol = sym;
	}
	break;
    case ICONSTANT:
    case FCONSTANT:
	{
	    t = token->id;
	    sym = find_symbol(token->name, &constants, CONSTANT);
	    sym->value = token->v.u;
	    sym->type = token->v.type;
	    sym->src = source;
	    match(t);
	    ret = expr_node(t == ICONSTANT ? INTEGER_LITERAL : FLOAT_LITERAL, t, NULL, NULL);
	    ret->node.symbol = sym;
	}
	break;
    case SCONSTANT:
	{
	    t = token->id;
	    sym = find_symbol(token->name, &constants, CONSTANT);
	    sym->type = token->v.type;
	    sym->src = source;
	    match(t);
	    ret = expr_node(STRING_LITERAL, t, NULL, NULL);
	    ret->node.symbol = sym;
	}
	break;
    case '(':
	{
	    struct token *ahead = lookahead();
	    if (is_typename(ahead)) {
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
	error("invalid postfix expression at '%k'", token);
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
	uexpr = expr_node(UNARY_OP, t, unary_expr(), NULL);
	break;
    case '&':
    case '*':
    case '+':
    case '-':
    case '~':
    case '!':
	t = token->id;
	match(t);
	uexpr = expr_node(UNARY_OP, t, cast_expr(), NULL);
	break;
    case SIZEOF:  
	t = token->id;
	match(token->id);	
	ahead = lookahead();
	if (token->id == '(' && is_typename(ahead)) {
	    struct expr *texpr = typename_expr();
	    if (token->id == '{') {
		texpr->node.kids[0] = NODE(initializer_list());
		texpr = postfix_expr1(texpr);
	    }
	    uexpr = expr_node(UNARY_OP, t, texpr, NULL);
	} else {
	    uexpr = expr_node(UNARY_OP, t, unary_expr(), NULL);
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

    if (token->id == '(' && is_typename(ahead)) {
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
	mulp1 = expr_node(BINARY_OP, t, mulp1, cast_expr());
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
	add1 = expr_node(BINARY_OP, t, add1, multiple_expr());
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
	shift1 = expr_node(BINARY_OP, t, shift1, additive_expr());
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
	rel = expr_node(BINARY_OP, t, rel, shift_expr());
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
	equl = expr_node(BINARY_OP, t, equl, relation_expr());
    }

    return equl;
}

static struct expr * and_expr()
{
    struct expr * and1;

    and1 = equality_expr();
    while (token->id == '&') {
	match('&');
	and1 = expr_node(BINARY_OP, '&', and1, equality_expr());
    }

    return and1;
}

static struct expr * exclusive_or()
{
    struct expr * eor;

    eor = and_expr();
    while (token->id == '^') {
	match('^');
	eor = expr_node(BINARY_OP, '^', eor, and_expr());
    }

    return eor;
}

static struct expr * inclusive_or()
{
    struct expr * ior;

    ior = exclusive_or();
    while (token->id == '|') {
	match('|');
	ior = expr_node(BINARY_OP, '|', ior, exclusive_or());
    }

    return ior;
}

static struct expr * logic_and()
{
    struct expr * and1;

    and1 = inclusive_or();
    while (token->id == AND) {
	match(AND);
	and1 = expr_node(BINARY_OP, AND, and1, inclusive_or());
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
	    ret = expr_node(BINARY_OP, t, ret, cast_expr());
	    break;
	case '+':
	case '-':
	    t = token->id;
	    match(token->id);
	    ret = expr_node(BINARY_OP, t, ret, multiple_expr());
	    break;
	case LSHIFT:
	case RSHIFT:
	    t = token->id;
	    match(token->id);
	    ret = expr_node(BINARY_OP, t, ret, additive_expr());
	    break;
	case '>':
	case '<':
	case LEQ:
	case GEQ:
	    t = token->id;
	    match(token->id);
	    ret = expr_node(BINARY_OP, t, ret, shift_expr());
	    break;
	case EQ:
	case NEQ:
	    t = token->id;
	    match(token->id);
	    ret = expr_node(BINARY_OP, t, ret, relation_expr());
	    break;
	case '&':
	    t = token->id;
	    match(token->id);
	    ret = expr_node(BINARY_OP, t, ret, equality_expr());
	    break;
	case '^':
	    t = token->id;
	    match(token->id);
	    ret = expr_node(BINARY_OP, t, ret, and_expr());
	    break;
	case '|':
	    t = token->id;
	    match(token->id);
	    ret = expr_node(BINARY_OP, t, ret, exclusive_or());
	    break;
	case AND:
	    t = token->id;
	    match(token->id);
	    ret = expr_node(BINARY_OP, t, ret, inclusive_or());
	    break;
	case OR:
	    t = token->id;
	    match(token->id);
	    ret = expr_node(BINARY_OP, t, ret, logic_and());
	    break;
	default:
	    if (token->id == '?') {
		struct expr *r, *e, *c;
		match('?');
		e = expression();
		match(':');
		c = cond_expr(NULL);
		r = expr_node(COLON_OP, ':', e, c);
		ret = expr_node(COND_OP, '?', ret, r);
	    }
	    return ret;
	}
    }
}

struct expr * assign_expression()
{
    struct expr *assign1;
    struct token *ahead = lookahead();

    if (token->id == '(' && is_typename(ahead)) {
	// type-name
	struct expr *texpr = typename_expr();
	if (token->id == '{') {
	    // unary
	    texpr->node.kids[0] = NODE(initializer_list());
	    texpr = postfix_expr1(texpr);
	    if (is_assign_op(token->id)) {
		int t = token->id;
		match(token->id);
		assign1 = expr_node(BINARY_OP, t, texpr, assign_expression());
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
	    assign1 = expr_node(BINARY_OP, t, uexpr, assign_expression());
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
	expr = expr_node(COMMA_OP, ',', expr, assign_expression());
    }

    return expr;
}

struct expr * constant_expression()
{
    return cond_expr(NULL);
}
