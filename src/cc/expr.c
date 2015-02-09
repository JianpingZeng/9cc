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

    match('(');
    type1 = expr_node(0, CAST, NULL, NULL);
    // TODO
    match(ID);
    match(')');
    return type1;
}

// TODO
static struct expr * postfix_expr()
{
    int t;
    
    switch (token->id) {
    case ID:
    case ICONSTANT:
    case FCONSTANT:
    case SCONSTANT:
	t = token->id;
	match(t);
	
	break;
    case '(':
	break;
    default:
	error("invalid postfix expression");
    }
}

static struct expr * unary_expr()
{
    struct expr * uexpr;
    int t;

    BEGIN_CALL(unary_expr);

    switch (token->id) {
    case INCR:
    case DECR:
	t = token->id;
	match(t);
	uexpr = expr_node(UNARY_OPERATOR, t, unary_expr(), NULL);
	break;
    case '&':
    case '*':
    case '+':
    case '-':
    case '~':
    case '!':
	t = token->id;
	match(t);
	uexpr = expr_node(UNARY_OPERATOR, t, cast_expr(), NULL);
	break;
    case SIZEOF:
	t = token->id;
	match(t);
	if (token->id == '(') {
	    struct expr *texpr = typename_expr();
	    if (token->id == '{') {
		match('{');
		texpr->node.kids[0] = initializer_list();
		if (token->id == ',')
		    match(',');
		match('}');
	    }

	    uexpr = expr_node(UNARY_OPERATOR, t, texpr, NULL);
	}
	else {
	    uexpr = expr_node(UNARY_OPERATOR, t, unary_expr(), NULL);
	}
	break;
    default:
	uexpr = postfix_expr();
	break;
    }
    
    END_CALL(unary_expr);
    return uexpr;
}

static struct expr * cast_expr()
{
    BEGIN_CALL(cast_expr);
    struct expr * cast1;

    if (token->id == '(') {
	cast1 = typename_expr();
	if (token->id == '{') {
	    match('{');
	    cast1->node.kids[0] = initializer_list();
	    if (token->id == ',')
		match(',');
	    match('}');
	}
	else {
	    cast1->node.kids[0] = cast_expr();
	}
    }
    else {
	cast1 = unary_expr();
    }

    END_CALL(cast_expr);
    return cast1;
}

static struct expr * multiple_expr()
{
    BEGIN_CALL(multiple_expr);
    struct expr * mulp1;

    mulp1 = cast_expr();
    while (token->id == '*' || token->id == '/' || token->id == '%') {
	int t = token->id;
	match(token->id);
	mulp1 = expr_node(0, t, mulp1, cast_expr());
    }
    END_CALL(multiple_expr);
    return mulp1;
}

static struct expr * additive_expr()
{
    BEGIN_CALL(additive_expr);
    struct expr * add1;

    add1 = multiple_expr();
    while (token->id == '+' || token->id == '-') {
	int t = token->id;
	match(token->id);
	add1 = expr_node(0, t, add1, multiple_expr());
    }
    END_CALL(additive_expr);
    return add1;
}

static struct expr * shift_expr()
{
    BEGIN_CALL(shift_expr);
    struct expr * shift1;

    shift1 = additive_expr();
    while (token->id == LSHIFT || token->id == RSHIFT) {
	int t = token->id;
	match(token->id);
	shift1 = expr_node(0, t, shift1, additive_expr());
    }
    END_CALL(shift_expr);
    return shift1;
}

static struct expr * relation_expr()
{
    BEGIN_CALL(relation_expr);
    struct expr * rel;

    rel = shift_expr();
    while (token->id == '<' || token->id == '>' || token->id == LEQ || token->id == GEQ) {
	int t = token->id;
	match(token->id);
	rel = expr_node(0, t, rel, shift_expr());
    }
    END_CALL(relation_expr);
    return rel;
}

static struct expr * equality_expr()
{
    BEGIN_CALL(equality_expr);
    struct expr * equl;

    equl = relation_expr();
    while (token->id == EQ || token->id == NEQ) {
	int t = token->id;
	match(token->id);
	equl = expr_node(0, t, equl, relation_expr());
    }
    END_CALL(equality_expr);
    return equl;
}

static struct expr * and_expr()
{
    BEGIN_CALL(and_expr);
    struct expr * and1;

    and1 = equality_expr();
    while (token->id == '&') {
	match('&');
	and1 = expr_node(0, '&', and1, equality_expr());
    }
    END_CALL(and_expr);
    return and1;
}

static struct expr * exclusive_or()
{
    BEGIN_CALL(exclusive_or);
    struct expr * eor;

    eor = and_expr();
    while (token->id == '^') {
	match('^');
	eor = expr_node(0, '^', eor, and_expr());
    }
    END_CALL(exclusive_or);
    return eor;
}

static struct expr * inclusive_or()
{
    BEGIN_CALL(inclusive_or);
    struct expr * ior;

    ior = exclusive_or();
    while (token->id == '|') {
	match('|');
	ior = expr_node(0, '|', ior, exclusive_or());
    }
    END_CALL(inclusive_or);
    return ior;
}

static struct expr * logic_and()
{
    BEGIN_CALL(logic_and);
    struct expr * and1;

    and1 = inclusive_or();
    while (token->id == AND) {
	match(AND);
	and1 = expr_node(0, AND, and1, inclusive_or());
    }
    END_CALL(logic_and);
    return and1;
}

static struct expr * logic_or()
{
    BEGIN_CALL(logic_or);
    struct expr * or1;

    or1 = logic_and();
    while (token->id == OR) {
	match(OR);
	or1 = expr_node(0, OR, or1, logic_and());
    }
    END_CALL(logic_or);
    return or1;
}

static struct expr * cond_expr(struct expr *e)
{
    struct expr * ret;
    int t;

    BEGIN_CALL(cond_expr);

    ret = e;
    for (;;) {
	switch (token->id) {
	case '*':
	case '/':
	case '%':
	    t = token->id;
	    match(token->id);
	    ret = expr_node(0, t, ret, cast_expr());
	    break;
	case '+':
	case '-':
	    t = token->id;
	    match(token->id);
	    ret = expr_node(0, t, ret, multiple_expr());
	    break;
	case LSHIFT:
	case RSHIFT:
	    t = token->id;
	    match(token->id);
	    ret = expr_node(0, t, ret, additive_expr());
	    break;
	case '>':
	case '<':
	case LEQ:
	case GEQ:
	    t = token->id;
	    match(token->id);
	    ret = expr_node(0, t, ret, shift_expr());
	    break;
	case EQ:
	case NEQ:
	    t = token->id;
	    match(token->id);
	    ret = expr_node(0, t, ret, relation_expr());
	    break;
	case '&':
	    t = token->id;
	    match(token->id);
	    ret = expr_node(0, t, ret, equality_expr());
	    break;
	case '^':
	    t = token->id;
	    match(token->id);
	    ret = expr_node(0, t, ret, and_expr());
	    break;
	case '|':
	    t = token->id;
	    match(token->id);
	    ret = expr_node(0, t, ret, exclusive_or());
	    break;
	case AND:
	    t = token->id;
	    match(token->id);
	    ret = expr_node(0, t, ret, inclusive_or());
	    break;
	case OR:
	    t = token->id;
	    match(token->id);
	    ret = expr_node(0, t, ret, logic_and());
	    break;
	default:
	    END_CALL(cond_expr);
	    return ret;
	}
    }
}

static struct expr * assign_expr()
{
    BEGIN_CALL(assign_expr);
    struct expr * assign1;

    if (token->id == '(') {
        struct expr *texpr = typename_expr();
	if (token->id == '{') {
	    // unary
	    match('{');
	    texpr->node.kids[0] = initializer_list();
	    if (token->id == ',')
		match(',');
	    match('}');
	    if (is_assign_op(token->id)) {
		int t = token->id;
		match(token->id);
		assign1 = expr_node(0, t, texpr, assign_expr());
	    }
	    else {
		assign1 = cond_expr(texpr);
	    }
	}
	else {
	    // cast
	    texpr->node.kids[0] = cast_expr();
	    assign1 = cond_expr(texpr);
	}
    }
    else {
	struct expr * uexpr = unary_expr();
	if (is_assign_op(token->id)) {
	    int t = token->id;
	    match(token->id);
	    assign1 = expr_node(0, t, uexpr, assign_expr());
	}
	else {
	    assign1 = cond_expr(uexpr);
	}
    }

    END_CALL(assign_expr);
    return assign1;
}

struct expr * expr()
{
    BEGIN_CALL(expr);
    struct expr *expr1;

    expr1 = assign_expr();
    while (token->id == ',') {
	match(token->id);
	expr1 = expr_node(0, ',', expr1, assign_expr());
    }

    END_CALL(expr);

    return expr1;
}
