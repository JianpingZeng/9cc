#include "cc.h"

static node_t * statement(void);
static node_t * do_compound_stmt(bool func);
static struct vector * predefined_identifiers(void);
static void post_funcdef(struct vector *v, struct vector *predefines);
static node_t * ensure_return(node_t *expr, struct source src);

static const char *__continue;
static const char *__break;
static struct vector *__cases;
static node_t *__default;
static node_t *__switch_ty;

#define SET_LOOP_CONTEXT(cont, brk)		\
    const char *__saved_continue = __continue;	\
    const char *__saved_break = __break;	\
    __continue = cont;				\
    __break = brk;				\
    enter_scope()

#define RESTORE_LOOP_CONTEXT()			\
    __continue = __saved_continue;		\
    __break = __saved_break;			\
    exit_scope()

#define SET_SWITCH_CONTEXT(brk, ty)		\
    const char *__saved_break = __break;	\
    struct vector *__saved_cases = __cases;	\
    node_t *__saved_default = __default;	\
    node_t *__saved_switch_ty = __switch_ty;	\
    __break = brk;				\
    __cases = vec_new();			\
    __default = NULL;				\
    __switch_ty = ty

#define RESTORE_SWITCH_CONTEXT()		\
    vec_free(__cases);				\
    __break = __saved_break;			\
    __cases = __saved_cases;			\
    __default = __saved_default;		\
    __switch_ty = __saved_switch_ty

#define CONTINUE_CONTEXT  (__continue)
#define BREAK_CONTEXT     (__break)
#define IN_SWITCH         (__cases)
#define CASES             (__cases)
#define DEFLT             (__default)
#define SWITCH_TYPE       (__switch_ty)

static node_t * expr_stmt(void)
{
    node_t *ret = NULL;
    
    if (token->id == ';')
        ret = ast_null_stmt();
    else if (firstexpr(token))
        ret = expression();
    else
        error("missing statement before '%s'", token->name);
    
    expect(';');
    return ret;
}

/**
 * The entire **if** statement forms its own block scope, as do the
 * substatements even if they are not compound statements. This serves
 * to restrict the scope of objects and types that might be created
 * as a side effect of using compound literal or type names.
 */
static node_t * if_stmt(void)
{
    node_t *ret = NULL;
    node_t *cond;
    node_t *thenpart;
    node_t *elsepart = NULL;
    struct source src = source;

    enter_scope();

    SAVE_ERRORS;
    expect(IF);
    expect('(');
    cond = bool_expr();
    expect(')');

    enter_scope();
    thenpart = statement();
    exit_scope();

    if (token->id == ELSE) {
        expect(ELSE);
	enter_scope();
	elsepart = statement();
	exit_scope();
    }

    exit_scope();

    if (NO_ERROR) {
	ret = ast_if(cond, thenpart, elsepart);
	AST_SRC(ret) = src;
    }
    
    return ret;
}

/**
 * Each iterative statement(do/while/for) forms its own block scope,
 * as do the substatements even if they are not compound statements.
 */
static node_t * while_stmt(void)
{
    node_t *ret = NULL;
    node_t *cond;
    node_t *body;
    struct source src = source;

    enter_scope();

    SAVE_ERRORS;
    expect(WHILE);
    expect('(');
    cond = bool_expr();
    expect(')');

    const char *beg = gen_label();
    const char *end = gen_label();
    SET_LOOP_CONTEXT(beg, end);
    body = statement();
    RESTORE_LOOP_CONTEXT();

    exit_scope();

    if (NO_ERROR) {
	struct vector *v = vec_new();
	vec_push(v, ast_label(beg));
	vec_push(v, ast_if(cond, body, ast_jump(end)));
	vec_push(v, ast_jump(beg));
	vec_push(v, ast_label(end));
	ret = ast_compound(src, (node_t **)vtoa(v));
    }
    
    return ret;
}

static node_t * do_while_stmt(void)
{
    node_t *ret = NULL;
    node_t *body;
    node_t *cond;
    struct source src = source;
    const char *beg = gen_label();
    const char *end = gen_label();

    enter_scope();

    SAVE_ERRORS;
    expect(DO);
    SET_LOOP_CONTEXT(beg, end);
    body = statement();
    RESTORE_LOOP_CONTEXT();
    expect(WHILE);
    expect('(');
    cond = bool_expr();
    expect(')');
    expect(';');

    exit_scope();

    if (NO_ERROR) {
	struct vector *v = vec_new();
	vec_push(v, ast_label(beg));
	vec_push(v, body);
	vec_push(v, ast_if(cond, ast_jump(beg), NULL));
	vec_push(v, ast_label(end));
	ret = ast_compound(src, (node_t **)vtoa(v));
    }
    
    return ret;
}

static node_t * for_stmt(void)
{
    node_t *ret = NULL;
    node_t **decl = NULL;
    node_t *init = NULL;
    node_t *cond = NULL;
    node_t *ctrl = NULL;
    node_t *body;
    struct source src = source;
    const char *beg = gen_label();
    const char *mid = gen_label();
    const char *end = gen_label();

    enter_scope();

    SAVE_ERRORS;
    expect(FOR);
    expect('(');
    
    if (token->id == ';') {
        expect(';');
    } else {
        if (firstdecl(token)) {
            // declaration
            decl = declaration();
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

    SET_LOOP_CONTEXT(mid, end);
    body = statement();
    RESTORE_LOOP_CONTEXT();
    
    exit_scope();

    if (NO_ERROR) {
	struct vector *v = vec_new();
	if (decl)
	    vec_add_array(v, (void **)decl);
	else if (init)
	    vec_push(v, init);
	vec_push(v, ast_label(beg));
	if (cond)
	    vec_push(v, ast_if(cond, NULL, ast_jump(end)));
	if (body)
	    vec_push(v, body);
	vec_push(v, ast_label(mid));
	if (ctrl)
	    vec_push(v, ctrl);
	vec_push(v, ast_jump(beg));
	vec_push(v, ast_label(end));
	ret = ast_compound(src, (node_t **)vtoa(v));
    }
	
    return ret;
}

static node_t * switch_jmp(node_t *var, node_t *case_node)
{
    node_t *cond;
    long index = STMT_INDEX(case_node);
    const char *label = STMT_LABEL(case_node);
    node_t *index_node = int_literal_node(AST_TYPE(var), (union value){.u = index});

    cond = ast_bop(EQ, inttype, var, index_node);
    return ast_if(cond, ast_jump(label), NULL);
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
static node_t * switch_stmt(void)
{
    node_t *ret = NULL;
    node_t *expr;
    node_t *body;
    struct source src = source;
    const char *end = gen_label();

    SAVE_ERRORS;
    expect(SWITCH);
    expect('(');
    expr = switch_expr();
    expect(')');

    SET_SWITCH_CONTEXT(end, expr ? AST_TYPE(expr) : NULL);

    body = statement();
    
    if (NO_ERROR) {
        struct vector *v = vec_new();
	node_t *var = tmpvar(AST_TYPE(expr));
	vec_push(v, ast_bop('=', AST_TYPE(expr), var, expr));
	for (int i = 0; i < vec_len(CASES); i++) {
	    node_t *case_node = vec_at(CASES, i);
	    vec_push(v, switch_jmp(var, case_node));
	}
	const char *label = DEFLT ? STMT_LABEL(DEFLT) : end;
	vec_push(v, ast_jump(label));
	vec_push(v, body);
	vec_push(v, ast_label(end));
	ret = ast_compound(src, (node_t **)vtoa(v));
    }

    RESTORE_SWITCH_CONTEXT();
    
    return ret;
}

static void check_case_duplicates(node_t *node)
{
    for (int i = vec_len(CASES)-1; i >= 0; i--) {
	node_t *n = vec_at(CASES, i);
	if (STMT_INDEX(n) == STMT_INDEX(node)) {
	    errorf(AST_SRC(node),
		   "duplicate case value '%lld', previous case defined here: %s:%u:%u",
		   STMT_INDEX(node), AST_SRC(n).file, AST_SRC(n).line, AST_SRC(n).column);
	    break;
	}
    }
}

static node_t * case_stmt(void)
{
    node_t *body;
    struct source src = source;
    node_t *ret = ast_compound(src, NULL);

    SAVE_ERRORS;
    expect(CASE);
    STMT_INDEX(ret) = intexpr1(SWITCH_TYPE);
    expect(':');
    
    if (!IN_SWITCH)
	errorf(src, "'case' statement not in switch statement");

    // only check when intexpr is okay.
    if (NO_ERROR) {
	check_case_duplicates(ret);
	vec_push(CASES, ret);
    }
    
    // always parse even if not in a switch statement
    body = statement();

    if (NO_ERROR) {
	const char *label = gen_label();
	struct vector *v = vec_new();
	vec_push(v, ast_label(label));
	vec_push(v, body);
	STMT_LIST(ret) = (node_t **)vtoa(v);
	STMT_LABEL(ret) = label;
    } else {
	ret = NULL;
    }
    
    return ret;
}

static node_t * default_stmt(void)
{
    node_t *body;
    struct source src = source;
    node_t *ret = ast_compound(src, NULL);

    SAVE_ERRORS;
    expect(DEFAULT);
    expect(':');

    // print before parsing statement
    if (!IN_SWITCH)
	errorf(src, "'default' statement not in switch statement");

    if (DEFLT)
	errorf(src,
	       "multiple default labels in one switch, previous case defined here:%s:%u:%u",
	       AST_SRC(DEFLT).file, AST_SRC(DEFLT).line, AST_SRC(DEFLT).column);
    
    DEFLT = ret;
    body = statement();

    if (NO_ERROR) {
	const char *label = gen_label();
	struct vector *v = vec_new();
	vec_push(v, ast_label(label));
	vec_push(v, body);
	STMT_LIST(ret) = (node_t **)vtoa(v);
	STMT_LABEL(ret) = label;
    } else {
	ret = NULL;
    }

    return ret;
}

static node_t * label_stmt(void)
{
    node_t *body;
    const char *name = NULL;
    struct source src = source;
    node_t *ret = ast_compound(src, NULL);
    const char *label = gen_label();

    SAVE_ERRORS;
    name = token->name;
    expect(ID);
    expect(':');

    // install label before parsing body
    if (NO_ERROR) {
        node_t *n = map_get(LABELS, name);
	if (n)
	    errorf(src,
		   "redefinition of label '%s', previous label defined here:%s:%u:%u",
		   name, AST_SRC(n).file, AST_SRC(n).line, AST_SRC(n).column);
	map_put(LABELS, name, ret);
    }

    body = statement();
    
    if (NO_ERROR) {
	struct vector *v = vec_new();
	vec_push(v, ast_label(label));
	vec_push(v, body);
	STMT_LIST(ret) = (node_t **)vtoa(v);
	STMT_LABEL(ret) = label;
    } else {
	ret = NULL;
    }

    return ret;
}

static node_t * goto_stmt(void)
{
    node_t *ret = NULL;
    struct source src = source;
    const char *name;

    SAVE_ERRORS;
    expect(GOTO);
    name = token->name;
    expect(ID);
    expect(';');

    if (NO_ERROR) {
	ret = ast_jump(name);
	vec_push(GOTOS, ret);
	AST_SRC(ret) = src;
    }

    return ret;
}

static node_t * break_stmt(void)
{
    node_t *ret = NULL;
    struct source src = source;

    SAVE_ERRORS;
    expect(BREAK);
    expect(';');

    if (!BREAK_CONTEXT)
	errorf(src, "'break' statement not in loop or switch statement");

    if (NO_ERROR) {
	ret = ast_jump(BREAK_CONTEXT);
	AST_SRC(ret) = src;
    }
    
    return ret;
}

static node_t * continue_stmt(void)
{
    node_t *ret = NULL;
    struct source src = source;

    SAVE_ERRORS;
    expect(CONTINUE);
    expect(';');
 
    if (!CONTINUE_CONTEXT)
	errorf(src, "'continue' statement not in loop statement");

    if (NO_ERROR) {
	ret = ast_jump(CONTINUE_CONTEXT);
	AST_SRC(ret) = src;
    }
    
    return ret;
}

static node_t * return_stmt(void)
{
    node_t *ret = NULL;
    node_t *expr;
    struct source src = source;

    SAVE_ERRORS;
    expect(RETURN);
    expr = expr_stmt();
    expr = ensure_return(expr, src);
    
    if (NO_ERROR) {
	ret = ast_return(expr);
	AST_SRC(ret) = src;
    }
    
    return ret;
}

static node_t * statement(void)
{
    switch (token->id) {
    case '{':       return do_compound_stmt(false);
    case IF:        return if_stmt();
    case SWITCH:    return switch_stmt();
    case WHILE:     return while_stmt();
    case DO:        return do_while_stmt();
    case FOR:       return for_stmt();
    case GOTO:      return goto_stmt();
    case CONTINUE:  return continue_stmt();
    case BREAK:     return break_stmt();
    case RETURN:    return return_stmt();
    case CASE:      return case_stmt();
    case DEFAULT:   return default_stmt();
    case ID:
	if (lookahead()->id == ':')
	    return label_stmt();
	// go through
    default:
	return expr_stmt();
    }
}

static node_t * do_compound_stmt(bool func)
{
    struct source src = source;
    struct vector *v = vec_new();
    struct vector *predefines = NULL;
    
    expect('{');
    enter_scope();

    if (func)
	// add predefined identifiers
	predefines = predefined_identifiers();
    
    while (firstdecl(token) || firstexpr(token) || firststmt(token)) {
        if (firstdecl(token))
            // declaration
            vec_add_array(v, (void **)declaration());
        else
            // statement
            vec_push_safe(v, statement());
    }

    if (func)
	post_funcdef(v, predefines);

    expect('}');
    exit_scope();
    
    return ast_compound(src, (node_t **)vtoa(v));
}

node_t * compound_stmt(void)
{
    return do_compound_stmt(true);
}

void backfill_labels(void)
{
    for (int i = 0; i < vec_len(GOTOS); i++) {
	node_t *goto_stmt = vec_at(GOTOS, i);
	const char *name = STMT_LABEL(goto_stmt);
	node_t *label_stmt = map_get(LABELS, name);
	if (label_stmt)
	    STMT_LABEL(goto_stmt) = STMT_LABEL(label_stmt);
	else
	    errorf(AST_SRC(goto_stmt), "use of undeclared label '%s'", name);
    }
}

static struct vector * predefined_identifiers(void)
{
    struct vector *v = vec_new();

    {
	/**
	 * Predefined identifier: __func__
	 * The identifier __func__ is implicitly declared by C99
	 * implementations as if the following declaration appeared
	 * after the opening brace of each function definition:
	 *
	 * static const char __func__[] = "function-name";
	 *
	 */
	const char *name = strs("__func__");
	node_t *type = array_type(qual(CONST, chartype));
	node_t *decl = define_localvar(name, type, STATIC);
	// initializer
	node_t *literal = new_string_literal(FNAME);
	AST_SRC(literal) = source;
	init_string(type, literal);
	DECL_BODY(decl) = literal;
	vec_push(v, decl);
    }
    
    return v;
}

static void post_funcdef(struct vector *v, struct vector *predefines)
{
    struct vector *used = vec_new();
    // remove if no ref.
    for (int i = vec_len(predefines) - 1; i >= 0; i--) {
	node_t *decl = vec_at(predefines, i);
	node_t *sym = DECL_SYM(decl);
	if (SYM_REFS(sym)) {
	    vec_push_front(v, decl);
	    vec_push_front(used, decl);
	}
    }
    filter_local(used, true);
    vec_free(predefines);
}

static node_t * ensure_return(node_t *expr, struct source src)
{
    // return immediately if expr is NULL. (parsing failed)
    if (expr == NULL)
	return NULL;

    if (isvoid(rtype(FTYPE))) {
	if (!isnullstmt(expr) && !isvoid(AST_TYPE(expr)))
	    errorf(src, "void function '%s' should not return a value", FNAME);
    } else {
        if (!isnullstmt(expr)) {
	    node_t *ty1 = AST_TYPE(expr);
	    node_t *ty2 = rtype(FTYPE);
	    if (!(expr = retconv(ty2, expr)))
		errorf(src,
		       "returning '%s' from function '%s' with incompatible result type '%s'",
		       type2s(ty1), FNAME, type2s(ty2));
	} else {
	    errorf(src, "non-void function '%s' should return a value", FNAME);
	}
    }
    return expr;
}
