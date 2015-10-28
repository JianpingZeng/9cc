#include "cc.h"

static node_t * statement(void);
static node_t * do_compound_stmt(bool func);
static struct vector * predefined_identifiers(void);
static void post_funcdef(struct vector *v, struct vector *predefines);
static void ensure_return(node_t *expr, struct source src);

static const char *__continue;
static const char *__break;
static struct vector *__cases;
static node_t *__default;

#define SET_LOOP_CONTEXT(cont, brk)		\
    const char *__saved_continue = __continue;	\
    const char *__saved_break = __break;	\
    __continue = cont;				\
    __break = brk

#define RESTORE_LOOP_CONTEXT()			\
    __continue = __saved_continue;		\
    __break = __saved_break

#define SET_SWITCH_CONTEXT(brk)			\
    const char *__saved_break = __break;	\
    struct vector *__saved_cases = __cases;	\
    node_t *__saved_default = __default;	\
    __break = brk;				\
    __cases = vec_new();			\
    __default = NULL

#define RESTORE_SWITCH_CONTEXT()		\
    vec_free(__cases);				\
    __break = __saved_break;			\
    __cases = __saved_cases;			\
    __default = __saved_default

#define CONTINUE_CONTEXT  (__continue)
#define BREAK_CONTEXT     (__break)
#define IN_SWITCH         (__cases)
#define CASES             (__cases)
#define DEFLT             (__default)
#define FTYPE             (current_ftype)
#define FNAME             (current_fname)

static node_t * tmpvar(node_t *ty)
{
    const char *name = gen_tmpname();
    node_t *sym = install(name, &identifiers, GLOBAL);
    SYM_TYPE(sym) = ty;

    node_t *n = alloc_node();
    AST_ID(n) = REF_EXPR;
    EXPR_SYM(n) = sym;
    AST_TYPE(n) = ty;
    return n;
}

static node_t * switch_jmp(node_t *var, node_t *case_node)
{
    node_t *cond;
    int index = STMT_CASE_INDEX(case_node);
    const char *label = STMT_LABEL(case_node);

    cond = ast_bop(EQ, inttype, var, new_integer_literal(index));
    return ast_if(cond, ast_jump(label), NULL);
}

static void check_case_duplicates(node_t *node)
{
    for (int i = vec_len(CASES)-1; i >= 0; i--) {
	node_t *n = vec_at(CASES, i);
	if (STMT_CASE_INDEX(n) == STMT_CASE_INDEX(node)) {
	    errorf(AST_SRC(node),
		   "duplicate case value '%d', previous case defined here: %s:%u:%u",
		   STMT_CASE_INDEX(node), AST_SRC(n).file, AST_SRC(n).line, AST_SRC(n).column);
	    break;
	}
    }
}

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

static node_t * if_stmt(void)
{
    node_t *ret = NULL;
    node_t *cond;
    node_t *thenpart;
    node_t *elsepart = NULL;
    struct source src = source;

    SAVE_ERRORS;
    expect(IF);
    expect('(');
    cond = bool_expr();
    expect(')');
    thenpart = statement();

    if (token->id == ELSE) {
        expect(ELSE);
	elsepart = statement();
    }

    if (NO_ERROR) {
	ret = ast_stmt(IF_STMT, src, ast_if(cond, ast_gen(thenpart), ast_gen(elsepart)));
	STMT_IF_COND(ret) = cond;
	STMT_IF_THEN(ret) = thenpart;
	STMT_IF_ELSE(ret) = elsepart;
    }
    
    return ret;
}

static node_t * while_stmt(void)
{
    node_t *ret = NULL;
    node_t *cond;
    node_t *body;
    struct source src = source;

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

    if (NO_ERROR) {
	struct vector *v = vec_new();
	vec_push(v, ast_dest(beg));
	vec_push(v, ast_if(cond, ast_gen(body), ast_jump(end)));
	vec_push(v, ast_jump(beg));
	vec_push(v, ast_dest(end));
	ret = ast_stmt(WHILE_STMT, src, ast_compound((node_t **)vtoa(v)));
	STMT_WHILE_COND(ret) = cond;
	STMT_WHILE_BODY(ret) = body;
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

    if (NO_ERROR) {
	struct vector *v = vec_new();
	vec_push(v, ast_dest(beg));
	vec_push(v, ast_gen(body));
	vec_push(v, ast_if(cond, ast_jump(beg), NULL));
	vec_push(v, ast_dest(end));
	ret = ast_stmt(DO_WHILE_STMT, src, ast_compound((node_t **)vtoa(v)));
	STMT_WHILE_COND(ret) = cond;
	STMT_WHILE_BODY(ret) = body;
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

    SAVE_ERRORS;
    expect(FOR);
    expect('(');

    enter_scope();
    
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
	vec_push(v, ast_dest(beg));
	if (cond)
	    vec_push(v, ast_if(cond, NULL, ast_jump(end)));
	if (body)
	    vec_push(v, ast_gen(body));
	vec_push(v, ast_dest(mid));
	if (ctrl)
	    vec_push(v, ctrl);
	vec_push(v, ast_jump(beg));
	vec_push(v, ast_dest(end));
	ret = ast_stmt(FOR_STMT, src, ast_compound((node_t **)vtoa(v)));
	STMT_FOR_DECL(ret) = decl;
	STMT_FOR_INIT(ret) = init;
	STMT_FOR_COND(ret) = cond;
	STMT_FOR_CTRL(ret) = ctrl;
	STMT_FOR_BODY(ret) = body;
    }
	
    return ret;
}

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

    SET_SWITCH_CONTEXT(end);

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
	vec_push(v, ast_gen(body));
	vec_push(v, ast_dest(end));
	ret = ast_stmt(SWITCH_STMT, src, ast_compound((node_t **)vtoa(v)));
	STMT_SWITCH_EXPR(ret) = expr;
	STMT_SWITCH_BODY(ret) = body;
    }

    RESTORE_SWITCH_CONTEXT();
    
    return ret;
}

static node_t * case_stmt(void)
{
    node_t *body;
    struct source src = source;
    node_t *ret = ast_stmt(CASE_STMT, source, NULL);
    const char *label = gen_label();

    SAVE_ERRORS;
    expect(CASE);
    STMT_CASE_INDEX(ret) = intexpr();
    expect(':');
    STMT_LABEL(ret) = label;
    
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
	struct vector *v = vec_new();
	vec_push(v, ast_dest(label));
	vec_push(v, ast_gen(body));
	STMT_GEN(ret) = ast_compound((node_t **)vtoa(v));
	STMT_CASE_BODY(ret) = body;
    } else {
	ret = NULL;
    }
    
    return ret;
}

static node_t * default_stmt(void)
{
    node_t *body;
    struct source src = source;
    node_t *ret = ast_stmt(DEFAULT_STMT, source, NULL);
    const char *label = gen_label();

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
    STMT_LABEL(DEFLT) = label;
    body = statement();

    if (NO_ERROR) {
	struct vector *v = vec_new();
	vec_push(v, ast_dest(label));
	vec_push(v, ast_gen(body));
	STMT_GEN(ret) = ast_compound((node_t **)vtoa(v));
	STMT_CASE_BODY(ret) = body;
    } else {
	ret = NULL;
    }

    return ret;
}

static node_t * label_stmt(void)
{
    node_t *body;
    const char *label = NULL;
    struct source src = source;
    node_t *ret = ast_stmt(LABEL_STMT, source, NULL);

    SAVE_ERRORS;
    label = token->name;
    expect(ID);
    expect(':');

    if (NO_ERROR) {
        node_t *n = map_get(labels, label);
	if (n)
	    errorf(src,
		   "redefinition of label '%s', previous label defined here:%s:%u:%u",
		   label, AST_SRC(n).file, AST_SRC(n).line, AST_SRC(n).column);
	map_put(labels, label, ret);
    }

    STMT_LABEL(ret) = label;
    body = statement();
    
    if (NO_ERROR) {
	struct vector *v = vec_new();
	vec_push(v, ast_dest(label));
	vec_push(v, ast_gen(body));
	STMT_GEN(ret) = ast_compound((node_t **)vtoa(v));
	STMT_CASE_BODY(ret) = body;
    } else {
	ret = NULL;
    }

    return ret;
}

static node_t * goto_stmt(void)
{
    node_t *ret = NULL;
    struct source src = source;
    const char *label;

    SAVE_ERRORS;
    expect(GOTO);
    label = token->name;
    expect(ID);
    expect(';');

    if (NO_ERROR) {
	ret = ast_stmt(GOTO_STMT, src, ast_jump(label));
	STMT_LABEL(ret) = label;
	vec_push(gotos, ret);
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

    if (NO_ERROR)
	ret = ast_stmt(BREAK_STMT, src, ast_jump(BREAK_CONTEXT));
    
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

    if (NO_ERROR)
	ret = ast_stmt(CONTINUE_STMT, src, ast_jump(CONTINUE_CONTEXT));
    
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
    ensure_return(expr, src);
    
    if (NO_ERROR) {
	ret = ast_stmt(RETURN_STMT, src, ast_return(expr));
	STMT_RETURN_BODY(ret) = expr;
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
    node_t *ret = ast_stmt(COMPOUND_STMT, source, NULL);
    struct vector *v = vec_new();
    struct vector *predefines = NULL;
    
    expect('{');
    enter_scope();

    if (func) {
	// add predefined identifiers
	predefines = predefined_identifiers();
	vec_add(v, predefines);
    }
    
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

    STMT_BLKS(ret) = (node_t **)vtoa(v);
    // gen
    for (int i = 0; i < vec_len(v); i++) {
	node_t *n = vec_at(v, i);
        vec_set(v, i, ast_gen(n));
    }
    STMT_GEN(ret) = ast_compound((node_t **)vtoa(v));

    expect('}');
    exit_scope();
    
    return ret;
}

node_t * compound_stmt(void)
{
    return do_compound_stmt(true);
}

void backfill_labels(void)
{
    for (int i = 0; i < vec_len(gotos); i++) {
	node_t *goto_stmt = vec_at(gotos, i);
	const char *label = STMT_LABEL(goto_stmt);
	node_t *label_stmt = map_get(labels, label);
	if (!label_stmt)
	    errorf(AST_SRC(goto_stmt), "use of undeclared label '%s'", label);
    }
}

static struct vector * predefined_identifiers(void)
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
    struct vector *v = vec_new();
    
    const char *name = strs("__func__");
    node_t *sym = lookup(name, identifiers);
    if (sym && currentscope(sym)) {
	// redefinition of predefined identifier
	errorf(AST_SRC(sym), "redefinition of predefined identifier '%s'", name);
    } else {
	node_t *decl = ast_decl(VAR_DECL, SCOPE);
	node_t *type = array_type(qual(CONST, chartype));
	
	sym = install(name, &identifiers, SCOPE);
	AST_SRC(sym) = source;
	SYM_PREDEFINE(sym) = true;
	SYM_DEFINED(sym) = true;
	SYM_SCLASS(sym) = STATIC;
	SYM_TYPE(sym) = type;
	DECL_SYM(decl) = sym;

	node_t *literal = new_string_literal(FNAME);
	AST_SRC(literal) = source;
	// initializer
	init_string(type, literal);
	DECL_BODY(decl) = literal;
	
	vec_push(v, decl);
    }
    
    return v;
}

static void post_funcdef(struct vector *v, struct vector *predefines)
{
    // remove if no ref.
    struct vector *used = vec_new();
    size_t len = vec_len(predefines);
    for (int i = 0; i < len; i++) {
	node_t *decl = vec_at(predefines, i);
	node_t *sym = DECL_SYM(decl);
	if (SYM_REFS(sym))
	    vec_push(used, decl);
    }
    if (vec_len(used) != len) {
	while (len--)
	    vec_pop_front(v);
	for (int i = vec_len(used) - 1; i >= 0; i--)
	    vec_push_front(v, vec_at(used, i));
    }
    vec_free(predefines);
    vec_free(used);
}

static void ensure_return(node_t *expr, struct source src)
{
    if (isvoid(rtype(FTYPE))) {
	if (expr && !is_null_stmt(expr) && !isvoid(AST_TYPE(expr)))
	    errorf(src, "void function '%s' should not return a value", FNAME);
    } else {
	// error only if expr is not NULL to inhibit
	// redundant errors.
	if (expr) {
	    if (!is_null_stmt(expr)) {
		node_t *ty1 = AST_TYPE(expr);
		node_t *ty2 = rtype(FTYPE);
		if (!(expr = ret_conv(ty2, expr)))
		    errorf(src,
			   "returning '%s' from function '%s' with incompatible result type '%s'",
			   type2s(ty1), FNAME, type2s(ty2));
	    } else {
		errorf(src, "non-void function '%s' should return a value", FNAME);
	    }
	}
    }
}
