#include "cc.h"

static const char * __continue;
static const char * __break;
static struct vector * __cases;
static const char *__default;

#define SET_JUMP_CONTEXT(cont, brk)		\
    const char *__saved_continue = __continue;	\
    const char *__saved_break = __break;	\
    __continue = cont;				\
    __break = brk

#define RESTORE_JUMP_CONTEXT()			\
    __continue = __saved_continue;		\
    __break = __saved_break

#define SET_SWITCH_CONTEXT(brk)			\
    const char *__saved_break = __break;	\
    struct vector *__saved_cases = __cases;	\
    const char *__saved_default = __default;	\
    __break = brk;				\
    __cases = vec_new();			\
    __default = NULL

#define RESTORE_SWITCH_CONTEXT()		\
    __break = __saved_break;			\
    vec_free(__cases);				\
    __cases = __saved_cases;			\
    __default = __saved_default

#define BREAK_CONTEXT      (__break)
#define CONTINUE_CONTEXT   (__continue)
#define CASES              (__cases)
#define DEFLT              (__default)

static node_t * tmp_var(node_t *ty)
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
    const char *label = STMT_CASE_NAME(case_node);

    cond = ast_bop(EQ, inttype, var, new_integer_literal(index));
    return ast_if(cond, ast_jump(label), NULL);
}

static node_t * simplify_stmt(node_t *stmt)
{
    switch (AST_ID(stmt)) {
    case COMPOUND_STMT:
	{
	    struct vector *v = vec_new();
	    for (int i = 0; i < LIST_LEN(STMT_BLKS(stmt)); i++) {
		node_t *node = STMT_BLKS(stmt)[i];
		if (isstmt(node))
		    vec_push(v, simplify_stmt(node));
		else
		    vec_push(v, node);
	    }
	    node_t *ret = ast_compound((node_t **)vtoa(v));
	    vec_free(v);
	    return ret;
	}
    case IF_STMT:
	{
	    node_t *cond = STMT_COND(stmt);
	    node_t *then = STMT_THEN(stmt);
	    node_t *els = STMT_ELSE(stmt);

	    then = simplify_stmt(then);
	    if (els)
		els = simplify_stmt(els);

	    return ast_if(cond, then, els);
	}
    case WHILE_STMT:
	{
	    const char *beg = gen_label();
	    const char *end = gen_label();
	    node_t * cond = STMT_WHILE_COND(stmt);
	    
	    SET_JUMP_CONTEXT(beg, end);
	    node_t *body = simplify_stmt(STMT_WHILE_BODY(stmt));
	    RESTORE_JUMP_CONTEXT();

	    struct vector *v = vec_new();
	    vec_push(v, ast_dest(beg));
	    vec_push(v, ast_if(cond, body, ast_jump(end)));
	    vec_push(v, ast_jump(beg));
	    vec_push(v, ast_dest(end));
	    return ast_compound((node_t **)vtoa(v));
	}
    case DO_WHILE_STMT:
	{
	    const char *beg = gen_label();
	    const char *end = gen_label();
	    node_t *cond = STMT_WHILE_COND(stmt);

	    SET_JUMP_CONTEXT(beg, end);
	    node_t *body = simplify_stmt(STMT_WHILE_BODY(stmt));
	    RESTORE_JUMP_CONTEXT();

	    struct vector *v = vec_new();
	    vec_push(v, ast_dest(beg));
	    vec_push(v, body);
	    vec_push(v, ast_if(cond, ast_jump(beg), NULL));
	    vec_push(v, ast_dest(end));
	    return ast_compound((node_t **)vtoa(v));
	}
    case FOR_STMT:
	{
	    const char *beg = gen_label();
	    const char *mid = gen_label();
	    const char *end = gen_label();
	    node_t **decl = STMT_FOR_DECL(stmt);
	    node_t *init = STMT_FOR_INIT(stmt);
	    node_t *cond = STMT_FOR_COND(stmt);
	    node_t *ctrl = STMT_FOR_CTRL(stmt);

	    SET_JUMP_CONTEXT(mid, end);
	    node_t *body = simplify_stmt(STMT_FOR_BODY(stmt));
	    RESTORE_JUMP_CONTEXT();

	    struct vector *v = vec_new();
	    if (decl)
		vec_add_array(v, (void **)decl);
	    else if (init)
		vec_push(v, init);

	    vec_push(v, ast_dest(beg));
	    if (cond)
		vec_push(v, ast_if(cond, NULL, ast_jump(end)));
	    if (body)
		vec_push(v, body);
	    vec_push(v, ast_dest(mid));
	    if (ctrl)
		vec_push(v, ctrl);
	    vec_push(v, ast_jump(beg));
	    vec_push(v, ast_dest(end));
	    return ast_compound((node_t **)vtoa(v));
	}
    case SWITCH_STMT:
	{
	    const char *end = gen_label();
	    node_t *expr = STMT_SWITCH_EXPR(stmt);
	    
	    SET_SWITCH_CONTEXT(end);

	    node_t *body = simplify_stmt(STMT_SWITCH_BODY(stmt));
	    struct vector *v = vec_new();
	    node_t *var = tmp_var(AST_TYPE(expr));

	    vec_push(v, ast_bop('=', AST_TYPE(expr), var, expr));
	    for (int i = 0; i < vec_len(CASES); i++) {
		node_t *case_node = vec_at(CASES, i);
		vec_push(v, switch_jmp(var, case_node));
	    }

	    vec_push(v, ast_jump(DEFLT ? DEFLT : end));
	    vec_push(v, body);
	    vec_push(v, ast_dest(end));
	    
	    RESTORE_SWITCH_CONTEXT();
	    
	    return ast_compound((node_t **)vtoa(v));
	}
    case CASE_STMT:
	{
	    const char *label = gen_label();
	    node_t *body = simplify_stmt(STMT_CASE_BODY(stmt));

	    STMT_CASE_NAME(stmt) = label;
	    vec_push(CASES, stmt);
	    
	    struct vector *v = vec_new();
	    vec_push(v, ast_dest(label));
	    vec_push(v, body);
	    return ast_compound((node_t **)vtoa(v));
	}
    case DEFAULT_STMT:
	{
	    const char *label = gen_label();
	    node_t *body = simplify_stmt(STMT_CASE_BODY(stmt));

	    STMT_CASE_NAME(stmt) = label;
	    DEFLT = label;
	    
	    struct vector *v = vec_new();
	    vec_push(v, ast_dest(label));
	    vec_push(v, body);
	    return ast_compound((node_t **)vtoa(v));
	}
    case LABEL_STMT:
	{
	    node_t *body = simplify_stmt(STMT_LABEL_BODY(stmt));
	    node_t *label = ast_label(STMT_LABEL_NAME(stmt));
	    
	    struct vector *v = vec_new();
	    vec_push(v, label);
	    vec_push(v, body);
	    return ast_compound((node_t **)vtoa(v));
	}
    case GOTO_STMT:
        return ast_goto(STMT_LABEL_NAME(stmt));
    case BREAK_STMT:
        return ast_jump(BREAK_CONTEXT);
    case CONTINUE_STMT:
        return ast_jump(CONTINUE_CONTEXT);
    case RETURN_STMT:
        return ast_return(STMT_RETURN_EXPR(stmt));
    default:
	// NULL_STMT or expression
	return stmt;
    }
}

void simplify(node_t *tree)
{
    struct vector *v = vec_new();
    for (int i = 0; i < LIST_LEN(DECL_EXTS(tree)); i++) {
	node_t *node = DECL_EXTS(tree)[i];
	
	if (isfuncdecl(node)) {
	    if (isfuncdef(node)) {
		node_t *stmt = DECL_BODY(node);
		DECL_BODY(node) = simplify_stmt(stmt);
	    }
	    vec_push(v, node);
	} else if (isvardecl(node)) {
	    vec_push(v, node);
	}
    }

    DECL_EXTS(tree) = (node_t **)vtoa(v);
}
