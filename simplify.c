#include "cc.h"

static const char * __continue;
static const char * __break;

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
    __break = brk

#define RESTORE_SWITCH_CONTEXT()		\
    __break = __saved_break

#define BREAK_CONTEXT      (__break)
#define CONTINUE_CONTEXT   (__continue)

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
	    RESTORE_SWITCH_CONTEXT();
	    
	    node_t **cases = STMT_SWITCH_CASES(stmt);
	    if (cases) {
		for (int i = 0; i < LIST_LEN(cases); i++) {
		    
		}
	    }

	    // TODO:
	}
    case CASE_STMT:
    case DEFAULT_STMT:
	{
	    const char *label = gen_label();
	    node_t *body = simplify_stmt(STMT_CASE_BODY(stmt));

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
	// NULL_STMT
	CCAssertf(AST_ID(stmt) == NULL_STMT,
		  "null statement expect, but got '%s'",
		  nname(stmt));
	return stmt;
    }
}

void simplify(node_t *tree)
{
    CCAssert(AST_ID(tree) == TU_DECL);
    for (int i = 0; i < LIST_LEN(DECL_EXTS(tree)); i++) {
	node_t *node = DECL_EXTS(tree)[i];
	
	if (isfuncdef(node)) {
	    node_t *stmt = DECL_BODY(node);
	    DECL_BODY(node) = simplify_stmt(stmt);
	}
    }
}
