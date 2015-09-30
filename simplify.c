#include "cc.h"

static node_t * simplify_stmt(node_t *stmt)
{
    println("simplify");
    switch (AST_ID(stmt)) {
    case COMPOUND_STMT:
	{
	    node_t **blks = STMT_BLKS(stmt);
	    for (int i = 0; i < array_len((void **)blks); i++) {
		node_t *node = blks[i];
		if (isstmt(node))
		    simplify_stmt(node);
	    }
	}
	break;
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
    default:
	break;
    }
}

void simplify(node_t *tree)
{
    CCAssert(AST_ID(tree) == TU_DECL);
    
    struct vector *v = vec_new();
    node_t **exts = DECL_EXTS(tree);
    
    for (int i = 0; i < array_len((void **)exts); i++) {
	node_t *node = exts[i];
	switch (AST_ID(node)) {
	case FUNC_DECL:
	    if (isfuncdef(node)) {
		node_t *stmt = DECL_BODY(node);
		vec_push(v, simplify_stmt(stmt));
	    } else {
		vec_push(v, node);
	    }
	    break;
	default:
	    vec_push(v, node);
	    break;
	}
    }

    DECL_EXTS(tree) = (node_t **)vtoa(v);
}
