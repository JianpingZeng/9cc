#include "cc.h"

static node_t * simplify_stmt(node_t *stmt)
{
    switch (AST_ID(stmt)) {
    case COMPOUND_STMT:
	{
	    node_t **blks = STMT_BLKS(stmt);
	    for (int i = 0; i < LIST_LEN(blks); i++) {
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
    for (int i = 0; i < LIST_LEN(DECL_EXTS(tree)); i++) {
	node_t *node = exts[i];
	
	if (isfuncdef(node)) {
	    node_t *stmt = DECL_BODY(node);
	    DECL_BODY(node) = simplify_stmt(stmt);
	}
    }
}
