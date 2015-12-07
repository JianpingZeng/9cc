#include "cc.h"

static node_t * simplify_stmt(node_t *stmt)
{
    switch (AST_ID(stmt)) {
    case COMPOUND_STMT:
    {
        node_t **blks = STMT_BLKS(stmt);
        for (int i = 0; i < LIST_LEN(blks); i++) {
            node_t *n = blks[i];
            if (isdecl(n)) {
                
            } else if (isstmt(n)) {
                
            } else {
                
            }
        }
    }
    break;
    case BREAK_STMT:
    case CASE_STMT:
    case CONTINUE_STMT:
    case DEFAULT_STMT:
    case WHILE_STMT:
    case DO_WHILE_STMT:
    case FOR_STMT:
    case GOTO_STMT:
    case LABEL_STMT:
    case IF_STMT:
    case RETURN_STMT:
    case SWITCH_STMT:
        break;
    case NULL_STMT:
        return stmt;
    default:
        cc_assert(0);
    }
}

static node_t * simplify_function(node_t *decl)
{
    node_t *stmt = DECL_BODY(decl);
    DECL_BODY(decl) = simplify_stmt(stmt);
    return decl;
}

static struct vector *filter_global(struct vector *v)
{
    cc_assert(SCOPE == GLOBAL);
    struct vector *r = vec_new();
    struct map *map = map_new();
    map->cmpfn = nocmp;
    for (int i = 0; i < vec_len(v); i++) {
        node_t *decl = vec_at(v, i);
        if (isfuncdef(decl)) {
            vec_push(r, decl);
            vec_add_array(r, (void **)DECL_X(decl).svars);
        } else if (isvardecl(decl)) {
            node_t *sym = DECL_SYM(decl);
            if (SYM_SCLASS(sym) == EXTERN)
                continue;
            node_t *decl1 = map_get(map, sym);
            if (decl1) {
                if (DECL_BODY(decl))
                    DECL_BODY(decl1) = DECL_BODY(decl);
            } else {
                vec_push(r, decl);
                map_put(map, sym, decl);
            }
        }
    }
    map_free(map);
    return r;
}

node_t * simplify(node_t *tree)
{
    cc_assert(istudecl(tree) && errors == 0);

    for (int i = 0; i < LIST_LEN(DECL_EXTS(tree)); i++) {
        node_t *decl = DECL_EXTS(tree)[i];
        if (isfuncdef(decl)) {
            node_t *node = simplify_function(decl);
        } else {
            
        }
    }
    
    return tree;
}
