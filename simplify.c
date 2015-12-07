#include "cc.h"

static const char *__continue;
static const char *__break;
static struct vector *__cases;
static const char *__default;
static struct map *labels;
static struct vector *gotos;

#define SET_LOOP_CONTEXT(cont, brk)             \
    const char *__saved_continue = __continue;  \
    const char *__saved_break = __break;        \
    __continue = cont;                          \
    __break = brk

#define RESTORE_LOOP_CONTEXT()                  \
    __continue = __saved_continue;              \
    __break = __saved_break

#define SET_SWITCH_CONTEXT(brk)                 \
    const char *__saved_break = __break;        \
    struct vector *__saved_cases = __cases;     \
    const char *__saved_default = __default;    \
    __break = brk;                              \
    __cases = vec_new();                        \
    __default = NULL

#define RESTORE_SWITCH_CONTEXT()                \
    __break = __saved_break;                    \
    vec_free(__cases);                          \
    __cases = __saved_cases;                    \
    __default = __saved_default

#define SET_FUNCDEF_CONTEXT()                   \
    labels = map_new();                         \
    gotos = vec_new()

#define RESTORE_FUNCDEF_CONTEXT()               \
    map_free(labels);                           \
    labels = NULL;                              \
    vec_free(gotos);                            \
    gotos = NULL

#define BREAK_CONTEXT     (__break)
#define CONTINUE_CONTEXT  (__continue)
#define CASES             (__cases)
#define DEFLT             (__default)

static node_t * switch_jmp(node_t *var, node_t *case_node)
{
    node_t *cond;
    long index = STMT_CASE_INDEX(case_node);
    const char *label = STMT_X(case_node).label;
    node_t *index_node = int_literal_node
        (AST_TYPE(var), (union value){.u = index});

    cond = ast_bop(EQ, inttype, var, index_node);
    return ast_if(cond, ast_jump(label), NULL);
}

static node_t *define_tmpvar(node_t * ty)
{
    const char *name = gen_tmpname();
    node_t *decl = make_localvar(name, ty, 0);
    node_t *n = alloc_node();
    AST_ID(n) = REF_EXPR;
    EXPR_SYM(n) = DECL_SYM(decl);
    AST_TYPE(n) = ty;
    SYM_REFS(DECL_SYM(decl))++;
    return n;
}

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
        node_t *cond = STMT_WHILE_COND(stmt);
        node_t *body = STMT_WHILE_BODY(stmt);

        SET_LOOP_CONTEXT(beg, end);
        body = simplify_stmt(body);
        RESTORE_LOOP_CONTEXT();

        struct vector *v = vec_new();
        vec_push(v, ast_label(beg));
        vec_push(v, ast_if(cond, body, ast_jump(end)));
        vec_push(v, ast_jump(beg));
        vec_push(v, ast_label(end));
        return ast_compound((node_t **) vtoa(v));
    }
    case DO_WHILE_STMT:
    {
        const char *beg = gen_label();
        const char *end = gen_label();
        node_t *cond = STMT_WHILE_COND(stmt);
        node_t *body = STMT_WHILE_BODY(stmt);

        SET_LOOP_CONTEXT(beg, end);
        body = simplify_stmt(body);
        RESTORE_LOOP_CONTEXT();

        struct vector *v = vec_new();
        vec_push(v, ast_label(beg));
        vec_push(v, body);
        vec_push(v, ast_if(cond, ast_jump(beg), NULL));
        vec_push(v, ast_label(end));
        return ast_compound((node_t **) vtoa(v));
    }
    case FOR_STMT:
    {
        
    }
    case SWITCH_STMT:
    {
        const char *end = gen_label();
        node_t *expr = STMT_SWITCH_EXPR(stmt);
        node_t *body = STMT_SWITCH_BODY(stmt);

        SET_SWITCH_CONTEXT(end);

        body = simplify_stmt(body);

        struct vector *v = vec_new();
        node_t *var = define_tmpvar(AST_TYPE(expr));
        vec_push(v, ast_bop('=', AST_TYPE(expr), var, expr));
        for (int i = 0; i < vec_len(CASES); i++) {
            node_t *case_node = vec_at(CASES, i);
            vec_push(v, switch_jmp(var, case_node));
        }
        const char *label = DEFLT ? DEFLT : end;
        vec_push(v, ast_jump(label));
        vec_push(v, body);
        vec_push(v, ast_label(end));

        RESTORE_SWITCH_CONTEXT();

        return ast_compound((node_t **) vtoa(v));
    }
    case CASE_STMT:
    {
        const char *label = gen_label();
        node_t *body = STMT_CASE_BODY(stmt);

        body = simplify_stmt(body);
        STMT_X(stmt).label = label;
        vec_push(CASES, stmt);

        struct vector *v = vec_new();
        vec_push(v, ast_label(label));
        vec_push(v, body);
        return ast_compound((node_t **) vtoa(v));
    }
    case DEFAULT_STMT:
    {
        const char *label = gen_label();
        node_t *body = STMT_CASE_BODY(stmt);

        body = simplify_stmt(body);
        STMT_X(stmt).label = label;
        DEFLT = label;

        struct vector *v = vec_new();
        vec_push(v, ast_label(label));
        vec_push(v, body);
        return ast_compound((node_t **) vtoa(v));
    }
    case LABEL_STMT:
    {
        const char *label = gen_label();
        const char *name = STMT_LABEL_NAME(stmt);
        node_t *body = STMT_LABEL_BODY(stmt);

        body = simplify_stmt(body);
        map_put(labels, name, (char *)label);

        struct vector *v = vec_new();
        vec_push(v, ast_label(label));
        vec_push(v, body);
        return ast_compound((node_t **)vtoa(v));
    }
    case GOTO_STMT:
    {
        node_t *ret = ast_jump(STMT_LABEL_NAME(stmt));
        vec_push(gotos, ret);
        return ret;
    }
    case BREAK_STMT:
        return ast_jump(BREAK_CONTEXT);
    case CONTINUE_STMT:
        return ast_jump(CONTINUE_CONTEXT);
    case RETURN_STMT:
        return ast_return(STMT_RETURN_EXPR(stmt));
    case NULL_STMT:
        return stmt;
    default:
        cc_assert(0);
    }
}

static void backfill_labels(void)
{
    
}

static node_t * simplify_function(node_t *decl)
{
    node_t *stmt = DECL_BODY(decl);

    SET_FUNCDEF_CONTEXT();
    DECL_BODY(decl) = simplify_stmt(stmt);
    RESTORE_FUNCDEF_CONTEXT();
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
