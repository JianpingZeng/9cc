#include "cc.h"

static const char *__continue;
static const char *__break;
static struct vector *__cases;
static const char *__default;
static struct map *labels;
static struct vector *gotos;
static struct vector *localvars;
static struct vector *staticvars;

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
    gotos = vec_new();                          \
    localvars = vec_new();                      \
    staticvars = vec_new()

#define RESTORE_FUNCDEF_CONTEXT()               \
    map_free(labels);                           \
    labels = NULL;                              \
    vec_free(gotos);                            \
    gotos = NULL;                               \
    vec_free(localvars);                        \
    localvars = NULL;                           \
    vec_free(staticvars);                       \
    staticvars = NULL

#define BREAK_CONTEXT     (__break)
#define CONTINUE_CONTEXT  (__continue)
#define CASES             (__cases)
#define DEFLT             (__default)

static node_t * switch_jmp(node_t *var, node_t *case_node)
{
    node_t *cond;
    long index = STMT_CASE_INDEX(case_node);
    const char *label = STMT_X_LABEL(case_node);
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

static node_t * simplify_decl(node_t *decl)
{
    cc_assert(isdecl(decl));
    
    node_t *sym = DECL_SYM(decl);
    int sclass = SYM_SCLASS(sym);

    if (!isvardecl(decl))
        return NULL;
    
    if (SYM_REFS(sym) == 0) {
        if (!SYM_PREDEFINE(sym))
            warningf(AST_SRC(sym),
                     "unused variable '%s'", SYM_NAME(sym));
        return NULL;
    } else {
        if (sclass == EXTERN) {
            return NULL;
        } else if (sclass == STATIC) {
            SYM_X_LABEL(sym) = gen_static_label();
            vec_push(staticvars, decl);
            return decl;
        } else {
            vec_push(localvars, decl);
            return decl;
        }
    }
}

static node_t ** simplify_decls(node_t **decls)
{
    struct vector *v = vec_new();
    for (int i = 0; i < LIST_LEN(decls); i++) {
        node_t *decl = decls[i];
        vec_push_safe(v, simplify_decl(decl));
    }
    return (node_t **)vtoa(v);
}

static node_t * simplify_stmt(node_t *stmt)
{
    cc_assert(isstmt(stmt));
    
    switch (AST_ID(stmt)) {
    case COMPOUND_STMT:
        {
            struct vector *v = vec_new();
            node_t **blks = STMT_BLKS(stmt);
            for (int i = 0; i < LIST_LEN(blks); i++) {
                node_t *n = blks[i];
                if (isdecl(n))
                    vec_push_safe(v, simplify_decl(n));
                else if (isstmt(n))
                    vec_push_safe(v, simplify_stmt(n));
                else
                    vec_push(v, n);
            }
            return ast_compound((node_t **)vtoa(v));
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
            const char *beg = gen_label();
	    const char *mid = gen_label();
	    const char *end = gen_label();
	    node_t **decl = STMT_FOR_DECL(stmt);
	    node_t *init = STMT_FOR_INIT(stmt);
	    node_t *cond = STMT_FOR_COND(stmt);
	    node_t *ctrl = STMT_FOR_CTRL(stmt);
            node_t *body = STMT_FOR_BODY(stmt);

            decl = simplify_decls(decl);

            SET_LOOP_CONTEXT(mid, end);
            body = simplify_stmt(body);
            RESTORE_LOOP_CONTEXT();

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
            return ast_compound((node_t **) vtoa(v));
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

            return ast_compound((node_t **)vtoa(v));
        }
    case CASE_STMT:
        {
            const char *label = gen_label();
            node_t *body = STMT_CASE_BODY(stmt);

            body = simplify_stmt(body);
            STMT_X_LABEL(stmt) = label;
            vec_push(CASES, stmt);

            struct vector *v = vec_new();
            vec_push(v, ast_label(label));
            vec_push(v, body);
            return ast_compound((node_t **)vtoa(v));
        }
    case DEFAULT_STMT:
        {
            const char *label = gen_label();
            node_t *body = STMT_CASE_BODY(stmt);

            body = simplify_stmt(body);
            DEFLT = label;

            struct vector *v = vec_new();
            vec_push(v, ast_label(label));
            vec_push(v, body);
            return ast_compound((node_t **)vtoa(v));
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
            const char *name = STMT_LABEL_NAME(stmt);
            node_t *ret = ast_jump(name);
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

static void check_control_flow(void)
{
    
}

static void backfill_labels(void)
{
    for (int i = 0; i < vec_len(gotos); i++) {
        node_t *goto_stmt = vec_at(gotos, i);
        const char *name = GEN_LABEL(goto_stmt);
        const char *label = map_get(labels, name);
        cc_assert(label);
        GEN_LABEL(goto_stmt) = label;
    }
}

static node_t * simplify_function(node_t *decl)
{
    node_t *stmt = DECL_BODY(decl);

    SET_FUNCDEF_CONTEXT();
    
    DECL_BODY(decl) = simplify_stmt(stmt);
    DECL_X_LVARS(decl) = (node_t **)vtoa(localvars);
    DECL_X_SVARS(decl) = (node_t **)vtoa(staticvars);
    backfill_labels();
    // check control flow and return stmt
    check_control_flow();

    RESTORE_FUNCDEF_CONTEXT();
    
    return decl;
}

static const char *glabel(const char *label)
{
    if (opts.fleading_underscore)
        return format("_%s", label);
    else
        return label;
}

static struct vector * filter_global(node_t **exts)
{
    struct vector *r = vec_new();
    struct map *map = map_new();
    map->cmpfn = nocmp;
    for (int i = 0; i < LIST_LEN(exts); i++) {
        node_t *decl = exts[i];
        if (isfuncdef(decl)) {
            vec_push(r, decl);
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

    struct vector *v = vec_new();
    struct vector *exts = filter_global(DECL_EXTS(tree));

    for (int i = 0; i < vec_len(exts); i++) {
        node_t *decl = vec_at(exts, i);
        node_t *sym = DECL_SYM(decl);

        if (SYM_SCLASS(sym) == STATIC && SYM_REFS(sym) == 0) {
            if (isfuncdef(decl))
                warningf(AST_SRC(sym), "unused function '%s'", SYM_NAME(sym));
            else if (isvardecl(decl))
                warningf(AST_SRC(sym), "unused variable '%s'", SYM_NAME(sym));
            continue;
        }
        
        if (isfuncdef(decl)) {
            node_t *node = simplify_function(decl);
            vec_push(v, node);
            vec_add_array(v, (void **)DECL_X_SVARS(decl));
        } else if (isvardecl(decl)) {
            vec_push(v, decl);
        }

        SYM_X_LABEL(sym) = glabel(SYM_NAME(sym));
    }

    DECL_EXTS(tree) = (node_t **)vtoa(v);
    
    return tree;
}

node_t * reduce(node_t *expr)
{
    return expr;
}
