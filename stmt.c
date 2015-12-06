#include "cc.h"

static node_t *statement(void);
static node_t *compound_stmt(void (*) (void), void (*) (void));
static void predefined_ids(void);
static void filter_local(void);

static const char *__continue;
static const char *__break;
static struct vector *__cases;
static node_t *__default;
static node_t *__switch_ty;
static struct vector *__compound_vector;

size_t extra_stack_size;
static struct vector *gotos;
static struct map *labels;
static node_t *functype;
static const char *funcname;
static struct vector *localvars;
static struct vector *staticvars;

#define SET_FUNCDEF_CONTEXT(fty, id)            \
    gotos = vec_new();                          \
    labels = map_new();                         \
    functype = fty;                             \
    funcname = id;                              \
    localvars = vec_new();                      \
    staticvars = vec_new();                     \
    extra_stack_size = 0

#define RESTORE_FUNCDEF_CONTEXT()               \
    vec_free(gotos);                            \
    gotos = NULL;                               \
    map_free(labels);                           \
    labels = NULL;                              \
    functype = NULL;                            \
    funcname = NULL;                            \
    vec_free(localvars);                        \
    localvars = NULL;                           \
    vec_free(staticvars);                       \
    staticvars = NULL;                          \
    extra_stack_size = 0

#define SET_COMPOUND_CONTEXT(v)                         \
    struct vector *__saved_vector = __compound_vector;  \
    __compound_vector = v

#define RESTORE_COMPOUND_CONTEXT()              \
    __compound_vector = __saved_vector

#define SET_LOOP_CONTEXT(cont, brk)             \
    const char *__saved_continue = __continue;  \
    const char *__saved_break = __break;        \
    __continue = cont;                          \
    __break = brk;                              \
    enter_scope()

#define RESTORE_LOOP_CONTEXT()                  \
    __continue = __saved_continue;              \
    __break = __saved_break;                    \
    exit_scope()

#define SET_SWITCH_CONTEXT(brk, ty)             \
    const char *__saved_break = __break;        \
    struct vector *__saved_cases = __cases;     \
    node_t *__saved_default = __default;        \
    node_t *__saved_switch_ty = __switch_ty;    \
    __break = brk;                              \
    __cases = vec_new();                        \
    __default = NULL;                           \
    __switch_ty = ty

#define RESTORE_SWITCH_CONTEXT()                \
    vec_free(__cases);                          \
    __break = __saved_break;                    \
    __cases = __saved_cases;                    \
    __default = __saved_default;                \
    __switch_ty = __saved_switch_ty

#define COMPOUND_VECTOR   (__compound_vector)
#define CONTINUE_CONTEXT  (__continue)
#define BREAK_CONTEXT     (__break)
#define IN_SWITCH         (__cases)
#define CASES             (__cases)
#define DEFLT             (__default)
#define SWITCH_TYPE       (__switch_ty)

static node_t *expr_stmt(void)
{
    node_t *ret = NULL;

    if (token->id == ';')
        ret = ast_null_stmt();
    else if (first_expr(token))
        ret = reduce(expression());
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
static node_t *if_stmt(void)
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
static node_t *while_stmt(void)
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
        ret = ast_compound(src, (node_t **) vtoa(v));
    }

    return ret;
}

static node_t *do_while_stmt(void)
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
        ret = ast_compound(src, (node_t **) vtoa(v));
    }

    return ret;
}

static node_t *for_stmt(void)
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
        if (first_decl(token)) {
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
        ret = ast_compound(src, (node_t **) vtoa(v));
    }

    return ret;
}

static node_t *switch_jmp(node_t * var, node_t * case_node)
{
    node_t *cond;
    long index = STMT_INDEX(case_node);
    const char *label = STMT_LABEL(case_node);
    node_t *index_node = int_literal_node(AST_TYPE(var), (union value){
            .u = index });

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

/**
 * Switch Statements Notes:
 *
 * 1. The control expression is subject to the usual unary convresion.
 *
 * 2. When comparing the control expression and the **case** expressions,
 *    the **case** expressions are converted to the type of the control
 *    expression (after the usual unary conversion).
 */
static node_t *switch_stmt(void)
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
        node_t *var = define_tmpvar(AST_TYPE(expr));
        vec_push(v, ast_bop('=', AST_TYPE(expr), var, expr));
        for (int i = 0; i < vec_len(CASES); i++) {
            node_t *case_node = vec_at(CASES, i);
            vec_push(v, switch_jmp(var, case_node));
        }
        const char *label = DEFLT ? STMT_LABEL(DEFLT) : end;
        vec_push(v, ast_jump(label));
        vec_push(v, body);
        vec_push(v, ast_label(end));
        ret = ast_compound(src, (node_t **) vtoa(v));
    }

    RESTORE_SWITCH_CONTEXT();

    return ret;
}

static void check_case_duplicates(node_t * node)
{
    for (int i = vec_len(CASES) - 1; i >= 0; i--) {
        node_t *n = vec_at(CASES, i);
        if (STMT_INDEX(n) == STMT_INDEX(node)) {
            errorf(AST_SRC(node),
                   "duplicate case value '%lld', "
                   "previous case defined here: %s:%u:%u",
                   STMT_INDEX(node), AST_SRC(n).file,
                   AST_SRC(n).line, AST_SRC(n).column);
            break;
        }
    }
}

static node_t *case_stmt(void)
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
        STMT_LIST(ret) = (node_t **) vtoa(v);
        STMT_LABEL(ret) = label;
    } else {
        ret = NULL;
    }

    return ret;
}

static node_t *default_stmt(void)
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
               "multiple default labels in one switch, "
               "previous case defined here:%s:%u:%u",
               AST_SRC(DEFLT).file, AST_SRC(DEFLT).line,
               AST_SRC(DEFLT).column);

    DEFLT = ret;
    body = statement();

    if (NO_ERROR) {
        const char *label = gen_label();
        struct vector *v = vec_new();
        vec_push(v, ast_label(label));
        vec_push(v, body);
        STMT_LIST(ret) = (node_t **) vtoa(v);
        STMT_LABEL(ret) = label;
    } else {
        ret = NULL;
    }

    return ret;
}

static node_t *label_stmt(void)
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
        node_t *n = map_get(labels, name);
        if (n)
            errorf(src,
                   "redefinition of label '%s', "
                   "previous label defined here:%s:%u:%u",
                   name, AST_SRC(n).file, AST_SRC(n).line,
                   AST_SRC(n).column);
        map_put(labels, name, ret);
    }

    body = statement();

    if (NO_ERROR) {
        struct vector *v = vec_new();
        vec_push(v, ast_label(label));
        vec_push(v, body);
        STMT_LIST(ret) = (node_t **) vtoa(v);
        STMT_LABEL(ret) = label;
    } else {
        ret = NULL;
    }

    return ret;
}

static node_t *goto_stmt(void)
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
        vec_push(gotos, ret);
        AST_SRC(ret) = src;
    }

    return ret;
}

static node_t *break_stmt(void)
{
    node_t *ret = NULL;
    struct source src = source;

    SAVE_ERRORS;
    expect(BREAK);
    expect(';');

    if (!BREAK_CONTEXT)
        errorf(src,
               "'break' statement not in loop or switch statement");

    if (NO_ERROR) {
        ret = ast_jump(BREAK_CONTEXT);
        AST_SRC(ret) = src;
    }

    return ret;
}

static node_t *continue_stmt(void)
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

static node_t *ensure_return(node_t * expr, struct source src)
{
    // return immediately if expr is NULL. (parsing failed)
    if (expr == NULL)
        return NULL;

    if (isvoid(rtype(functype))) {
        if (!isnullstmt(expr) && !isvoid(AST_TYPE(expr)))
            errorf(src,
                   "void function '%s' should not return a value",
                   funcname);
    } else {
        if (!isnullstmt(expr)) {
            node_t *ty1 = AST_TYPE(expr);
            node_t *ty2 = rtype(functype);
            if (!(expr = assignconv(ty2, expr)))
                errorf(src,
                       "returning '%s' from function '%s' "
                       "with incompatible result type '%s'",
                       type2s(ty1), funcname, type2s(ty2));
        } else {
            errorf(src,
                   "non-void function '%s' should return a value",
                   funcname);
        }
    }
    return expr;
}

static node_t *return_stmt(void)
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

static node_t *statement(void)
{
    switch (token->id) {
    case '{':
        return compound_stmt(NULL, NULL);
    case IF:
        return if_stmt();
    case SWITCH:
        return switch_stmt();
    case WHILE:
        return while_stmt();
    case DO:
        return do_while_stmt();
    case FOR:
        return for_stmt();
    case GOTO:
        return goto_stmt();
    case CONTINUE:
        return continue_stmt();
    case BREAK:
        return break_stmt();
    case RETURN:
        return return_stmt();
    case CASE:
        return case_stmt();
    case DEFAULT:
        return default_stmt();
    case ID:
        if (lookahead()->id == ':')
            return label_stmt();
        // go through
    default:
        return expr_stmt();
    }
}

static node_t *compound_stmt(void (*enter_hook) (void),
                             void (*exit_hook) (void))
{
    struct source src = source;
    struct vector *v = vec_new();

    expect('{');
    enter_scope();

    SET_COMPOUND_CONTEXT(v);

    if (enter_hook)
        enter_hook();

    while (first_decl(token) || first_expr(token) || first_stmt(token)) {
        if (first_decl(token))
            // declaration
            vec_add_array(v, (void **)declaration());
        else
            // statement
            vec_push_safe(v, statement());
    }

    if (exit_hook)
        exit_hook();

    RESTORE_COMPOUND_CONTEXT();

    expect('}');
    exit_scope();

    return ast_compound(src, (node_t **) vtoa(v));
}

static void backfill_labels(void)
{
    for (int i = 0; i < vec_len(gotos); i++) {
        node_t *goto_stmt = vec_at(gotos, i);
        const char *name = STMT_LABEL(goto_stmt);
        node_t *label_stmt = map_get(labels, name);
        if (label_stmt)
            STMT_LABEL(goto_stmt) = STMT_LABEL(label_stmt);
        else
            errorf(AST_SRC(goto_stmt),
                   "use of undeclared label '%s'", name);
    }
}

void func_body(node_t *decl)
{
    node_t *sym = DECL_SYM(decl);
    
    SET_FUNCDEF_CONTEXT(SYM_TYPE(sym), SYM_NAME(sym));
    
    node_t *stmt = compound_stmt(predefined_ids, filter_local);
    DECL_BODY(decl) = stmt;
    // check goto labels
    backfill_labels();
    // TODO: check control flow and return stmt

    DECL_X(decl).lvars = (node_t **)vtoa(localvars);
    DECL_X(decl).svars = (node_t **)vtoa(staticvars);
    
    RESTORE_FUNCDEF_CONTEXT();
}

node_t *make_localvar(const char *name, node_t * ty, int sclass)
{
    node_t *decl = make_localdecl(name, ty, sclass);
    vec_push(COMPOUND_VECTOR, decl);
    return decl;
}

static void predefined_ids(void)
{
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
        node_t *decl = make_localvar(name, type, STATIC);
        SYM_PREDEFINE(DECL_SYM(decl)) = true;
        // initializer
        node_t *literal = new_string_literal(funcname);
        AST_SRC(literal) = source;
        init_string(type, literal);
        DECL_BODY(decl) = literal;
    }
}

static void filter_local(void)
{
    struct vector *v = vec_new();
    for (int i = 0; i < vec_len(COMPOUND_VECTOR); i++) {
        node_t *n = vec_at(COMPOUND_VECTOR, i);
        if (isdecl(n)) {
            if (isvardecl(n)) {
                node_t *sym = DECL_SYM(n);
                int sclass = SYM_SCLASS(sym);
                if (SYM_REFS(sym)) {
                    if (sclass == STATIC) {
                        vec_push(v, n);
                        vec_push(staticvars, n);
                    } else if (sclass != EXTERN) {
                        vec_push(v, n);
                        vec_push(localvars, n);
                    }
                } else {
                    if (!SYM_PREDEFINE(sym))
                        warningf(AST_SRC(sym),
                                 "unused variable '%s'",
                                 SYM_NAME(sym));
                }
            }
        } else {
            vec_push(v, n);
        }
    }
}
