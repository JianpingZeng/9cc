#include "cc.h"

static node_t *statement(void);
static node_t *compound_stmt(void (*) (void));
static struct vector * filter_decls(struct vector *decls);
static void warning_unused(void);

static node_t *__loop;
static node_t *__switch;
static struct vector *__cases;
static node_t *__default;
static node_t *__switch_ty;

#define SET_LOOP_CONTEXT(loop)                  \
    node_t *__saved_loop = __loop;              \
    __loop = loop

#define RESTORE_LOOP_CONTEXT()                  \
    __loop = __saved_loop

#define SET_SWITCH_CONTEXT(sw, ty)              \
    node_t *__saved_sw = __switch;              \
    struct vector *__saved_cases = __cases;     \
    node_t *__saved_default = __default;        \
    node_t *__saved_switch_ty = __switch_ty;    \
    __switch = sw;                              \
    __cases = vec_new();                        \
    __default = NULL;                           \
    __switch_ty = ty

#define RESTORE_SWITCH_CONTEXT()                \
    __switch = __saved_sw;                      \
    __cases = __saved_cases;                    \
    __default = __saved_default;                \
    __switch_ty = __saved_switch_ty

#define IN_LOOP       (__loop)
#define IN_SWITCH     (__switch)
#define CASES         (__cases)
#define DEFLT         (__default)
#define SWITCH_TYPE   (__switch_ty)

// funcdef context
struct vector *funcalls;
static struct vector *gotos;
static struct map *labels;
static node_t *functype;
static const char *funcname;
static struct vector *staticvars;
static struct vector *localvars;
static struct vector *allvars;

static node_t *expr_stmt(void)
{
    node_t *ret = NULL;

    if (token->id == ';') {
        ret = ast_stmt(NULL_STMT, source);
    } else if (first_expr(token)) {
        ret = ast_stmt(EXPR_STMT, source);
        STMT_EXPR_BODY(ret) = reduce(expression());
    } else {
        error("missing statement before '%s'", token->name);
    }
    
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
    node_t *ret = ast_stmt(IF_STMT, source);
    node_t *cond;
    node_t *thenpart;
    node_t *elsepart = NULL;

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
        STMT_COND(ret) = cond;
        STMT_THEN(ret) = thenpart;
        STMT_ELSE(ret) = elsepart;
    } else {
        ret = NULL;
    }

    return ret;
}

/**
 * Each iterative statement(do/while/for) forms its own block scope,
 * as do the substatements even if they are not compound statements.
 */
static node_t *while_stmt(void)
{
    node_t *ret = ast_stmt(WHILE_STMT, source);
    node_t *cond;
    node_t *body;

    enter_scope();

    SAVE_ERRORS;
    expect(WHILE);
    expect('(');
    cond = bool_expr();
    expect(')');

    SET_LOOP_CONTEXT(ret);
    body = statement();
    RESTORE_LOOP_CONTEXT();

    exit_scope();

    if (NO_ERROR) {
        STMT_WHILE_COND(ret) = cond;
        STMT_WHILE_BODY(ret) = body;
    } else {
        ret = NULL;
    }

    return ret;
}

static node_t *do_while_stmt(void)
{
    node_t *ret = ast_stmt(DO_WHILE_STMT, source);
    node_t *body;
    node_t *cond;

    enter_scope();

    SAVE_ERRORS;
    expect(DO);
    
    SET_LOOP_CONTEXT(ret);
    body = statement();
    RESTORE_LOOP_CONTEXT();

    expect(WHILE);
    expect('(');
    cond = bool_expr();
    expect(')');
    expect(';');

    exit_scope();

    if (NO_ERROR) {
        STMT_WHILE_COND(ret) = cond;
        STMT_WHILE_BODY(ret) = body;
    } else {
        ret = NULL;
    }

    return ret;
}

static node_t *for_stmt(void)
{
    node_t *ret = ast_stmt(FOR_STMT, source);
    node_t *body;

    enter_scope();

    SAVE_ERRORS;
    expect(FOR);
    expect('(');

    if (token->id == ';') {
        expect(';');
    } else {
        if (first_decl(token)) {
            // declaration
            STMT_FOR_DECL(ret) = filter_decls(declaration());
        } else {
            // expression
            STMT_FOR_INIT(ret) = expression();
            expect(';');
        }
    }

    if (token->id != ';')
        STMT_FOR_COND(ret) = bool_expr();

    expect(';');

    if (token->id != ')')
        STMT_FOR_CTRL(ret) = expression();

    expect(')');

    SET_LOOP_CONTEXT(ret);
    body = statement();
    RESTORE_LOOP_CONTEXT();

    exit_scope();

    if (NO_ERROR)
        STMT_FOR_BODY(ret) = body;
    else
        ret = NULL;

    return ret;
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
    node_t *ret = ast_stmt(SWITCH_STMT, source);
    node_t *expr;
    node_t *body;

    SAVE_ERRORS;
    expect(SWITCH);
    expect('(');
    expr = switch_expr();
    expect(')');

    SET_SWITCH_CONTEXT(ret, expr ? AST_TYPE(expr) : NULL);

    body = statement();

    if (NO_ERROR) {
        STMT_SWITCH_EXPR(ret) = expr;
        STMT_SWITCH_BODY(ret) = body;
        STMT_SWITCH_CASES(ret) = CASES;
        STMT_SWITCH_DEFAULT(ret) = DEFLT;
    } else {
        ret = NULL;
    }

    RESTORE_SWITCH_CONTEXT();
    
    return ret;
}

static void check_case_duplicates(node_t * node)
{
    for (int i = vec_len(CASES) - 1; i >= 0; i--) {
        node_t *n = vec_at(CASES, i);
        if (STMT_CASE_INDEX(n) == STMT_CASE_INDEX(node)) {
            errorf(AST_SRC(node),
                   "duplicate case value '%lld', "
                   "previous case defined here: %s:%u:%u",
                   STMT_CASE_INDEX(node),
                   AST_SRC(n).file,
                   AST_SRC(n).line,
                   AST_SRC(n).column);
            break;
        }
    }
}

static node_t *case_stmt(void)
{
    node_t *ret = ast_stmt(CASE_STMT, source);
    node_t *body;

    SAVE_ERRORS;
    expect(CASE);
    STMT_CASE_INDEX(ret) = intexpr1(SWITCH_TYPE);
    expect(':');

    if (!IN_SWITCH)
        errorf(AST_SRC(ret), "'case' statement not in switch statement");

    // only check when intexpr is okay.
    if (NO_ERROR) {
        check_case_duplicates(ret);
        vec_push(CASES, ret);
    }

    // always parse even if not in a switch statement
    body = statement();

    if (NO_ERROR)
        STMT_CASE_BODY(ret) = body;
    else
        ret = NULL;

    return ret;
}

static node_t *default_stmt(void)
{
    node_t *ret = ast_stmt(DEFAULT_STMT, source);
    node_t *stmt;

    SAVE_ERRORS;
    expect(DEFAULT);
    expect(':');

    // print before parsing statement
    if (!IN_SWITCH)
        errorf(AST_SRC(ret), "'default' statement not in switch statement");

    if (DEFLT)
        errorf(AST_SRC(ret),
               "multiple default labels in one switch, "
               "previous case defined here:%s:%u:%u",
               AST_SRC(DEFLT).file,
               AST_SRC(DEFLT).line,
               AST_SRC(DEFLT).column);

    DEFLT = ret;
    
    stmt = statement();

    if (NO_ERROR)
        STMT_CASE_BODY(ret) = stmt;
    else
        ret = NULL;

    return ret;
}

static node_t *label_stmt(void)
{
    node_t *ret = ast_stmt(LABEL_STMT, source);
    node_t *stmt;
    const char *name;

    SAVE_ERRORS;
    name = token->name;
    expect(ID);
    expect(':');

    // install label before parsing body
    if (NO_ERROR) {
        node_t *n = map_get(labels, name);
        if (n)
            errorf(AST_SRC(ret),
                   "redefinition of label '%s', "
                   "previous label defined here:%s:%u:%u",
                   name,
                   AST_SRC(n).file,
                   AST_SRC(n).line,
                   AST_SRC(n).column);
        map_put(labels, name, ret);
        STMT_LABEL_NAME(ret) = name;
    }

    stmt = statement();

    if (NO_ERROR) {
        STMT_LABEL_BODY(ret) = stmt;
        STMT_X_LABEL(ret) = gen_label();
    } else {
        ret = NULL;
    }

    return ret;
}

static node_t *goto_stmt(void)
{
    node_t *ret = ast_stmt(GOTO_STMT, source);

    SAVE_ERRORS;
    expect(GOTO);
    STMT_LABEL_NAME(ret) = token->name;
    expect(ID);
    expect(';');

    if (NO_ERROR)
        vec_push(gotos, ret);
    else
        ret = NULL;

    return ret;
}

static node_t *break_stmt(void)
{
    node_t *ret = ast_stmt(BREAK_STMT, source);

    SAVE_ERRORS;
    expect(BREAK);
    expect(';');

    if (!IN_LOOP && !IN_SWITCH)
        errorf(AST_SRC(ret),
               "'break' statement not in loop or switch statement");

    if (!NO_ERROR)
        ret = NULL;

    return ret;
}

static node_t *continue_stmt(void)
{
    node_t *ret = ast_stmt(CONTINUE_STMT, source);

    SAVE_ERRORS;
    expect(CONTINUE);
    expect(';');

    if (!IN_LOOP)
        errorf(AST_SRC(ret),
               "'continue' statement not in loop statement");

    if (!NO_ERROR)
        ret = NULL;

    return ret;
}

static node_t *ensure_return(node_t * expr, struct source src)
{
    // return immediately if expr is NULL. (parsing failed)
    if (expr == NULL)
        return NULL;

    if (isvoid(rtype(functype))) {
        if (!isnullstmt(expr) && !isvoid(AST_TYPE(expr)))
            errorf(src, "void function should not return a value");
    } else {
        if (!isnullstmt(expr)) {
            node_t *ty1 = AST_TYPE(expr);
            node_t *ty2 = rtype(functype);
            if (!(expr = assignconv(ty2, expr)))
                errorf(src,
                       "returning '%s' from function "
                       "with incompatible result type '%s'",
                       type2s(ty1), type2s(ty2));
        } else {
            errorf(src, "non-void function should return a value");
        }
    }
    return expr;
}

static node_t *return_stmt(void)
{
    node_t *ret = ast_stmt(RETURN_STMT, source);
    node_t *expr;

    SAVE_ERRORS;
    expect(RETURN);
    node_t *stmt = expr_stmt();
    expr = STMT_EXPR_BODY(stmt);
    expr = ensure_return(expr, AST_SRC(ret));

    if (NO_ERROR)
        STMT_RETURN_EXPR(ret) = expr;
    else
        ret = NULL;

    return ret;
}

static node_t *statement(void)
{
    switch (token->id) {
    case '{':
        return compound_stmt(NULL);
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

static node_t *compound_stmt(void (*enter_hook) (void))
{
    node_t *ret = ast_stmt(COMPOUND_STMT, source);
    struct vector *v = vec_new();

    expect('{');
    enter_scope();

    if (enter_hook)
        enter_hook();

    while (first_decl(token) || first_expr(token) || first_stmt(token)) {
        if (first_decl(token))
            // declaration
            vec_add(v, filter_decls(declaration()));
        else
            // statement
            vec_push_safe(v, statement());
    }

    STMT_BLKS(ret) = v;

    expect('}');
    exit_scope();

    return ret;
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
        const char *name = "__func__";
        node_t *type = array_type(qual(CONST, chartype));
        node_t *decl = make_localvar(name, type, STATIC);
        SYM_PREDEFINE(DECL_SYM(decl)) = true;
        // initializer
        node_t *literal = new_string_literal(funcname);
        init_string(type, literal);
        DECL_BODY(decl) = literal;
    }
}

static void backfill_labels(void)
{
    for (int i = 0; i < vec_len(gotos); i++) {
        node_t *goto_stmt = vec_at(gotos, i);
        const char *name = STMT_LABEL_NAME(goto_stmt);
        node_t *label_stmt = map_get(labels, name);
        if (label_stmt) {
            STMT_X_LABEL(goto_stmt) = STMT_X_LABEL(label_stmt);
            // update refs
            STMT_LABEL_REFS(label_stmt)++;
        } else {
            errorf(AST_SRC(goto_stmt), "use of undeclared label '%s'", name);
        }
    }
}

static void set_funcdef_context(node_t *fty, const char *name)
{
    gotos = vec_new();
    labels = map_new();
    functype = fty;
    funcname = name;
    staticvars = vec_new();
    localvars = vec_new();
    allvars = vec_new();
    funcalls = vec_new();
}

static void restore_funcdef_context(void)
{
    gotos = NULL;
    labels = NULL;
    functype = NULL;
    funcname = NULL;
    staticvars = NULL;
    localvars = NULL;
    allvars = NULL;
    funcalls = NULL;
}

void func_body(node_t *decl)
{
    node_t *sym = DECL_SYM(decl);
    
    set_funcdef_context(SYM_TYPE(sym), SYM_NAME(sym));
    
    node_t *stmt = compound_stmt(predefined_ids);
    // check goto labels
    backfill_labels();
    // check unused
    warning_unused();

    // save
    DECL_X_LVARS(decl) = localvars;
    DECL_X_SVARS(decl) = staticvars;
    DECL_X_CALLS(decl) = funcalls;
    
    restore_funcdef_context();

    DECL_BODY(decl) = stmt;
}

static struct vector * filter_decls(struct vector *decls)
{
    for (int i = 0; i < vec_len(decls); i++) {
        node_t *decl = vec_at(decls, i);
        node_t *sym = DECL_SYM(decl);

        if (!isvardecl(decl) || SYM_SCLASS(sym) == EXTERN)
            continue;

        vec_push(allvars, decl);
    }
    return decls;
}

static void warning_unused(void)
{
    for (int i = 0; i < vec_len(allvars); i++) {
        node_t *decl = vec_at(allvars, i);
        node_t *sym = DECL_SYM(decl);

        // ONLY warning, not filter out
        // because it may contains side-effect such as function calls.
        if (SYM_REFS(sym) == 0) {
            if (SYM_PREDEFINE(sym))
                continue;       // filter-out predefined symbols
            else
                warningf(AST_SRC(sym), "unused variable '%s'", SYM_NAME(sym));
        }
        if (SYM_SCLASS(sym) == STATIC) {
            SYM_X_LABEL(sym) = gen_static_label();
            vec_push(staticvars, decl);
        } else {
            vec_push(localvars, decl);
        }
    }
}

node_t *make_localvar(const char *name, node_t * ty, int sclass)
{
    node_t *decl = make_localdecl(name, ty, sclass);
    vec_push(allvars, decl);
    return decl;
}
