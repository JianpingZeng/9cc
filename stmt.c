#include "cc.h"

static node_t *statement(void);

static node_t *__loop;
static node_t *__switch;
static struct vector *__cases;
static node_t *__default;
static struct type *__switch_ty;

#define SET_LOOP_CONTEXT(loop)                  \
    node_t *__saved_loop = __loop;              \
    __loop = loop

#define RESTORE_LOOP_CONTEXT()                  \
    __loop = __saved_loop

#define SET_SWITCH_CONTEXT(sw, ty)                      \
    node_t *__saved_sw = __switch;                      \
    struct vector *__saved_cases = __cases;             \
    node_t *__saved_default = __default;                \
    struct type *__saved_switch_ty = __switch_ty;       \
    __switch = sw;                                      \
    __cases = vec_new();                                \
    __default = NULL;                                   \
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


/// expression-statement:
///   expression[opt] ';'
///
static node_t *expr_stmt(void)
{
    node_t *ret = NULL;

    if (token->id == ';') {
        ret = ast_stmt(NULL_STMT, source);
    } else if (first_expr(token)) {
        ret = ast_stmt(EXPR_STMT, source);
        node_t *expr = expression();
        if (expr)
            STMT_EXPR_BODY(ret) = expr;
        else
            ret = NULL;
    } else {
        error("missing statement before '%s'", tok2s(token));
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
/// selection-statement:
///   'if' '(' expression ')' statement
///   'if' '(' expression ')' statement 'else' statement
///
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
/// iteration-statement:
///   'while' '(' expression ')' statement
///
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

/// iteration-statement:
///   'do' statement 'while' '(' expression ')' ';'
///
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

/// iteration-statement:
///   'for' '(' expression[opt] ';' expression[opt] ';' expression[opt] ')' statement
///   'for' '(' declaration expression[opt] ';' expression[opt] ')' statement
///
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
            STMT_FOR_INIT(ret) = decls2expr(declaration());
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
/// selection-statement:
///   'switch' '(' expression ')' statement
///
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
        STMT_SWITCH_CASES(ret) = vtoa(CASES, PERM);
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
            error_at(AST_SRC(node),
                     "duplicate case value '%lld', previous case defined here: %s:%u:%u",
                     STMT_CASE_INDEX(node),
                     AST_SRC(n).file,
                     AST_SRC(n).line,
                     AST_SRC(n).column);
            break;
        }
    }
}

/// labeled-statement:
///   'case' constant-expression ':' statement
///
static node_t *case_stmt(void)
{
    node_t *ret = ast_stmt(CASE_STMT, source);
    node_t *body;

    SAVE_ERRORS;
    expect(CASE);
    STMT_CASE_INDEX(ret) = intexpr1(SWITCH_TYPE);
    expect(':');

    if (!IN_SWITCH)
        error_at(AST_SRC(ret), "'case' statement not in switch statement");

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

/// labeled-statement:
///   'default' ':' statement
///
static node_t *default_stmt(void)
{
    node_t *ret = ast_stmt(DEFAULT_STMT, source);
    node_t *stmt;

    SAVE_ERRORS;
    expect(DEFAULT);
    expect(':');

    // print before parsing statement
    if (!IN_SWITCH)
        error_at(AST_SRC(ret), "'default' statement not in switch statement");

    if (DEFLT)
        error_at(AST_SRC(ret),
                 "multiple default labels in one switch, previous case defined here:%s:%u:%u",
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

/// labled-statement:
///   identifier ':' statement
///
static node_t *label_stmt(void)
{
    node_t *ret = ast_stmt(LABEL_STMT, source);
    node_t *stmt;
    const char *name;

    SAVE_ERRORS;
    if (token->id == ID)
        name = TOK_ID_STR(token);
    expect(ID);
    expect(':');

    // install label before parsing body
    if (NO_ERROR) {
        node_t *n = map_get(funcinfo.labels, name);
        if (n)
            error_at(AST_SRC(ret),
                     "redefinition of label '%s', previous label defined here:%s:%u:%u",
                     name,
                     AST_SRC(n).file,
                     AST_SRC(n).line,
                     AST_SRC(n).column);
        map_put(funcinfo.labels, name, ret);
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

/// jump-statement:
///   'goto' identifier ';'
///
static node_t *goto_stmt(void)
{
    node_t *ret = ast_stmt(GOTO_STMT, source);

    SAVE_ERRORS;
    expect(GOTO);
    if (token->id == ID)
        STMT_LABEL_NAME(ret) = TOK_ID_STR(token);
    expect(ID);
    expect(';');

    if (NO_ERROR)
        vec_push(funcinfo.gotos, ret);
    else
        ret = NULL;

    return ret;
}

/// jump-statement:
///   'break' ';'
///
static node_t *break_stmt(void)
{
    node_t *ret = ast_stmt(BREAK_STMT, source);

    SAVE_ERRORS;
    expect(BREAK);
    expect(';');

    if (!IN_LOOP && !IN_SWITCH)
        error_at(AST_SRC(ret),
                 "'break' statement not in loop or switch statement");

    if (!NO_ERROR)
        ret = NULL;

    return ret;
}

/// jump-statement:
///   'continue' ';'
///
static node_t *continue_stmt(void)
{
    node_t *ret = ast_stmt(CONTINUE_STMT, source);

    SAVE_ERRORS;
    expect(CONTINUE);
    expect(';');

    if (!IN_LOOP)
        error_at(AST_SRC(ret),
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

    if (isvoid(rtype(funcinfo.type))) {
        if (!isnullstmt(expr) && !isvoid(AST_TYPE(expr)))
            error_at(src, "void function should not return a value");
    } else {
        if (!isnullstmt(expr)) {
            struct type *ty1 = AST_TYPE(expr);
            struct type *ty2 = rtype(funcinfo.type);
            if (!(expr = assignconv(ty2, expr)))
                error_at(src,
                         "returning '%s' from function with incompatible result type '%s'",
                         type2s(ty1), type2s(ty2));
        } else {
            error_at(src, "non-void function should return a value");
        }
    }
    return expr;
}

/// jump-statement:
///   'return' expression[opt] ';'
///
static node_t *return_stmt(void)
{
    node_t *ret = ast_stmt(RETURN_STMT, source);
    node_t *expr;

    SAVE_ERRORS;
    expect(RETURN);
    node_t *stmt = expr_stmt();
    if (stmt) {
        expr = STMT_EXPR_BODY(stmt);
        expr = ensure_return(expr, AST_SRC(ret));
    }

    if (NO_ERROR)
        STMT_RETURN_EXPR(ret) = expr;
    else
        ret = NULL;

    return ret;
}

/// statement:
///   labeled-statement
///   compound-statement
///   expression-statement
///   selection-statement
///   iteration-statement
///   jump-statement
///
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

/// compound-statement:
///   '{' block-item-list[opt] '}'
///
/// block-item-list:
///   block-item
///   block-item-list block-item
///
/// block-item:
///   declaration
///   statement
///
node_t *compound_stmt(void (*enter_hook) (void))
{
    node_t *ret = ast_stmt(COMPOUND_STMT, source);
    struct list *list = NULL;

    expect('{');
    enter_scope();

    if (enter_hook)
        enter_hook();

    while (first_decl(token) || first_expr(token) || first_stmt(token)) {
        if (first_decl(token)) {
            // declaration
            node_t *expr = decls2expr(declaration());
            if (expr) {
                node_t *stmt = ast_stmt(EXPR_STMT, AST_SRC(expr));
                STMT_EXPR_BODY(stmt) = expr;
                list = list_append(list, stmt);
            }
        } else {
            // statement
            node_t *stmt = statement();
            if (stmt)
                list = list_append(list, stmt);
        }
    }

    STMT_BLKS(ret) = ltoa(&list, FUNC);

    expect('}');
    exit_scope();

    return ret;
}
