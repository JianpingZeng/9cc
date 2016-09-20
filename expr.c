#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include "cc.h"

static struct expr *cast_expr(void);
static struct expr *cond_expr(void);
static struct expr *cond_expr1(struct expr *o);
static struct expr *unary_expr(void);
static struct expr *logicop(int op, struct expr *l, struct expr *r);
static struct expr *commaop(int op, struct expr *l, struct expr *r);
static struct expr *assignop(int op, struct expr *l, struct expr *r);
static bool is_nullptr(struct expr *node);

#define INTEGER_MAX(type)    (TYPE_LIMITS(type).max.i)
#define UINTEGER_MAX(type)   (TYPE_LIMITS(type).max.u)

static void ensure_type(struct expr *node, bool(*is) (struct type *))
{
    const char *name;
    if (is == isint)
        name = "integer";
    else if (is == isscalar)
        name = "scalar";
    else if (is == isarith)
        name = "arithmetic";
    else if (is == isrecord)
        name = "struct or union";
    else if (is == isfunc)
        name = "function";
    else if (is == isptr)
        name = "pointer";
    else
        assert(0);

    if (!is(EXPR_TYPE(node)))
        error_at(EXPR_SRC(node), "expect type '%s', not '%s'", name, type2s(EXPR_TYPE(node)));
}

/**
 * Object,Lvalue,Designator
 *
 * An _object_ is a region of memory that can be examined and stored into.
 *
 * An _lvalue_ is an expression that refers to an _object_ in such a way
 * that the object may be examined or altered.
 *
 * Only an _lvalue_ expression **may be** used on the left-hand side of an
 * assignment.
 *
 * An _lvalue_ dose **NOT** necessarily permit modification of the _object_
 * it designates.
 *
 * A _function_ designator is a value of function type. It is neither an
 * _object_ nor an _lvalue_.
 *
 * Functions and objects are often treated **differently** in C.
 */
bool islvalue(struct expr *node)
{
    if (EXPR_ID(node) == STRING_LITERAL)
        return true;
    if (EXPR_ID(node) == PAREN_EXPR)
        return islvalue(EXPR_OPERAND(node, 0));
    if (EXPR_ID(node) == SUBSCRIPT_EXPR)
        return true;
    if (EXPR_ID(node) == UNARY_OPERATOR && EXPR_OP(node) == '*') {
        if (isfunc(EXPR_TYPE(node)))
            return false;
        return true;
    }
    if (EXPR_ID(node) == MEMBER_EXPR)
        return EXPR_OP(node) ==
            DEREF ? true : islvalue(EXPR_OPERAND(node, 0));
    if (EXPR_ID(node) == COMPOUND_LITERAL)
        return true;
    if (EXPR_ID(node) == REF_EXPR) {
        if (EXPR_OP(node) == ENUM)
            return false;
        if (isfunc(EXPR_TYPE(node)))
            return false;
        return true;
    }

    return false;
}

static void ensure_lvalue(struct expr *node)
{
    if (!islvalue(node))
        error_at(EXPR_SRC(node), "expect lvalue at '%s'", expr2s(node));
}

static void ensure_assignable(struct expr *node)
{
    struct type *ty = EXPR_TYPE(node);
    struct source src = EXPR_SRC(node);
    if (!islvalue(node))
        error_at(src, "expression is not assignable '%s'", expr2s(node));
    else if (EXPR_ID(node) == PAREN_EXPR)
        ensure_assignable(EXPR_OPERAND(node, 0));
    else if (isarray(ty))
        error_at(src, "array type '%s' is not assignable", type2s(ty));
    else if (isconst(ty))
        error_at(src, "read-only variable '%s' is not assignable", expr2s(node));
}

static bool is_bitfield(struct expr *node)
{
    if (EXPR_ID(node) != MEMBER_EXPR)
        return false;

    struct type *ty = EXPR_TYPE(EXPR_OPERAND(node, 0));
    if (isptr(ty))
        ty = rtype(ty);
    const char *name = EXPR_NAME(node);
    struct field *field = find_field(ty, name);
    return field && FIELD_ISBIT(field);
}

static const char * castname(struct type *ty, struct expr *l)
{
    if (isfloat(ty) && isfloat(EXPR_TYPE(l)))
	return FloatCast;
    else if (isfloat(ty) && isint(EXPR_TYPE(l)))
        return IntegerToFloatCast;
    else if (isint(ty) && isint(EXPR_TYPE(l)))
	return IntegralCast;
    else if (isint(ty) && isfloat(EXPR_TYPE(l)))
	return FloatToIntegerCast;
    else
	return BitCast;
}

static struct expr *wrap(struct type *ty, struct expr *node)
{
    assert(isarith(ty));
    assert(isarith(EXPR_TYPE(node)));

    if (eqarith(ty, EXPR_TYPE(node)))
        return node;
    else
        return ast_conv(ty, node, castname(ty, node));
}

static struct expr *bitconv(struct type *ty, struct expr *node)
{
    if (eqtype(ty, EXPR_TYPE(node)))
        return node;
    else
        return ast_conv(ty, node, castname(ty, node));
}

static struct expr *decay(struct expr *node)
{
    assert(node);
    switch (TYPE_KIND(EXPR_TYPE(node))) {
    case FUNCTION:
        // FunctionToPointerDecay
        return ast_conv(ptr_type(EXPR_TYPE(node)), node, FunctionToPointerDecay);

    case ARRAY:
        // ArrayToPointerDecay
        return ast_conv(ptr_type(rtype(EXPR_TYPE(node))), node, ArrayToPointerDecay);

    default:
        return node;
    }
}

static struct expr *ltor(struct expr *node)
{
    // LValueToRValue
    return ast_conv(unqual(EXPR_TYPE(node)), node, LValueToRValue);
}

// Universal Unary Conversion
static struct expr *conv(struct expr *node)
{
    if (node == NULL)
        return NULL;
    if (islvalue(node))
        node = ltor(node);

    switch (TYPE_KIND(EXPR_TYPE(node))) {
    case _BOOL:
    case CHAR:
    case SHORT:
        return ast_conv(inttype, node, IntegralCast);

    case ENUM:
        return ast_conv(rtype(EXPR_TYPE(node)), node, IntegralCast);

    case FUNCTION:
    case ARRAY:
        return decay(node);

    default:
        return node;
    }
}

// Default function argument conversion
static struct expr *conva(struct expr *node)
{
    if (node == NULL)
        return NULL;
    if (islvalue(node))
        node = ltor(node);

    switch (TYPE_KIND(EXPR_TYPE(node))) {
    case FLOAT:
        return ast_conv(doubletype, node, FloatCast);

    default:
        return conv(node);
    }
}

// Universal Binary Conversion
static struct type *conv2(struct type * l, struct type * r)
{
    assert(isarith(l));
    assert(isarith(r));

    assert(TYPE_SIZE(l) >= TYPE_SIZE(inttype));
    assert(TYPE_SIZE(r) >= TYPE_SIZE(inttype));

    struct type *max = TYPE_RANK(l) > TYPE_RANK(r) ? l : r;
    if (isfloat(l) || isfloat(r) || TYPE_OP(l) == TYPE_OP(r))
        return max;

    struct type *u = TYPE_OP(l) == UNSIGNED ? l : r;
    struct type *s = TYPE_OP(l) == INT ? l : r;
    assert(unqual(s) == s);

    if (TYPE_RANK(u) >= TYPE_RANK(s))
        return u;

    if (TYPE_SIZE(u) < TYPE_SIZE(s)) {
        return s;
    } else {
        if (s == inttype)
            return unsignedinttype;
        else if (s == longtype)
            return unsignedlongtype;
        else
            return unsignedlonglongtype;
    }

    return l;
}

/**
 *  Assignment Conversions
 *
 *  Left side type              Permitted right side type
 *  ------------------------------------------------------
 *  any arith                   any arith
 *
 *  _Bool                       any pointer
 *
 *  struct or union             compatible struct or union
 *
 *  (void *)                    (a) the constant 0
 *                              (b) pointer to (object) T
 *                              (c) (void *)
 *
 *  pointer to (object) T       (a) the constant 0
 *                              (b) pointer to T2, where
 *                                  T and T2 are compatible
 *
 *  pointer to (function) F     (a) the constant 0
 *                              (b) pointer to F2, where
 *                                  F and F2 are compatible
 */

struct expr *assignconv(struct type *ty, struct expr *node)
{
    struct type *ty2;

    if (isfunc(EXPR_TYPE(node)) || isarray(EXPR_TYPE(node)))
        node = decay(node);
    if (islvalue(node))
        node = ltor(node);

    ty2 = EXPR_TYPE(node);

    if (isarith(ty) && isarith(ty2)) {
        return wrap(ty, node);
    } else if (isbool(ty) && isptr(ty2)) {
        return ast_conv(ty, node, PointerToBoolean);
    } else if ((isstruct(ty) && isstruct(ty2)) ||
               (isunion(ty) && isunion(ty2))) {
        if (eqtype(unqual(ty), unqual(ty2)))
            return bitconv(ty, node);
    } else if (isptr(ty) && isptr(ty2)) {
        if (is_nullptr(node)) {
            // always allowed
        } else if (isptrto(ty, VOID) || isptrto(ty2, VOID)) {
            struct type *vty = isptrto(ty, VOID) ? ty : ty2;
            struct type *tty = vty == ty ? ty2 : ty;
            if (isptrto(tty, FUNCTION)) {
                return NULL;
            } else {
                struct type *rty1 = rtype(ty);
                struct type *rty2 = rtype(ty2);
                if (!qual_contains(rty1, rty2))
                    return NULL;
            }
        } else {
            struct type *rty1 = rtype(ty);
            struct type *rty2 = rtype(ty2);
            if (eqtype(unqual(rty1), unqual(rty2))) {
                if (!qual_contains(rty1, rty2))
                    return NULL;
            } else {
                return NULL;
            }
        }
        return bitconv(ty, node);
    } else if (isptr(ty) && isint(ty2)) {
        if (is_nullptr(node))
            return bitconv(ty, node);
    }
    return NULL;
}

/**
 *  Explicit Casting Conversions
 *
 *  Destination type            Permitted source type
 *  --------------------------------------------------
 *  any arith                   any arith
 *
 *  any integer                 any pointer
 *
 *  pointer to (object) T, or   (a) any integer type
 *  (void *)                    (b) (void *)
 *                              (c) pointer to (object) Q, for any Q
 *
 *  pointer to (function) T     (a) any integer type
 *                              (b) pointer to (function) Q, for any Q
 *
 *  struct or union             none; not a permitted cast
 *
 *  array or function           none; not a permitted cast
 *
 *  void                        any type
 */

static bool is_castable(struct type * dst, struct type * src)
{
    if (isvoid(dst))
        return true;
    if (isarith(dst) && isarith(src))
        return true;
    if (isint(dst) && isptr(src))
        return true;
    if (isptrto(dst, FUNCTION)) {
        if (isint(src) || isptrto(src, FUNCTION))
            return true;
    } else if (isptr(dst)) {
        if (isint(src) || isptrto(src, VOID))
            return true;
        if (isptr(src) && !isfunc(rtype(src)))
            return true;
    }

    return false;
}

static void integer_constant(struct token *t, struct symbol * sym)
{
    int base = t->u.lit.base;
    int suffix = t->u.lit.suffix;
    unsigned long long n = t->u.lit.v.u;
    struct type *ty;

    // character constant
    if (t->u.lit.chr) {
        bool wide = t->u.lit.chr == 2;
        SYM_TYPE(sym) = wide ? wchartype : unsignedchartype;
        SYM_VALUE(sym).u = wide ? (wchar_t)n : (unsigned char)n;
        return;
    }
    
    switch (suffix) {
    case UNSIGNED + LONG + LONG:
        ty = unsignedlonglongtype;
        break;
    case LONG + LONG:
        if (n > INTEGER_MAX(longlongtype) && base != 0)
            ty = unsignedlonglongtype;
        else
            ty = longlongtype;
        break;
    case UNSIGNED + LONG:
        if (n > UINTEGER_MAX(unsignedlongtype))
            ty = unsignedlonglongtype;
        else
            ty = unsignedlongtype;
        break;
    case LONG:
        if (base == 0) {
            if (n > INTEGER_MAX(longtype))
                ty = longlongtype;
            else
                ty = longtype;
        } else {
            if (n > INTEGER_MAX(longlongtype))
                ty = unsignedlonglongtype;
            else if (n > UINTEGER_MAX(unsignedlongtype))
                ty = longlongtype;
            else if (n > INTEGER_MAX(longtype))
                ty = unsignedlongtype;
            else
                ty = longtype;
        }
        break;
    case UNSIGNED:
        if (n > UINTEGER_MAX(unsignedlongtype))
            ty = unsignedlonglongtype;
        else if (n > UINTEGER_MAX(unsignedinttype))
            ty = unsignedlongtype;
        else
            ty = unsignedinttype;
        break;
    default:
        if (base == 0) {
            if (n > INTEGER_MAX(longtype))
                ty = longlongtype;
            else if (n > INTEGER_MAX(inttype))
                ty = longtype;
            else
                ty = inttype;
        } else {
            if (n > INTEGER_MAX(longlongtype))
                ty = unsignedlonglongtype;
            else if (n > UINTEGER_MAX(unsignedlongtype))
                ty = longlongtype;
            else if (n > INTEGER_MAX(longtype))
                ty = unsignedlongtype;
            else if (n > UINTEGER_MAX(unsignedinttype))
                ty = longtype;
            else if (n > INTEGER_MAX(inttype))
                ty = unsignedinttype;
            else
                ty = inttype;
        }
        break;
    }

    // overflow
    if (TYPE_OP(ty) == INT && n > INTEGER_MAX(longlongtype))
        error("integer constant overflow: %s", TOK_LIT_STR(t));

    SYM_TYPE(sym) = ty;
    SYM_VALUE(sym) = t->u.lit.v;
}

static void float_constant(struct token *t, struct symbol * sym)
{
    int suffix = t->u.lit.suffix;
    switch (suffix) {
    case FLOAT:
        SYM_TYPE(sym) = floattype;
        break;
    case LONG + DOUBLE:
        SYM_TYPE(sym) = longdoubletype;
        break;
    default:
        SYM_TYPE(sym) = doubletype;
        break;
    }
}

static void string_constant(struct token *t, struct symbol * sym)
{
    const char *s = TOK_LIT_STR(t);
    bool wide = s[0] == 'L' ? true : false;
    struct type *ty;
    if (wide) {
        size_t len = strlen(s) - 3;
        wchar_t *ws = xmalloc(sizeof(wchar_t) * (len+1));
        errno = 0;
        size_t wlen = mbstowcs(ws, s + 2, len);
        if (errno == EILSEQ)
            error("invalid multibyte sequence: %s", s);
        assert(wlen <= len + 1);
        ty = array_type(wchartype);
        TYPE_LEN(ty) = wlen;
        set_typesize(ty);
    } else {
        ty = array_type(chartype);
        TYPE_LEN(ty) = strlen(s) - 1;
        set_typesize(ty);
    }
    SYM_TYPE(sym) = ty;
}

static struct expr *literal_expr(struct token *t, int id,
                                 void (*cnst) (struct token *, struct symbol *))
{
    const char *name = TOK_LIT_STR(t);
    struct symbol *sym = lookup(name, constants);
    if (!sym) {
        sym = install(name, &constants, CONSTANT, PERM);
        cnst(t, sym);
    }
    struct expr *expr = ast_expr(id, SYM_TYPE(sym), NULL, NULL);
    EXPR_SRC(expr) = t->src;
    EXPR_SYM(expr) = sym;
    return expr;
}

struct expr *new_integer_literal(int i)
{
    struct token t = {.id = ICONSTANT, .u.lit.str = strd(i), .u.lit.v.i = i};
    return literal_expr(&t, INTEGER_LITERAL, integer_constant);
}

static struct expr *new_uint_literal(unsigned long l)
{
    struct token t = {.id = ICONSTANT, .u.lit.str = stru(l), .u.lit.v.u = l};
    return literal_expr(&t, INTEGER_LITERAL, integer_constant);
}

struct expr *new_string_literal(const char *string)
{
    struct token t = {.id = SCONSTANT, .u.lit.str = format("\"%s\"", string)};
    return literal_expr(&t, STRING_LITERAL, string_constant);
}

static struct vector *argcast1(struct type **params, size_t nparams,
                               struct expr **args, size_t nargs,
                               bool oldstyle)
{
    struct vector *v = vec_new();
    size_t cmp1;

    if (oldstyle)
        cmp1 = MIN(nparams, nargs);
    else
        cmp1 = nparams;

    for (size_t i = 0; i < cmp1; i++) {
        struct type *dst = params[i];
        struct expr *arg = args[i];
        struct type *src = EXPR_TYPE(arg);
        struct expr *ret = assignconv(dst, arg);
        if (ret) {
            vec_push(v, ret);
        } else {
            if (oldstyle)
                warning(INCOMPATIBLE_TYPES, type2s(src), type2s(dst));
            else
                error(INCOMPATIBLE_TYPES, type2s(src), type2s(dst));
        }
    }
    for (size_t i = cmp1; i < nargs; i++) {
        struct expr *arg = args[i];
        vec_push(v, conva(arg));
    }

    return v;
}

static struct expr **argscast(struct type *fty, struct expr **args)
{
    struct vector *v = vec_new();
    assert(isfunc(fty));

    /* There are 5 cases:
     *
     * 1. function declaration with prototype
     * 2. function definition with prototype
     * 3. function declaration with oldstyle
     * 4. function definition with oldstyle
     * 5. no function declaration/definition found
     */

    struct type **params = TYPE_PROTO(fty);
    size_t len1 = length(params);
    size_t len2 = length(args);
    bool oldstyle = TYPE_OLDSTYLE(fty);

    if (oldstyle) {
        if (len1 > len2)
            warning("too few arguments to function call");

        v = argcast1(params, len1, args, len2, oldstyle);
    } else {
        if (len1 == 0) {
            if (len2 > 0) {
                error("too many arguments to function call, expected %d, have %d",
                      len1, len2);
                return NULL;
            }
            return vtoa(v, PERM);
        }

        bool vargs = TYPE_VARG(fty);
        assert(len1 >= 1);
        if (len1 <= len2) {
            if (!vargs && len1 < len2) {
                error("too many arguments to function call, expected %d, have %d",
                      len1, len2);
                return NULL;
            }
            SAVE_ERRORS;
            v = argcast1(params, len1, args, len2, oldstyle);
            if (HAS_ERROR)
                return NULL;
        } else {
            if (vargs)
                error("too few arguments to function call, expected at least %d, have %d",
                      len1, len2);
            else
                error("too few arguments to function call, expected %d, have %d",
                      len1, len2);
            return NULL;
        }
    }
    return vtoa(v, PERM);
}

static struct expr *compound_literal(struct type * ty)
{
    struct expr *ret;
    struct expr *inits;

    inits = initializer_list(ty);
    ret = ast_expr(COMPOUND_LITERAL, ty, inits, NULL);
    EXPR_SRC(ret) = EXPR_SRC(inits);
    
    // define local variable
    if (SCOPE >= LOCAL) {
        const char *label = gen_compound_label();
        struct symbol *sym = make_localvar(label, ty, 0);
        SYM_INIT(sym) = inits;
        // set sym
        EXPR_SYM(ret) = sym;
        SYM_REFS(sym)++;
    }

    return ret;
}

static struct type *cast_type(void)
{
    struct type *ty;

    expect('(');
    ty = typename();
    expect(')');

    return ty;
}

static struct expr *make_ref_expr(struct symbol *sym, struct source src)
{
    struct expr *ret = ast_expr(REF_EXPR, SYM_TYPE(sym), NULL, NULL);
    EXPR_SYM(ret) = sym;
    EXPR_SRC(ret) = src;
    SYM_REFS(sym)++;
    return ret;
}

/// primary-expression:
///   identifier
///   constant
///   string-literal
///   '(' expression ')'
///
static struct expr *primary_expr(void)
{
    int t = token->id;
    struct symbol *sym;
    struct expr *ret = NULL;

    switch (t) {
    case ID:
        sym = lookup(TOK_ID_STR(token), identifiers);
        if (sym) {
            ret = make_ref_expr(sym, source);
            if (isenum(SYM_TYPE(sym)) && SYM_SCLASS(sym) == ENUM)
                EXPR_OP(ret) = ENUM;        // enum ids
        } else {
            error("use of undeclared identifier '%s'", tok2s(token));
        }
        expect(t);
        break;
    case ICONSTANT:
        ret = literal_expr(token, INTEGER_LITERAL, integer_constant);
        expect(t);
        break;
    case FCONSTANT:
        ret = literal_expr(token, FLOAT_LITERAL, float_constant);
        expect(t);
        break;
    case SCONSTANT:
        ret = literal_expr(token, STRING_LITERAL, string_constant);
        expect(t);
        break;
    case '(':
        if (first_typename(lookahead())) {
            struct type *ty = cast_type();
            ret = compound_literal(ty);
        } else {
            struct source src = source;
            expect('(');
            struct expr *e = expression();
            if (e) {
                ret =
                    ast_expr(PAREN_EXPR, EXPR_TYPE(e), e, NULL);
                EXPR_SRC(ret) = src;
            }
            expect(')');
        }
        break;
    default:
        error("invalid postfix expression at '%s'", tok2s(token));
        break;
    }

    return ret;
}

// []
static struct expr *subscript(struct expr *node)
{
    struct expr *e;
    struct expr *ret = NULL;
    struct source src = source;

    expect('[');
    e = conv(expression());
    expect(']');
    if (node == NULL || e == NULL)
        return ret;

    SAVE_ERRORS;
    bool kind1 = isptr(EXPR_TYPE(node)) && isint(EXPR_TYPE(e));
    bool kind2 = isint(EXPR_TYPE(node)) && isptr(EXPR_TYPE(e));
    if (kind1 || kind2) {
        struct type *ptr = isptr(EXPR_TYPE(node)) ? EXPR_TYPE(node) : EXPR_TYPE(e);
        if (isptrto(ptr, FUNCTION))
            error_at(src,
                     "subscript of pointer to function type '%s'",
                     type2s(rtype(ptr)));
    } else {
        if (!isptr(EXPR_TYPE(node)) && !isptr(EXPR_TYPE(e)))
            error_at(src, "subscripted value is not an array or pointer");
        else
            error_at(src, "array subscript is not an integer");
    }
    if (NO_ERROR) {
        struct type *ty = isptr(EXPR_TYPE(node)) ? EXPR_TYPE(node) : EXPR_TYPE(e);
        ret = ast_expr(SUBSCRIPT_EXPR, rtype(ty), node, e);
        EXPR_SRC(ret) = EXPR_SRC(node);
    }
    return ret;
}

/// argument-expression-list:
///   assignment-expression
///   argument-expression-list ',' assignment-expression
///
static struct expr **argument_expr_list(void)
{
    struct vector *v = NULL;

    if (first_expr(token)) {
        v = vec_new();
        for (;;) {
            vec_push_safe(v, assign_expr());
            if (token->id != ',')
                break;
            expect(',');
        }
    } else if (token->id != ')') {
        error("expect assignment expression");
    }

    return vtoa(v, PERM);
}

static struct expr *flatten_call(struct expr *node)
{
    switch (EXPR_ID(node)) {
    case CONV_EXPR:
    case CAST_EXPR:
    case PAREN_EXPR:
        return flatten_call(EXPR_OPERAND(node, 0));
    case UNARY_OPERATOR:
        if (EXPR_OP(node) == '&' || EXPR_OP(node) == '*')
            return flatten_call(EXPR_OPERAND(node, 0));
        else
            return node;
    default:
        return node;
    }
}

static void builtin_funcall(struct expr *call, struct expr *ref)
{
    const char *fname = SYM_NAME(EXPR_SYM(ref));
    if (!strcmp(fname, BUILTIN_VA_ARG_P)) {
        // __builtin_va_arg_p
        struct expr **args = EXPR_ARGS(call);
        struct expr *arg1 = args[1];
        assert(isptr(EXPR_TYPE(arg1)));
        struct type *ty = rtype(EXPR_TYPE(arg1));
        // save the type
        EXPR_VA_ARG_TYPE(call) = ty;
        
        if (isrecord(ty) && TYPE_SIZE(ty) <= MAX_STRUCT_PARAM_SIZE) {
            const char *label = gen_tmpname();
            struct symbol *sym = make_localvar(label, ty, 0);
            // passing address
            struct expr *operand = make_ref_expr(sym, SYM_SRC(sym));
            // update arg1
            args[1] = ast_uop('&', ptr_type(ty), operand);
        } else {
            // update arg1 to NULL
            args[1] = NULL;
        }
    }
}

static struct expr *funcall(struct expr *node)
{
    struct expr **args;
    struct expr *ret = NULL;
    struct source src = source;
    
    SAVE_ERRORS;
    expect('(');
    args = argument_expr_list();
    expect(')');
    if (node == NULL || HAS_ERROR)
        return ret;
    
    if (isptrto(EXPR_TYPE(node), FUNCTION)) {
        struct type *fty = rtype(EXPR_TYPE(node));
        if ((args = argscast(fty, args))) {
            ret = ast_expr(CALL_EXPR, rtype(fty), node, NULL);
            EXPR_ARGS(ret) = args;
            EXPR_SRC(ret) = src;
            vec_push(funcinfo.calls, ret);
            // handle builtin calls
            struct expr *tmp = flatten_call(node);
            if (EXPR_ID(tmp) == REF_EXPR && isfunc(EXPR_TYPE(tmp)))
                builtin_funcall(ret, tmp);
        }
    } else {
        ensure_type(node, isfunc);
    }

    return ret;
}

// '.', '->'
static struct expr *direction(struct expr *node)
{
    int t = token->id;
    struct expr *ret = NULL;
    const char *name = NULL;
    struct source src = source;

    expect(t);
    if (token->id == ID)
        name = TOK_ID_STR(token);
    expect(ID);
    if (node == NULL || name == NULL)
        return ret;

    SAVE_ERRORS;
    struct field *field = NULL;
    struct type *ty = EXPR_TYPE(node);
    if (t == '.') {
        ensure_type(node, isrecord);
    } else {
        if (!isptr(ty) || !isrecord(rtype(ty)))
            error("pointer to struct/union type expected, not type '%s'", type2s(ty));
        else
            ty = rtype(ty);
    }
    if (isrecord(ty)) {
        field = find_field(ty, name);
        if (field == NULL)
            field_not_found_error(ty, name);
    }
    if (NO_ERROR) {
        if (opts.ansi) {
            // The result has the union of both sets of qualifiers.
            int q = qual_union(EXPR_TYPE(node), FIELD_TYPE(field));
            ret = ast_expr(MEMBER_EXPR, qual(q, FIELD_TYPE(field)), node,  NULL);
        } else {
            ret = ast_expr(MEMBER_EXPR,  FIELD_TYPE(field), node,  NULL);
        }
        EXPR_NAME(ret) = FIELD_NAME(field);
        EXPR_OP(ret) = t;
        EXPR_SRC(ret) = src;
    }
    return ret;
}

static void ensure_additive_ptr(struct expr *node)
{
    assert(isptr(EXPR_TYPE(node)));
    struct type *rty = rtype(EXPR_TYPE(node));
    if (isfunc(rty) || isincomplete(rty))
        error_at(EXPR_SRC(node),
                 "increment/decrement of invalid type '%s' (pointer to unknown size)",
                 type2s(EXPR_TYPE(node)));
}

static void ensure_increment(struct expr *node)
{
    ensure_type(node, isscalar);
    ensure_assignable(node);
    if (isptr(EXPR_TYPE(node)))
        ensure_additive_ptr(node);
}

static struct expr *post_increment(struct expr *node)
{
    int t = token->id;
    struct expr *ret = NULL;
    struct source src = source;

    expect(t);
    if (node == NULL)
        return ret;

    SAVE_ERRORS;
    ensure_increment(node);
    if (NO_ERROR) {
        ret = ast_uop(t, EXPR_TYPE(node), node);
        EXPR_SRC(ret) = src;
    }
    return ret;
}

static struct expr *postfix_expr1(struct expr *ret)
{
    for (; token->id == '[' || token->id == '(' || token->id == '.'
             || token->id == DEREF || token->id == INCR || token->id == DECR;) {
        switch (token->id) {
        case '[':
            ret = subscript(conv(ret));
            break;
        case '(':
            ret = funcall(conv(ret));
            break;
        case '.':
        case DEREF:
            ret = direction(ret);
            break;
        case INCR:
        case DECR:
            ret = post_increment(ret);
            break;
        default:
            assert(0);
        }
    }

    return ret;
}

/// postfix-expression:
///   primary-expression
///   postfix-expression '[' expression ']'
///   postfix-expression '(' argument-expression-list[opt] ')'
///   postfix-expression '.' identifier
///   postfix-expression '->' identifier
///   postfix-expression '++'
///   postfix-expression '--'
///   '(' type-name ')' '{' initializer-list '}'
///   '(' type-name ')' '{' initializer-list ',' '}'
///
static struct expr *postfix_expr(void)
{
    struct expr *expr = primary_expr();

    return postfix_expr1(expr);
}

static struct expr *sizeof_expr(void)
{
    int t = token->id;
    struct expr *ret = NULL;
    struct source src = source;

    expect(t);

    struct token *ahead = lookahead();
    struct expr *n = NULL;
    struct type *ty = NULL;

    if (token->id == '(' && first_typename(ahead)) {
        ty = cast_type();
        if (token->id == '{') {
            struct expr *node = compound_literal(ty);
            n = ast_uop(t, ty, postfix_expr1(node));
        }
    } else {
        n = unary_expr();
    }

    ty = n ? EXPR_TYPE(n) : ty;
    if (ty == NULL)
        return ret;

    SAVE_ERRORS;
    if (isfunc(ty) || isvoid(ty))
        error("'sizeof' to a '%s' type is invalid", type2s(ty));
    else if (isincomplete(ty))
        error("'sizeof' to an incomplete type '%s' is invalid",
              type2s(ty));
    else if (n && is_bitfield(n))
        error("'sizeof' to a bitfield is invalid");

    if (NO_ERROR) {
        ret = new_uint_literal(TYPE_SIZE(ty));
        EXPR_SRC(ret) = src;
    }

    return ret;
}

static struct expr *pre_increment(void)
{
    int t = token->id;
    struct expr *ret = NULL;
    struct source src = source;

    expect(t);
    struct expr *operand = unary_expr();
    if (operand == NULL)
        return ret;

    SAVE_ERRORS;
    ensure_increment(operand);
    if (NO_ERROR) {
        ret = ast_uop(t, EXPR_TYPE(operand), operand);
        EXPR_PREFIX(ret) = true;
        EXPR_SRC(ret) = src;
    }

    return ret;
}

static struct expr *minus_plus(void)
{
    int t = token->id;
    struct expr *ret = NULL;
    struct source src = source;

    expect(t);
    struct expr *operand = conv(cast_expr());
    if (operand == NULL)
        return ret;

    SAVE_ERRORS;
    ensure_type(operand, isarith);
    if (NO_ERROR) {
        ret = ast_uop(t, EXPR_TYPE(operand), operand);
        EXPR_SRC(ret) = src;
    }

    return ret;
}

static struct expr *bitwise_not(void)
{
    int t = token->id;
    struct expr *ret = NULL;
    struct source src = source;

    expect(t);
    struct expr *operand = conv(cast_expr());
    if (operand == NULL)
        return ret;

    SAVE_ERRORS;
    ensure_type(operand, isint);
    if (NO_ERROR) {
        ret = ast_uop(t, EXPR_TYPE(operand), operand);
        EXPR_SRC(ret) = src;
    }

    return ret;
}

static struct expr *logical_not(void)
{
    int t = token->id;
    struct expr *ret = NULL;
    struct source src = source;

    expect(t);
    struct expr *operand = conv(cast_expr());
    if (operand == NULL)
        return ret;

    SAVE_ERRORS;
    ensure_type(operand, isscalar);
    if (NO_ERROR) {
        ret = ast_uop(t, inttype, operand);
        EXPR_SRC(ret) = src;
    }

    return ret;
}

/**
 * The usual conversions are _NOT_ applied to the operand of the '&'
 * operator, and its result is never an lvalue.
 */
static struct expr *address(void)
{
    int t = token->id;
    struct expr *ret = NULL;
    struct source src = source;

    expect(t);
    struct expr *operand = cast_expr();
    if (operand == NULL)
        return ret;

    SAVE_ERRORS;
    if (!isfunc(EXPR_TYPE(operand))) {
        ensure_lvalue(operand);
        if (EXPR_SYM(operand)
            && SYM_SCLASS(EXPR_SYM(operand)) == REGISTER)
            error("address of register variable requested");
        else if (is_bitfield(operand))
            error("address of bitfield requested");
    }
    if (NO_ERROR) {
        ret = ast_uop(t, ptr_type(EXPR_TYPE(operand)), operand);
        EXPR_SRC(ret) = src;
    }

    return ret;
}

static struct expr *indirection(void)
{
    int t = token->id;
    struct expr *ret = NULL;
    struct source src = source;

    expect(t);
    struct expr *operand = conv(cast_expr());
    if (operand == NULL)
        return ret;

    SAVE_ERRORS;
    ensure_type(operand, isptr);
    if (NO_ERROR) {
        ret = ast_uop(t, rtype(EXPR_TYPE(operand)), operand);
        EXPR_SRC(ret) = src;
    }

    return ret;
}

/// unary-expression:
///   postfix-expression
///   '++' unary-expression
///   '--' unary-expression
///   unary-operator cast-expression
///   'sizeof' unary-expression
///   'sizeof' '(' type-name ')'
///
/// unary-operator:
///   '&' '*' '+' '-' '~' '!'
///
static struct expr *unary_expr(void)
{
    switch (token->id) {
    case INCR:
    case DECR:
        return pre_increment();
    case '+':
    case '-':
        return minus_plus();
    case '~':
        return bitwise_not();
    case '!':
        return logical_not();
    case '&':
        return address();
    case '*':
        return indirection();
    case SIZEOF:
        return sizeof_expr();
    default:
        return postfix_expr();
    }
}

/// cast-expression:
///   unary-expression
///   '(' type-name ')' cast-expression
///
static struct expr *cast_expr(void)
{
    struct token *ahead = lookahead();
    struct source src = source;

    if (token->id == '(' && first_typename(ahead)) {
        struct type *ty = cast_type();
        if (token->id == '{') {
            struct expr *node = compound_literal(ty);
            return postfix_expr1(node);
        }

        struct expr *ret = NULL;
        struct expr *cast = cast_expr();
        if (cast == NULL)
            return ret;
        cast = decay(cast);

        if (is_castable(ty, EXPR_TYPE(cast))) {
            ret = ast_expr(CAST_EXPR, ty, cast, NULL);
            EXPR_SRC(ret) = src;
        } else {
            error_at(EXPR_SRC(cast),
                     INCOMPATIBLE_TYPES,
                     type2s(EXPR_TYPE(cast)), type2s(ty));
        }

        return ret;
    }
    return unary_expr();
}

/// multiplicative-expression:
///   cast-expression
///   multiplicative-expression '*' cast-expression
///   multiplicative-expression '/' cast-expression
///   multiplicative-expression '%' cast-expression
///
static struct expr *multiple_expr(void)
{
    struct expr *mulp1;

    mulp1 = cast_expr();
    while (token->id == '*' || token->id == '/' || token->id == '%') {
        int t = token->id;
        struct source src = source;
        expect(t);
        mulp1 = bop(t, conv(mulp1), conv(cast_expr()));
        if (mulp1) EXPR_SRC(mulp1) = src;
    }

    return mulp1;
}

/// additive-expression:
///   multiplicative-expression
///   additive-expression '+' multiplicative-expression
///   additive-expression '-' multiplicative-expression
///
static struct expr *additive_expr(void)
{
    struct expr *add1;

    add1 = multiple_expr();
    while (token->id == '+' || token->id == '-') {
        int t = token->id;
        struct source src = source;
        expect(t);
        add1 = bop(t, conv(add1), conv(multiple_expr()));
        if (add1) EXPR_SRC(add1) = src;
    }

    return add1;
}

/// shift-expression:
///   additive-expression
///   shift-expression '<<' additive-expression
///   shift-expression '>>' additive-expression
///
static struct expr *shift_expr(void)
{
    struct expr *shift1;

    shift1 = additive_expr();
    while (token->id == LSHIFT || token->id == RSHIFT) {
        int t = token->id;
        struct source src = source;
        expect(t);
        shift1 = bop(t, conv(shift1), conv(additive_expr()));
        if (shift1) EXPR_SRC(shift1) = src;
    }

    return shift1;
}

/// relational-expression:
///   shift-expression
///   relational-expression '<' shift-expression
///   relational-expression '>' shift-expression
///   relational-expression '<=' shift-expression
///   relational-expression '>=' shift-expression
///
static struct expr *relation_expr(void)
{
    struct expr *rel;

    rel = shift_expr();
    while (token->id == '<' || token->id == '>' || token->id == LEQ || token->id == GEQ) {
        int t = token->id;
        struct source src = source;
        expect(t);
        rel = bop(t, conv(rel), conv(shift_expr()));
        if (rel) EXPR_SRC(rel) = src;
    }

    return rel;
}

/// equality-expression:
///   relational-expression
///   equality-expression '==' relational-expression
///   equality-expression '!=' relational-expression
///
static struct expr *equality_expr(void)
{
    struct expr *equl;

    equl = relation_expr();
    while (token->id == EQ || token->id == NEQ) {
        int t = token->id;
        struct source src = source;
        expect(t);
        equl = bop(t, conv(equl), conv(relation_expr()));
        if (equl) EXPR_SRC(equl) = src;
    }

    return equl;
}

/// AND-expression:
///   equality-expression
///   AND-expression '&' equality-expression
///
static struct expr *and_expr(void)
{
    struct expr *and1;

    and1 = equality_expr();
    while (token->id == '&') {
        struct source src = source;
        expect('&');
        and1 = bop('&', conv(and1), conv(equality_expr()));
        if (and1) EXPR_SRC(and1) = src;
    }

    return and1;
}

/// exclusive-OR-expression:
///   AND-expression
///   exclusive-OR-expression '^' AND-expression
///
static struct expr *exclusive_or(void)
{
    struct expr *eor;

    eor = and_expr();
    while (token->id == '^') {
        struct source src = source;
        expect('^');
        eor = bop('^', conv(eor), conv(and_expr()));
        if (eor) EXPR_SRC(eor) = src;
    }

    return eor;
}

/// inclusive-OR-expression:
///   exclusive-OR-expression
///   inclusive-OR-expression '|' exclusive-OR-expression
///
static struct expr *inclusive_or(void)
{
    struct expr *ior;

    ior = exclusive_or();
    while (token->id == '|') {
        struct source src = source;
        expect('|');
        ior = bop('|', conv(ior), conv(exclusive_or()));
        if (ior) EXPR_SRC(ior) = src;
    }

    return ior;
}

/// logical-AND-expression:
///   inclusive-OR-expression
///   logical-AND-expression '&&' inclusive-OR-expression
///
static struct expr *logic_and(void)
{
    struct expr *and1;

    and1 = inclusive_or();
    while (token->id == AND) {
        struct source src = source;
        expect(AND);
        and1 = logicop(AND, conv(and1), conv(inclusive_or()));
        if (and1) EXPR_SRC(and1) = src;
    }

    return and1;
}

/// logical-OR-expression:
///   logical-AND-expression
///   logical-OR-expression '||' logical-AND-expression
///
static struct expr *logic_or(void)
{
    struct expr *or1;

    or1 = logic_and();
    while (token->id == OR) {
        struct source src = source;
        expect(OR);
        or1 = logicop(OR, conv(or1), conv(logic_and()));
        if (or1) EXPR_SRC(or1) = src;
    }

    return or1;
}

static struct expr *cond_expr1(struct expr *cond)
{
#define INCOMPATIBLE_TYPES2                                             \
    "imcompatible types '%s' and '%s' in conditional expression"

    struct expr *ret = NULL;
    struct expr *then, *els;
    struct type *ty = NULL;
    struct source src = source;

    expect('?');
    then = conv(expression());
    expect(':');
    els = conv(cond_expr());

    if (cond == NULL || then == NULL || els == NULL)
        return ret;

    struct type *ty1 = EXPR_TYPE(then);
    struct type *ty2 = EXPR_TYPE(els);

    SAVE_ERRORS;
    ensure_type(cond, isscalar);

    if (isarith(ty1) && isarith(ty2)) {
        ty = conv2(ty1, ty2);
        then = wrap(ty, then);
        els = wrap(ty, els);
    } else if ((isstruct(ty1) && isstruct(ty2)) ||
               (isunion(ty1) && isunion(ty2))) {
        if (!eqtype(ty1, ty2))
            error(INCOMPATIBLE_TYPES2, type2s(ty1), type2s(ty2));
        ty = ty1;
    } else if (isvoid(ty1) && isvoid(ty2)) {
        ty = voidtype;
    } else if (isptr(ty1) && isptr(ty2)) {
        if (is_nullptr(then) || is_nullptr(els)) {
            struct type *nty = is_nullptr(then) ? ty1 : ty2;
            struct type *tty = nty == ty1 ? ty2 : ty1;
            ty = ptr_type(compose(rtype(tty), rtype(nty)));
            then = bitconv(ty, then);
            els = bitconv(ty, els);
        } else if (isptrto(ty1, VOID) || isptrto(ty2, VOID)) {
            struct type *vty = isptrto(ty1, VOID) ? ty1 : ty2;
            struct type *tty = vty == ty1 ? ty2 : ty1;
            if (isptrto(tty, FUNCTION)) {
                error(INCOMPATIBLE_TYPES2, type2s(ty1), type2s(ty2));
            } else {
                ty = ptr_type(compose(rtype(vty), rtype(tty)));
                then = bitconv(ty, then);
                els = bitconv(ty, els);
            }
        } else {
            struct type *rty1 = rtype(ty1);
            struct type *rty2 = rtype(ty2);
            if (eqtype(unqual(rty1), unqual(rty2))) {
                ty = ptr_type(compose(rty1, rty2));
                then = bitconv(ty, then);
                els = bitconv(ty, els);
            } else {
                error(INCOMPATIBLE_TYPES2, type2s(ty1), type2s(ty2));
            }
        }
    } else if (((isptr(ty1) && isint(ty2)) ||
                (isint(ty1) && isptr(ty2))) && !opts.ansi) {
        struct type *pty = isptr(ty1) ? ty1 : ty2;
        ty = pty;
        then = bitconv(ty, then);
        els = bitconv(ty, els);
    } else {
        error("type mismatch in conditional expression: '%s' and '%s'",
              type2s(ty1), type2s(ty2));
    }

    if (NO_ERROR) {
        ret = ast_expr(COND_EXPR, ty, NULL, NULL);
        EXPR_COND(ret) = cond;
        EXPR_THEN(ret) = then;
        EXPR_ELSE(ret) = els;
        EXPR_SRC(ret) = src;
    }

    return ret;
}

/// conditional-expression:
///   logical-OR-expression
///   logical-OR-expression '?' expression ':' conditional-expression
///
static struct expr *cond_expr(void)
{
    struct expr *or1 = logic_or();
    if (token->id == '?')
        return cond_expr1(conv(or1));
    return or1;
}

/// assignment-expression:
///   conditional-expression
///   unary-expression assignment-operator assignment-expression
///
/// assignment-operator:
///   '=' '*=' '/=' '%=' '+=' '-=' '<<=' '>>=' '&=' '^=' '|='
///
struct expr *assign_expr(void)
{
    struct expr *or1 = logic_or();
    if (token->id == '?')
        return cond_expr1(conv(or1));
    if (is_assign_op(token->id)) {
        int t = token->id;
        expect(t);
        return assignop(t, or1, assign_expr());
    }
    return or1;
}

/// expression:
///   assignment-expression
///   expression ',' assignment-expression
///
struct expr *expression(void)
{
    struct expr *assign1;

    assign1 = assign_expr();
    while (token->id == ',') {
        struct source src = source;
        expect(',');
        assign1 = commaop(',', assign1, assign_expr());
        if (assign1) EXPR_SRC(assign1) = src;
    }
    return assign1;
}

struct expr *bop(int op, struct expr *l, struct expr *r)
{
    struct expr *node = NULL;
    struct type *ty;

    if (l == NULL || r == NULL)
        return NULL;

    SAVE_ERRORS;
    switch (op) {
    case '*':
    case '/':
        ensure_type(l, isarith);
        ensure_type(r, isarith);
        if (NO_ERROR) {
            ty = conv2(EXPR_TYPE(l), EXPR_TYPE(r));
            node = ast_bop(op, ty, wrap(ty, l), wrap(ty, r));
        }
        break;
    case '%':
    case LSHIFT:
    case RSHIFT:
    case '&':
    case '^':
    case '|':
        ensure_type(l, isint);
        ensure_type(r, isint);
        if (NO_ERROR) {
            ty = conv2(EXPR_TYPE(l), EXPR_TYPE(r));
            node = ast_bop(op, ty, wrap(ty, l), wrap(ty, r));
        }
        break;
    case '+':
        if (isptr(EXPR_TYPE(l))) {
            ensure_additive_ptr(l);
            ensure_type(r, isint);
            if (NO_ERROR)
                node = ast_bop(op, EXPR_TYPE(l), l, r);
        } else if (isptr(EXPR_TYPE(r))) {
            ensure_additive_ptr(r);
            ensure_type(l, isint);
            if (NO_ERROR)
                node = ast_bop(op, EXPR_TYPE(r), l, r);
        } else {
            ensure_type(l, isarith);
            ensure_type(r, isarith);
            if (NO_ERROR) {
                ty = conv2(EXPR_TYPE(l), EXPR_TYPE(r));
                node =
                    ast_bop(op, ty, wrap(ty, l), wrap(ty, r));
            }
        }
        break;
    case '-':
        if (isptr(EXPR_TYPE(l))) {
            if (isint(EXPR_TYPE(r))) {
                ensure_additive_ptr(l);
                if (NO_ERROR)
                    node = ast_bop(op, EXPR_TYPE(l), l, r);
            } else if (isptr(EXPR_TYPE(r))) {
                ensure_additive_ptr(l);
                ensure_additive_ptr(r);
                struct type *rty1 = rtype(EXPR_TYPE(l));
                struct type *rty2 = rtype(EXPR_TYPE(r));
                if (!eqtype(unqual(rty1), unqual(rty2)))
                    error("'%s' and '%s' are not pointers to compatible types",
                          type2s(EXPR_TYPE(l)), type2s(EXPR_TYPE(r)));
                if (NO_ERROR)
                    node = ast_bop(op, inttype, l, r);
            } else {
                error("expect integer or pointer type, not type '%s'",
                      type2s(EXPR_TYPE(r)));
            }
        } else {
            ensure_type(l, isarith);
            ensure_type(r, isarith);
            if (NO_ERROR) {
                ty = conv2(EXPR_TYPE(l), EXPR_TYPE(r));
                node = ast_bop(op, ty, wrap(ty, l), wrap(ty, r));
            }
        }
        break;
        // scalar op scalar
    case '>':
    case '<':
    case LEQ:
    case GEQ:
    case EQ:
    case NEQ:
        if (isptr(EXPR_TYPE(l)) && isptr(EXPR_TYPE(r))) {
            // both ptr
            if (eqtype(EXPR_TYPE(l), EXPR_TYPE(r))) {
                node = ast_bop(op, inttype, l, r);
            } else if (op == EQ || op == NEQ) {
                if (isptrto(EXPR_TYPE(l), VOID)
                    || isptrto(EXPR_TYPE(r), VOID)) {
                    struct expr *l1 = isptrto(EXPR_TYPE(l), VOID) ? l : ast_conv(ptr_type(voidtype), l, BitCast);
                    struct expr *r1 = isptrto(EXPR_TYPE(r), VOID) ? r : ast_conv(ptr_type(voidtype), r, BitCast);
                    node = ast_bop(op, inttype, l1, r1);
                } else if (is_nullptr(l) || is_nullptr(r)) {
                    node = ast_bop(op, inttype, l, r);
                }
            }
            
            if (!node) {
                if (!opts.ansi)
                    node = ast_bop(op, inttype, l, ast_conv(EXPR_TYPE(l), r, BitCast));
                else
                    error("comparison of incompatible pointer types ('%s' and '%s')",
                          type2s(EXPR_TYPE(l)), type2s(EXPR_TYPE(r)));
            }
        } else if (isarith(EXPR_TYPE(l)) && isarith(EXPR_TYPE(r))) {
            // both arith
            ty = conv2(EXPR_TYPE(l), EXPR_TYPE(r));
            node = ast_bop(op, inttype, wrap(ty, l), wrap(ty, r));
        } else if (isptr(EXPR_TYPE(l))) {
            // ptr op int
            ensure_type(r, isint);
            if (NO_ERROR)
                node = ast_bop(op, inttype, l, ast_conv(EXPR_TYPE(l), r, IntegerToPointerCast));
        } else if (isptr(EXPR_TYPE(r))) {
            // int op ptr
            ensure_type(l, isint);
            if (NO_ERROR)
                node = ast_bop(op, inttype, ast_conv(EXPR_TYPE(r), l, IntegerToPointerCast), r);
        } else {
            error("comparison of invalid types ('%s' and '%s')",
                  type2s(EXPR_TYPE(l)), type2s(EXPR_TYPE(r)));
        }
        break;
    default:
        error("unknown op '%s'", id2s(op));
        assert(0);
    }
    return node;
}

static struct expr *logicop(int op, struct expr *l, struct expr *r)
{
    struct expr *ret = NULL;

    if (l == NULL || r == NULL)
        return NULL;

    SAVE_ERRORS;
    ensure_type(l, isscalar);
    ensure_type(r, isscalar);
    if (NO_ERROR)
        ret = ast_bop(op, inttype, l, r);

    return ret;
}

static struct expr *commaop(int op, struct expr *l, struct expr *r)
{
    if (l == NULL || r == NULL)
        return NULL;

    if (isarray(EXPR_TYPE(l)) || isfunc(EXPR_TYPE(l)))
        l = decay(l);
    if (isarray(EXPR_TYPE(r)) || isfunc(EXPR_TYPE(r)))
        r = decay(r);
    if (islvalue(l))
        l = ltor(l);
    if (islvalue(r))
        r = ltor(r);

    return ast_bop(op, EXPR_TYPE(r), l, r);
}

static int splitop(int op)
{
    switch (op) {
    case MULEQ:
        return '*';
    case DIVEQ:
        return '/';
    case MODEQ:
        return '%';
    case ADDEQ:
        return '+';
    case MINUSEQ:
        return '-';
    case LSHIFTEQ:
        return LSHIFT;
    case RSHIFTEQ:
        return RSHIFT;
    case BANDEQ:
        return '&';
    case BOREQ:
        return '|';
    case XOREQ:
        return '^';
    default:
        assert(0);
    }
}

static struct expr *assignop(int op, struct expr *l, struct expr *r)
{
    struct expr *ret = NULL;

    if (l == NULL || r == NULL)
        return NULL;

    struct type *retty = unqual(EXPR_TYPE(l));
    SAVE_ERRORS;
    ensure_assignable(l);
    if (HAS_ERROR)
        return NULL;
    if (op != '=') {
        // compound assignment
        int op2 = splitop(op);
        struct expr *l1 = conv(l);
        struct expr *r1 = conv(r);
        if (op2 == '+' || op2 == '-') {
            struct type *ty1 = EXPR_TYPE(l1);
            struct type *ty2 = EXPR_TYPE(r1);
            if (!((isarith(ty1) && isarith(ty2)) ||
                  (isptr(ty1) && isint(ty2))))
                error(INCOMPATIBLE_TYPES, type2s(ty2), type2s(ty1));
        }
        r = bop(op2, l1, r1);
    }

    if (NO_ERROR) {
        struct type *ty1 = EXPR_TYPE(l);
        struct type *ty2 = EXPR_TYPE(r);
        r = assignconv(retty, r);
        if (r)
            ret = ast_bop('=', retty, l, r);
        else
            error(INCOMPATIBLE_TYPES, type2s(ty2), type2s(ty1));
    }
    return ret;
}

static bool is_nullptr(struct expr *node)
{
    assert(isptr(EXPR_TYPE(node)) || isint(EXPR_TYPE(node)));

    struct expr *cnst = eval(node, inttype);
    if (cnst == NULL)
        return false;
    if (isiliteral(cnst))
        return SYM_VALUE(EXPR_SYM(cnst)).u == 0;
    return false;
}

long intexpr1(struct type * ty)
{
    struct source src = source;
    struct expr *cond = cond_expr();
    if (cond == NULL)
        // parsing expression failed
        return 0;
    if (ty == NULL)
        ty = EXPR_TYPE(cond);
    if (!isint(EXPR_TYPE(cond)) || !isint(ty)) {
        error_at(src, "expression is not an integer constant expression");
        return 0;
    }
    struct expr *cnst = eval(cond, ty);
    if (cnst == NULL) {
        error_at(src, "expression is not a compile-time constant");
        return 0;
    }
    assert(isiliteral(cnst));
    return ILITERAL_VALUE(cnst).i;
}

/// constant-expression:
///   conditional-expression
///
long intexpr(void)
{
    return intexpr1(NULL);
}

// if/do/while/for
struct expr *bool_expr(void)
{
    // Conversion for expression in conditional statement
    struct expr *node = expression();
    if (node == NULL)
        return NULL;
    // warning for assignment expression
    if (EXPR_ID(node) == BINARY_OPERATOR && EXPR_OP(node) == '=')
        warning("using the result of an assignment as a condition without parentheses '%s'",
                expr2s(node));
    if (islvalue(node))
        node = ltor(node);
    return decay(node);
}

// switch
struct expr *switch_expr(void)
{
    struct expr *node = conv(expression());
    if (node == NULL)
        return NULL;
    if (!isint(EXPR_TYPE(node))) {
        error("statement requires expression of integer type ('%s' invalid)",
              type2s(EXPR_TYPE(node)));
        return NULL;
    }
    return node;
}

struct expr *decls2expr(struct symbol **decls)
{
    struct expr *ret = NULL;

    for (int i = 0; decls[i]; i++) {
        struct symbol *sym = decls[i];
        if (SYM_INIT(sym) && SYM_SCLASS(sym) != STATIC) {
            SYM_X_KIND(sym) = SYM_KIND_LREF;
            struct expr *l = make_ref_expr(sym, SYM_SRC(sym));
            struct expr *r = SYM_INIT(sym);
            struct expr *n = ast_bop('=', SYM_TYPE(sym), l, r);
            if (ret)
                ret = commaop(',', ret, n);
            else
                ret = n;
        }
    }

    return ret;
}
