#include "cc.h"

static node_t *cast_expr(void);
static node_t *cond_expr(void);
static node_t *cond_expr1(node_t * o);
static node_t *unary_expr(void);
static node_t *logicop(int op, node_t * l, node_t * r);
static node_t *commaop(int op, node_t * l, node_t * r);
static node_t *assignop(int op, node_t * l, node_t * r);
static bool is_nullptr(node_t * node);

#define INTEGER_MAX(type)    (TYPE_LIMITS(type).max.i)
#define UINTEGER_MAX(type)   (TYPE_LIMITS(type).max.u)

#define SAVE_SOURCE        struct source src = source
#define SET_SOURCE(node)   if (node) AST_SRC(node) = src

static void ensure_type(node_t * node, bool(*is) (struct type *))
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

    if (!is(AST_TYPE(node)))
        error_at(AST_SRC(node), "expect type '%s', not '%s'", name, type2s(AST_TYPE(node)));
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
bool islvalue(node_t * node)
{
    if (AST_ID(node) == STRING_LITERAL)
        return true;
    if (AST_ID(node) == PAREN_EXPR)
        return islvalue(EXPR_OPERAND(node, 0));
    if (AST_ID(node) == SUBSCRIPT_EXPR)
        return true;
    if (AST_ID(node) == UNARY_OPERATOR && EXPR_OP(node) == '*') {
        if (isfunc(AST_TYPE(node)))
            return false;
        return true;
    }
    if (AST_ID(node) == MEMBER_EXPR)
        return EXPR_OP(node) ==
            DEREF ? true : islvalue(EXPR_OPERAND(node, 0));
    if (AST_ID(node) == COMPOUND_LITERAL)
        return true;
    if (AST_ID(node) == REF_EXPR) {
        if (EXPR_OP(node) == ENUM)
            return false;
        if (isfunc(AST_TYPE(node)))
            return false;
        return true;
    }

    return false;
}

static void ensure_lvalue(node_t * node)
{
    if (!islvalue(node))
        error_at(AST_SRC(node), "expect lvalue at '%s'", expr2s(node));
}

static void ensure_assignable(node_t * node)
{
    struct type *ty = AST_TYPE(node);
    struct source src = AST_SRC(node);
    if (!islvalue(node))
        error_at(src, "expression is not assignable '%s'", expr2s(node));
    else if (AST_ID(node) == PAREN_EXPR)
        ensure_assignable(EXPR_OPERAND(node, 0));
    else if (isarray(ty))
        error_at(src, "array type '%s' is not assignable", type2s(ty));
    else if (isconst(ty))
        error_at(src, "read-only variable '%s' is not assignable", expr2s(node));
}

static bool is_bitfield(node_t * node)
{
    if (AST_ID(node) != MEMBER_EXPR)
        return false;

    struct type *ty = AST_TYPE(EXPR_OPERAND(node, 0));
    if (isptr(ty))
        ty = rtype(ty);
    const char *name = AST_NAME(node);
    struct field *field = find_field(ty, name);
    return field && FIELD_ISBIT(field);
}

static const char * castname(struct type *ty, node_t *l)
{
    if (isfloat(ty) && isfloat(AST_TYPE(l)))
	return FloatCast;
    else if (isfloat(ty) && isint(AST_TYPE(l)))
        return IntegerToFloatCast;
    else if (isint(ty) && isint(AST_TYPE(l)))
	return IntegralCast;
    else if (isint(ty) && isfloat(AST_TYPE(l)))
	return FloatToIntegerCast;
    else
	return BitCast;
}

static node_t *wrap(struct type * ty, node_t * node)
{
    assert(isarith(ty));
    assert(isarith(AST_TYPE(node)));

    if (eqarith(ty, AST_TYPE(node)))
        return node;
    else
        return ast_conv(ty, node, castname(ty, node));
}

static node_t *bitconv(struct type * ty, node_t * node)
{
    if (eqtype(ty, AST_TYPE(node)))
        return node;
    else
        return ast_conv(ty, node, castname(ty, node));
}

static node_t *decay(node_t * node)
{
    assert(node);
    switch (TYPE_KIND(AST_TYPE(node))) {
    case FUNCTION:
        // FunctionToPointerDecay
        return ast_conv(ptr_type(AST_TYPE(node)), node, FunctionToPointerDecay);

    case ARRAY:
        // ArrayToPointerDecay
        return ast_conv(ptr_type(rtype(AST_TYPE(node))), node, ArrayToPointerDecay);

    default:
        return node;
    }
}

static node_t *ltor(node_t * node)
{
    // LValueToRValue
    return ast_conv(unqual(AST_TYPE(node)), node, LValueToRValue);
}

// Universal Unary Conversion
static node_t *conv(node_t * node)
{
    if (node == NULL)
        return NULL;
    if (islvalue(node))
        node = ltor(node);

    switch (TYPE_KIND(AST_TYPE(node))) {
    case _BOOL:
    case CHAR:
    case SHORT:
        return ast_conv(inttype, node, IntegralCast);

    case ENUM:
        return ast_conv(rtype(AST_TYPE(node)), node, IntegralCast);

    case FUNCTION:
    case ARRAY:
        return decay(node);

    default:
        return node;
    }
}

// Default function argument conversion
static node_t *conva(node_t * node)
{
    if (node == NULL)
        return NULL;
    if (islvalue(node))
        node = ltor(node);

    switch (TYPE_KIND(AST_TYPE(node))) {
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

node_t *assignconv(struct type * ty, node_t * node)
{
    struct type *ty2;

    if (isfunc(AST_TYPE(node)) || isarray(AST_TYPE(node)))
        node = decay(node);
    if (islvalue(node))
        node = ltor(node);

    ty2 = AST_TYPE(node);

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

static node_t *literal_expr(struct token *t, int id, void (*cnst) (struct token *, struct symbol *))
{
    const char *name = TOK_LIT_STR(t);
    struct symbol *sym = lookup(name, constants);
    if (!sym) {
        sym = install(name, &constants, CONSTANT, PERM);
        cnst(t, sym);
    }
    node_t *expr = ast_expr(id, SYM_TYPE(sym), NULL, NULL);
    AST_SRC(expr) = t->src;
    EXPR_SYM(expr) = sym;
    return expr;
}

node_t *new_integer_literal(int i)
{
    struct token *t = new_token(&(struct token){
            .id = ICONSTANT, .u.lit.str = strd(i), .u.lit.v.i = i});
    return literal_expr(t, INTEGER_LITERAL, integer_constant);
}

static node_t *new_uint_literal(unsigned long l)
{
    struct token *t = new_token(&(struct token){
            .id = ICONSTANT, .u.lit.str = stru(l), .u.lit.v.u = l});
    return literal_expr(t, INTEGER_LITERAL, integer_constant);
}

node_t *new_string_literal(const char *string)
{
    struct token *t = new_token(&(struct token){
            .id = SCONSTANT, .u.lit.str = format("\"%s\"", string)});
    return literal_expr(t, STRING_LITERAL, string_constant);
}

static struct vector *argcast1(struct type **params, size_t nparams,
                               node_t **args, size_t nargs,
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
        node_t *arg = args[i];
        struct type *src = AST_TYPE(arg);
        node_t *ret = assignconv(dst, arg);
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
        node_t *arg = args[i];
        vec_push(v, conva(arg));
    }

    return v;
}

static node_t **argscast(struct type *fty, node_t **args)
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

static node_t *compound_literal(struct type * ty)
{
    node_t *ret;
    node_t *inits;

    inits = initializer_list(ty);
    ret = ast_expr(COMPOUND_LITERAL, ty, inits, NULL);
    AST_SRC(ret) = AST_SRC(inits);
    
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

static node_t *make_ref_expr(struct symbol *sym, struct source src)
{
    node_t *ret = ast_expr(REF_EXPR, SYM_TYPE(sym), NULL, NULL);
    EXPR_SYM(ret) = sym;
    AST_SRC(ret) = src;
    SYM_REFS(sym)++;
    return ret;
}

/// primary-expression:
///   identifier
///   constant
///   string-literal
///   '(' expression ')'
///
static node_t *primary_expr(void)
{
    int t = token->id;
    struct symbol *sym;
    node_t *ret = NULL;

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
            node_t *e = expression();
            if (e) {
                ret =
                    ast_expr(PAREN_EXPR, AST_TYPE(e), e, NULL);
                AST_SRC(ret) = src;
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
static node_t *subscript(node_t * node)
{
    node_t *e;
    node_t *ret = NULL;
    struct source src = source;

    expect('[');
    e = conv(expression());
    expect(']');
    if (node == NULL || e == NULL)
        return ret;

    SAVE_ERRORS;
    bool kind1 = isptr(AST_TYPE(node)) && isint(AST_TYPE(e));
    bool kind2 = isint(AST_TYPE(node)) && isptr(AST_TYPE(e));
    if (kind1 || kind2) {
        struct type *ptr = isptr(AST_TYPE(node)) ? AST_TYPE(node) : AST_TYPE(e);
        if (isptrto(ptr, FUNCTION))
            error_at(src,
                     "subscript of pointer to function type '%s'",
                     type2s(rtype(ptr)));
    } else {
        if (!isptr(AST_TYPE(node)) && !isptr(AST_TYPE(e)))
            error_at(src, "subscripted value is not an array or pointer");
        else
            error_at(src, "array subscript is not an integer");
    }
    if (NO_ERROR) {
        struct type *ty = isptr(AST_TYPE(node)) ? AST_TYPE(node) : AST_TYPE(e);
        ret = ast_expr(SUBSCRIPT_EXPR, rtype(ty), node, e);
        AST_SRC(ret) = AST_SRC(node);
    }
    return ret;
}

/// argument-expression-list:
///   assignment-expression
///   argument-expression-list ',' assignment-expression
///
static node_t **argument_expr_list(void)
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

static node_t *flatten_call(node_t *node)
{
    switch (AST_ID(node)) {
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

static void builtin_funcall(node_t *call, node_t *ref)
{
    const char *fname = SYM_NAME(EXPR_SYM(ref));
    if (!strcmp(fname, BUILTIN_VA_ARG_P)) {
        // __builtin_va_arg_p
        node_t **args = EXPR_ARGS(call);
        node_t *arg1 = args[1];
        assert(isptr(AST_TYPE(arg1)));
        struct type *ty = rtype(AST_TYPE(arg1));
        // save the type
        EXPR_VA_ARG_TYPE(call) = ty;
        
        if (isrecord(ty) && TYPE_SIZE(ty) <= MAX_STRUCT_PARAM_SIZE) {
            const char *label = gen_tmpname();
            struct symbol *sym = make_localvar(label, ty, 0);
            // passing address
            node_t *operand = make_ref_expr(sym, SYM_SRC(sym));
            // update arg1
            args[1] = ast_uop('&', ptr_type(ty), operand);
        } else {
            // update arg1 to NULL
            args[1] = NULL;
        }
    }
}

static node_t *funcall(node_t * node)
{
    node_t **args;
    node_t *ret = NULL;
    struct source src = source;
    
    SAVE_ERRORS;
    expect('(');
    args = argument_expr_list();
    expect(')');
    if (node == NULL || HAS_ERROR)
        return ret;
    
    if (isptrto(AST_TYPE(node), FUNCTION)) {
        struct type *fty = rtype(AST_TYPE(node));
        if ((args = argscast(fty, args))) {
            ret = ast_expr(CALL_EXPR, rtype(fty), node, NULL);
            EXPR_ARGS(ret) = args;
            AST_SRC(ret) = src;
            vec_push(funcinfo.calls, ret);
            // handle builtin calls
            node_t *tmp = flatten_call(node);
            if (AST_ID(tmp) == REF_EXPR && isfunc(AST_TYPE(tmp)))
                builtin_funcall(ret, tmp);
        }
    } else {
        ensure_type(node, isfunc);
    }

    return ret;
}

// '.', '->'
static node_t *direction(node_t * node)
{
    int t = token->id;
    node_t *ret = NULL;
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
    struct type *ty = AST_TYPE(node);
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
            int q = qual_union(AST_TYPE(node), FIELD_TYPE(field));
            ret = ast_expr(MEMBER_EXPR, qual(q, FIELD_TYPE(field)), node,  NULL);
        } else {
            ret = ast_expr(MEMBER_EXPR,  FIELD_TYPE(field), node,  NULL);
        }
        AST_NAME(ret) = FIELD_NAME(field);
        EXPR_OP(ret) = t;
        AST_SRC(ret) = src;
    }
    return ret;
}

static void ensure_additive_ptr(node_t * node)
{
    assert(isptr(AST_TYPE(node)));
    struct type *rty = rtype(AST_TYPE(node));
    if (isfunc(rty) || isincomplete(rty))
        error_at(AST_SRC(node),
                 "increment/decrement of invalid type '%s' (pointer to unknown size)",
                 type2s(AST_TYPE(node)));
}

static void ensure_increment(node_t * node)
{
    ensure_type(node, isscalar);
    ensure_assignable(node);
    if (isptr(AST_TYPE(node)))
        ensure_additive_ptr(node);
}

static node_t *post_increment(node_t * node)
{
    int t = token->id;
    node_t *ret = NULL;
    struct source src = source;

    expect(t);
    if (node == NULL)
        return ret;

    SAVE_ERRORS;
    ensure_increment(node);
    if (NO_ERROR) {
        ret = ast_uop(t, AST_TYPE(node), node);
        AST_SRC(ret) = src;
    }
    return ret;
}

static node_t *postfix_expr1(node_t * ret)
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
static node_t *postfix_expr(void)
{
    node_t *expr = primary_expr();

    return postfix_expr1(expr);
}

static node_t *sizeof_expr(void)
{
    int t = token->id;
    node_t *ret = NULL;
    struct source src = source;

    expect(t);

    struct token *ahead = lookahead();
    node_t *n = NULL;
    struct type *ty = NULL;

    if (token->id == '(' && first_typename(ahead)) {
        ty = cast_type();
        if (token->id == '{') {
            node_t *node = compound_literal(ty);
            n = ast_uop(t, ty, postfix_expr1(node));
        }
    } else {
        n = unary_expr();
    }

    ty = n ? AST_TYPE(n) : ty;
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
        AST_SRC(ret) = src;
    }

    return ret;
}

static node_t *pre_increment(void)
{
    int t = token->id;
    node_t *ret = NULL;
    struct source src = source;

    expect(t);
    node_t *operand = unary_expr();
    if (operand == NULL)
        return ret;

    SAVE_ERRORS;
    ensure_increment(operand);
    if (NO_ERROR) {
        ret = ast_uop(t, AST_TYPE(operand), operand);
        EXPR_PREFIX(ret) = true;
        AST_SRC(ret) = src;
    }

    return ret;
}

static node_t *minus_plus(void)
{
    int t = token->id;
    node_t *ret = NULL;
    struct source src = source;

    expect(t);
    node_t *operand = conv(cast_expr());
    if (operand == NULL)
        return ret;

    SAVE_ERRORS;
    ensure_type(operand, isarith);
    if (NO_ERROR) {
        ret = ast_uop(t, AST_TYPE(operand), operand);
        AST_SRC(ret) = src;
    }

    return ret;
}

static node_t *bitwise_not(void)
{
    int t = token->id;
    node_t *ret = NULL;
    struct source src = source;

    expect(t);
    node_t *operand = conv(cast_expr());
    if (operand == NULL)
        return ret;

    SAVE_ERRORS;
    ensure_type(operand, isint);
    if (NO_ERROR) {
        ret = ast_uop(t, AST_TYPE(operand), operand);
        AST_SRC(ret) = src;
    }

    return ret;
}

static node_t *logical_not(void)
{
    int t = token->id;
    node_t *ret = NULL;
    struct source src = source;

    expect(t);
    node_t *operand = conv(cast_expr());
    if (operand == NULL)
        return ret;

    SAVE_ERRORS;
    ensure_type(operand, isscalar);
    if (NO_ERROR) {
        ret = ast_uop(t, inttype, operand);
        AST_SRC(ret) = src;
    }

    return ret;
}

/**
 * The usual conversions are _NOT_ applied to the operand of the '&'
 * operator, and its result is never an lvalue.
 */
static node_t *address(void)
{
    int t = token->id;
    node_t *ret = NULL;
    struct source src = source;

    expect(t);
    node_t *operand = cast_expr();
    if (operand == NULL)
        return ret;

    SAVE_ERRORS;
    if (!isfunc(AST_TYPE(operand))) {
        ensure_lvalue(operand);
        if (EXPR_SYM(operand)
            && SYM_SCLASS(EXPR_SYM(operand)) == REGISTER)
            error("address of register variable requested");
        else if (is_bitfield(operand))
            error("address of bitfield requested");
    }
    if (NO_ERROR) {
        ret = ast_uop(t, ptr_type(AST_TYPE(operand)), operand);
        AST_SRC(ret) = src;
    }

    return ret;
}

static node_t *indirection(void)
{
    int t = token->id;
    node_t *ret = NULL;
    struct source src = source;

    expect(t);
    node_t *operand = conv(cast_expr());
    if (operand == NULL)
        return ret;

    SAVE_ERRORS;
    ensure_type(operand, isptr);
    if (NO_ERROR) {
        ret = ast_uop(t, rtype(AST_TYPE(operand)), operand);
        AST_SRC(ret) = src;
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
static node_t *unary_expr(void)
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
static node_t *cast_expr(void)
{
    struct token *ahead = lookahead();
    struct source src = source;

    if (token->id == '(' && first_typename(ahead)) {
        struct type *ty = cast_type();
        if (token->id == '{') {
            node_t *node = compound_literal(ty);
            return postfix_expr1(node);
        }

        node_t *ret = NULL;
        node_t *cast = cast_expr();
        if (cast == NULL)
            return ret;
        cast = decay(cast);

        if (is_castable(ty, AST_TYPE(cast))) {
            ret = ast_expr(CAST_EXPR, ty, cast, NULL);
            AST_SRC(ret) = src;
        } else {
            error_at(AST_SRC(cast),
                     INCOMPATIBLE_TYPES,
                     type2s(AST_TYPE(cast)), type2s(ty));
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
static node_t *multiple_expr(void)
{
    node_t *mulp1;

    mulp1 = cast_expr();
    while (token->id == '*' || token->id == '/' || token->id == '%') {
        int t = token->id;
        SAVE_SOURCE;
        expect(t);
        mulp1 = bop(t, conv(mulp1), conv(cast_expr()));
        SET_SOURCE(mulp1);
    }

    return mulp1;
}

/// additive-expression:
///   multiplicative-expression
///   additive-expression '+' multiplicative-expression
///   additive-expression '-' multiplicative-expression
///
static node_t *additive_expr(void)
{
    node_t *add1;

    add1 = multiple_expr();
    while (token->id == '+' || token->id == '-') {
        int t = token->id;
        SAVE_SOURCE;
        expect(t);
        add1 = bop(t, conv(add1), conv(multiple_expr()));
        SET_SOURCE(add1);
    }

    return add1;
}

/// shift-expression:
///   additive-expression
///   shift-expression '<<' additive-expression
///   shift-expression '>>' additive-expression
///
static node_t *shift_expr(void)
{
    node_t *shift1;

    shift1 = additive_expr();
    while (token->id == LSHIFT || token->id == RSHIFT) {
        int t = token->id;
        SAVE_SOURCE;
        expect(t);
        shift1 = bop(t, conv(shift1), conv(additive_expr()));
        SET_SOURCE(shift1);
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
static node_t *relation_expr(void)
{
    node_t *rel;

    rel = shift_expr();
    while (token->id == '<' || token->id == '>' || token->id == LEQ || token->id == GEQ) {
        int t = token->id;
        SAVE_SOURCE;
        expect(t);
        rel = bop(t, conv(rel), conv(shift_expr()));
        SET_SOURCE(rel);
    }

    return rel;
}

/// equality-expression:
///   relational-expression
///   equality-expression '==' relational-expression
///   equality-expression '!=' relational-expression
///
static node_t *equality_expr(void)
{
    node_t *equl;

    equl = relation_expr();
    while (token->id == EQ || token->id == NEQ) {
        int t = token->id;
        SAVE_SOURCE;
        expect(t);
        equl = bop(t, conv(equl), conv(relation_expr()));
        SET_SOURCE(equl);
    }

    return equl;
}

/// AND-expression:
///   equality-expression
///   AND-expression '&' equality-expression
///
static node_t *and_expr(void)
{
    node_t *and1;

    and1 = equality_expr();
    while (token->id == '&') {
        SAVE_SOURCE;
        expect('&');
        and1 = bop('&', conv(and1), conv(equality_expr()));
        SET_SOURCE(and1);
    }

    return and1;
}

/// exclusive-OR-expression:
///   AND-expression
///   exclusive-OR-expression '^' AND-expression
///
static node_t *exclusive_or(void)
{
    node_t *eor;

    eor = and_expr();
    while (token->id == '^') {
        SAVE_SOURCE;
        expect('^');
        eor = bop('^', conv(eor), conv(and_expr()));
        SET_SOURCE(eor);
    }

    return eor;
}

/// inclusive-OR-expression:
///   exclusive-OR-expression
///   inclusive-OR-expression '|' exclusive-OR-expression
///
static node_t *inclusive_or(void)
{
    node_t *ior;

    ior = exclusive_or();
    while (token->id == '|') {
        SAVE_SOURCE;
        expect('|');
        ior = bop('|', conv(ior), conv(exclusive_or()));
        SET_SOURCE(ior);
    }

    return ior;
}

/// logical-AND-expression:
///   inclusive-OR-expression
///   logical-AND-expression '&&' inclusive-OR-expression
///
static node_t *logic_and(void)
{
    node_t *and1;

    and1 = inclusive_or();
    while (token->id == AND) {
        SAVE_SOURCE;
        expect(AND);
        and1 = logicop(AND, conv(and1), conv(inclusive_or()));
        SET_SOURCE(and1);
    }

    return and1;
}

/// logical-OR-expression:
///   logical-AND-expression
///   logical-OR-expression '||' logical-AND-expression
///
static node_t *logic_or(void)
{
    node_t *or1;

    or1 = logic_and();
    while (token->id == OR) {
        SAVE_SOURCE;
        expect(OR);
        or1 = logicop(OR, conv(or1), conv(logic_and()));
        SET_SOURCE(or1);
    }

    return or1;
}

static node_t *cond_expr1(node_t * cond)
{
#define INCOMPATIBLE_TYPES2                                             \
    "imcompatible types '%s' and '%s' in conditional expression"

    node_t *ret = NULL;
    node_t *then, *els;
    struct type *ty = NULL;
    struct source src = source;

    expect('?');
    then = conv(expression());
    expect(':');
    els = conv(cond_expr());

    if (cond == NULL || then == NULL || els == NULL)
        return ret;

    struct type *ty1 = AST_TYPE(then);
    struct type *ty2 = AST_TYPE(els);

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
        AST_SRC(ret) = src;
    }

    return ret;
}

/// conditional-expression:
///   logical-OR-expression
///   logical-OR-expression '?' expression ':' conditional-expression
///
static node_t *cond_expr(void)
{
    node_t *or1 = logic_or();
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
node_t *assign_expr(void)
{
    node_t *or1 = logic_or();
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
node_t *expression(void)
{
    node_t *assign1;

    assign1 = assign_expr();
    while (token->id == ',') {
        SAVE_SOURCE;
        expect(',');
        assign1 = commaop(',', assign1, assign_expr());
        SET_SOURCE(assign1);
    }
    return assign1;
}

node_t *bop(int op, node_t * l, node_t * r)
{
    node_t *node = NULL;
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
            ty = conv2(AST_TYPE(l), AST_TYPE(r));
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
            ty = conv2(AST_TYPE(l), AST_TYPE(r));
            node = ast_bop(op, ty, wrap(ty, l), wrap(ty, r));
        }
        break;
    case '+':
        if (isptr(AST_TYPE(l))) {
            ensure_additive_ptr(l);
            ensure_type(r, isint);
            if (NO_ERROR)
                node = ast_bop(op, AST_TYPE(l), l, r);
        } else if (isptr(AST_TYPE(r))) {
            ensure_additive_ptr(r);
            ensure_type(l, isint);
            if (NO_ERROR)
                node = ast_bop(op, AST_TYPE(r), l, r);
        } else {
            ensure_type(l, isarith);
            ensure_type(r, isarith);
            if (NO_ERROR) {
                ty = conv2(AST_TYPE(l), AST_TYPE(r));
                node =
                    ast_bop(op, ty, wrap(ty, l), wrap(ty, r));
            }
        }
        break;
    case '-':
        if (isptr(AST_TYPE(l))) {
            if (isint(AST_TYPE(r))) {
                ensure_additive_ptr(l);
                if (NO_ERROR)
                    node = ast_bop(op, AST_TYPE(l), l, r);
            } else if (isptr(AST_TYPE(r))) {
                ensure_additive_ptr(l);
                ensure_additive_ptr(r);
                struct type *rty1 = rtype(AST_TYPE(l));
                struct type *rty2 = rtype(AST_TYPE(r));
                if (!eqtype(unqual(rty1), unqual(rty2)))
                    error("'%s' and '%s' are not pointers to compatible types",
                          type2s(AST_TYPE(l)), type2s(AST_TYPE(r)));
                if (NO_ERROR)
                    node = ast_bop(op, inttype, l, r);
            } else {
                error("expect integer or pointer type, not type '%s'",
                      type2s(AST_TYPE(r)));
            }
        } else {
            ensure_type(l, isarith);
            ensure_type(r, isarith);
            if (NO_ERROR) {
                ty = conv2(AST_TYPE(l), AST_TYPE(r));
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
        if (isptr(AST_TYPE(l)) && isptr(AST_TYPE(r))) {
            // both ptr
            if (eqtype(AST_TYPE(l), AST_TYPE(r))) {
                node = ast_bop(op, inttype, l, r);
            } else if (op == EQ || op == NEQ) {
                if (isptrto(AST_TYPE(l), VOID)
                    || isptrto(AST_TYPE(r), VOID)) {
                    node_t *l1 = isptrto(AST_TYPE(l), VOID) ? l : ast_conv(ptr_type(voidtype), l, BitCast);
                    node_t *r1 = isptrto(AST_TYPE(r), VOID) ? r : ast_conv(ptr_type(voidtype), r, BitCast);
                    node = ast_bop(op, inttype, l1, r1);
                } else if (is_nullptr(l) || is_nullptr(r)) {
                    node = ast_bop(op, inttype, l, r);
                }
            }
            
            if (!node) {
                if (!opts.ansi)
                    node = ast_bop(op, inttype, l, ast_conv(AST_TYPE(l), r, BitCast));
                else
                    error("comparison of incompatible pointer types ('%s' and '%s')",
                          type2s(AST_TYPE(l)), type2s(AST_TYPE(r)));
            }
        } else if (isarith(AST_TYPE(l)) && isarith(AST_TYPE(r))) {
            // both arith
            ty = conv2(AST_TYPE(l), AST_TYPE(r));
            node = ast_bop(op, inttype, wrap(ty, l), wrap(ty, r));
        } else if (isptr(AST_TYPE(l))) {
            // ptr op int
            ensure_type(r, isint);
            if (NO_ERROR)
                node = ast_bop(op, inttype, l, ast_conv(AST_TYPE(l), r, IntegerToPointerCast));
        } else if (isptr(AST_TYPE(r))) {
            // int op ptr
            ensure_type(l, isint);
            if (NO_ERROR)
                node = ast_bop(op, inttype, ast_conv(AST_TYPE(r), l, IntegerToPointerCast), r);
        } else {
            error("comparison of invalid types ('%s' and '%s')",
                  type2s(AST_TYPE(l)), type2s(AST_TYPE(r)));
        }
        break;
    default:
        error("unknown op '%s'", id2s(op));
        assert(0);
    }
    return node;
}

static node_t *logicop(int op, node_t * l, node_t * r)
{
    node_t *ret = NULL;

    if (l == NULL || r == NULL)
        return NULL;

    SAVE_ERRORS;
    ensure_type(l, isscalar);
    ensure_type(r, isscalar);
    if (NO_ERROR)
        ret = ast_bop(op, inttype, l, r);

    return ret;
}

static node_t *commaop(int op, node_t * l, node_t * r)
{
    if (l == NULL || r == NULL)
        return NULL;

    if (isarray(AST_TYPE(l)) || isfunc(AST_TYPE(l)))
        l = decay(l);
    if (isarray(AST_TYPE(r)) || isfunc(AST_TYPE(r)))
        r = decay(r);
    if (islvalue(l))
        l = ltor(l);
    if (islvalue(r))
        r = ltor(r);

    return ast_bop(op, AST_TYPE(r), l, r);
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

static node_t *assignop(int op, node_t * l, node_t * r)
{
    node_t *ret = NULL;

    if (l == NULL || r == NULL)
        return NULL;

    struct type *retty = unqual(AST_TYPE(l));
    SAVE_ERRORS;
    ensure_assignable(l);
    if (HAS_ERROR)
        return NULL;
    if (op != '=') {
        // compound assignment
        int op2 = splitop(op);
        node_t *l1 = conv(l);
        node_t *r1 = conv(r);
        if (op2 == '+' || op2 == '-') {
            struct type *ty1 = AST_TYPE(l1);
            struct type *ty2 = AST_TYPE(r1);
            if (!((isarith(ty1) && isarith(ty2)) ||
                  (isptr(ty1) && isint(ty2))))
                error(INCOMPATIBLE_TYPES, type2s(ty2), type2s(ty1));
        }
        r = bop(op2, l1, r1);
    }

    if (NO_ERROR) {
        struct type *ty1 = AST_TYPE(l);
        struct type *ty2 = AST_TYPE(r);
        r = assignconv(retty, r);
        if (r)
            ret = ast_bop('=', retty, l, r);
        else
            error(INCOMPATIBLE_TYPES, type2s(ty2), type2s(ty1));
    }
    return ret;
}

static bool is_nullptr(node_t * node)
{
    assert(isptr(AST_TYPE(node)) || isint(AST_TYPE(node)));

    node_t *cnst = eval(node, inttype);
    if (cnst == NULL)
        return false;
    if (isiliteral(cnst))
        return SYM_VALUE(EXPR_SYM(cnst)).u == 0;
    return false;
}

long intexpr1(struct type * ty)
{
    struct source src = source;
    node_t *cond = cond_expr();
    if (cond == NULL)
        // parsing expression failed
        return 0;
    if (ty == NULL)
        ty = AST_TYPE(cond);
    if (!isint(AST_TYPE(cond)) || !isint(ty)) {
        error_at(src, "expression is not an integer constant expression");
        return 0;
    }
    node_t *cnst = eval(cond, ty);
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
node_t *bool_expr(void)
{
    // Conversion for expression in conditional statement
    node_t *node = expression();
    if (node == NULL)
        return NULL;
    // warning for assignment expression
    if (AST_ID(node) == BINARY_OPERATOR && EXPR_OP(node) == '=')
        warning("using the result of an assignment as a condition without parentheses '%s'",
                expr2s(node));
    if (islvalue(node))
        node = ltor(node);
    return decay(node);
}

// switch
node_t *switch_expr(void)
{
    node_t *node = conv(expression());
    if (node == NULL)
        return NULL;
    if (!isint(AST_TYPE(node))) {
        error("statement requires expression of integer type ('%s' invalid)",
              type2s(AST_TYPE(node)));
        return NULL;
    }
    return node;
}

node_t *decls2expr(struct symbol **decls)
{
    node_t *ret = NULL;

    for (int i = 0; decls[i]; i++) {
        struct symbol *sym = decls[i];
        if (SYM_INIT(sym) && SYM_SCLASS(sym) != STATIC) {
            SYM_X_KIND(sym) = SYM_KIND_LREF;
            node_t *l = make_ref_expr(sym, SYM_SRC(sym));
            node_t *r = SYM_INIT(sym);
            node_t *n = ast_bop('=', SYM_TYPE(sym), l, r);
            if (ret)
                ret = commaop(',', ret, n);
            else
                ret = n;
        }
    }

    return ret;
}
