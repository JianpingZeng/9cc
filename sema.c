#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include "cc.h"

// cast name
#define BitCast                 "BitCast"
#define LValueToRValue          "LValueToRValue"
#define FunctionToPointerDecay  "FunctionToPointerDecay"
#define ArrayToPointerDecay     "ArrayToPointerDecay"
#define IntegralCast            "IntegralCast"
#define FloatCast               "FloatingCast"
#define IntegerToFloatCast      "IntegralToFloating"
#define FloatToIntegerCast      "FloatingToIntegral"
#define PointerToBoolean        "PointerToBoolean"
#define IntegerToPointerCast    "IntegerToPointer"
#define PointerToIntegerCast    "PointerToInteger"

#define INTEGER_MAX(type)    (TYPE_LIMITS(type).max.i)
#define UINTEGER_MAX(type)   (TYPE_LIMITS(type).max.u)

static struct expr *new_uint_literal(unsigned long l);
static struct expr *make_ref_expr(struct symbol *sym, struct source src);

struct goto_info {
    const char *id;
    struct source src;
};

/// decl

static void ensure_bitfield(struct field *field)
{
    const char *name = FIELD_NAME(field);
    struct type *ty = FIELD_TYPE(field);
    struct source src = FIELD_SRC(field);
    int bitsize = FIELD_BITSIZE(field);
    int bits = BITS(TYPE_SIZE(ty));

    if (!isint(ty)) {
        if (name)
            error_at(src,
                     "bit-field '%s' has non-integral type '%s'",
                     name, type2s(ty));
        else
            error_at(src,
                     "anonymous bit-field has non-integral type '%s'",
                     type2s(ty));
    }

    if (bitsize < 0) {
        if (name)
            error_at(src,
                     "bit-field '%s' has negative width '%d'",
                     name, bitsize);
        else
            error_at(src,
                     "anonymous bit-field has negative width '%d'",
                     bitsize);
    }

    if (bitsize == 0 && name)
        error_at(src,
                 "named bit-field '%s' has zero width",
                 name);

    if (bitsize > bits) {
        if (name)
            error_at(src,
                     "size of bit-field '%s' (%d bits) exceeds size of its type (%d bits)",
                     name, bitsize, bits);
        else
            error_at(src,
                     "anonymous bit-field (%d bits) exceeds size of its type (%d bits)",
                     bitsize, bits);
    }
}

void ensure_inline(struct type *ty, int fspec, struct source src)
{
    if (fspec == INLINE) {
        if (isfunc(ty))
            TYPE_INLINE(ty) = 1;
        else
            error_at(src, "'inline' can only appear on functions");
    }
}

static void ensure_nonbitfield(struct field * field, size_t total, bool last)
{
    struct type *ty = FIELD_TYPE(field);
    struct source src = FIELD_SRC(field);
        
    if (isarray(ty)) {
        ensure_array(ty, source, CONSTANT);
        if (isincomplete(ty)) {
            if (last) {
                if (total == 1)
                    error_at(src,
                             "flexible array cannot be the only member");
            } else {
                error_at(src,
                         "field has incomplete type '%s'",
                         type2s(ty));
            }
        }
    } else if (isfunc(ty)) {
        error_at(src, "field has invalid type '%s'", TYPE_NAME(ty));
    } else if (isincomplete(ty)) {
        error_at(src, "field has incomplete type '%s'", type2s(ty));
    }
}

void ensure_field(struct field * field, size_t total, bool last)
{
    if (FIELD_ISBIT(field))
        ensure_bitfield(field);
    else
        ensure_nonbitfield(field, total, last);
}

void ensure_decl(struct symbol * sym, int sclass, int level)
{
    if (level == PARAM)
        return;

    struct type *ty = SYM_TYPE(sym);
    struct source src = SYM_SRC(sym);
    if (isvardecl(sym)) {
        if (isincomplete(ty) && SYM_DEFINED(sym))
            error_at(src, "variable has incomplete type '%s'", type2s(ty));
    }
}

/**
 *  1. Array qualifiers may appear only when in a function parameter.
 *
 *  2. Array qualifiers 'const', 'volatile', 'restrict', 'static' may
 *     appear within the _outermost_ brackets.
 *
 *  3. 'static' is an optimization hint, asserting that the actual array
 *     argument will be non-null and will have the declared size and
 *     type upon entry to the function.
 *
 *  4. The star modifier '*' or non-constant expression describe a
 *     variable length array. The '*' can only appear in array parameter
 *     declarations within function prototypes that are not part of
 *     a function definition.
 */
static void ensure_array_sub(struct type *atype, struct source src, int level, bool outermost)
{
    if (TYPE_A_STAR(atype) && level != PARAM)
        error_at(src, "star modifier used outside of function prototype");
    
    if (TYPE_A_CONST(atype) || TYPE_A_RESTRICT(atype) ||
        TYPE_A_VOLATILE(atype) || TYPE_A_STATIC(atype)) {
        if (level != PARAM)
            error_at(src,
                     "type qualifier used in array declarator outside of function prototype");
        else if (!outermost)
            error_at(src,
                     "type qualifier used in non-outermost array type derivation");
    }
            

    struct type *rty = rtype(atype);
    if (isarray(rty))
        ensure_array_sub(rty, src, level, false);
    else if (isfunc(rty))
        error_at(src, "array of function is invalid");
    
    set_typesize(atype);
}

void ensure_array(struct type * atype, struct source src, int level)
{
    ensure_array_sub(atype, src, level, true);

    struct type *rty = rtype(atype);
    if (isincomplete(rty))
        error_at(src, "array has incomplete element type '%s'", type2s(rty));
}

void ensure_func(struct type * ftype, struct source src)
{
    struct type *rty = rtype(ftype);
    if (isarray(rty))
        error_at(src, "function cannot return array type '%s'", type2s(rty));
    else if (isfunc(rty))
        error_at(src, "function cannot return function type '%s'", type2s(rty));
}

void ensure_main(struct type *ftype, const char *name, struct source src)
{
    if (!isfunc(ftype) || !name || strcmp(name, "main"))
        return;
    
    struct type *rty = rtype(ftype);
    struct type **params = TYPE_PROTO(ftype);
    size_t len = length(params);
    if (rty != inttype)
        error_at(src, "return type of 'main' is not 'int'");
    for (int i = 0; i < MIN(3, len); i++) {
        struct type *ty = params[i];
        if (i == 0) {
            if (ty != inttype)
                error_at(src, "first parameter of 'main' is not 'int'");
        } else if (i == 1 || i == 2) {
            if (!isptrto(ty, POINTER) ||
                !isptrto(rtype(ty), CHAR))
                error_at(src, "%s parameter of 'main' is not 'char **'",
                         i == 1 ? "second" : "third");
        }
    }
    if (len == 1 || len > 3)
        error_at(src,
                 "expect 0, 2 or 3 parameters for 'main', have %d",
                 len);
}

void ensure_params(struct symbol *params[])
{
    for (int i = 0; params[i]; i++) {
        struct symbol *sym = params[i];
        struct type *ty = SYM_TYPE(sym);
        // params id is required in prototype
        if (is_anonymous(SYM_NAME(sym)))
            error_at(SYM_SRC(sym), "parameter name omitted");
        if (isenum(ty) || isstruct(ty) || isunion(ty)) {
            if (!SYM_DEFINED(TYPE_TSYM(ty)))
                error_at(SYM_SRC(sym),
                         "variable has incomplete type '%s'",
                         type2s(ty));
        }
    }
}

void redefinition_error(struct source src, struct symbol * sym)
{
    error_at(src,
             "redefinition of '%s', previous definition at %s:%u:%u",
             SYM_NAME(sym),
             SYM_SRC(sym).file,
             SYM_SRC(sym).line,
             SYM_SRC(sym).column);
}

void conflicting_types_error(struct source src, struct symbol * sym)
{
    error_at(src,
             "conflicting types for '%s', previous at %s:%u:%u",
             SYM_NAME(sym),
             SYM_SRC(sym).file,
             SYM_SRC(sym).line,
             SYM_SRC(sym).column);
}

void field_not_found_error(struct type * ty, const char *name)
{
    if (isincomplete(ty))
        error("incomplete definition of type '%s'", type2s(ty));
    else
        error("'%s' has no field named '%s'", type2s(ty), name);
}

/// expr

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

static struct expr *commaop(struct expr *l, struct expr *r, struct source src)
{
    struct expr *ret;

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

    ret = ast_bop(',', EXPR_TYPE(r), l, r);
    EXPR_SRC(ret) = src;
    return ret;
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

static struct expr *assignop(int op, struct expr *l, struct expr *r, struct source src)
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
        r = actions.bop(op2, l1, r1, src);
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

static struct expr *condop(struct expr *cond, struct expr *then, struct expr *els, struct source src)
{
#define INCOMPATIBLE_TYPES2  "imcompatible types '%s' and '%s' in conditional expression"

    struct expr *ret = NULL;
    struct type *ty = NULL;
    
    cond = conv(cond);
    then = conv(then);
    els = conv(els);
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

static struct expr *logicop(int op, struct expr *l, struct expr *r, struct source src)
{
    struct expr *ret = NULL;

    l = conv(l);
    r = conv(r);
    if (l == NULL || r == NULL)
        return NULL;

    SAVE_ERRORS;
    ensure_type(l, isscalar);
    ensure_type(r, isscalar);
    if (NO_ERROR) {
        ret = ast_bop(op, inttype, l, r);
        EXPR_SRC(ret) = src;
    }

    return ret;
}

static struct expr *bop(int op, struct expr *l, struct expr *r, struct source src)
{
    struct expr *node = NULL;
    struct type *ty;

    l = conv(l);
    r = conv(r);
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

static struct expr *castop(struct type *ty, struct expr *cast, struct source src)
{
    struct expr *ret = NULL;

    if (cast == NULL)
        return NULL;
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

static struct expr *pre_increment(int t, struct expr *operand, struct source src)
{
    struct expr *ret = NULL;

    if (operand == NULL)
        return NULL;

    SAVE_ERRORS;
    ensure_increment(operand);
    if (NO_ERROR) {
        ret = ast_uop(t, EXPR_TYPE(operand), operand);
        EXPR_PREFIX(ret) = true;
        EXPR_SRC(ret) = src;
    }

    return ret;
}

static struct expr *minus_plus(int t, struct expr *operand, struct source src)
{
    struct expr *ret = NULL;

    operand = conv(operand);
    if (operand == NULL)
        return NULL;

    SAVE_ERRORS;
    ensure_type(operand, isarith);
    if (NO_ERROR) {
        ret = ast_uop(t, EXPR_TYPE(operand), operand);
        EXPR_SRC(ret) = src;
    }

    return ret;
}

static struct expr *bitwise_not(struct expr *operand, struct source src)
{
    struct expr *ret = NULL;

    operand = conv(operand);
    if (operand == NULL)
        return NULL;

    SAVE_ERRORS;
    ensure_type(operand, isint);
    if (NO_ERROR) {
        ret = ast_uop('~', EXPR_TYPE(operand), operand);
        EXPR_SRC(ret) = src;
    }

    return ret;
}

static struct expr *logical_not(struct expr *operand, struct source src)
{
    struct expr *ret = NULL;

    operand = conv(operand);
    if (operand == NULL)
        return NULL;

    SAVE_ERRORS;
    ensure_type(operand, isscalar);
    if (NO_ERROR) {
        ret = ast_uop('!', inttype, operand);
        EXPR_SRC(ret) = src;
    }

    return ret;
}

/**
 * The usual conversions are _NOT_ applied to the operand of the '&'
 * operator, and its result is never an lvalue.
 */
static struct expr *address(struct expr *operand, struct source src)
{
    struct expr *ret = NULL;

    if (operand == NULL)
        return NULL;

    SAVE_ERRORS;
    if (!isfunc(EXPR_TYPE(operand))) {
        ensure_lvalue(operand);
        if (EXPR_SYM(operand) && SYM_SCLASS(EXPR_SYM(operand)) == REGISTER)
            error("address of register variable requested");
        else if (is_bitfield(operand))
            error("address of bitfield requested");
    }
    if (NO_ERROR) {
        ret = ast_uop('&', ptr_type(EXPR_TYPE(operand)), operand);
        EXPR_SRC(ret) = src;
    }

    return ret;
}

static struct expr *indirection(struct expr *operand, struct source src)
{
    struct expr *ret = NULL;

    operand = conv(operand);
    if (operand == NULL)
        return NULL;

    SAVE_ERRORS;
    ensure_type(operand, isptr);
    if (NO_ERROR) {
        ret = ast_uop('*', rtype(EXPR_TYPE(operand)), operand);
        EXPR_SRC(ret) = src;
    }

    return ret;
}

static struct expr * sizeofop(struct type *ty, struct expr *n, struct source src)
{
    struct expr *ret = NULL;

    ty = n ? EXPR_TYPE(n) : ty;
    if (ty == NULL)
        return NULL;

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

// []
static struct expr * subscript(struct expr *node, struct expr *index, struct source src)
{
    struct expr *ret = NULL;

    node = conv(node);
    index = conv(index);
    if (node == NULL || index == NULL)
        return NULL;

    SAVE_ERRORS;
    bool kind1 = isptr(EXPR_TYPE(node)) && isint(EXPR_TYPE(index));
    bool kind2 = isint(EXPR_TYPE(node)) && isptr(EXPR_TYPE(index));
    if (kind1 || kind2) {
        struct type *ptr = isptr(EXPR_TYPE(node)) ? EXPR_TYPE(node) : EXPR_TYPE(index);
        if (isptrto(ptr, FUNCTION))
            error_at(src,
                     "subscript of pointer to function type '%s'",
                     type2s(rtype(ptr)));
    } else {
        if (!isptr(EXPR_TYPE(node)) && !isptr(EXPR_TYPE(index)))
            error_at(src, "subscripted value is not an array or pointer");
        else
            error_at(src, "array subscript is not an integer");
    }
    if (NO_ERROR) {
        struct type *ty = isptr(EXPR_TYPE(node)) ? EXPR_TYPE(node) : EXPR_TYPE(index);
        ret = ast_expr(SUBSCRIPT_EXPR, rtype(ty), node, index);
        EXPR_SRC(ret) = EXPR_SRC(node);
    }
    return ret;
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
            struct symbol *sym = mklocalvar(label, ty, 0);
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

static struct expr * funcall(struct expr *node, struct expr **args, struct source src)
{
    struct expr *ret = NULL;
    
    node = conv(node);
    if (node == NULL)
        return NULL;
    
    if (isptrto(EXPR_TYPE(node), FUNCTION)) {
        struct type *fty = rtype(EXPR_TYPE(node));
        if ((args = argscast(fty, args))) {
            ret = ast_expr(CALL_EXPR, rtype(fty), node, NULL);
            EXPR_ARGS(ret) = args;
            EXPR_SRC(ret) = src;
            vec_push(func.calls, ret);
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
static struct expr * direction(struct expr *node, int t, const char *name, struct source src)
{
    struct expr *ret = NULL;

    if (node == NULL || name == NULL)
        return NULL;

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

static struct expr * post_increment(struct expr *node, int t, struct source src)
{
    struct expr *ret = NULL;

    if (node == NULL)
        return NULL;

    SAVE_ERRORS;
    ensure_increment(node);
    if (NO_ERROR) {
        ret = ast_uop(t, EXPR_TYPE(node), node);
        EXPR_SRC(ret) = src;
    }
    return ret;
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

struct expr *new_int_literal(long i)
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

static struct expr *make_ref_expr(struct symbol *sym, struct source src)
{
    struct expr *ret = ast_expr(REF_EXPR, SYM_TYPE(sym), NULL, NULL);
    EXPR_SYM(ret) = sym;
    EXPR_SRC(ret) = src;
    SYM_REFS(sym)++;
    return ret;
}

static struct expr * id(struct token *tok)
{
    struct expr *ret = NULL;
    struct symbol *sym;

    sym = lookup(TOK_ID_STR(tok), identifiers);
    if (sym) {
        ret = make_ref_expr(sym, source);
        if (isenum(SYM_TYPE(sym)) && SYM_SCLASS(sym) == ENUM)
            EXPR_OP(ret) = ENUM;        // enum ids
    } else {
        error("use of undeclared identifier '%s'", tok2s(tok));
    }

    return ret;
}

static struct expr * iconst(struct token *tok)
{
    return literal_expr(tok, INTEGER_LITERAL, integer_constant);
}

static struct expr * fconst(struct token *tok)
{
    return literal_expr(tok, FLOAT_LITERAL, float_constant);
}

static struct expr * sconst(struct token *tok)
{
    return literal_expr(tok, STRING_LITERAL, string_constant);
}

static struct expr * paren(struct expr *e, struct source src)
{
    struct expr *ret = NULL;
    
    if (e) {
        ret = ast_expr(PAREN_EXPR, EXPR_TYPE(e), e, NULL);
        EXPR_SRC(ret) = src;
    }

    return ret;
}

static struct expr * compound_literal(struct type *ty, struct expr *inits, struct source src)
{
    struct expr *ret;
    
    ret = ast_expr(COMPOUND_LITERAL, ty, inits, NULL);
    EXPR_SRC(ret) = EXPR_SRC(inits);
    
    // define local variable
    if (SCOPE >= LOCAL) {
        const char *label = gen_compound_label();
        struct symbol *sym = mklocalvar(label, ty, 0);
        SYM_INIT(sym) = inits;
        // set sym
        EXPR_SYM(ret) = sym;
        SYM_REFS(sym)++;
    }

    return ret;
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

struct expr *binop(int op, struct expr *l, struct expr *r)
{
    return actions.bop(op, l, r, source);
}

struct expr *assign(struct symbol *sym, struct expr *r)
{
    struct expr *l = make_ref_expr(sym, SYM_SRC(sym));
    return ast_bop('=', SYM_TYPE(sym), l, r);
}

/// stmt

void ensure_return(struct expr *expr, bool isnull, struct source src)
{
    // return immediately if expr is NULL. (parsing failed)    
    if (expr == NULL)
        return;

    if (isvoid(rtype(func.type))) {
        if (!isnull && !isvoid(EXPR_TYPE(expr)))
            error_at(src, "void function should not return a value");
    } else {
        if (!isnull) {
            struct type *ty1 = EXPR_TYPE(expr);
            struct type *ty2 = rtype(func.type);
            if (!(expr = assignconv(ty2, expr)))
                error_at(src,
                         "returning '%s' from function with incompatible result type '%s'",
                         type2s(ty1), type2s(ty2));
        } else {
            error_at(src, "non-void function should return a value");
        }
    }
}

void check_case_duplicates(struct cse *cse, struct swtch *swtch)
{
    assert(cse && swtch);
    
    for (struct cse *c = swtch->cases; c; c = c->link) {
        if (c->value == cse->value) {
            struct source prev = c->src;
            error_at(cse->src,
                     "duplicate case value '%lld', previous case defined here: %s:%u:%u",
                     cse->value, prev.file, prev.line, prev.column);
            break;
        }
    }
}

void ensure_gotos(void)
{
    for (int i = 0; i < vec_len(func.gotos); i++) {
        struct goto_info *info = vec_at(func.gotos, i);
        const char *name = info->id;
        struct symbol *sym = lookup(name, func.labels);
        if (!sym || !SYM_DEFINED(sym))
            error_at(info->src, "use of undeclared label '%s'", name);
    }
}


void mark_goto(const char *id, struct source src)
{
    struct goto_info *info = xmalloc(sizeof(struct goto_info));
    info->id = id;
    info->src = src;
    vec_push(func.gotos, info);
}

static void branch(struct expr *expr, int tlab, int flab)
{
    assert(tlab == 0 || flab == 0);
    // TODO: 
}

static void jmpto(int label)
{
    // TODO: 
}

static void ret(struct expr *expr)
{
    // TODO: 
}

static void label(int label)
{
    // TODO: 
}

static void gen(struct expr *expr)
{
    // TODO: 
}

/// decl

static void dclvar(struct symbol *n)
{
    if (opts.ast_dump)
        print_symbol(n);
}

static void defvar(struct symbol *n)
{
    if (opts.ast_dump)
        print_symbol(n);
    else
        IR->defvar(n);
}

static void dclfun(struct symbol *n)
{
    if (opts.ast_dump)
        print_symbol(n);
}

static void defun(struct symbol *n)
{
    if (opts.ast_dump)
        print_symbol(n);
    else
        IR->defun(n);
}

static void deftype(struct symbol *n)
{
    if (opts.ast_dump)
        print_type(n);
}

/// init/finalize

static void init(int argc, char *argv[])
{
    IR->init(argc, argv);
}

static void finalize(void)
{
    if (opts.E || opts.ast_dump)
        return;
    IR->finalize();
}

struct actions actions = {
    .init = init,
    .finalize = finalize,

    // decl
    .dclvar = dclvar,
    .defvar = defvar,
    .dclfun = dclfun,
    .defun = defun,
    .deftype = deftype,

    // expr
    .commaop = commaop,
    .assignop = assignop,
    .condop = condop,
    .logicop = logicop,
    .bop = bop,
    .castop = castop,
    .pre_increment = pre_increment,
    .minus_plus = minus_plus,
    .bitwise_not = bitwise_not,
    .logical_not = logical_not,
    .address = address,
    .indirection = indirection,
    .sizeofop = sizeofop,
    .subscript = subscript,
    .funcall = funcall,
    .direction = direction,
    .post_increment = post_increment,
    .id = id,
    .iconst = iconst,
    .fconst = fconst,
    .sconst = sconst,
    .paren = paren,
    .compound_literal = compound_literal,

    // stmt
    .branch = branch,
    .jump = jmpto,
    .ret = ret,
    .label = label,
    .gen = gen,
};
