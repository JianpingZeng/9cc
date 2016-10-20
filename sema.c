#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include "cc.h"

#define INTEGER_MAX(type)    (TYPE_LIMITS(type).max.i)
#define UINTEGER_MAX(type)   (TYPE_LIMITS(type).max.u)
#define INIT(name)  .name = do_##name
#define call(func)  do_##func
#define events(func)  ev_##func

#define add_to_list(stmt)                       \
    do {                                        \
        *func.stmt = stmt;                      \
        func.stmt = &stmt->next;                \
    } while (0)

#define check_designator(d)  ensure_designator(d) ? (d) : NULL

#define ERR_INCOMPATIBLE_TYPES   "incompatible type conversion from '%s' to '%s'"
#define ERR_INCOMPATIBLE_TYPES2  "imcompatible types '%s' and '%s' in conditional expression"
#define ERR_REDEFINITION         "redefinition of '%s', previous definition at %s:%u:%u"
#define ERR_CONFLICTING_TYPES    "conflicting types for '%s', previous at %s:%u:%u"
#define ERR_DUPLICATE_MEMBER     "duplicate member '%s', previous declaration at %s:%u:%u"
#define ERR_TYPE                 "expect type '%s', not '%s'"
#define ERR_INLINE               "'inline' can only appear on functions"
#define ERR_ARRAY_OF_FUNC        "array of function is invalid"
#define ERR_FUNC_RET_ARRAY       "function cannot return array type '%s'"
#define ERR_FUNC_RET_FUNC        "function cannot return function type '%s'"
#define ERR_INCOMPLETE_VAR       "variable '%s' has incomplete type '%s'"
#define ERR_INCOMPLETE_ELEM      "array has incomplete element type '%s'"
#define ERR_INIT_EMPTY_RECORD    "initializer for aggregate with no elements requires explicit braces"
#define ERR_INIT_OVERRIDE        "initializer overrides prior initialization"

static struct expr *do_bop(int op, struct expr *l, struct expr *r, struct source src);
static struct expr *do_assignop(int op, struct expr *l, struct expr *r, struct source src);
static struct expr *assignconv(struct type *ty, struct expr *node);
static void init_string(struct type *ty, struct expr *node);
static void doglobal(struct symbol *sym, void *context);

struct func func;

///
/// events
///

// declare a global variable
static void ev_dclgvar(struct symbol *n)
{
    if (opts.ast_dump)
        ast_dump_vardecl(n);
}

// declare a function
static void ev_dclfun(struct symbol *n)
{
    if (opts.ast_dump)
        ast_dump_funcdecl(n);
}

// declare/define a type: struct/union/enum/typedef
static void ev_deftype(struct symbol *n)
{
    if (opts.ast_dump)
        ast_dump_typedecl(n);
}

// define a local variable
static void ev_deflvar(struct symbol *n)
{
    // TODO: 
}

// define a local static variable
static void ev_defsvar(struct symbol *n)
{
    if (opts.ast_dump || errors())
        return;
    IR->defvar(n);
}

// define a global variable
static void ev_defgvar(struct symbol *n)
{
    if (opts.ast_dump) {
        ast_dump_vardecl(n);
        return;
    }
    if (errors())
        return;
    IR->defvar(n);
}

// define a function
static void ev_defun(struct symbol *n)
{
    if (opts.ast_dump) {
        ast_dump_funcdef(n);
        return;
    }
    if (errors())
        return;
    IR->defun(n);
}

// a funcall
static void ev_funcall(struct expr *call)
{
    // compare the func.call and call
    // and set the larger to func.call
    // TODO: 
}

/// init/finalize

static void init(int argc, char *argv[])
{
    IR->init(argc, argv);
}

static void finalize(void)
{
    if (opts.ast_dump || errors())
        return;
    foreach(identifiers, GLOBAL, doglobal, NULL);
    IR->finalize();
}

///
/// type check
///

/*
 * Check for:
 * - `array of function`
 * - `function returning array`
 * - `function returning function`
 * recursively. Above cases are always invalid.
 */

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
static void ensure_func_array(struct type *ty, bool param, struct source src, bool outermost)
{
    if (isarray(ty)) {
        struct type *rty = rtype(ty);

        if (isfunc(rty))
            error_at(src, ERR_ARRAY_OF_FUNC);
        
        if (TYPE_A_STAR(ty) && !param)
            error_at(src, "star modifier used outside of function prototype");

        if (TYPE_A_CONST(ty) ||
            TYPE_A_RESTRICT(ty) ||
            TYPE_A_VOLATILE(ty) ||
            TYPE_A_STATIC(ty)) {
            if (!param)
                error_at(src,
                         "type qualifier used in array declarator outside of function prototype");
            if (!outermost)
                error_at(src,
                         "type qualifier used in non-outermost array type derivation");
        }

        ensure_func_array(rty, param, src, false);
        set_typesize(ty);       // calculate array size
    } else if (isfunc(ty)) {
        struct type *rty = rtype(ty);
        if (isarray(rty))
            error_at(src, ERR_FUNC_RET_ARRAY, type2s(rty));
        else if (isfunc(rty))
            error_at(src, ERR_FUNC_RET_FUNC, type2s(rty));

        ensure_func_array(rty, false, src, true);
    } else if (isptr(ty)) {
        struct type *rty = rtype(ty);
        ensure_func_array(rty, param, src, false);
    }
}

static void finish_type(struct type *ty, bool param, struct source src)
{
    ensure_func_array(ty, param, src, true);
}

static void check_func_array_in_funcdef(struct type *ty, struct source src)
{
    if (isarray(ty)) {
        struct type *rty = rtype(ty);
        if (TYPE_A_STAR(ty))
            error_at(src, "variable length array must be bound in function definition");
        check_func_array_in_funcdef(rty, src);
    } else if (isptr(ty)) {
        struct type *rty = rtype(ty);
        check_func_array_in_funcdef(rty, src);
    }
}

static void ensure_inline(struct type *ty, int fspec, struct source src)
{
    if (fspec == INLINE) {
        if (isfunc(ty))
            TYPE_INLINE(ty) = 1;
        else
            error_at(src, ERR_INLINE);
    }
}

static void check_bitfield(struct field *p)
{
    struct type *ty = p->type;
    int bitsize = p->bitsize;
    int bits = BITS(TYPE_SIZE(ty));

    if (!isint(ty)) {
        if (p->name)
            error_at(p->src,
                     "bit-field '%s' has non-integral type '%s'",
                     p->name, type2s(ty));
        else
            error_at(p->src,
                     "anonymous bit-field has non-integral type '%s'",
                     type2s(ty));
    }

    if (bitsize < 0) {
        if (p->name)
            error_at(p->src,
                     "bit-field '%s' has negative width '%d'",
                     p->name, bitsize);
        else
            error_at(p->src,
                     "anonymous bit-field has negative width '%d'",
                     bitsize);
    }

    if (bitsize == 0 && p->name)
        error_at(p->src,
                 "named bit-field '%s' has zero width",
                 p->name);

    if (bitsize > bits) {
        if (p->name)
            error_at(p->src,
                     "size of bit-field '%s' (%d bits) exceeds size of its type (%d bits)",
                     p->name, bitsize, bits);
        else
            error_at(p->src,
                     "anonymous bit-field (%d bits) exceeds size of its type (%d bits)",
                     bitsize, bits);
    }
}

static void ensure_nonbitfield(struct field *p, bool one)
{
    struct type *ty = p->type;

    finish_type(ty, false,  p->src);

    if (isarray(ty)) {
        if (isincomplete(ty)) {
            if (one)
                error_at(p->src, "flexible array cannot be the only member");
            else if (p->link)   // NOT the last field
                error_at(p->src, "field has incomplete type '%s'", type2s(ty));
        }
    } else if (isfunc(ty)) {
        error_at(p->src, "field has invalid type '%s'", TYPE_NAME(ty));
    } else if (isincomplete(ty)) {
        error_at(p->src, "field has incomplete type '%s'", type2s(ty));
    }
}

static void ensure_fields(struct symbol *sym)
{
    struct field *first = sym->u.s.flist;
    bool one = first && first->link == NULL;

    for (struct field *p = first; p; p = p->link) {
        if (isindirect(p))
            continue;
        if (isbitfield(p))
            check_bitfield(p);
        else
            ensure_nonbitfield(p, one);
    }
}

static void check_main_func(struct type *ftype, const char *name, struct source src)
{
    assert(isfunc(ftype));
    assert(name);
    
    if (strcmp(name, "main"))
        return;
    
    struct type *rty = rtype(ftype);
    struct type **proto = TYPE_PROTO(ftype);
    size_t len = length(proto);

    if (rty != inttype && rty != voidtype)
        error_at(src, "return type of 'main' is not 'int'");

    for (int i = 0; i < MIN(3, len); i++) {
        struct type *ty = proto[i];
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

static void check_params_in_funcdef(struct symbol *params[])
{
    for (int i = 0; params[i]; i++) {
        struct symbol *sym = params[i];
        struct type *ty = sym->type;
        // parameter name is required in prototype
        if (sym->anonymous)
            error_at(sym->src, "parameter name omitted");
        // get the original type without decay
        if (isptr(ty) && TYPE_P_DECAY(ty))
            ty = TYPE_P_DECAY(ty);

        // check variable length array (star modifier)
        check_func_array_in_funcdef(ty, sym->src);
        
        // check incomplete type
        if (isenum(ty) || isstruct(ty) || isunion(ty)) {
            if (!TYPE_TSYM(ty)->defined)
                error_at(sym->src,
                         "variable has incomplete type '%s'",
                         type2s(ty));
        } else if (isarray(ty)) {
            struct type *rty = rtype(ty);
            if (isincomplete(rty))
                error_at(sym->src,
                         ERR_INCOMPLETE_ELEM,
                         type2s(rty));
        }
    }
}

static void ensure_return(struct expr *expr, bool isnull, struct source src)
{
    // return immediately if expr is NULL. (parsing failed)    
    if (expr == NULL)
        return;

    if (isvoid(rtype(func.type))) {
        if (!isnull && !isvoid(expr->type))
            error_at(src, "void function should not return a value");
    } else {
        if (!isnull) {
            struct type *ty1 = expr->type;
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

static void ensure_gotos(void)
{
    for (struct goinfo *p = func.gotos; p; p = p->link) {
        struct symbol *sym = lookup(p->id, func.labels);
        if (!sym || !sym->defined)
            error_at(p->src, "use of undeclared label '%s'", p->id);
    }
}

static struct expr *ensure_init(struct expr *init, struct type *ty, struct symbol *sym, struct source src)
{    
    // TODO:
    return init;
}

static bool ensure_designator(struct desig *d)
{
    if (isincomplete(d->type)) {
        int id;
        if (d->id == DESIG_FIELD)
            id = STRUCT;
        else if (d->id == DESIG_INDEX)
            id = ARRAY;
        else
            id = TYPE_KIND(d->type);

        error_at(d->src,
                 "%s designator of incomplete type '%s'",
                 id2s(id), type2s(d->type));
        return false;
    }
    return true;
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
static bool islvalue(struct expr *node)
{
    // TODO: 
    return true;
}

static bool ensure_assignable(struct expr *node, struct source src)
{
    struct type *ty = node->type;
    if (!islvalue(node)) {
        error_at(src, "expression is not assignable");
        return false;
    } else if (isarray(ty)) {
        error_at(src, "array type '%s' is not assignable", type2s(ty));
        return false;
    } else if (isconst(ty)) {
        error_at(src, "read-only variable is not assignable");
        return false;
    }
    return true;
}

static bool isbfield(struct expr *node)
{
    if (node->op != MEMBER)
        return false;

    return isbitfield(node->u.field);
}

static struct expr *cast(struct type *ty, struct expr *n)
{
    //TODO:
    return n;
}

static struct expr *wrap(struct type *ty, struct expr *node)
{
    assert(isarith(ty));
    assert(isarith(node->type));

    if (eqarith(ty, node->type))
        return node;
    else
        return cast(ty, node);
}

static struct expr *bitconv(struct type *ty, struct expr *node)
{
    if (eqtype(ty, node->type))
        return node;
    else
        return cast(ty, node);
}

static struct expr *decay(struct expr *node)
{
    assert(node);
    switch (TYPE_KIND(node->type)) {
    case FUNCTION:
        // FunctionToPointerDecay
        return cast(ptr_type(node->type), node);

    case ARRAY:
        // ArrayToPointerDecay
        return cast(ptr_type(rtype(node->type)), node);

    default:
        return node;
    }
}

static struct expr *ltor(struct expr *node)
{
    // LValueToRValue
    return cast(unqual(node->type), node);
}

static struct expr *lvalue(struct expr *n)
{
    assert(OPKIND(n->op) == INDIR);
    return n->kids[0];
}

static struct expr *rvalue(struct expr *n)
{
    struct type *ty;
    assert(isptr(n->type));
    
    ty = unqual(rtype(n->type));
    return ast_expr(mkop(INDIR, ty), ty, n, NULL);
}

static struct expr *rettype(struct expr *n, struct type *ty)
{
    struct expr *ret = ast_expr(n->op, ty, n->kids[0], n->kids[1]);
    ret->sym = n->sym;
    ret->u = n->u;
    ret->vtype = n->vtype;
    return ret;
}

// Universal Unary Conversion
static struct expr *conv(struct expr *node)
{
    if (node == NULL)
        return NULL;
    if (islvalue(node))
        node = ltor(node);

    switch (TYPE_KIND(node->type)) {
    case _BOOL:
    case CHAR:
    case SHORT:
        return cast(inttype, node);

    case ENUM:
        return cast(rtype(node->type), node);

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

    switch (TYPE_KIND(node->type)) {
    case FLOAT:
        return cast(doubletype, node);

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
    assert(isptr(node->type) || isint(node->type));

    struct expr *cnst = eval(node, inttype);
    if (cnst == NULL)
        return false;
    if (isiliteral(cnst))
        return cnst->sym->value.u == 0;
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

static struct expr *assignconv(struct type *ty, struct expr *node)
{
    struct type *ty2;

    if (isfunc(node->type) || isarray(node->type))
        node = decay(node);
    if (islvalue(node))
        node = ltor(node);

    ty2 = node->type;

    if (isarith(ty) && isarith(ty2)) {
        return wrap(ty, node);
    } else if (isbool(ty) && isptr(ty2)) {
        return cast(ty, node);
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

static bool can_cast(struct type * dst, struct type * src)
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

// return NULL on error.
static struct expr **argcast1(struct type **params, size_t nparams,
                              struct expr **args, size_t nargs,
                              bool oldstyle, struct source src)
{
    struct list *list = NULL;
    size_t ncmp;

    if (oldstyle)
        ncmp = MIN(nparams, nargs);
    else
        ncmp = nparams;

    for (size_t i = 0; i < ncmp; i++) {
        struct type *dty = params[i];
        struct expr *arg = args[i];
        struct type *sty = arg->type;
        struct expr *ret = assignconv(dty, arg);
        if (ret) {
            list = list_append(list, ret);
        } else {
            if (oldstyle) {
                warning_at(src, ERR_INCOMPATIBLE_TYPES, type2s(sty), type2s(dty));
            } else {
                error_at(src, ERR_INCOMPATIBLE_TYPES, type2s(sty), type2s(dty));
                return NULL;
            }
        }
    }
    for (size_t i = ncmp; i < nargs; i++) {
        struct expr *arg = args[i];
        list = list_append(list, conva(arg));
    }

    return ltoa(&list, FUNC);
}

/*
 * There are 5 cases:
 *
 * 1. function declaration with prototype
 * 2. function definition with prototype
 * 3. function declaration with oldstyle
 * 4. function definition with oldstyle
 * 5. no function declaration/definition found
 */
// return NULL on error.
static struct expr **argscast(struct type *fty, struct expr **args, struct source src)
{
    assert(isfunc(fty));
    
    struct list *list = NULL;
    struct type **params = TYPE_PROTO(fty);
    size_t len1 = length(params);
    size_t len2 = length(args);
    bool oldstyle = TYPE_OLDSTYLE(fty);

    if (oldstyle) {
        if (len1 > len2)
            warning_at(src, "too few arguments to function call");

        return argcast1(params, len1, args, len2, oldstyle, src);
    }

    if (len1 == 0) {
        if (len2 > 0) {
            error_at(src,
                     "too many arguments to function call, expected %d, have %d",
                     len1, len2);
            return NULL;
        }
        return ltoa(&list, FUNC);
    }

    bool vargs = TYPE_VARG(fty);
    if (len1 <= len2) {
        if (!vargs && len1 < len2) {
            error_at(src,
                     "too many arguments to function call, expected %d, have %d",
                     len1, len2);
            return NULL;
        }

        return argcast1(params, len1, args, len2, oldstyle, src);
    } else {
        if (vargs)
            error_at(src,
                     "too few arguments to function call, expected at least %d, have %d",
                     len1, len2);
        else
            error_at(src,
                     "too few arguments to function call, expected %d, have %d",
                     len1, len2);
        return NULL;
    }
}

static bool ensure_additive_ptr(struct expr *node, struct source src)
{
    assert(isptr(node->type));
    struct type *rty = rtype(node->type);
    if (isfunc(rty) || isincomplete(rty)) {
        error_at(src,
                 "increment/decrement of invalid type '%s' (pointer to unknown size)",
                 type2s(node->type));
        return false;
    }
    return true;
}

static bool ensure_increment(struct expr *node, struct source src)
{
    if (!isscalar(node->type)) {
        error_at(src, ERR_TYPE, "scalar", type2s(node->type));
        return false;
    }

    if (!ensure_assignable(node, src))
        return false;

    if (isptr(node->type))
        return ensure_additive_ptr(node, src);

    return true;
}

///
/// private
///

static void skip_balance(int l, int r, const char *name)
{
    int nests = 0;

    while (1) {
        if (token->id == EOI)
            break;
        if (token->id == r) {
            if (nests-- == 0)
                break;
        } else if (token->id == l) {
            nests++;
        }
        gettok();
    }

    if (token->id == r)
        gettok();
    else
        error("unclosed %s, missing '%s'", name, id2s(r));
}

static void skip_syntax(int (*first) (struct token *))
{    
    while (1) {
        if (token->id == EOI)
            break;
        if (first(token))
            break;
        gettok();
    }
}

static void field_not_found_error(struct source src, struct type *ty, const char *name)
{
    if (isincomplete(ty))
        error_at(src, "incomplete definition of type '%s'", type2s(ty));
    else
        error_at(src, "'%s' has no field named '%s'", type2s(ty), name);
}

static void oldparam(struct symbol *sym, void *context)
{
    struct symbol **params = context;

    assert(sym->name);

    // _NOT_ a variable
    if (sym->sclass == TYPEDEF || isfunc(sym->type)) {
        warning_at(sym->src, "empty declaraion");
        return;
    }
        
    for (int j = 0; params[j]; j++) {
        struct symbol *s = params[j];
        assert(s->name);
        if (s->name == sym->name) {
            // replace id with declared symbol
            params[j] = sym;
            return;
        }
    }

    // _NOT_ found in id list
    error_at(sym->src, "parameter named '%s' is missing", sym->name);
}

static void mkfuncdecl(struct symbol *sym, struct type *ty, int sclass, struct source src)
{
    sym->type = ty;
    sym->src = src;
    sym->defined = true;
    sym->sclass = sclass;
}

static void predefined_ids(void)
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
    struct type *type = array_type(qual(CONST, chartype));
    // initializer
    struct expr *literal = cnsts(func.name);
    init_string(type, literal);
    
    struct symbol *sym = mklocal("__func__", type, STATIC);
    sym->predefine = true;
    sym->u.init = literal;
}

static void func_body(struct symbol *sym)
{
    struct stmt *stmt = NULL;

    func.gotos = NULL;
    func.labels = new_table(NULL, LOCAL);
    func.type = sym->type;
    func.name = sym->name;
    func.xcall = NULL;
    func.stmt = &stmt;

    // compound statement
    compound_stmt(predefined_ids, 0, 0, NULL);
    // check goto labels
    ensure_gotos();

    // save
    sym->u.f.xcall = func.xcall;
    sym->u.f.stmt = stmt;

    free_table(func.labels);
    func.labels = NULL;
    func.type = NULL;
    func.name = NULL;
    func.xcall = NULL;
    func.stmt = NULL;
}

static void doglobal(struct symbol *sym, void *context)
{
    // typedefs and enum ids are _defined_
    if (sym->defined ||
        sym->sclass == EXTERN ||
        isfunc(sym->type))
        return;

    sym->defined = true;
    events(defgvar)(sym);
}

static void init_string(struct type *ty, struct expr *node)
{
    int len1 = TYPE_LEN(ty);
    int len2 = TYPE_LEN(node->type);
    if (len1 > 0) {
        if (len1 < len2 - 1)
            warning("initializer-string for char array is too long");
    } else if (isincomplete(ty)) {
        TYPE_LEN(ty) = len2;
        set_typesize(ty);
    }
}

static struct desig *next_designator1(struct desig *desig, bool initial)
{
    assert(desig);
    
    switch (desig->id) {
    case DESIG_FIELD:
        {
            struct desig *prev = desig->prev;

            assert(prev);
            assert(isrecord(prev->type));

            struct field *field = desig->u.field->link;
            // skip indirect field
            while (field && isindirect(field))
                field = field->link;
            if (field) {
                struct desig *d = new_desig_field(field, source);
                d->offset = prev->offset + field->offset;
                d->prev = copy_desig(prev);
                return check_designator(d);
            } else {
                return next_designator1(prev, false);
            }
        }
        break;

    case DESIG_INDEX:
        {
            struct desig *prev = desig->prev;

            assert(prev);
            assert(isarray(prev->type));

            size_t len = TYPE_LEN(prev->type);
            long idx = desig->u.index;
            if (len == 0 || idx < len - 1) {
                struct type *rty = desig->type;
                struct desig *d = new_desig_index(idx+1, source);
                d->type = rty;
                d->offset = desig->offset + TYPE_SIZE(rty);
                d->prev = copy_desig(prev);
                return check_designator(d);
            } else {
                return next_designator1(prev, false);
            }
        }
        break;

    case DESIG_NONE:
        assert(desig->prev == NULL);
        if (!initial) {
            error("excess elements in %s initializer", TYPE_NAME(desig->type));
            return NULL;
        }
        if (isrecord(desig->type)) {
            struct field *first = TYPE_FIELDS(desig->type);
            if (first) {
                struct desig *d = new_desig_field(first, source);
                d->offset = desig->offset + first->offset;
                d->prev = copy_desig(desig);
                return d;
            } else if (isincomplete(desig->type)) {
                error("initialize incomplete type '%s'", type2s(desig->type));
                return NULL;
            } else {
                // empty record
                error("excess elements in %s initializer", TYPE_NAME(desig->type));
                return NULL;
            }
        } else if (isarray(desig->type)) {
            struct type *rty = rtype(desig->type);
            struct desig *d = new_desig_index(0, source);
            d->type = rty;
            d->offset = desig->offset;
            d->prev = copy_desig(desig);
            return d;
        } else {
            return desig;
        }
        break;
    }
    
    CC_UNAVAILABLE
}

static void offset_init(struct desig *desig, struct expr *expr, struct init **ilist)
{
    assert(desig && expr && ilist);
    assert(!isarray(desig->type));

    struct type *ty = desig->type;
    struct init *p;
    
    if (isincomplete(ty) || isfunc(ty)) {
        error_at(desig->src, "'%s' cannot have an initializer",
                 TYPE_NAME(ty));
        return;
    }

    //possible: scalar/struct/union

    // assignment conversion
    struct expr *n = assignconv(ty, expr);
    if (n == NULL) {
        error_at(desig->src, ERR_INCOMPATIBLE_TYPES,
                 type2s(expr->type), type2s(ty));
        return;
    }

    // check override
    for (; (p = *ilist); ilist = &p->link) {
        if (p->offset > desig->offset)
            break;
        if (p->offset == desig->offset) {
            if (desig->id == DESIG_FIELD &&
                isbitfield(desig->u.field)) {
                // bitfield
                if (p->boff < desig->u.field->bitoff)
                    continue;
                else if (p->boff > desig->u.field->bitoff)
                    break;
                // fall through
            }

            // overlapped
            warning_at(desig->src, ERR_INIT_OVERRIDE);
            p = p->link;    // remove from the list
            break;
        }
    }

    // insert
    struct init *init = NEWS0(struct init, FUNC);
    init->type = desig->type;
    init->offset = desig->offset;
    init->body = n;
    if (desig->id == DESIG_FIELD) {
        assert(!isindirect(desig->u.field));
        init->boff = desig->u.field->bitoff;
        init->bsize = desig->u.field->bitsize;
    }
    init->link = p;
    *ilist = init;
}

static void string_init(struct desig *desig, struct expr *expr, struct init **pinit)
{
    assert(desig && expr && pinit);
    
    // TODO:
    dlog("%s: (offset=%ld) <expr %p> <string>", desig2s(desig), desig->offset, expr);
}

static struct expr *incr(int op, struct expr *expr, struct expr *cnst, struct source src)
{
    return call(assignop)('=', expr, call(bop)(op, expr, cnst, src), src);
}

static struct expr *mkref(struct symbol *sym, struct source src)
{
    int op;
    struct type *ty = sym->type;
    struct expr *ret;

    if (has_static_extent(sym))
        op = ADDRG;
    else if (sym->scope == PARAM)
        op = ADDRP;
    else
        op = ADDRL;

    if (isfunc(ty))
        ret = ast_expr(mkop(op, funcptype), ty, NULL, NULL);
    else if (isarray(ty))
        ret = ast_expr(mkop(op, voidptype), ty, NULL, NULL);
    else
        ret = ast_expr(mkop(op, voidptype), ptr_type(ty), NULL, NULL);
    
    ret->sym = sym;
    use(sym);

    if (isptr(ret->type))
        return rvalue(ret);
    else
        return ret;
}

// implicit function declaration: int id();
static struct symbol * implicit_func_decl(const char *id)
{
    struct type *ftype = func_type(inttype);
    struct list *list = NULL;
    ftype->u.f.oldstyle = true;
    ftype->u.f.proto = ltoa(&list, PERM);
            
    struct symbol *sym = install(id, &externals, GLOBAL, PERM);
    sym->sclass = EXTERN;
    sym->type = ftype;
    sym->src = source;

    events(dclfun)(sym);
    warning("implicit declaration of '%s'", id);

    return sym;
}

static void integer_constant(struct token *t, struct symbol * sym)
{
    int base = t->u.lit.base;
    int suffix = t->u.lit.suffix;
    unsigned long n = t->u.lit.v.u;
    struct type *ty;

    // character constant
    if (t->u.lit.chr) {
        bool wide = t->u.lit.chr == 2;
        sym->type = wide ? wchartype : unsignedchartype;
        sym->value.u = wide ? (wchar_t)n : (unsigned char)n;
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

    sym->type = ty;
    sym->value = t->u.lit.v;
}

static void float_constant(struct token *t, struct symbol * sym)
{
    int suffix = t->u.lit.suffix;
    switch (suffix) {
    case FLOAT:
        sym->type = floattype;
        break;
    case LONG + DOUBLE:
        sym->type = longdoubletype;
        break;
    default:
        sym->type = doubletype;
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
        free(ws);
        assert(wlen <= len + 1);
        ty = array_type(wchartype);
        TYPE_LEN(ty) = wlen;
        set_typesize(ty);
    } else {
        ty = array_type(chartype);
        TYPE_LEN(ty) = strlen(s) - 1;
        set_typesize(ty);
    }
    sym->type = ty;
}

static struct expr *literal_expr(struct token *t, int op,
                                 void (*cnst) (struct token *, struct symbol *))
{
    struct expr *expr;
    const char *name = TOK_LIT_STR(t);
    struct symbol *sym = lookup(name, constants);
    if (!sym) {
        sym = install(name, &constants, CONSTANT, PERM);
        cnst(t, sym);
    }
    if (t->id == SCONSTANT)
        expr = ast_expr(mkop(op, voidptype), sym->type, NULL, NULL);
    else
        expr = ast_expr(mkop(op, sym->type), sym->type, NULL, NULL);
    expr->sym = sym;
    return expr;
}

static int top(int op)
{
    switch (op) {
    case '*': return MUL;
    case '/': return DIV;
    case '%': return MOD;
    case LSHIFT: return SHL;
    case RSHIFT: return SHR;
    case '&': return BAND;
    case '^': return XOR;
    case '|': return BOR;
    case '+': return ADD;
    case '-': return SUB;
    case '>': return GT;
    case '<': return LT;
    case GEQ: return GE;
    case LEQ: return LE;
    case EQL: return EQ;
    case NEQ: return NE;
    default: assert(0 && "unexpected binary operator");
    }
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

///
/// actions
///

/// decl

static struct symbol *do_enum_id(const char *name, int val, struct symbol *sym, struct source src)
{
    struct symbol *s = lookup(name, identifiers);
    if (s && is_current_scope(s))
        error_at(src, ERR_REDEFINITION,
                 name, s->src.file, s->src.line, s->src.column);

    s = install(name, &identifiers, cscope, cscope < LOCAL ? PERM : FUNC);
    s->type = sym->type;
    s->src = src;
    s->sclass = ENUM;
    s->value.u = val;
    s->defined = true;
    return s;
}

static void do_direct_field(struct symbol *sym, struct field *field)
{
    struct field **pp = &sym->u.s.flist;
    struct field *p;

    while ((p = *pp)) {
        if (field->name && field->name == p->name)
            error_at(field->src,
                     ERR_DUPLICATE_MEMBER,
                     field->name, p->src.file, p->src.line, p->src.column);
        pp = &p->link;
    }

    *pp = field;
}

static void do_indirect_field(struct symbol *sym, struct field *field)
{
    struct field *first = TYPE_FIELDS(field->type);
    struct field **pp;
    struct field *indir = NULL;
    struct field **indirp = &indir;

    call(direct_field)(sym, field);
    
    for (struct field *q = first; q; q = q->link) {
        struct field *p;
        
        pp = &sym->u.s.flist;
        while ((p = *pp)) {
            if (q->name && q->name == p->name)
                error_at(q->src,
                         ERR_DUPLICATE_MEMBER,
                         q->name, p->src.file, p->src.line, p->src.column);
            pp = &p->link;
        }
        if (isindirect(q)) {
            struct field *n = new_indirect_field(q->indir);
            struct list *list = list_append(NULL, field);
            for (int i = 0; q->of[i]; i++)
                list = list_append(list, q->of[i]);
            n->of = ltoa(&list, PERM);
            n->offset = q->offset;
            *indirp = n;
            indirp = &n->link;
        } else if (q->name) {
            struct field *n = new_indirect_field(q);
            struct list *list = list_append(NULL, field);
            n->of = ltoa(&list, PERM);
            n->offset = q->offset;
            *indirp = n;
            indirp = &n->link;
        }
    }

    if (indir)
        *pp = indir;
}

static void do_array_index(struct type *atype, struct expr *assign, struct source src)
{
    if (!assign)
        return;

    if (isint(assign->type)) {
        TYPE_A_ASSIGN(atype) = assign;
        // try evaluate the length
        struct expr *n = eval(assign, longtype);
        if (n) {
            assert(isiliteral(n));
            TYPE_LEN(atype) = n->sym->value.i;
            if (n->sym->value.i < 0)
                error_at(src, "array has negative size");
        } else {
            error_at(src, "expect constant expression");
        }
    } else {
        error_at(src,
                 "size of array has non-integer type '%s'",
                 type2s(assign->type));
    }
}

static struct symbol ** do_prototype(struct type *ftype, struct symbol *params[])
{    
    for (int i = 0; params[i]; i++) {
        struct symbol *p = params[i];
        struct type *ty = p->type;
        if (isvoid(ty)) {
            if (i == 0) {
                if (!p->anonymous) {
                    error_at(p->src,
                             "argument may not have 'void' type");
                    p->type = inttype;
                } else if (isqual(ty)) {
                    error_at(p->src,
                             "'void' as parameter must not have type qualifier");
                    p->type = inttype;
                } else if (TYPE_VARG(ftype)) {
                    error_at(p->src,
                             "'void' must be the first and only parameter if specified");
                    p->type = inttype;
                }
            } else {
                error_at(p->src,
                         "'void' must be the first and only parameter if specified");
                p->type = inttype;
            }
        }
    }

    // make it empty
    if (length(params) == 1 && isvoid(params[0]->type))
        params[0] = NULL;

    return params;
}

static void do_enumdecl(struct symbol *sym, struct symbol *ids[])
{
    sym->defined = true;
    sym->u.s.ids = ids;
    events(deftype)(sym);
}

static void do_recorddecl(struct symbol *sym)
{
    ensure_fields(sym);
    sym->defined = true;
    set_typesize(sym->type);
    events(deftype)(sym);
}

static struct symbol *do_globaldecl(const char *id, struct type *ty, int sclass, int fspec, struct source src)
{
    struct symbol *sym;

    assert(id);
    assert(cscope == GLOBAL);

    if (sclass == AUTO || sclass == REGISTER) {
        error_at(src, "illegal storage class on file-scoped variable");
        sclass = 0;
    }

    finish_type(ty, false, src);

    if (isfunc(ty))
        check_main_func(ty, id, src);

    ensure_inline(ty, fspec, src);

    sym = lookup(id, identifiers);
    if (!sym || sym->scope != cscope) {
        sym = install(id, &identifiers, cscope, PERM);
        sym->type = ty;
        sym->src = src;
        sym->sclass = sclass;
    } else if (eqtype(ty, sym->type)) {
        if (sclass == STATIC && sym->sclass != STATIC)
            error_at(src, "static declaration of '%s' follows non-static declaration", id);
        else if (sym->sclass == STATIC && sclass != STATIC)
            error_at(src, "non-static declaration of '%s' follows static declaration", id);

        if (sclass != EXTERN)
            sym->sclass = sclass;
    } else {
        error_at(src, ERR_CONFLICTING_TYPES,
                 sym->name, sym->src.file, sym->src.line, sym->src.column);
    }

    if (token->id == '=') {
        gettok();
        if (!(isscalar(ty) || isarray(ty) || isrecord(ty))) {
            error("'%s' cannot have an initializer", TYPE_NAME(ty));
            initializer(NULL);
        } else if (istag(ty) && isincomplete(ty)) {
            error("variable '%s' has incomplete type '%s'", id, type2s(ty));
            initializer(NULL);
        } else {
            struct source src2 = source;
            struct expr *init = initializer(ty);

            if (sclass == EXTERN)
                warning_at(src, "'extern' variable has an initializer");

            if (sym->defined)
                error_at(src, ERR_REDEFINITION,
                         sym->name, sym->src.file, sym->src.line, sym->src.column);

            init = ensure_init(init, ty, sym, src2);
            sym->u.init = init;
        }

        sym->defined = true;
    }

    // check incomplete type after intialized
    if (isincomplete(ty))
        error_at(src, ERR_INCOMPLETE_VAR, id, type2s(ty));

    // actions
    if (sym->u.init)
        events(defgvar)(sym);
    else if (isfunc(ty))
        events(dclfun)(sym);
    else
        events(dclgvar)(sym);

    return sym;
}

static struct symbol *do_localdecl(const char *id, struct type * ty, int sclass, int fspec, struct source src)
{
    struct symbol *sym;

    assert(id);
    assert(cscope >= LOCAL);

    if (sclass == 0)
        sclass = isfunc(ty) ? EXTERN : AUTO;

    finish_type(ty, false, src);
    
    if (isfunc(ty)) {
        check_main_func(ty, id, src);
        if (sclass != EXTERN) {
            error_at(src,
                     "function declared in block scope cannot have '%s' storage class",
                     id2s(sclass));
            sclass = EXTERN;
        }
    }

    ensure_inline(ty, fspec, src);

    sym = lookup(id, identifiers);
    if (sclass == EXTERN) {
        if (sym == NULL || !is_current_scope(sym) || eqtype(ty, sym->type)) {
            struct symbol *p = lookup(id, globals);
            if (p == NULL || eqtype(ty, p->type)) {
                p = lookup(id, externals);
                if (p && !eqtype(ty, p->type))
                    error_at(src, ERR_REDEFINITION,
                             p->name, p->src.file, p->src.line, p->src.column);
            } else {
                error_at(src, ERR_REDEFINITION,
                         p->name, p->src.file, p->src.line, p->src.column);
            }
        } else {
            error_at(src, ERR_REDEFINITION,
                     sym->name, sym->src.file, sym->src.line, sym->src.column);
        }
    } else {
        if (sym && is_current_scope(sym))
            error_at(src, ERR_REDEFINITION,
                     sym->name, sym->src.file, sym->src.line, sym->src.column);
    }

    sym = install(id, &identifiers, cscope, sclass == EXTERN ? PERM : FUNC);
    sym->type = ty;
    sym->src = src;
    sym->sclass = sclass;
    if (sclass != EXTERN)
        sym->defined = true;

    if (sclass == EXTERN) {
        struct symbol *p = install(id, &externals, GLOBAL, PERM);
        p->type = ty;
        p->src = src;
        p->sclass = EXTERN;
    }

    if (token->id == '=') {
        gettok();
        if (!(isscalar(ty) || isarray(ty) || isrecord(ty))) {
            error("'%s' cannot have an initializer", TYPE_NAME(ty));
            initializer(NULL);
        } else if (sclass == EXTERN) {
            error("'extern' variable cannot have an initializer");
            initializer(NULL);
        } else if (istag(ty) && isincomplete(ty)) {
            error("variable '%s' has incomplete type '%s'", id, type2s(ty));
            initializer(NULL);
        } else {
            struct source src2 = source;
            struct expr *init = initializer(ty);
            init = ensure_init(init, ty, sym, src2);
            if (init) {
                sym->u.init = init;
                // gen assign expr
                if (sclass != STATIC)
                    actions.gen(assign(sym, init));
            }
        }
    }

    // check incomplete type after initialized
    if (isincomplete(ty))
        error_at(src, ERR_INCOMPLETE_VAR, id, type2s(ty));

    // actions
    if (isfunc(ty))
        events(dclfun)(sym);
    else if (sclass == EXTERN)
        events(dclgvar)(sym);
    else if (sclass == STATIC)
        events(defsvar)(sym);
    else
        events(deflvar)(sym);

    return sym;
}

// id maybe NULL
static struct symbol *do_paramdecl(const char *id, struct type * ty, int sclass, int fspec, struct source src)
{
    struct symbol *sym;
    bool nonnull = false;

    if (sclass && sclass != REGISTER) {
        error_at(src,
                 "invalid storage class specifier '%s' in function declarator",
                 id2s(sclass));
        sclass = 0;
    }

    if (fspec == INLINE)
        error_at(src, ERR_INLINE);

    finish_type(ty, true, src);

    if (isfunc(ty)) {
        struct type *fty = ty;
        ty = ptr_type(fty);
        ty->u.p.decay = fty;
    } else if (isarray(ty)) {
        struct type *aty = ty;
        struct type *rty = rtype(ty);
        // check incomplete
        if (isincomplete(rty))
            error_at(src, ERR_INCOMPLETE_ELEM, type2s(rty));
        
        ty = ptr_type(rty);
        ty->u.p.decay = aty;
        // apply array qualifiers
        if (TYPE_A_CONST(aty))
            ty = qual(CONST, ty);
        if (TYPE_A_VOLATILE(aty))
            ty = qual(RESTRICT, ty);
        if (TYPE_A_RESTRICT(aty))
            ty = qual(VOLATILE, ty);
        if (TYPE_A_STATIC(aty))
            nonnull = true;
    } else if (isenum(ty) || isstruct(ty) || isunion(ty)) {
        if (!TYPE_TSYM(ty)->defined || TYPE_TSYM(ty)->scope == cscope)
            warning_at(src,
                       "declaration of '%s' will not be visible outside of this function",
                       type2s(ty));
    }
        
    if (id) {
        sym = lookup(id, identifiers);
        if (sym && sym->scope == cscope)
            error_at(src, ERR_REDEFINITION,
                     sym->name, sym->src.file, sym->src.line, sym->src.column);
        sym = install(id, &identifiers, cscope, FUNC);
    } else {
        sym = anonymous(&identifiers, cscope, FUNC);
    }
    
    sym->type = ty;
    sym->src = src;
    sym->sclass = sclass;
    sym->nonnull = nonnull;
    sym->defined = true;

    if (token->id == '=') {
        error("C does not support default arguments");
        initializer(NULL);
    }
    
    return sym;
}

// level: GLOBAL/PARAM/LOCAL
static void do_typedefdecl(const char *id, struct type *ty, int fspec, int level, struct source src)
{
    int sclass = TYPEDEF;
    struct symbol *sym;

    assert(id);

    if (level == PARAM)
        error_at(src,
                 "invalid storage class specifier '%s' in function declarator",
                 id2s(sclass));

    if (fspec == INLINE)
        error_at(src, ERR_INLINE);

    finish_type(ty, level == PARAM, src);

    sym = lookup(id, identifiers);
    if (sym && is_current_scope(sym))
        error_at(src, ERR_REDEFINITION,
                 sym->name, sym->src.file, sym->src.line, sym->src.column);

    sym = install(id, &identifiers, cscope, cscope < LOCAL ? PERM : FUNC);
    sym->type = ty;
    sym->src = src;
    sym->sclass = sclass;
    sym->defined = true;

    if (token->id == '=') {
        error("illegal initializer (only variable can be initialized)");
        initializer(NULL);
    }

    events(deftype)(sym);
}

// id maybe NULL
static void do_funcdef(const char *id, struct type *ty, int sclass, int fspec,
                       struct symbol *params[], struct source src)
{
    struct symbol *sym;

    assert(cscope == PARAM);

    if (sclass && sclass != EXTERN && sclass != STATIC) {
        error("invalid storage class specifier '%s'", id2s(sclass));
        sclass = 0;
    }

    finish_type(ty, false, src);
    
    if (id) {
        sym = lookup(id, identifiers);
        if (!sym || sym->scope != GLOBAL) {
            sym = install(id, &identifiers, GLOBAL, PERM);
            mkfuncdecl(sym, ty, sclass, src);
        } else if (eqtype(ty, sym->type) && !sym->defined) {
            if (sclass == STATIC && sym->sclass != STATIC)
                error_at(src,
                         "static declaaration of '%s' follows non-static declaration",
                         id);
            else
                mkfuncdecl(sym, ty, sclass, src);
        } else {
            error_at(src, ERR_REDEFINITION,
                     sym->name, sym->src.file, sym->src.line, sym->src.column);
        }

        check_main_func(ty, id, src);
    } else {
        sym = anonymous(&identifiers, GLOBAL, PERM);
        mkfuncdecl(sym, ty, sclass, src);
    }

    if (fspec == INLINE)
        TYPE_INLINE(ty) = INLINE;

    // old style function parameters declaration
    if (TYPE_OLDSTYLE(ty)) {
        int i;

        foreach(identifiers, PARAM, oldparam, params);

        for (i = 0; params[i]; i++) {
            struct symbol *p = params[i];
            if (!p->defined)
                params[i] = call(paramdecl)(p->name, inttype, 0, 0, p->src);
            // check void
            if (isvoid(p->type)) {
                error_at(p->src, "argument may not have 'void' type");
                p->type = inttype;
            }
        }

        struct type **proto = newarray(sizeof(struct type *), length(params) + 1, PERM);
        for (i = 0; params[i]; i++)
            proto[i] = params[i]->type;

        proto[i] = NULL;
        TYPE_PROTO(ty) = proto;
    }

    TYPE_PARAMS(ty) = params;
    check_params_in_funcdef(params);

    if (token->id == '{') {
        // function definition
        func_body(sym);
        exit_scope();
        events(defun)(sym);
    } else {
        // oldstyle
        assert(TYPE_OLDSTYLE(ty));
        error("expect function body after function declarator");
    }
}

/// expr

static struct expr *do_commaop(struct expr *l, struct expr *r, struct source src)
{
    if (l == NULL || r == NULL)
        return NULL;

    if (isarray(l->type) || isfunc(l->type))
        l = decay(l);
    if (isarray(r->type) || isfunc(r->type))
        r = decay(r);
    if (islvalue(l))
        l = ltor(l);
    if (islvalue(r))
        r = ltor(r);

    return ast_expr(RIGHT, r->type, l, r);
}

static struct expr *do_assignop(int op, struct expr *l, struct expr *r, struct source src)
{
    if (l == NULL || r == NULL)
        return NULL;

    if (!ensure_assignable(l, src))
        return NULL;

    if (op != '=') {
        // compound assignment
        int op2 = splitop(op);
        struct expr *l1 = conv(l);
        struct expr *r1 = conv(r);
        if (op2 == '+' || op2 == '-') {
            struct type *ty1 = l1->type;
            struct type *ty2 = r1->type;
            if (!((isarith(ty1) && isarith(ty2)) ||
                  (isptr(ty1) && isint(ty2)))) {
                error_at(src, ERR_INCOMPATIBLE_TYPES, type2s(ty2), type2s(ty1));
                return NULL;
            }
        }
        r = call(bop)(op2, l1, r1, src);
    }

    struct type *retty = unqual(l->type);
    struct type *ty1 = l->type;
    struct type *ty2 = r->type;

    r = assignconv(retty, r);
    if (!r) {
        error_at(src, ERR_INCOMPATIBLE_TYPES, type2s(ty2), type2s(ty1));
        return NULL;
    }

    return ast_expr(mkop(ASGN, retty), retty, l, r);
}

static struct expr *do_condop(struct expr *cond, struct expr *then, struct expr *els, struct source src)
{
    struct type *ty = NULL;
    
    cond = conv(cond);
    then = conv(then);
    els = conv(els);
    if (cond == NULL || then == NULL || els == NULL)
        return NULL;

    if (!isscalar(cond->type)) {
        error_at(src, ERR_TYPE, "scalar", type2s(cond->type));
        return NULL;
    }
    
    struct type *ty1 = then->type;
    struct type *ty2 = els->type;

    if (isarith(ty1) && isarith(ty2)) {
        ty = conv2(ty1, ty2);
        then = wrap(ty, then);
        els = wrap(ty, els);
    } else if ((isstruct(ty1) && isstruct(ty2)) ||
               (isunion(ty1) && isunion(ty2))) {
        if (!eqtype(ty1, ty2)) {
            error_at(src, ERR_INCOMPATIBLE_TYPES2, type2s(ty1), type2s(ty2));
            return NULL;
        }
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
                error_at(src, ERR_INCOMPATIBLE_TYPES2, type2s(ty1), type2s(ty2));
                return NULL;
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
                error_at(src, ERR_INCOMPATIBLE_TYPES2, type2s(ty1), type2s(ty2));
                return NULL;
            }
        }
    } else if (!opts.ansi &&
               ((isptr(ty1) && isint(ty2)) ||
                (isint(ty1) && isptr(ty2)))) {
        ty = isptr(ty1) ? ty1 : ty2;
        then = bitconv(ty, then);
        els = bitconv(ty, els);
    } else {
        error_at(src, "type mismatch in conditional expression: '%s' and '%s'",
                 type2s(ty1), type2s(ty2));
        return NULL;
    }

    return ast_expr(COND, ty, cond, ast_expr(RIGHT, ty, then, els));
}

static struct expr *do_logicop(int op, struct expr *l, struct expr *r, struct source src)
{
    assert(op == ANDAND || op == OROR);
    
    l = conv(l);
    r = conv(r);
    if (l == NULL || r == NULL)
        return NULL;

    if (!isscalar(l->type)) {
        error_at(src, ERR_TYPE, "scalar", type2s(l->type));
        return NULL;
    }
    if (!isscalar(r->type)) {
        error_at(src, ERR_TYPE, "scalar", type2s(r->type));
        return NULL;
    }

    return ast_expr(op == ANDAND ? AND : OR, inttype, l, r);
}

static struct expr *do_bop(int op, struct expr *l, struct expr *r, struct source src)
{
    struct type *ty;

    l = conv(l);
    r = conv(r);
    if (l == NULL || r == NULL)
        return NULL;

    switch (op) {
    case '*':
    case '/':
        if (!isarith(l->type)) {
            error_at(src, ERR_TYPE, "arith", type2s(l->type));
            return NULL;
        }
        if (!isarith(r->type)) {
            error_at(src, ERR_TYPE, "arith", type2s(r->type));
            return NULL;
        }
        ty = conv2(l->type, r->type);
        return ast_expr(mkop(top(op), ty), ty, wrap(ty, l), wrap(ty, r));
    case '%':
    case LSHIFT:
    case RSHIFT:
    case '&':
    case '^':
    case '|':
        if (!isint(l->type)) {
            error_at(src, ERR_TYPE, "integer", type2s(l->type));
            return NULL;
        }
        if (!isint(r->type)) {
            error_at(src, ERR_TYPE, "integer", type2s(r->type));
            return NULL;
        }
        ty = conv2(l->type, r->type);
        return ast_expr(mkop(top(op), ty), ty, wrap(ty, l), wrap(ty, r));
    case '+':
        if (isptr(l->type) && isint(r->type)) {
            if (!ensure_additive_ptr(l, src))
                return NULL;

            size_t size = TYPE_SIZE(rtype(l->type));
            struct expr *mul = call(bop)('*', r, cnsti(size, unsignedlongtype), src);
            return ast_expr(mkop(top(op), l->type), l->type, l, mul);
        } else if (isptr(r->type) && isint(l->type)) {
            if (!ensure_additive_ptr(r, src))
                return NULL;

            size_t size = TYPE_SIZE(rtype(r->type));
            struct expr *mul = call(bop)('*', l, cnsti(size, unsignedlongtype), src);
            return ast_expr(mkop(top(op), r->type), r->type, mul, r);
        } else {
            if (!isarith(l->type)) {
                error_at(src, ERR_TYPE, "arith", type2s(l->type));
                return NULL;
            }
            if (!isarith(r->type)) {
                error_at(src, ERR_TYPE, "arith", type2s(r->type));
                return NULL;
            }
            ty = conv2(l->type, r->type);
            return ast_expr(mkop(top(op), ty), ty, wrap(ty, l), wrap(ty, r));
        }
    case '-':
        if (isptr(l->type) && isint(r->type)) {
            if (!ensure_additive_ptr(l, src))
                return NULL;

            size_t size = TYPE_SIZE(rtype(l->type));
            struct expr *mul = call(bop)('*', r, cnsti(size, unsignedlongtype), src);
            return ast_expr(mkop(top(op), l->type), l->type, l, mul);
        } else if (isptr(l->type) && isptr(r->type)) {
            if (!ensure_additive_ptr(l, src) || !ensure_additive_ptr(r, src))
                return NULL;
            struct type *rty1 = rtype(l->type);
            struct type *rty2 = rtype(r->type);
            if (!eqtype(unqual(rty1), unqual(rty2))) {
                error_at(src, "'%s' and '%s' are not pointers to compatible types",
                         type2s(l->type), type2s(r->type));
                return NULL;
            }
            
            return ast_expr(mkop(top(op), l->type), inttype, l, r);
        } else {
            if (!isarith(l->type)) {
                error_at(src, ERR_TYPE, "arith", type2s(l->type));
                return NULL;
             }
            if (!isarith(r->type)) {
                error_at(src, ERR_TYPE, "arith", type2s(r->type));
                return NULL;
            }
            ty = conv2(l->type, r->type);
            return ast_expr(mkop(top(op), ty), ty, wrap(ty, l), wrap(ty, r));
        }
        // scalar op scalar
    case '>':
    case '<':
    case LEQ:
    case GEQ:
    case EQL:
    case NEQ:
        if (isptr(l->type) && isptr(r->type)) {
            // both ptr
            if (eqtype(l->type, r->type))
                return ast_expr(mkop(top(op), l->type), inttype, l, r);

            if (op == EQL || op == NEQ) {
                if (isptrto(l->type, VOID)) {
                    ty = ptr_type(voidtype);
                    return ast_expr(mkop(top(op), ty), inttype, l, cast(ty, r));
                } else if (isptrto(r->type, VOID)) {
                    ty = ptr_type(voidtype);
                    return ast_expr(mkop(top(op), ty), inttype, cast(ty, l), r);
                } else if (is_nullptr(l)) {
                    return ast_expr(mkop(top(op), r->type), inttype, l, r);
                } else if (is_nullptr(r)) {
                    return ast_expr(mkop(top(op), l->type), inttype, l, r);
                }
            }
            
            if (!opts.ansi) {
                return ast_expr(mkop(top(op), l->type), inttype, l, cast(l->type, r));
            } else {
                error_at(src, "comparison of incompatible pointer types ('%s' and '%s')",
                         type2s(l->type), type2s(r->type));
                return NULL;
            }
        } else if (isarith(l->type) && isarith(r->type)) {
            // both arith
            ty = conv2(l->type, r->type);
            return ast_expr(mkop(top(op), ty), inttype, wrap(ty, l), wrap(ty, r));
        } else if (isptr(l->type) && isint(r->type)) {
            // ptr op int
            return ast_expr(mkop(top(op), l->type), inttype, l, cast(l->type, r));
        } else if (isptr(r->type) && isint(l->type)) {
            // int op ptr
            return ast_expr(mkop(top(op), r->type), inttype, cast(r->type, l), r);
        } else {
            error_at(src, "comparison of invalid types ('%s' and '%s')",
                     type2s(l->type), type2s(r->type));
            return NULL;
        }
    default:
        assert(0 && "unknown binary operator");
    }
}

/// cast

static struct expr *do_castop(struct type *ty, struct expr *cast, struct source src)
{
    int op;

    if (cast == NULL)
        return NULL;
    cast = decay(cast);

    if (!can_cast(ty, cast->type)) {
        error_at(src,
                 ERR_INCOMPATIBLE_TYPES,
                 type2s(cast->type), type2s(ty));
        return NULL;
    }

    switch (TYPE_OP(cast->type)) {
    case INT:
        op = mkop(CVI, ty);
        break;

    case UNSIGNED:
        op = mkop(CVU, ty);
        break;

    case FLOAT:
        op = mkop(CVF, ty);
        break;

    case POINTER:
        op = mkop(CVP, ty);
        break;

    default:
        assert(0 && "unknown source type in cast");
    }

    return ast_expr(op, ty, cast, NULL);
}

/// unary

static struct expr * do_pre_increment(int t, struct expr *operand, struct source src)
{
    if (operand == NULL)
        return NULL;

    if (!ensure_increment(operand, src))
        return NULL;

    return incr(t == INCR ? ADD : SUB, operand, cnsti(1, inttype), src);
}

static struct expr * do_minus_plus(int t, struct expr *operand, struct source src)
{
    operand = conv(operand);
    if (operand == NULL)
        return NULL;

    if (!isarith(operand->type)) {
        error_at(src, ERR_TYPE, "arith", type2s(operand->type));
        return NULL;
    }

    if (t == '+')
        return operand;
    else
        return ast_expr(mkop(NEG, operand->type), operand->type, operand, NULL);
}

static struct expr * do_bitwise_not(struct expr *operand, struct source src)
{
    operand = conv(operand);
    if (operand == NULL)
        return NULL;

    if (!isint(operand->type)) {
        error_at(src, ERR_TYPE, "integer", type2s(operand->type));
        return NULL;
    }
    
    return ast_expr(mkop(NOT, operand->type), operand->type, operand, NULL);
}

static struct expr * do_logical_not(struct expr *operand, struct source src)
{    
    operand = conv(operand);
    if (operand == NULL)
        return NULL;

    if (!isscalar(operand->type)) {
        error_at(src, ERR_TYPE, "scalar", type2s(operand->type));
        return NULL;
    }

    struct expr *t1 = mkref(mklocal(gen_tmpname(), inttype, REGISTER), src);
    struct expr *t2 = mkref(mklocal(gen_tmpname(), inttype, REGISTER), src);
    struct expr *then = call(assignop)('=', t1, cnsti(0, inttype), src);
    struct expr *els = call(assignop)('=', t2, cnsti(1, inttype), src);

    return call(condop)(operand, then, els, src);
}

/**
 * The usual conversions are _NOT_ applied to the operand of the '&'
 * operator, and its result is never an lvalue.
 */
static struct expr * do_address(struct expr *operand, struct source src)
{
    if (operand == NULL)
        return NULL;

    struct type *ty = operand->type;
    if (!isfunc(ty)) {
        if (!islvalue(operand)) {
            error_at(src, "expect lvalue");
            return NULL;
        }
        if (operand->sym && operand->sym->sclass == REGISTER) {
            error_at(src, "address of register variable requested");
            return NULL;
        } else if (isbfield(operand)) {
            error_at(src, "address of bitfield requested");
            return NULL;
        }
    } 

    if (isfunc(ty) || isarray(ty))
        return rettype(operand, ptr_type(ty));
    else
        return lvalue(operand);
}

static struct expr * do_indirection(struct expr *operand, struct source src)
{
    operand = conv(operand);
    if (operand == NULL)
        return NULL;

    if (!isptr(operand->type)) {
        error_at(src, ERR_TYPE, "pointer", type2s(operand->type));
        return NULL;
    }

    return rvalue(operand);
}

static struct expr * do_sizeofop(struct type *ty, struct expr *n, struct source src)
{
    ty = n ? n->type : ty;
    if (ty == NULL)
        return NULL;

    if (isfunc(ty) || isvoid(ty)) {
        error_at(src, "'sizeof' to a '%s' type is invalid", type2s(ty));
        return NULL;
    } else if (isincomplete(ty)) {
        error_at(src, "'sizeof' to an incomplete type '%s' is invalid", type2s(ty));
        return NULL;
    } else if (n && isbfield(n)) {
        error_at(src, "'sizeof' to a bitfield is invalid");
        return NULL;
    }

    return cnsti(TYPE_SIZE(ty), unsignedlongtype);
}

/// postfix

// e1[e2]
// == *(e1+e2)
static struct expr * do_subscript(struct expr *node, struct expr *index, struct source src)
{
    node = conv(node);
    index = conv(index);
    if (node == NULL || index == NULL)
        return NULL;

    bool kind1 = isptr(node->type) && isint(index->type);
    bool kind2 = isint(node->type) && isptr(index->type);
    if (kind1 || kind2) {
        struct type *ptr = isptr(node->type) ? node->type : index->type;
        if (isptrto(ptr, FUNCTION)) {
            error_at(src,
                     "subscript of pointer to function type '%s'",
                     type2s(rtype(ptr)));
            return NULL;
        }
        
        struct expr *expr = call(bop)('+', node, index, src);
        return rvalue(expr);
    } else {
        if (!isptr(node->type) && !isptr(index->type))
            error_at(src, "subscripted value is not an array or pointer");
        else
            error_at(src, "array subscript is not an integer");

        return NULL;
    }
}

static struct expr * do_funcall(struct expr *node, struct expr **args, struct source src)
{
    struct expr *ret;
    
    node = conv(node);
    if (node == NULL)
        return NULL;

    if (!isptrto(node->type, FUNCTION)) {
        error_at(src, "expect type 'function', not '%s'", type2s(node->type));
        return NULL;
    }
    
    struct type *fty = rtype(node->type);
    if ((args = argscast(fty, args, src)) == NULL)
        return NULL;
    
    ret = ast_expr(CALL, rtype(fty), node, NULL);
    ret->u.args = args;
    
    events(funcall)(ret);

    return ret;
}

// '.', '->'
static struct expr * do_direction(struct expr *node, int t, const char *name, struct source src)
{
    struct expr *ret;
    struct field *field;
    struct type *ty, *fty;
    
    if (node == NULL || name == NULL)
        return NULL;
    
    ty = node->type;
    if (t == '.') {
        if (!isrecord(ty)) {
            error_at(src, "expect type 'struct/union', not '%s'", type2s(ty));
            return NULL;
        }
    } else {
        if (!isptr(ty) || !isrecord(rtype(ty))) {
            error_at(src, "pointer to struct/union type expected, not type '%s'", type2s(ty));
            return NULL;
        } else {
            ty = rtype(ty);
        }
    }

    field = find_field(ty, name);
    if (field == NULL) {
        field_not_found_error(src, ty, name);
        return NULL;
    }
    
    fty = direct(field)->type;
    if (opts.ansi) {
        // The result has the union of both sets of qualifiers.
        int q = qual_union(node->type, fty);
        fty = qual(q, fty);
    }
    ret = ast_expr(MEMBER, fty, node,  NULL);
    ret->u.field = field;
    return ret;
}

static struct expr * do_post_increment(struct expr *node, int t, struct source src)
{
    struct expr *ret;

    if (node == NULL)
        return NULL;

    if (!ensure_increment(node, src))
        return NULL;

    ret = ast_expr(RIGHT,
                   node->type,
                   ast_expr(RIGHT,
                            node->type,
                            node,
                            incr(t == INCR ? ADD : SUB, node, cnsti(1, inttype), src)),
                   node);
    return ret;
}

static struct expr * do_compound_literal(struct type *ty, struct expr *inits, struct source src)
{
    return inits;
}

/// primary

static struct expr * do_iconst(struct token *tok)
{
    return literal_expr(tok, CNST, integer_constant);
}

static struct expr * do_fconst(struct token *tok)
{
    return literal_expr(tok, CNST, float_constant);
}

static struct expr * do_sconst(struct token *tok)
{
    return literal_expr(tok, CNST, string_constant);
}

static struct expr * do_id(struct token *tok)
{
    const char *id = TOK_ID_STR(tok);
    struct symbol *sym;

    sym = lookup(id, identifiers);
    if (sym) {
        if (isenum(sym->type) && sym->sclass == ENUM)
            // enum ids
            return cnsti(sym->value.i, rtype(sym->type));
        else
            return mkref(sym, source);
    } else if (lookahead()->id == '(') {
        // lookup in externals
        sym = lookup(id, externals);
        if (sym == NULL) {
            // implicit function declaration: int id();
            sym = implicit_func_decl(id);
            return mkref(sym, source);
        } else if (isfunc(sym->type) || isptrto(sym->type, FUNCTION)) {
            warning("use of out-of-scope declaration of '%s', previous declaration is here: %s:%u:%u",
                    id, sym->src.file, sym->src.line, sym->src.column);
            return mkref(sym, source);
        } else {
            error("use of '%s' does not match previous declaration at: %s:%u:%u",
                  id, sym->src.file, sym->src.line, sym->src.column);
            return NULL;
        }
    } else {
        error("use of undeclared identifier '%s'", id);
        return NULL;
    }
}

static struct expr * do_paren(struct expr *e, struct source src)
{
    e->paren = true;
    return e;
}

/// constant-expression:
///   conditional-expression
///
static long do_intexpr(struct expr *cond, struct type *ty, struct source src)
{
    if (cond == NULL)
        // parsing expression failed
        return 0;
    if (ty == NULL)
        ty = cond->type;
    if (!isint(cond->type) || !isint(ty)) {
        error_at(src, "expression is not an integer constant expression");
        return 0;
    }
    struct expr *cnst = eval(cond, ty);
    if (cnst == NULL) {
        error_at(src, "expression is not a compile-time constant");
        return 0;
    }
    assert(isiliteral(cnst));
    return cnst->sym->value.i;
}

// if/do/while/for
static struct expr *do_bool_expr(struct expr *node, struct source src)
{
    // Conversion for expression in conditional statement
    if (node == NULL)
        return NULL;
    // warning for assignment expression
    if (node->op == ASGN && !node->paren)
        warning_at(src, "using the result of an assignment as a condition without parentheses");
    if (islvalue(node))
        node = ltor(node);
    return decay(node);
}

// switch
static struct expr *do_switch_expr(struct expr *expr, struct source src)
{
    struct expr *node = conv(expr);
    if (node == NULL)
        return NULL;
    if (!isint(node->type)) {
        error_at(src, "statement requires expression of integer type ('%s' invalid)",
                 type2s(node->type));
        return NULL;
    }
    return node;
}

/// init

static void do_element_init(struct desig **pdesig, struct expr *expr, struct init **pinit)
{
    struct desig *desig = *pdesig;
    if (!desig || !expr)
        return;

    if (isstruct(desig->type) || isunion(desig->type)) {
        struct field *first = TYPE_FIELDS(desig->type);
        if (first == NULL) {
            // empty record
            if (iszinit(expr))
                offset_init(desig, expr, pinit);
            else
                error_at(desig->src, ERR_INIT_EMPTY_RECORD);
        } else if (eqtype(unqual(desig->type), unqual(expr->type))) {
            offset_init(desig, expr, pinit);
        } else {
            // set to first field
            struct desig *d = new_desig_field(first, source);
            d->offset = desig->offset + first->offset;
            d->prev = desig;
            *pdesig = d;
            call(element_init)(pdesig, expr, pinit);
        }
    } else if (isarray(desig->type)) {
        if (isstring(desig->type) && issliteral(expr)) {
            // string
            string_init(desig, expr, pinit);
        } else {
            // set to first index
            struct type *rty = rtype(desig->type);
            struct desig *d = new_desig_index(0, source);
            d->type = rty;
            d->offset = desig->offset;
            d->prev = desig;
            *pdesig = d;
            call(element_init)(pdesig, expr, pinit);
        }
    } else {
        // scalar type
        if (desig->braces)
            warning_at(desig->src, "too many braces around scalar initializer");

        offset_init(desig, expr, pinit);
    }
}

static struct desig *do_designator(struct desig *desig, struct desig **ds)
{
    assert(desig && ds);

    desig = copy_desig(desig);

    for (int i = 0; ds[i]; i++) {
        struct desig *d = ds[i];
        switch (d->id) {
        case DESIG_FIELD:
            {
                const char *name = d->u.name;
                assert(name);
                if (!isrecord(desig->type)) {
                    error_at(d->src,
                             "%s designator cannot initialize non-%s type '%s'",
                             id2s(STRUCT), id2s(STRUCT), type2s(desig->type));
                    return NULL;
                }
                struct field *field = find_field(desig->type, name);
                if (!field) {
                    field_not_found_error(d->src, desig->type, name);
                    return NULL;
                }
                // indirect
                if (isindirect(field)) {
                    for (int i = 0; field->of[i]; i++) {
                        struct field *p = field->of[i];
                        struct desig *d = new_desig_field(p, p->src);
                        d->offset = desig->offset + p->offset;
                        d->prev = desig;
                        desig = d;
                    }
                    field = direct(field);
                }
                d->offset = desig->offset + field->offset;
                d->type = field->type;
                d->u.field = field;
                d->prev = desig;
                desig = d;

                // check incomplete type
                if (!ensure_designator(d))
                    return NULL;
            }
            break;

        case DESIG_INDEX:
            {
                if (!isarray(desig->type)) {
                    error_at(d->src,
                             "%s designator cannot initialize non-%s type '%s'",
                             id2s(ARRAY), id2s(ARRAY), type2s(desig->type));
                    return NULL;
                }
                size_t len = TYPE_LEN(desig->type);
                if (len && d->u.index >= len) {
                    error_at(d->src,
                             "array designator index [%ld] exceeds array bounds (%lu)",
                             d->u.index, len);
                    return NULL;
                }
                struct type *rty = rtype(desig->type);
                d->offset = desig->offset + d->u.index * TYPE_SIZE(rty);
                d->type = rty;
                d->prev = desig;
                desig = d;

                // check incomplete type
                if (!ensure_designator(d))
                    return NULL;
            }
            break;

        default:
            assert(0 && "unexpected designator id");
        }
    }
    
    return desig;
}

static struct expr * do_initializer_list(struct type *ty, struct init *init)
{
    struct expr *n = ast_expr(COMPOUND, ty, NULL, NULL);
    // TODO: incomplete array type
    // TODO: sort inits
    // TODO: merge bitfields
    n->u.inits = init;
    return n;
}

/// stmt

static void do_branch(struct expr *expr, int tlab, int flab)
{
    assert(tlab == 0 || flab == 0);
    struct stmt *stmt = ast_stmt(CBR);
    stmt->u.cbr.expr = expr;
    stmt->u.cbr.tlab = tlab;
    stmt->u.cbr.flab = flab;
    add_to_list(stmt);
}

static void do_jump(int label)
{
    struct stmt *stmt = ast_stmt(JMP);
    stmt->u.label = label;
    add_to_list(stmt);
}

static void do_ret(struct expr *expr, bool isnull, struct source src)
{
    ensure_return(expr, isnull, src);
    struct stmt *stmt = ast_stmt(RET);
    stmt->u.expr = expr;
    add_to_list(stmt);
}

static void do_label(int label)
{
    struct stmt *stmt = ast_stmt(LABEL);
    stmt->u.label = label;
    add_to_list(stmt);
}

static void do_gen(struct expr *expr)
{
    if (expr) {
        struct stmt *stmt = ast_stmt(GEN);
        stmt->u.expr = expr;
        add_to_list(stmt);
    }
}

///
/// public
///

int first_decl(struct token *t)
{
    return t->kind == STATIC || first_typename(t);
}

int first_stmt(struct token *t)
{
    return t->kind == IF || first_expr(t);
}

int first_expr(struct token *t)
{
    return t->kind == ID;
}

int first_typename(struct token * t)
{
    return t->kind == INT || t->kind == CONST ||
        (t->id == ID && istypedef(TOK_ID_STR(t)));
}

void skip_to_brace(void)
{
    skip_balance('{', '}', "brace");
}

void skip_to_bracket(void)
{
    skip_balance('(', ')', "bracket");
}

void skip_to_squarebracket(void)
{
    skip_balance('[', ']', "square bracket");
}

void skip_to_decl(void)
{
    skip_syntax(first_decl);
}

void skip_to_stmt(void)
{
    skip_syntax(first_stmt);
}

void skip_to_expr(void)
{
    skip_syntax(first_expr);
}

struct symbol *lookup_typedef(const char *id)
{
    if (!id)
        return NULL;

    struct symbol *sym = lookup(id, identifiers);

    if (sym && sym->sclass == TYPEDEF)
        return sym;
    else
        return NULL;
}

bool istypedef(const char *id)
{
    return lookup_typedef(id) != NULL;
}

struct symbol *tag_symbol(int t, const char *tag, struct source src)
{
    struct type *ty = tag_type(t);
    struct symbol *sym = NULL;
    if (tag) {
        sym = lookup(tag, tags);
        if (sym && is_current_scope(sym)) {
            if (TYPE_OP(sym->type) == t && !sym->defined)
                return sym;

            error_at(src, ERR_REDEFINITION,
                     sym->name, sym->src.file, sym->src.line, sym->src.column);
        }

        sym = install(tag, &tags, cscope, PERM);
    } else {
        sym = anonymous(&tags, cscope, PERM);
    }

    sym->type = ty;
    sym->src = src;
    ty->u.s.tsym = sym;

    return sym;
}

struct symbol *mklocal(const char *name, struct type * ty, int sclass)
{
    return call(localdecl)(name, ty, sclass, 0, source);
}

struct desig *next_designator(struct desig *desig)
{
    if (!desig)
        return NULL;

    return next_designator1(desig, true);
}

struct expr *cnsti(long i, struct type *ty)
{
    int op = TYPE_OP(ty);
    struct token t = {.id = ICONSTANT, .u.lit.str = op == INT ? strd(i) : stru(i), .u.lit.v.i = i};
    return literal_expr(&t, CNST, integer_constant);
}

struct expr *cnsts(const char *string)
{
    struct token t = {.id = SCONSTANT, .u.lit.str = format("\"%s\"", string)};
    return literal_expr(&t, CNST, string_constant);
}

struct expr *assign(struct symbol *sym, struct expr *r)
{
    struct expr *l = mkref(sym, sym->src);
    return call(assignop)('=', l, r, sym->src);
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

void mark_goto(const char *id, struct source src)
{
    struct goinfo *p = NEW(sizeof(struct goinfo), FUNC);
    p->id = id;
    p->src = src;
    p->link = func.gotos;
    func.gotos = p;
}

struct actions actions = {
    .init = init,
    .finalize = finalize,

    // decl
    INIT(enumdecl),
    INIT(recorddecl),
    INIT(globaldecl),
    INIT(localdecl),
    INIT(paramdecl),
    INIT(typedefdecl),
    INIT(funcdef),

    INIT(array_index),
    INIT(prototype),
    INIT(enum_id),
    INIT(direct_field),
    INIT(indirect_field),

    // expr
    INIT(commaop),
    INIT(assignop),
    INIT(condop),
    INIT(logicop),
    INIT(bop),
    INIT(castop),
    INIT(pre_increment),
    INIT(minus_plus),
    INIT(bitwise_not),
    INIT(logical_not),
    INIT(address),
    INIT(indirection),
    INIT(sizeofop),
    INIT(subscript),
    INIT(funcall),
    INIT(direction),
    INIT(post_increment),
    INIT(id),
    INIT(iconst),
    INIT(fconst),
    INIT(sconst),
    INIT(paren),
    INIT(compound_literal),

    INIT(intexpr),
    INIT(bool_expr),
    INIT(switch_expr),

    // stmt
    INIT(branch),
    INIT(jump),
    INIT(ret),
    INIT(label),
    INIT(gen),

    // init
    INIT(element_init),
    INIT(designator),
    INIT(initializer_list),
};
