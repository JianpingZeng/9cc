#include "cc.h"

static void ensure_bitfield(node_t *field)
{
    const char *name = FIELD_NAME(field);
    node_t *ty = FIELD_TYPE(field);
    struct source src = AST_SRC(field);
    int bitsize = FIELD_BITSIZE(field);
    int bits = BITS(TYPE_SIZE(ty));

    if (!isint(ty)) {
        if (name)
            errorf(src,
                   "bit-field '%s' has non-integral type '%s'",
                   name, type2s(ty));
        else
            errorf(src,
                   "anonymous bit-field has non-integral type '%s'",
                   type2s(ty));
    }

    if (bitsize < 0) {
        if (name)
            errorf(src,
                   "bit-field '%s' has negative width '%d'",
                   name, bitsize);
        else
            errorf(src,
                   "anonymous bit-field has negative width '%d'",
                   bitsize);
    }

    if (bitsize == 0 && name)
        errorf(src,
               "named bit-field '%s' has zero width",
               name);

    if (bitsize > bits) {
        if (name)
            errorf(src,
                   "size of bit-field '%s' (%d bits) exceeds size of its type (%d bits)",
                   name, bitsize, bits);
        else
            errorf(src,
                   "anonymous bit-field (%d bits) exceeds size of its type (%d bits)",
                   bitsize, bits);
    }
}

void ensure_inline(node_t *ty, int fspec, struct source src)
{
    if (fspec == INLINE) {
        if (isfunc(ty))
            TYPE_INLINE(ty) = 1;
        else
            errorf(src, "'inline' can only appear on functions");
    }
}

void check_oldstyle(node_t *ftype)
{
    assert(isfunc(ftype));
    
    if (TYPE_PARAMS(ftype) && TYPE_OLDSTYLE(ftype))
        error("a parameter list without types is only allowed in a function definition");
}

static void ensure_nonbitfield(node_t * field, size_t total, bool last)
{
    node_t *ty = FIELD_TYPE(field);
    struct source src = AST_SRC(field);
        
    if (isarray(ty)) {
        ensure_array(ty, source, CONSTANT);
        if (isincomplete(ty)) {
            if (last) {
                if (total == 1)
                    errorf(src,
                           "flexible array cannot be the only member");
            } else {
                errorf(src,
                       "field has incomplete type '%s'",
                       type2s(ty));
            }
        }
    } else if (isfunc(ty)) {
        errorf(src, "field has invalid type '%s'", TYPE_NAME(ty));
    } else if (isincomplete(ty)) {
        errorf(src, "field has incomplete type '%s'", type2s(ty));
    }
}

void ensure_field(node_t * field, size_t total, bool last)
{
    if (FIELD_ISBIT(field))
        ensure_bitfield(field);
    else
        ensure_nonbitfield(field, total, last);
}

void ensure_decl(node_t * decl, int sclass, int kind)
{
    if (kind == PARAM)
        return;

    node_t *sym = DECL_SYM(decl);
    node_t *ty = SYM_TYPE(sym);
    struct source src = AST_SRC(sym);
    if (isvardecl(decl)) {
        if (isincomplete(ty) && SYM_DEFINED(sym))
            errorf(src, "variable has incomplete type '%s'", type2s(ty));
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
static void ensure_array_sub(node_t *atype, struct source src, int level, bool outermost)
{
    if (TYPE_A_STAR(atype) && level != PARAM)
        errorf(src, "star modifier used outside of function prototype");
    
    if (TYPE_A_CONST(atype) || TYPE_A_RESTRICT(atype) ||
        TYPE_A_VOLATILE(atype) || TYPE_A_STATIC(atype)) {
        if (level != PARAM)
            errorf(src,
                   "type qualifier used in array declarator outside of function prototype");
        else if (!outermost)
            errorf(src,
                   "type qualifier used in non-outermost array type derivation");
    }
            

    node_t *rty = rtype(atype);
    if (isarray(rty))
        ensure_array_sub(rty, src, level, false);
    else if (isfunc(rty))
        errorf(src, "array of function is invalid");
    
    set_typesize(atype);
}

void ensure_array(node_t * atype, struct source src, int level)
{
    ensure_array_sub(atype, src, level, true);

    node_t *rty = rtype(atype);
    if (isincomplete(rty))
        errorf(src,
               "array has incomplete element type '%s'",
               type2s(rty));
}

void ensure_func(node_t * ftype, struct source src)
{
    node_t *rty = rtype(ftype);
    if (isarray(rty))
        errorf(src, "function cannot return array type '%s'",
               type2s(rty));
    else if (isfunc(rty))
        errorf(src, "function cannot return function type '%s'",
               type2s(rty));
}

void ensure_main(node_t *ftype, const char *name, struct source src)
{
    if (!isfunc(ftype) || !name || strcmp(name, "main"))
        return;
    
    node_t *rty = rtype(ftype);
    struct vector *params = TYPE_PARAMS(ftype);
    size_t len = vec_len(params);
    if (rty != inttype)
        errorf(src, "return type of 'main' is not 'int'");
    for (int i = 0; i < MIN(3, len); i++) {
        node_t *param = vec_at(params, i);
        node_t *ty = SYM_TYPE(param);
        if (i == 0) {
            if (ty != inttype)
                errorf(src,
                       "first parameter of 'main' is not 'int'");
        } else if (i == 1 || i == 2) {
            if (!isptrto(ty, POINTER) ||
                !isptrto(rtype(ty), CHAR))
                errorf(src,
                       "%s parameter of 'main' is not 'char **'",
                       i == 1 ? "second" : "third");
        }
    }
    if (len == 1 || len > 3)
        errorf(src,
               "expect 0, 2 or 3 parameters for 'main', have %d",
               len);
}

void ensure_params(node_t *ftype)
{
    for (int i = 0; i < vec_len(TYPE_PARAMS(ftype)); i++) {
        node_t *sym = vec_at(TYPE_PARAMS(ftype), i);
        node_t *ty = SYM_TYPE(sym);
        SYM_DEFINED(sym) = true;
        // params id is required in prototype
        if (is_anonymous(SYM_NAME(sym)))
            errorf(AST_SRC(sym), "parameter name omitted");
        if (isenum(ty) || isstruct(ty) || isunion(ty)) {
            if (!SYM_DEFINED(TYPE_TSYM(ty)))
                errorf(AST_SRC(sym),
                       "variable has incomplete type '%s'",
                       type2s(ty));
        }
    }
}

void redefinition_error(struct source src, node_t * sym)
{
    errorf(src,
           "redefinition of '%s', previous definition at %s:%u:%u",
           SYM_NAME(sym),
           AST_SRC(sym).file,
           AST_SRC(sym).line,
           AST_SRC(sym).column);
}

void conflicting_types_error(struct source src, node_t * sym)
{
    errorf(src,
           "conflicting types for '%s', previous at %s:%u:%u",
           SYM_NAME(sym),
           AST_SRC(sym).file,
           AST_SRC(sym).line,
           AST_SRC(sym).column);
}

void field_not_found_error(node_t * ty, const char *name)
{
    if (isincomplete(ty))
        error("incomplete definition of type '%s'", type2s(ty));
    else
        error("'%s' has no field named '%s'", type2s(ty), name);
}
