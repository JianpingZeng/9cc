#include "cc.h"

// predefined types
node_t *chartype;                // char
node_t *unsignedchartype;        // unsigned char
node_t *signedchartype;          // signed char
node_t *wchartype;               // wchar_t
node_t *shorttype;               // short (int)
node_t *unsignedshorttype;       // unsigned short (int)
node_t *inttype;                 // int
node_t *unsignedinttype;         // unsigned (int)
node_t *longtype;                // long
node_t *unsignedlongtype;        // unsigned long (int)
node_t *longlongtype;            // long long (int)
node_t *unsignedlonglongtype;    // unsigned long long (int)
node_t *floattype;               // float
node_t *doubletype;              // double
node_t *longdoubletype;          // long double
node_t *voidtype;                // void
node_t *booltype;                // bool


static node_t *new_type(void)
{
    return alloc_type();
}

static node_t *install_type(const char *name, int kind, struct metrics m, int op)
{
    node_t *ty = new_type();

    _TYPE_NAME(ty) = name;
    _TYPE_KIND(ty) = kind;
    _TYPE_SIZE(ty) = m.size;
    _TYPE_ALIGN(ty) = m.align;
    _TYPE_RANK(ty) = m.rank;
    switch (op) {
    case INT:
        VALUE_I(_TYPE_LIMITS_MAX(ty)) = ONES(_TYPE_SIZE(ty)) >> 1;
        VALUE_I(_TYPE_LIMITS_MIN(ty)) =
            -VALUE_I(_TYPE_LIMITS_MAX(ty)) - 1;
        break;

    case UNSIGNED:
        VALUE_U(_TYPE_LIMITS_MAX(ty)) = ONES(_TYPE_SIZE(ty));
        VALUE_U(_TYPE_LIMITS_MIN(ty)) = 0;
        break;

    case FLOAT:
        if (_TYPE_SIZE(ty) == IM->floatmetrics.size) {
            VALUE_D(_TYPE_LIMITS_MAX(ty)) = FLT_MAX;
            VALUE_D(_TYPE_LIMITS_MIN(ty)) = FLT_MIN;
        } else if (_TYPE_SIZE(ty) == IM->doublemetrics.size) {
            VALUE_D(_TYPE_LIMITS_MAX(ty)) = DBL_MAX;
            VALUE_D(_TYPE_LIMITS_MIN(ty)) = DBL_MIN;
        } else {
            VALUE_D(_TYPE_LIMITS_MAX(ty)) = LDBL_MAX;
            VALUE_D(_TYPE_LIMITS_MIN(ty)) = LDBL_MIN;
        }
        break;

    default:
        break;
    }

    return ty;
}

void type_init(void)
{
#define INSTALL(type, name, kind, metrics, op)    type = install_type(name, kind, IM->metrics, op)

    // type                     name                    kind            metrics            op

    // bool
    INSTALL(booltype,          "_Bool",                 _BOOL,         boolmetrics,        UNSIGNED);
    // char
    INSTALL(chartype,          "char",                  CHAR,          charmetrics,        INT);
    INSTALL(unsignedchartype,  "unsigned char",         CHAR,          charmetrics,        UNSIGNED);
    INSTALL(signedchartype,    "signed char",           CHAR,          charmetrics,        INT);
    // wchar_t
    INSTALL(wchartype,         "wchar_t",               UNSIGNED,      wcharmetrics,       UNSIGNED);
    // short
    INSTALL(shorttype,         "short",                 SHORT,         shortmetrics,       INT);
    INSTALL(unsignedshorttype, "unsigned short",        SHORT,         shortmetrics,       UNSIGNED);
    // int
    INSTALL(inttype,           "int",                   INT,           intmetrics,         INT);
    INSTALL(unsignedinttype,   "unsigned int",          UNSIGNED,      intmetrics,         UNSIGNED);
    // long
    INSTALL(longtype,          "long",                  LONG,          longmetrics,        INT);
    INSTALL(unsignedlongtype,  "unsigned long",         LONG,          longmetrics,        UNSIGNED);
    // long long
    INSTALL(longlongtype,      "long long",             LONG + LONG,   longlongmetrics,    INT);
    INSTALL(unsignedlonglongtype, "unsigned long long", LONG + LONG,   longlongmetrics,    UNSIGNED);
    // float
    INSTALL(floattype,         "float",                 FLOAT,         floatmetrics,       FLOAT);
    // double
    INSTALL(doubletype,        "double",                DOUBLE,        doublemetrics,      FLOAT);
    INSTALL(longdoubletype,    "long double",           LONG + DOUBLE, longdoublemetrics,  FLOAT);
    // void
    INSTALL(voidtype,          "void",                  VOID,          zerometrics,        VOID);

#undef INSTALL
}

int type_op(node_t * type)
{
    int kind = TYPE_KIND(type);
    switch (kind) {
    case _BOOL:
        return UNSIGNED;

    case CHAR:
        if (unqual(type) == unsignedchartype)
            return UNSIGNED;
        else
            return INT;

    case SHORT:
        if (unqual(type) == unsignedshorttype)
            return UNSIGNED;
        else
            return INT;

    case LONG:
        if (unqual(type) == unsignedlongtype)
            return UNSIGNED;
        else
            return INT;

    case LONG + LONG:
        if (unqual(type) == unsignedlonglongtype)
            return UNSIGNED;
        else
            return INT;

    case DOUBLE:
    case LONG + DOUBLE:
        return FLOAT;

    case INT:
    case UNSIGNED:
    case FLOAT:
    default:
        return kind;
    }
}

void prepend_type(node_t ** typelist, node_t * type)
{
    attach_type(&type, *typelist);
    *typelist = type;
}

void attach_type(node_t ** typelist, node_t * type)
{
    if (*typelist) {
        node_t *tp = *typelist;
        while (tp && _TYPE_TYPE(tp)) {
            tp = _TYPE_TYPE(tp);
        }
        _TYPE_TYPE(tp) = type;
    } else {
        *typelist = type;
    }
}

static int combine(int qual1, int qual2)
{
    int ret = 0;
    if (isconst1(qual1) || isconst1(qual2))
        ret += CONST;
    if (isvolatile1(qual1) || isvolatile1(qual2))
        ret += VOLATILE;
    if (isrestrict1(qual1) || isrestrict1(qual2))
        ret += RESTRICT;
    return ret;
}

bool qual_contains(node_t * ty1, node_t * ty2)
{
    int qual1 = isqual(ty1) ? _TYPE_KIND(ty1) : 0;
    int qual2 = isqual(ty2) ? _TYPE_KIND(ty2) : 0;
    if (isconst1(qual2) && !isconst1(qual1))
        return false;
    if (isvolatile1(qual2) && !isvolatile1(qual1))
        return false;
    if (isrestrict1(qual2) && !isrestrict1(qual1))
        return false;
    return true;
}

int qual_union(node_t * ty1, node_t * ty2)
{
    return combine(_TYPE_KIND(ty1), _TYPE_KIND(ty2));
}

node_t *qual(int t, node_t * ty)
{
    assert(ty);
    if (t == 0)
        return ty;
    
    assert(isconst1(t) || isvolatile1(t) || isrestrict1(t));
    
    node_t *qty = new_type();
    if (isqual(ty))
        _TYPE_KIND(qty) = combine(t, _TYPE_KIND(ty));
    else
        _TYPE_KIND(qty) = t;
    _TYPE_TYPE(qty) = unqual(ty);
    return qty;
}

node_t *unqual(node_t * ty)
{
    return isqual(ty) ? _TYPE_TYPE(ty) : ty;
}

node_t *lookup_typedef(const char *id)
{
    if (!id)
        return NULL;

    node_t *sym = lookup(id, identifiers);

    if (sym && SYM_SCLASS(sym) == TYPEDEF)
        return SYM_TYPE(sym);
    else
        return NULL;
}

bool istypedef(const char *id)
{
    node_t *ty = lookup_typedef(id);
    return ty != NULL;
}

node_t *new_field(void)
{
    return alloc_field();
}

node_t *array_type(node_t * type)
{
    node_t *ty = new_type();
    _TYPE_KIND(ty) = ARRAY;
    _TYPE_NAME(ty) = "array";
    _TYPE_TYPE(ty) = type;

    return ty;
}

node_t *ptr_type(node_t * type)
{
    node_t *ty = new_type();
    _TYPE_KIND(ty) = POINTER;
    _TYPE_NAME(ty) = "pointer";
    _TYPE_TYPE(ty) = type;
    _TYPE_SIZE(ty) = IM->ptrmetrics.size;
    _TYPE_ALIGN(ty) = IM->ptrmetrics.align;

    return ty;
}

node_t *func_type(void)
{
    node_t *ty = new_type();
    _TYPE_KIND(ty) = FUNCTION;
    _TYPE_NAME(ty) = "function";
    _TYPE_ALIGN(ty) = IM->ptrmetrics.align;

    return ty;
}

node_t *tag_type(int t, const char *tag, struct source src)
{
    node_t *ty = new_type();
    _TYPE_KIND(ty) = t;
    _TYPE_TAG(ty) = tag;
    _TYPE_NAME(ty) = id2s(t);
    if (t == ENUM)
        _TYPE_TYPE(ty) = inttype;

    node_t *sym = NULL;
    if (tag) {
        sym = lookup(tag, tags);
        if (sym && is_current_scope(sym)) {
            if (TYPE_OP(SYM_TYPE(sym)) == t && !SYM_DEFINED(sym))
                return sym;

            redefinition_error(src, sym);
        }

        sym = install(tag, &tags, SCOPE);
    } else {
        sym = anonymous(&tags, SCOPE);
        _TYPE_TAG(ty) = SYM_NAME(sym);
    }

    SYM_TYPE(sym) = ty;
    AST_SRC(sym) = src;
    _TYPE_TSYM(ty) = sym;

    return sym;
}

static bool eqparams(struct vector * params1, struct vector * params2)
{
    if (params1 == params2) {
        return true;
    } else if (params1 == NULL || params2 == NULL) {
        return false;
    } else {
        int len1 = vec_len(params1);
        int len2 = vec_len(params2);
        if (len1 != len2)
            return false;
        for (int i = 0; i < len1; i++) {
            node_t *sym1 = vec_at(params1, i);
            node_t *sym2 = vec_at(params2, i);
            if (sym1 == sym2)
                continue;
            else if (sym1 == NULL || sym2 == NULL)
                return false;
            else if (eqtype(SYM_TYPE(sym1), SYM_TYPE(sym2)))
                continue;
            else
                return false;
        }

        return true;
    }
}

bool eqtype(node_t * ty1, node_t * ty2)
{
    if (ty1 == ty2)
        return true;
    else if (_TYPE_KIND(ty1) != _TYPE_KIND(ty2))
        return false;

    switch (_TYPE_KIND(ty1)) {
    case CONST:
    case VOLATILE:
    case RESTRICT:
    case CONST + VOLATILE:
    case CONST + RESTRICT:
    case VOLATILE + RESTRICT:
    case CONST + VOLATILE + RESTRICT:
        return eqtype(_TYPE_TYPE(ty1), _TYPE_TYPE(ty2));
    case ENUM:
    case UNION:
    case STRUCT:
        return false;
    case _BOOL:
    case CHAR:
    case SHORT:
    case INT:
    case UNSIGNED:
    case LONG:
    case LONG+LONG:
    case FLOAT:
    case DOUBLE:
    case LONG+DOUBLE:
    case VOID:
        return ty1 == ty2;
    case POINTER:
    case ARRAY:
        return eqtype(_TYPE_TYPE(ty1), _TYPE_TYPE(ty2));
    case FUNCTION:
        if (!eqtype(_TYPE_TYPE(ty1), _TYPE_TYPE(ty2)))
            return false;
        if (_TYPE_OLDSTYLE(ty1) && _TYPE_OLDSTYLE(ty2)) {
            // both oldstyle
            return true;
        } else if (!_TYPE_OLDSTYLE(ty1) && !_TYPE_OLDSTYLE(ty2)) {
            // both prototype
            return eqparams(_TYPE_PARAMS(ty1), _TYPE_PARAMS(ty2));
        } else {
            // one oldstyle, the other prototype
            node_t *oldty = _TYPE_OLDSTYLE(ty1) ? ty1 : ty2;
            node_t *newty = _TYPE_OLDSTYLE(ty1) ? ty2 : ty1;

            if (TYPE_VARG(newty))
                return false;
            
            for (int i = 0; i < vec_len(_TYPE_PARAMS(newty)); i++) {
                node_t *sym = vec_at(_TYPE_PARAMS(newty), i);
                node_t *ty = SYM_TYPE(sym);
                if (TYPE_KIND(ty) == _BOOL ||
                    TYPE_KIND(ty) == CHAR ||
                    TYPE_KIND(ty) == SHORT ||
                    TYPE_KIND(ty) == FLOAT)
                    return false;
            }

            if (_TYPE_PARAMS(oldty) == NULL)
                return true;

            return eqparams(_TYPE_PARAMS(oldty), _TYPE_PARAMS(newty));
        }

    default:
        assert(0);
        return false;
    }
}

node_t *find_field(node_t * sty, const char *name)
{
    int i;
    node_t *ty = unqual(sty);
    int len = vec_len(TYPE_FIELDS(ty));

    if (name == NULL)
        return NULL;
    for (i = 0; i < len; i++) {
        node_t *field = vec_at(TYPE_FIELDS(ty), i);
        if (FIELD_NAME(field) && !strcmp(name, FIELD_NAME(field)))
            return field;
    }

    return NULL;
}

int indexof_field(node_t * ty, node_t * field)
{
    for (int i = 0; i < vec_len(TYPE_FIELDS(ty)); i++) {
        node_t *f = vec_at(TYPE_FIELDS(ty), i);
        if (field == f)
            return i;
    }
    assert(0);
    return -1;
}

/* Structure alignment requirements
 *
 * The rule is that the structure will be padded out
 * to the size the type would occupy as an element
 * of an array of such types.
 *
 * The bitfields must be packed as tightly as possible.
 */
static unsigned struct_size(node_t * ty)
{
    int max = 1;
    node_t *prev = NULL;
    struct vector *fields = TYPE_FIELDS(ty);

    for (int i = 0; i < vec_len(fields); i++) {
        node_t *field = vec_at(fields, i);
        node_t *ty = FIELD_TYPE(field);

        if (FIELD_ISBIT(field)) {
            int bitsize = FIELD_BITSIZE(field);
            
            if (!isint(ty))
                continue;
            if (bitsize < 0 || (FIELD_NAME(field) && bitsize == 0))
                continue;

            if (bitsize > BITS(TYPE_SIZE(ty)))
                bitsize = BITS(TYPE_SIZE(ty));

            if (bitsize == 0) {
                if (prev == NULL) {
                    // the first field
                    FIELD_OFFSET(field) = 0;
                    FIELD_BITOFF(field) = 0;
                } else if (FIELD_ISBIT(prev)) {
                    int prev_end = FIELD_BITSIZE(prev) + FIELD_BITOFF(prev);
                    int prev_end_rounded = ROUNDUP(prev_end, 8);
                    int bytes = prev_end_rounded >> 3;
                    size_t offset = FIELD_OFFSET(prev) + bytes;
                    FIELD_OFFSET(field) = ROUNDUP(offset, TYPE_ALIGN(ty));
                    FIELD_BITOFF(field) = 0;
                } else {
                    size_t prev_end = FIELD_OFFSET(prev) + TYPE_SIZE(FIELD_TYPE(prev));
                    size_t prev_end_rounded = ROUNDUP(prev_end, TYPE_ALIGN(ty));
                    FIELD_OFFSET(field) = prev_end_rounded;
                    FIELD_BITOFF(field) = 0;
                }
                goto next;
            } else {
                if (prev == NULL) {
                    // the first field
                    FIELD_OFFSET(field) = 0;
                    FIELD_BITOFF(field) = 0;
                } else if (FIELD_ISBIT(prev)) {
                    int prev_end = FIELD_BITSIZE(prev) + FIELD_BITOFF(prev);
                    int prev_end_rounded = ROUNDUP(prev_end, 8);
                    if (bitsize + prev_end <= prev_end_rounded) {
                        FIELD_OFFSET(field) = FIELD_OFFSET(prev);
                        FIELD_BITOFF(field) = prev_end;
                    } else {
                        int bytes = prev_end_rounded >> 3;
                        FIELD_OFFSET(field) = FIELD_OFFSET(prev) + bytes;
                        FIELD_BITOFF(field) = 0;
                    }
                } else {
                    FIELD_OFFSET(field) = FIELD_OFFSET(prev) + TYPE_SIZE(FIELD_TYPE(prev));
                    FIELD_BITOFF(field) = 0;
                }
            }
        } else {
            int align = TYPE_ALIGN(ty);

            if (prev == NULL) {
                // the first field
                FIELD_OFFSET(field) = 0;
            } else if (FIELD_ISBIT(prev)) {
                int prev_end = FIELD_BITSIZE(prev) + FIELD_BITOFF(prev);
                int bytes = ROUNDUP(prev_end, 8) >> 3;
                size_t end = FIELD_OFFSET(prev) + bytes;
                FIELD_OFFSET(field) = ROUNDUP(end, align);
            } else {
                size_t end = FIELD_OFFSET(prev) + TYPE_SIZE(FIELD_TYPE(prev));
                FIELD_OFFSET(field) = ROUNDUP(end, align);
            }
        }

        max = MAX(max, TYPE_ALIGN(ty));
    next:
        prev = field;
    }

    TYPE_ALIGN(ty) = max;

    size_t offset = 0;
    if (prev) {
        if (FIELD_ISBIT(prev)) {
            int bitsize = FIELD_BITSIZE(prev);
            int bitoff = FIELD_BITOFF(prev);
            int bytes = ROUNDUP(bitoff + bitsize, 8) >> 3;
            offset = FIELD_OFFSET(prev) + bytes;
        } else {
            offset = FIELD_OFFSET(prev) + TYPE_SIZE(FIELD_TYPE(prev));
        }
    }
    return ROUNDUP(offset, max);
}

static unsigned union_size(node_t * ty)
{
    int max = 1;
    int size = 0;
    struct vector *fields = TYPE_FIELDS(ty);

    for (int i = 0; i < vec_len(fields); i++) {
        node_t *field = vec_at(fields, i);
        node_t *ty = FIELD_TYPE(field);
        int tysize = TYPE_SIZE(ty);

        if (tysize == 0)
            continue;

        if (FIELD_ISBIT(field)) {
            int bitsize = FIELD_BITSIZE(field);

            if (!isint(ty))
                continue;
            if (bitsize <= 0)
                continue;
        }

        size = MAX(size, tysize);
        max = MAX(max, TYPE_ALIGN(ty));
    }

    TYPE_ALIGN(ty) = max;

    return ROUNDUP(size, max);
}

static unsigned array_size(node_t * ty)
{
    unsigned size = 1;
    node_t *rty = ty;

    do {
        size *= TYPE_LEN(rty);
        rty = rtype(rty);
    } while (isarray(rty));

    size *= TYPE_SIZE(rty);

    // set align
    do {
        TYPE_ALIGN(ty) = TYPE_ALIGN(rty);
        ty = rtype(ty);
    } while (isarray(ty));

    return size;
}

void set_typesize(node_t * ty)
{
    if (isarray(ty))
        TYPE_SIZE(ty) = array_size(ty);
    else if (isstruct(ty))
        TYPE_SIZE(ty) = struct_size(ty);
    else if (isunion(ty))
        TYPE_SIZE(ty) = union_size(ty);
}

bool isincomplete(node_t * ty)
{
    if (isvoid(ty))
        return true;
    else if (isarray(ty))
        return TYPE_SIZE(ty) == 0;
    else if (isenum(ty) || isstruct(ty) || isunion(ty))
        return !SYM_DEFINED(TYPE_TSYM(ty));
    else
        return false;
}

node_t *unpack(node_t * ty)
{
    if (isenum(ty))
        return _TYPE_TYPE(unqual(ty));
    else
        return ty;
}

node_t *compose(node_t * ty1, node_t * ty2)
{
    if (isqual(ty1) && isqual(ty2)) {
        int kind = combine(_TYPE_KIND(ty1), _TYPE_KIND(ty2));
        return qual(kind, unqual(ty1));
    } else if (isqual(ty2)) {
        return qual(_TYPE_KIND(ty2), ty1);
    } else {
        return ty1;
    }
}

bool eqarith(node_t * ty1, node_t * ty2)
{
    assert(isarith(ty1));
    assert(isarith(ty2));

    return TYPE_KIND(ty1) == TYPE_KIND(ty2) &&
        TYPE_OP(ty1) == TYPE_OP(ty2);
}

bool isfunc(node_t * ty)
{
    return TYPE_OP(ty) == FUNCTION;
}

bool isarray(node_t * ty)
{
    return TYPE_OP(ty) == ARRAY;
}

bool isptr(node_t * ty)
{
    return TYPE_OP(ty) == POINTER;
}

bool isvoid(node_t * ty)
{
    return TYPE_OP(ty) == VOID;
}

bool isenum(node_t * ty)
{
    return TYPE_OP(ty) == ENUM;
}

bool isstruct(node_t * ty)
{
    return TYPE_OP(ty) == STRUCT;
}

bool isunion(node_t * ty)
{
    return TYPE_OP(ty) == UNION;
}

bool isrecord(node_t * type)
{
    return isstruct(type) || isunion(type);
}

bool istag(node_t * type)
{
    return isstruct(type) || isunion(type) || isenum(type);
}

bool isint(node_t * ty)
{
    return TYPE_OP(ty) == INT || TYPE_OP(ty) == UNSIGNED || isenum(ty);
}

bool isfloat(node_t * ty)
{
    return TYPE_OP(ty) == FLOAT;
}

bool isarith(node_t * ty)
{
    return isint(ty) || isfloat(ty);
}

bool isscalar(node_t * ty)
{
    return isarith(ty) || isptr(ty);
}

bool isptrto(node_t * ty, int kind)
{
    return isptr(ty) && TYPE_KIND(rtype(ty)) == kind;
}

bool isbool(node_t *ty)
{
    return unqual(ty) == booltype;
}
