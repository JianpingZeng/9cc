#include <limits.h>
#include <float.h>
#include <assert.h>
#include "cc.h"

// predefined types
struct type *chartype;                // char
struct type *unsignedchartype;        // unsigned char
struct type *signedchartype;          // signed char
struct type *wchartype;               // wchar_t
struct type *shorttype;               // short (int)
struct type *unsignedshorttype;       // unsigned short (int)
struct type *inttype;                 // int
struct type *unsignedinttype;         // unsigned (int)
struct type *longtype;                // long
struct type *unsignedlongtype;        // unsigned long (int)
struct type *longlongtype;            // long long (int)
struct type *unsignedlonglongtype;    // unsigned long long (int)
struct type *floattype;               // float
struct type *doubletype;              // double
struct type *longdoubletype;          // long double
struct type *voidtype;                // void
struct type *booltype;                // bool


struct field *alloc_field(void)
{
    return NEWS0(struct field, PERM);
}

struct type *alloc_type(void)
{
    return NEWS0(struct type, PERM);
}

static struct type *install_type(const char *name, int kind, struct metrics m, int op)
{
    struct type *ty = alloc_type();

    _TYPE_NAME(ty) = name;
    _TYPE_KIND(ty) = kind;
    _TYPE_SIZE(ty) = m.size;
    _TYPE_ALIGN(ty) = m.align;
    _TYPE_RANK(ty) = m.rank;
    switch (op) {
    case INT:
        _TYPE_LIMITS(ty).max.i = ONES(_TYPE_SIZE(ty)) >> 1;
        _TYPE_LIMITS(ty).min.i = - _TYPE_LIMITS(ty).max.i - 1;
        break;

    case UNSIGNED:
        _TYPE_LIMITS(ty).max.u = ONES(_TYPE_SIZE(ty));
        _TYPE_LIMITS(ty).min.u = 0;
        break;

    case FLOAT:
        if (_TYPE_SIZE(ty) == IM->floatmetrics.size) {
            _TYPE_LIMITS(ty).max.d = FLT_MAX;
            _TYPE_LIMITS(ty).min.d = FLT_MIN;
        } else if (_TYPE_SIZE(ty) == IM->doublemetrics.size) {
            _TYPE_LIMITS(ty).max.d = DBL_MAX;
            _TYPE_LIMITS(ty).min.d = DBL_MIN;
        } else {
            _TYPE_LIMITS(ty).max.d = LDBL_MAX;
            _TYPE_LIMITS(ty).min.d = LDBL_MIN;
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

int type_op(struct type * type)
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

void prepend_type(struct type ** typelist, struct type * type)
{
    attach_type(&type, *typelist);
    *typelist = type;
}

void attach_type(struct type ** typelist, struct type * type)
{
    if (*typelist) {
        struct type *tp = *typelist;
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

bool qual_contains(struct type * ty1, struct type * ty2)
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

int qual_union(struct type * ty1, struct type * ty2)
{
    return combine(_TYPE_KIND(ty1), _TYPE_KIND(ty2));
}

struct type *qual(int t, struct type * ty)
{
    assert(ty);
    if (t == 0)
        return ty;
    
    assert(isconst1(t) || isvolatile1(t) || isrestrict1(t));
    
    struct type *qty = alloc_type();
    if (isqual(ty))
        _TYPE_KIND(qty) = combine(t, _TYPE_KIND(ty));
    else
        _TYPE_KIND(qty) = t;
    _TYPE_TYPE(qty) = unqual(ty);
    return qty;
}

struct type *unqual(struct type * ty)
{
    return isqual(ty) ? _TYPE_TYPE(ty) : ty;
}

struct type *lookup_typedef(const char *id)
{
    if (!id)
        return NULL;

    struct symbol *sym = lookup(id, identifiers);

    if (sym && sym->sclass == TYPEDEF)
        return sym->type;
    else
        return NULL;
}

bool istypedef(const char *id)
{
    struct type *ty = lookup_typedef(id);
    return ty != NULL;
}

struct type *array_type(struct type * type)
{
    struct type *ty = alloc_type();
    _TYPE_KIND(ty) = ARRAY;
    _TYPE_NAME(ty) = "array";
    _TYPE_TYPE(ty) = type;

    return ty;
}

struct type *ptr_type(struct type * type)
{
    struct type *ty = alloc_type();
    _TYPE_KIND(ty) = POINTER;
    _TYPE_NAME(ty) = "pointer";
    _TYPE_TYPE(ty) = type;
    _TYPE_SIZE(ty) = IM->ptrmetrics.size;
    _TYPE_ALIGN(ty) = IM->ptrmetrics.align;

    return ty;
}

struct type *func_type(void)
{
    struct type *ty = alloc_type();
    _TYPE_KIND(ty) = FUNCTION;
    _TYPE_NAME(ty) = "function";
    _TYPE_ALIGN(ty) = IM->ptrmetrics.align;

    return ty;
}

struct symbol *tag_type(int t, const char *tag, struct source src)
{
    struct type *ty = alloc_type();
    _TYPE_KIND(ty) = t;
    _TYPE_TAG(ty) = tag;
    _TYPE_NAME(ty) = id2s(t);
    if (t == ENUM)
        _TYPE_TYPE(ty) = inttype;

    struct symbol *sym = NULL;
    if (tag) {
        sym = lookup(tag, tags);
        if (sym && is_current_scope(sym)) {
            if (TYPE_OP(sym->type) == t && !sym->defined)
                return sym;

            redefinition_error(src, sym);
        }

        sym = install(tag, &tags, cscope, PERM);
    } else {
        sym = anonymous(&tags, cscope, PERM);
        _TYPE_TAG(ty) = sym->name;
    }

    sym->type = ty;
    sym->src = src;
    _TYPE_TSYM(ty) = sym;

    return sym;
}

static bool eqparams(struct type *proto1[], struct type *proto2[])
{
    if (proto1 == proto2) {
        return true;
    } else if (proto1 == NULL || proto2 == NULL) {
        return false;
    } else {
        size_t len1 = length(proto1);
        size_t len2 = length(proto2);
        if (len1 != len2)
            return false;
        for (size_t i = 0; i < len1; i++) {
            struct type *ty1 = proto1[i];
            struct type *ty2 = proto2[i];
            if (ty1 == ty2)
                continue;
            else if (ty1 == NULL || ty2 == NULL)
                return false;
            else if (eqtype(ty1, ty2))
                continue;
            else
                return false;
        }

        return true;
    }
}

bool eqtype(struct type * ty1, struct type * ty2)
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
            return eqparams(_TYPE_PROTO(ty1), _TYPE_PROTO(ty2));
        } else {
            // one oldstyle, the other prototype
            struct type *oldty = _TYPE_OLDSTYLE(ty1) ? ty1 : ty2;
            struct type *newty = _TYPE_OLDSTYLE(ty1) ? ty2 : ty1;

            if (TYPE_VARG(newty))
                return false;
            
            for (size_t i = 0; _TYPE_PROTO(newty)[i]; i++) {
                struct type *ty = _TYPE_PROTO(newty)[i];
                if (TYPE_KIND(ty) == _BOOL ||
                    TYPE_KIND(ty) == CHAR ||
                    TYPE_KIND(ty) == SHORT ||
                    TYPE_KIND(ty) == FLOAT)
                    return false;
            }

            if (_TYPE_PROTO(oldty)[0] == NULL)
                return true;

            return eqparams(_TYPE_PROTO(oldty), _TYPE_PROTO(newty));
        }

    default:
        assert(0);
        return false;
    }
}

struct field *find_field(struct type * sty, const char *name)
{
    struct type *ty = unqual(sty);
    assert(isrecord(ty));

    if (name == NULL)
        return NULL;
    for (size_t i = 0; TYPE_FIELDS(ty)[i]; i++) {
        struct field *field = TYPE_FIELDS(ty)[i];
        if (field->name && !strcmp(name, field->name))
            return field;
    }

    return NULL;
}

int indexof_field(struct type * ty, struct field * field)
{
    for (int i = 0; TYPE_FIELDS(ty)[i]; i++) {
        struct field *f = TYPE_FIELDS(ty)[i];
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
static unsigned struct_size(struct type * ty)
{
    int max = 1;
    struct field *prev = NULL;
    struct field **fields = TYPE_FIELDS(ty);

    for (int i = 0; fields[i]; i++) {
        struct field *field = fields[i];
        struct type *ty = field->type;

        if (field->isbit) {
            int bitsize = field->bitsize;
            
            if (!isint(ty))
                continue;
            if (bitsize < 0 || (field->name && bitsize == 0))
                continue;

            if (bitsize > BITS(TYPE_SIZE(ty)))
                bitsize = BITS(TYPE_SIZE(ty));

            if (bitsize == 0) {
                if (prev == NULL) {
                    // the first field
                    field->offset = 0;
                    field->bitoff = 0;
                } else if (prev->isbit) {
                    int prev_end = prev->bitsize + prev->bitoff;
                    int prev_end_rounded = ROUNDUP(prev_end, 8);
                    int bytes = prev_end_rounded >> 3;
                    size_t offset = prev->offset + bytes;
                    field->offset = ROUNDUP(offset, TYPE_ALIGN(ty));
                    field->bitoff = 0;
                } else {
                    size_t prev_end = prev->offset + TYPE_SIZE(prev->type);
                    size_t prev_end_rounded = ROUNDUP(prev_end, TYPE_ALIGN(ty));
                    field->offset = prev_end_rounded;
                    field->bitoff = 0;
                }
                goto next;
            } else {
                if (prev == NULL) {
                    // the first field
                    field->offset = 0;
                    field->bitoff = 0;
                } else if (prev->isbit) {
                    int prev_end = prev->bitsize + prev->bitoff;
                    int prev_end_rounded = ROUNDUP(prev_end, 8);
                    if (bitsize + prev_end <= prev_end_rounded) {
                        field->offset = prev->offset;
                        field->bitoff = prev_end;
                    } else {
                        int bytes = prev_end_rounded >> 3;
                        field->offset = prev->offset + bytes;
                        field->bitoff = 0;
                    }
                } else {
                    field->offset = prev->offset + TYPE_SIZE(prev->type);
                    field->bitoff = 0;
                }
            }
        } else {
            int align = TYPE_ALIGN(ty);

            if (prev == NULL) {
                // the first field
                field->offset = 0;
            } else if (prev->isbit) {
                int prev_end = prev->bitsize + prev->bitoff;
                int bytes = ROUNDUP(prev_end, 8) >> 3;
                size_t end = prev->offset + bytes;
                field->offset = ROUNDUP(end, align);
            } else {
                size_t end = prev->offset + TYPE_SIZE(prev->type);
                field->offset = ROUNDUP(end, align);
            }
        }

        max = MAX(max, TYPE_ALIGN(ty));
    next:
        prev = field;
    }

    TYPE_ALIGN(ty) = max;

    size_t offset = 0;
    if (prev) {
        if (prev->isbit) {
            int bitsize = prev->bitsize;
            int bitoff = prev->bitoff;
            int bytes = ROUNDUP(bitoff + bitsize, 8) >> 3;
            offset = prev->offset + bytes;
        } else {
            offset = prev->offset + TYPE_SIZE(prev->type);
        }
    }
    return ROUNDUP(offset, max);
}

static unsigned union_size(struct type * ty)
{
    int max = 1;
    int size = 0;
    struct field **fields = TYPE_FIELDS(ty);

    for (int i = 0; fields[i]; i++) {
        struct field *field = fields[i];
        struct type *ty = field->type;
        int tysize = TYPE_SIZE(ty);

        if (tysize == 0)
            continue;

        if (field->isbit) {
            int bitsize = field->bitsize;

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

static unsigned array_size(struct type * ty)
{
    unsigned size = 1;
    struct type *rty = ty;

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

void set_typesize(struct type * ty)
{
    if (isarray(ty))
        TYPE_SIZE(ty) = array_size(ty);
    else if (isstruct(ty))
        TYPE_SIZE(ty) = struct_size(ty);
    else if (isunion(ty))
        TYPE_SIZE(ty) = union_size(ty);
}

bool isincomplete(struct type * ty)
{
    if (isvoid(ty))
        return true;
    else if (isarray(ty))
        return TYPE_SIZE(ty) == 0;
    else if (isenum(ty) || isstruct(ty) || isunion(ty))
        return !TYPE_TSYM(ty)->defined;
    else
        return false;
}

struct type *unpack(struct type * ty)
{
    if (isenum(ty))
        return _TYPE_TYPE(unqual(ty));
    else
        return ty;
}

struct type *compose(struct type * ty1, struct type * ty2)
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

bool eqarith(struct type * ty1, struct type * ty2)
{
    assert(isarith(ty1));
    assert(isarith(ty2));

    return TYPE_KIND(ty1) == TYPE_KIND(ty2) &&
        TYPE_OP(ty1) == TYPE_OP(ty2);
}

bool isfunc(struct type * ty)
{
    return TYPE_OP(ty) == FUNCTION;
}

bool isarray(struct type * ty)
{
    return TYPE_OP(ty) == ARRAY;
}

bool isptr(struct type * ty)
{
    return TYPE_OP(ty) == POINTER;
}

bool isvoid(struct type * ty)
{
    return TYPE_OP(ty) == VOID;
}

bool isenum(struct type * ty)
{
    return TYPE_OP(ty) == ENUM;
}

bool isstruct(struct type * ty)
{
    return TYPE_OP(ty) == STRUCT;
}

bool isunion(struct type * ty)
{
    return TYPE_OP(ty) == UNION;
}

bool isrecord(struct type * type)
{
    return isstruct(type) || isunion(type);
}

bool istag(struct type * type)
{
    return isstruct(type) || isunion(type) || isenum(type);
}

bool isint(struct type * ty)
{
    return TYPE_OP(ty) == INT || TYPE_OP(ty) == UNSIGNED || isenum(ty);
}

bool isfloat(struct type * ty)
{
    return TYPE_OP(ty) == FLOAT;
}

bool isarith(struct type * ty)
{
    return isint(ty) || isfloat(ty);
}

bool isscalar(struct type * ty)
{
    return isarith(ty) || isptr(ty);
}

bool isptrto(struct type * ty, int kind)
{
    return isptr(ty) && TYPE_KIND(rtype(ty)) == kind;
}

bool isbool(struct type *ty)
{
    return unqual(ty) == booltype;
}
