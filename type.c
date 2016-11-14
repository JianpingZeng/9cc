#include <limits.h>
#include <float.h>
#include <assert.h>
#include "cc.h"

// predefined types
struct type *chartype;          // char
struct type *uchartype;         // unsigned char
struct type *schartype;         // signed char
struct type *wchartype;         // wchar_t
struct type *shorttype;         // short (int)
struct type *ushorttype;        // unsigned short (int)
struct type *inttype;           // int
struct type *uinttype;          // unsigned (int)
struct type *longtype;          // long
struct type *ulongtype;         // unsigned long (int)
struct type *llongtype;         // long long (int)
struct type *ullongtype;        // unsigned long long (int)
struct type *floattype;         // float
struct type *doubletype;        // double
struct type *longdoubletype;    // long double
struct type *voidtype;          // void
struct type *booltype;          // bool
struct type *voidptype;         // void *
struct type *funcptype;         // void (*) ()
struct type *uptrtype;          // unsigned integer alias for pointer type
struct type *sptrtype;          // signed integer alias for pointer type

static struct type *alloc_type(void)
{
    return NEWS0(struct type, PERM);
}

static struct type *
install_type(const char *name, int kind, struct metrics m, int op)
{
    struct type *ty = alloc_type();

    ty->name = name;
    ty->kind = kind;
    ty->op = op;
    ty->size = m.size;
    ty->align = m.align;
    ty->rank = m.rank;
    switch (op) {
    case INT:
        ty->limits.max.i = ONES(ty->size) >> 1;
        ty->limits.min.i = - ty->limits.max.i - 1;
        break;

    case UNSIGNED:
        ty->limits.max.u = ONES(ty->size);
        ty->limits.min.u = 0;
        break;

    case FLOAT:
        if (kind == FLOAT) {
            ty->limits.max.f = FLT_MAX;
            ty->limits.min.f = FLT_MIN;
        } else if (kind == DOUBLE) {
            ty->limits.max.d = DBL_MAX;
            ty->limits.min.d = DBL_MIN;
        } else {
            ty->limits.max.ld = LDBL_MAX;
            ty->limits.min.ld = LDBL_MIN;
        }
        break;

    default:
        break;
    }

    return ty;
}

void type_init(void)
{
#define INSTALL(type, name, kind, metrics, op)    type = install_type(name, kind, IR->metrics, op)

    // type                     name                    kind            metrics            op

    // bool
    INSTALL(booltype,          "_Bool",                 _BOOL,         boolmetrics,        UNSIGNED);
    // char
    INSTALL(chartype,          "char",                  CHAR,          charmetrics,        INT);
    INSTALL(uchartype,         "unsigned char",         CHAR,          charmetrics,        UNSIGNED);
    INSTALL(schartype,         "signed char",           CHAR,          charmetrics,        INT);
    // wchar_t
    INSTALL(wchartype,         "wchar_t",               UNSIGNED,      wcharmetrics,       UNSIGNED);
    // short
    INSTALL(shorttype,         "short",                 SHORT,         shortmetrics,       INT);
    INSTALL(ushorttype,        "unsigned short",        SHORT,         shortmetrics,       UNSIGNED);
    // int
    INSTALL(inttype,           "int",                   INT,           intmetrics,         INT);
    INSTALL(uinttype,          "unsigned int",          UNSIGNED,      intmetrics,         UNSIGNED);
    // long
    INSTALL(longtype,          "long",                  LONG,          longmetrics,        INT);
    INSTALL(ulongtype,         "unsigned long",         LONG,          longmetrics,        UNSIGNED);
    // long long
    INSTALL(llongtype,         "long long",             LONG + LONG,   longlongmetrics,    INT);
    INSTALL(ullongtype,        "unsigned long long",    LONG + LONG,   longlongmetrics,    UNSIGNED);
    // float
    INSTALL(floattype,         "float",                 FLOAT,         floatmetrics,       FLOAT);
    // double
    INSTALL(doubletype,        "double",                DOUBLE,        doublemetrics,      FLOAT);
    INSTALL(longdoubletype,    "long double",           LONG + DOUBLE, longdoublemetrics,  FLOAT);
    // void
    INSTALL(voidtype,          "void",                  VOID,          voidmetrics,        VOID);
    // void *
    INSTALL(voidptype,         "void *",                POINTER,       ptrmetrics,         POINTER);
    // void (*) ()
    INSTALL(funcptype,         "void (*) ()",           FUNCTION,      ptrmetrics,         FUNCTION);

#undef INSTALL

    uptrtype = ulongtype;
    sptrtype = longtype;
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
    int qual1 = isqual(ty1) ? ty1->kind : 0;
    int qual2 = isqual(ty2) ? ty2->kind : 0;
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
    return combine(ty1->kind, ty2->kind);
}

struct type *qual(int t, struct type * ty)
{
    assert(ty);
    if (t == 0)
        return ty;
    
    assert(isconst1(t) || isvolatile1(t) || isrestrict1(t));
    
    struct type *qty = alloc_type();
    if (isqual(ty))
        qty->kind = combine(t, ty->kind);
    else
        qty->kind = t;
    qty->type = unqual(ty);
    return qty;
}

struct type *unqual(struct type * ty)
{
    return isqual(ty) ? ty->type : ty;
}

struct type *array_type(struct type * type)
{
    struct type *ty = alloc_type();
    ty->kind = ARRAY;
    ty->op = ARRAY;
    ty->name = "array";
    ty->type = type;

    return ty;
}

struct type *ptr_type(struct type * type)
{
    struct type *ty = alloc_type();
    ty->kind = POINTER;
    ty->op = POINTER;
    ty->name = "pointer";
    ty->type = type;
    ty->size = voidptype->size;
    ty->align = voidptype->align;

    return ty;
}

struct type *func_type(struct type * type)
{
    struct type *ty = alloc_type();
    ty->kind = FUNCTION;
    ty->op = FUNCTION;
    ty->name = "function";
    ty->type = type;
    ty->align = funcptype->align;

    return ty;
}

struct type *tag_type(int t)
{
    struct type *ty = alloc_type();
    ty->kind = t;
    ty->op = t;
    ty->name = id2s(t);
    if (t == ENUM) {
        ty->type = inttype;
        ty->size = ty->type->size;
        ty->align = ty->type->align;
        ty->rank = ty->type->rank;
        ty->limits = ty->type->limits;
    }

    return ty;
}

static bool cmparams(struct type *proto1[], struct type *proto2[],
                     bool (*cmp) (struct type *, struct type *))
{
    assert(proto1 && proto2);

    if (proto1 == proto2)
        return true;

    size_t len1 = length(proto1);
    size_t len2 = length(proto2);
    if (len1 != len2)
        return false;
    for (size_t i = 0; i < len1; i++) {
        struct type *ty1 = proto1[i];
        struct type *ty2 = proto2[i];

        assert(ty1 && ty2);

        if (ty1 == ty2)
            continue;
        else if (cmp(ty1, ty2))
            continue;
        else
            return false;
    }

    return true;
}

bool compatible(struct type *ty1, struct type *ty2)
{
    int kind1, kind2;
    
    ty1 = unqual(ty1);
    ty2 = unqual(ty2);

    kind1 = ty1->kind == ENUM ? ty1->type->kind : ty1->kind;
    kind2 = ty2->kind == ENUM ? ty2->type->kind : ty2->kind;

    if (kind1 != kind2)
        return false;

    switch (kind1) {
    case STRUCT:
    case UNION:
    case CHAR:
        // char/signed char/unsigned char are always different.
        return ty1 == ty2;
    case POINTER:
    case ARRAY:
        return compatible(rtype(ty1), rtype(ty2));
    case FUNCTION:
        if (!compatible(ty1->type, ty2->type))
            return false;
        if (ty1->u.f.oldstyle && ty2->u.f.oldstyle) {
            // both oldstyle
            return true;
        } else if (!ty1->u.f.oldstyle && !ty2->u.f.oldstyle) {
            // both prototype
            return cmparams(ty1->u.f.proto, ty2->u.f.proto, compatible);
        } else {
            // one oldstyle, the other prototype
            struct type *oldty = ty1->u.f.oldstyle ? ty1 : ty2;
            struct type *newty = ty1->u.f.oldstyle ? ty2 : ty1;

            if (TYPE_VARG(newty))
                return false;
            
            for (size_t i = 0; newty->u.f.proto[i]; i++) {
                struct type *ty = newty->u.f.proto[i];
                if (TYPE_KIND(ty) == _BOOL ||
                    TYPE_KIND(ty) == CHAR ||
                    TYPE_KIND(ty) == SHORT ||
                    TYPE_KIND(ty) == FLOAT)
                    return false;
            }

            if (isempty(oldty->u.f.proto))
                return true;

            return cmparams(oldty->u.f.proto, newty->u.f.proto, compatible);
        }
        return false;
    case _BOOL:
    case SHORT:
    case INT:
    case UNSIGNED:
    case LONG:
    case LONG + LONG:
    case FLOAT:
    case DOUBLE:
    case LONG + DOUBLE:
    case VOID:
        return true;
    default:
        CC_UNAVAILABLE();
    }
}

bool eqtype(struct type * ty1, struct type * ty2)
{
    if (ty1 == ty2)
        return true;
    else if (ty1->kind != ty2->kind)
        return false;

    switch (ty1->kind) {
    case CONST:
    case VOLATILE:
    case RESTRICT:
    case CONST + VOLATILE:
    case CONST + RESTRICT:
    case VOLATILE + RESTRICT:
    case CONST + VOLATILE + RESTRICT:
        return eqtype(ty1->type, ty2->type);
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
        return eqtype(ty1->type, ty2->type);
    case FUNCTION:
        if (!eqtype(ty1->type, ty2->type))
            return false;
        if (ty1->u.f.oldstyle && ty2->u.f.oldstyle) {
            // both oldstyle
            return true;
        } else if (!ty1->u.f.oldstyle && !ty2->u.f.oldstyle) {
            // both prototype
            return cmparams(ty1->u.f.proto, ty2->u.f.proto, eqtype);
        } else {
            // one oldstyle, the other prototype
            struct type *oldty = ty1->u.f.oldstyle ? ty1 : ty2;
            struct type *newty = ty1->u.f.oldstyle ? ty2 : ty1;

            if (TYPE_VARG(newty))
                return false;
            
            for (size_t i = 0; newty->u.f.proto[i]; i++) {
                struct type *ty = newty->u.f.proto[i];
                if (TYPE_KIND(ty) == _BOOL ||
                    TYPE_KIND(ty) == CHAR ||
                    TYPE_KIND(ty) == SHORT ||
                    TYPE_KIND(ty) == FLOAT)
                    return false;
            }

            if (isempty(oldty->u.f.proto))
                return true;

            return cmparams(oldty->u.f.proto, newty->u.f.proto, eqtype);
        }
    default:
        CC_UNAVAILABLE();
    }
}

struct field *find_field(struct type *ty, const char *name)
{
    assert(isrecord(ty));

    if (name == NULL)
        return NULL;

    for (struct field *p = TYPE_FIELDS(ty); p; p = p->link) {
        if (direct(p)->name == name)
            return p;
    }

    return NULL;
}

static void update_indirect_offset(struct type *ty)
{
    assert(isrecord(ty));

    struct field *first = TYPE_FIELDS(ty);
    for (struct field *p = first; p; p = p->link) {
        if (!isindirect(p))
            continue;
        p->offset += p->of[0]->offset;
    }
}

/* Structure alignment requirements
 *
 * The rule is that the structure will be padded out
 * to the size the type would occupy as an element
 * of an array of such types.
 *
 * The bitfields must be packed as tightly as possible.
 */
static void set_struct_size(struct type * ty)
{
    int max = 1;
    struct field *prev = NULL;
    struct field *first = TYPE_FIELDS(ty);

    for (struct field *field = first; field; field = field->link) {
        struct type *ty = field->type;

        if (isindirect(field))
            continue;

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

    update_indirect_offset(ty);

    TYPE_ALIGN(ty) = max;
    TYPE_SIZE(ty) = ROUNDUP(offset, max);
}

static void set_union_size(struct type * ty)
{
    int max = 1;
    int size = 0;
    struct field *first = TYPE_FIELDS(ty);

    for (struct field *field = first; field; field = field->link) {
        struct type *ty = field->type;
        int tysize = TYPE_SIZE(ty);

        if (tysize == 0)
            continue;
        if (isindirect(field))
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

    update_indirect_offset(ty);

    TYPE_ALIGN(ty) = max;
    TYPE_SIZE(ty) = ROUNDUP(size, max);
}

static void set_array_size(struct type * ty)
{
    struct type *rty = rtype(ty);

    TYPE_ALIGN(ty) = TYPE_ALIGN(rty);
    TYPE_SIZE(ty) = TYPE_LEN(ty) * TYPE_SIZE(rty);
}

void set_typesize(struct type * ty)
{
    if (isarray(ty))
        set_array_size(ty);
    else if (isstruct(ty))
        set_struct_size(ty);
    else if (isunion(ty))
        set_union_size(ty);
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

struct type *compose(struct type *ty1, struct type *ty2)
{
    if (isqual(ty1) && isqual(ty2)) {
        int kind = combine(ty1->kind, ty2->kind);
        return qual(kind, unqual(ty1));
    } else if (isqual(ty2)) {
        return qual(ty2->kind, ty1);
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

bool isstring(struct type *ty)
{
    if (!isarray(ty))
        return false;

    struct type *rty = rtype(ty);
    return TYPE_KIND(rty) == CHAR || unqual(rty) == wchartype;
}

short ty2op(struct type *ty)
{
    size_t size = TYPE_SIZE(ty);

    switch (TYPE_OP(ty)) {
    case INT:
        return I + MKOPSIZE(size);

    case UNSIGNED:
        return U + MKOPSIZE(size);

    case FLOAT:
        return F + MKOPSIZE(size);
        
    case FUNCTION:
        return P + MKOPSIZE(funcptype->size);
        
    case POINTER:
        return P + MKOPSIZE(size);
        
    case ARRAY:
    case STRUCT:
    case UNION:
        return S;

    case ENUM:
        return I + MKOPSIZE(size);
        
    case VOID:
    default:
        CC_UNAVAILABLE();
    }
}
