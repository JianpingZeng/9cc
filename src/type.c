#include "cc.h"

// predefined types
struct type   *chartype;               // char
struct type   *unsignedchartype;       // unsigned char
struct type   *signedchartype;         // signed char
struct type   *wchartype;              // wchar_t
struct type   *shorttype;              // short (int)
struct type   *unsignedshorttype;      // unsigned short (int)
struct type   *inttype;                // int
struct type   *unsignedinttype;        // unsigned (int)
struct type   *longtype;               // long
struct type   *unsignedlongtype;       // unsigned long (int)
struct type   *longlongtype;           // long long (int)
struct type   *unsignedlonglongtype;   // unsigned long long (int)
struct type   *floattype;              // float
struct type   *doubletype;             // double
struct type   *longdoubletype;         // long double
struct type   *voidtype;               // void
struct type   *booltype;	       // bool
struct type   *vartype;		       // variable type

struct metrics {
    size_t size;
    unsigned rank;
}
boolmetrics         = { sizeof (_Bool),     10},
charmetrics         = { sizeof (char),      20},
shortmetrics        = { sizeof (short),     30},
wcharmetrics        = { sizeof (wchar_t),   40},
intmetrics          = { sizeof (int),       40},
longmetrics         = { sizeof (long),      50},
longlongmetrics     = { sizeof (long long), 60},
floatmetrics        = { sizeof (float),     70},
doublemetrics       = { sizeof (double),    80},
longdoublemetrics   = { sizeof (long double), 90},
zerometrics         = { 0 };

static struct type * new_type()
{
    return NEWS(type);
}

static struct type * install_type(const char *name, int kind, struct metrics m)
{
    struct type *ty = new_type();
    
    ty->name = strings(name);
    ty->kind = kind;
    ty->size = m.size;
    ty->rank = m.rank;
    switch (op(ty)) {
        case INT:
            ty->limits.max.i = TWOS(ty->size) >> 1;
            ty->limits.min.i = -ty->limits.max.i - 1;
            break;
            
        case UNSIGNED:
            ty->limits.max.u = TWOS(ty->size);
            ty->limits.min.u = 0;
            break;
            
        case FLOAT:
            if (ty->size == sizeof (float)) {
                ty->limits.max.d = FLT_MAX;
                ty->limits.min.d = FLT_MIN;
            } else if (ty->size == sizeof (double)) {
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

void type_init()
{
#define INSTALL(type, name, kind, metrics)    type = install_type(name, kind, metrics)
    
    // type                     name                    kind            metrics             op
    
    // bool
    INSTALL(booltype,           "_Bool",                _BOOL,          boolmetrics);       // UNSIGNED
    // char
    INSTALL(chartype,           "char",                 CHAR,           charmetrics);       // INT
    INSTALL(unsignedchartype,   "unsigned char",        CHAR,           charmetrics);       // UNSIGNED
    INSTALL(signedchartype,     "signed char",          CHAR,           charmetrics);       // INT
    // wchar_t
    INSTALL(wchartype,          "wchar_t",              UNSIGNED,       wcharmetrics);      // UNSIGNED
    // short
    INSTALL(shorttype,          "short",                SHORT,          shortmetrics);      // INT
    INSTALL(unsignedshorttype,  "unsigned short",       SHORT,          shortmetrics);      // UNSIGNED
    // int
    INSTALL(inttype,            "int",                  INT,            intmetrics);        // INT
    INSTALL(unsignedinttype,    "unsigned int",         UNSIGNED,       intmetrics);        // UNSIGNED
    // long
    INSTALL(longtype,           "long",                 LONG,           longmetrics);       // INT
    INSTALL(unsignedlongtype,   "unsigned long",        LONG,           longmetrics);       // UNSIGNED
    // long long
    INSTALL(longlongtype,       "long long",            LONG+LONG,      longlongmetrics);   // INT
    INSTALL(unsignedlonglongtype, "unsigned long long", LONG+LONG,      longlongmetrics);   // UNSIGNED
    // float
    INSTALL(floattype,          "float",                FLOAT,          floatmetrics);      // FLOAT
    // double
    INSTALL(doubletype,         "double",               DOUBLE,         doublemetrics);     // FLOAT
    INSTALL(longdoubletype,     "long double",          LONG+DOUBLE,    longdoublemetrics); // FLOAT
    // void
    INSTALL(voidtype,           "void",                 VOID,           zerometrics);       // VOID
    // variable
    INSTALL(vartype,            "vartype",              ELLIPSIS,       zerometrics);       // ELLIPSIS

#undef INSTALL
}

int op(struct type *type)
{
    int kind = kind(type);
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
            
        case LONG+LONG:
            if (unqual(type) == unsignedlonglongtype)
                return UNSIGNED;
            else
                return INT;
            
        case DOUBLE:
        case LONG+DOUBLE:
            return FLOAT;
            
        case INT:
        case UNSIGNED:
        case FLOAT:
        default:
            return kind;
    }
}

void prepend_type(struct type **typelist, struct type *type)
{
    attach_type(&type, *typelist);
    *typelist = type;
}

void attach_type(struct type **typelist, struct type *type)
{
    if (*typelist) {
        struct type *tp = *typelist;
        while (tp && tp->type) {
            tp = tp->type;
        }
        tp->type = type;
    }
    else {
        *typelist = type;
    }
}

struct type * qual(int t, struct type *ty)
{
    assert(ty);
    struct type *qty = new_type();
    qty->type = unqual(ty);
    qty->q = ty->q;
    switch (t) {
        case CONST:     qty->q.is_const = 1;    break;
        case VOLATILE:  qty->q.is_volatile = 1; break;
        case RESTRICT:  qty->q.is_restrict = 1; break;
        case INLINE:    qty->q.is_inline = 1;   break;
        default:        assert(0);
    }
    return qty;
}

struct type * lookup_typedef_name(const char *id)
{
    if (!id)
        return NULL;
    
    struct symbol *sym = lookup(id, identifiers);
    
    if (sym && sym->sclass == TYPEDEF)
        return sym->type;
    else
        return NULL;
}

bool is_typedef_name(const char *id)
{
    struct type *ty = lookup_typedef_name(id);
    return ty != NULL;
}

struct field * new_field(char *id)
{
    struct field *field = NEWS(field);
    field->name = id;
    return field;
}

struct type * array_type()
{
    struct type *ty = new_type();
    ty->kind = ARRAY;
    ty->name = "array";
    
    return ty;
}

struct type * ptr_type(struct type *type)
{
    struct type *ty = new_type();
    ty->kind = POINTER;
    ty->type = type;
    ty->name = "pointer";
    
    return ty;
}

struct type * func_type()
{
    struct type *ty = new_type();
    ty->kind = FUNCTION;
    ty->name = "function";
    
    return ty;
}

struct symbol * tag_type(int t, const char *tag, struct source src)
{
    struct type *ty = new_type();
    ty->kind = t;
    ty->tag = tag;
    ty->name = tname(t);
    if (t == ENUM)
        ty->type = inttype;
    
    struct symbol *sym = NULL;
    if (tag) {
        sym = lookup(tag, tags);
        if (sym && currentscope(sym)) {
            if (op(sym->type) == t && !sym->defined)
                return sym;
            
            redefinition_error(src, sym);
        }

        sym = install(tag, &tags, SCOPE);
        sym->type = ty;
        sym->src = src;
    } else {
        sym = anonymous(&tags, SCOPE);
        sym->type = ty;
        sym->src = src;
        ty->tag = sym->name;
    }
    
    return sym;
}


struct symbol * tag_sym(struct type *ty)
{
    assert(ty->tag);
    struct symbol *sym = lookup(ty->tag, tags);
    assert(sym && sym->type == ty);
    return sym;
}

static bool eqparams(struct symbol **params1, struct symbol **params2)
{
    if (params1 == params2) {
        return true;
    } else if (params1 == NULL || params2 == NULL) {
        return false;
    } else {
        int len1 = array_len((void **)params1);
        int len2 = array_len((void **)params2);
        if (len1 != len2)
            return false;
        for (int i=0; i < len1; i++) {
            struct symbol *sym1 = params1[i];
            struct symbol *sym2 = params2[i];
            if (sym1 == sym2)
                continue;
            else if (sym1 == NULL || sym2 == NULL)
                return false;
            else if (eqtype(sym1->type, sym2->type))
                continue;
            else
                return false;
        }
        
        return true;
    }
}

//TODO
bool eqtype(struct type *ty1, struct type *ty2)
{
    if (ty1 == ty2)
        return true;
    else if (ty1 == NULL || ty2 == NULL)
        return false;
    
    if (op(ty1) != op(ty2))
        return false;
    else if (ty1->q.is_const != ty2->q.is_const ||
             ty1->q.is_volatile != ty2->q.is_volatile ||
             ty1->q.is_restrict != ty2->q.is_restrict ||
             ty1->q.is_inline != ty2->q.is_inline)
        return false;
    
    ty1 = unqual(ty1);
    ty2 = unqual(ty2);
    
    switch (op(ty1)) {
        case ENUM:
        case UNION:
        case STRUCT:
            return false;
            
        case INT:
        case UNSIGNED:
        case FLOAT:
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
                return eqparams(ty1->u.f.params, ty2->u.f.params);
            } else {
                // one oldstyle, the other prototype
                struct type *oldty = ty1->u.f.oldstyle ? ty1 : ty2;
                struct type *newty = ty1->u.f.oldstyle ? ty2 : ty1;
                if (newty->u.f.params) {
                    for (int i=0; newty->u.f.params[i]; i++) {
                        struct symbol *sym = newty->u.f.params[i];
                        if (sym->type) {
                            struct type *ty = unqual(sym->type);
                            if (kind(ty) == CHAR || kind(ty) == SHORT)
                                return false;
                            else if (op(ty) == FLOAT)
                                return false;
                            else if (op(ty) == ELLIPSIS)
                                return false;
                        }
                    }
                }
                
                if (oldty->u.f.params == NULL)
                    return true;
                
                return eqparams(oldty->u.f.params, newty->u.f.params);
            }
            
        default:
            assert(0);
            return false;
    }
}

bool eqarith(struct type *ty1, struct type *ty2)
{
    return kind(ty1) == kind(ty2) && op(ty1) == op(ty2);
}

bool isfunc(struct type *ty)
{
    return op(ty) == FUNCTION;
}

bool isarray(struct type *ty)
{
    return op(ty) == ARRAY;
}

bool isptr(struct type *ty)
{
    return op(ty) == POINTER;
}

bool isvoid(struct type *ty)
{
    return op(ty) == VOID;
}

bool isenum(struct type *ty)
{
    return op(ty) == ENUM;
}

bool isstruct(struct type *ty)
{
    return op(ty) == STRUCT;
}

bool isunion(struct type *ty)
{
    return op(ty) == UNION;
}

bool isint(struct type *ty)
{
    return op(ty) == INT || op(ty) == UNSIGNED || isenum(ty);
}

bool isfloat(struct type *ty)
{
    return op(ty) == FLOAT;
}

bool isarith(struct type *ty)
{
    return isint(ty) || isfloat(ty);
}

bool isscalar(struct type *ty)
{
    return isarith(ty) || isptr(ty);
}
