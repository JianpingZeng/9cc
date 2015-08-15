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

static void install_type(struct type **type, const char *name, int op, int size)
{
    struct type *ty = new_type();
    
    ty->name = strings(name);
    ty->op = op;
    ty->size = size;
    ty->reserved = 1;
    *type = ty;
}

void type_init()
{
#define INSTALL(type, name, op, size)    install_type(&type, name, op, size)
    // char
    INSTALL(chartype, "char",  CHAR, sizeof(char));
    INSTALL(unsignedchartype, "unsigned char", UNSIGNED, sizeof(unsigned char));
    INSTALL(signedchartype, "signed char", CHAR, sizeof(signed char));
    // wchar_t
    INSTALL(wchartype, "wchar_t", UNSIGNED, sizeof(wchar_t));
    // short
    INSTALL(shorttype, "short", INT, sizeof(short));
    INSTALL(unsignedshorttype, "unsigned short", UNSIGNED, sizeof(unsigned short));
    // int
    INSTALL(inttype, "int", INT, sizeof(int));
    INSTALL(unsignedinttype, "unsigned int", UNSIGNED, sizeof(unsigned));
    // long
    INSTALL(longtype, "long", INT, sizeof(long));
    INSTALL(unsignedlongtype, "unsigned long", UNSIGNED, sizeof(unsigned long));
    // long long
    INSTALL(longlongtype, "long long", INT, sizeof(long long));
    INSTALL(unsignedlonglongtype, "unsigned long long", UNSIGNED, sizeof(unsigned long long));
    // float
    INSTALL(floattype, "float", FLOAT, sizeof(float));
    // double
    INSTALL(doubletype, "double", DOUBLE, sizeof(double));
    INSTALL(longdoubletype, "long double", DOUBLE, sizeof(long double));
    // void
    INSTALL(voidtype, "void", VOID, 0);
    // bool
    INSTALL(booltype, "_Bool", INT, sizeof(int));
    
    // variable
    INSTALL(vartype, "vartype", ELLIPSIS, 0);
    

#define LIMITS(type, field, maxval, minval) \
        type->limits.max.field = maxval; \
	type->limits.min.field = minval

    LIMITS(chartype, i, CHAR_MAX, CHAR_MIN);
    LIMITS(unsignedchartype, u, UCHAR_MAX, 0);
    LIMITS(signedchartype, i, SCHAR_MAX, SCHAR_MIN);
    LIMITS(wchartype, u, WCHAR_MAX, 0);
    LIMITS(shorttype, i, SHRT_MAX, SHRT_MIN);
    LIMITS(unsignedshorttype, u, USHRT_MAX, 0);
    LIMITS(inttype, i, INT_MAX, INT_MIN);
    LIMITS(unsignedinttype, u, UINT_MAX, 0);
    LIMITS(longtype, i, LONG_MAX, LONG_MIN);
    LIMITS(unsignedlongtype, u, ULONG_MAX, 0);
    LIMITS(longlongtype, i, LLONG_MAX, LLONG_MIN);
    LIMITS(unsignedlonglongtype, u, ULLONG_MAX, 0);
    LIMITS(floattype, d, FLT_MAX, FLT_MIN);
    LIMITS(doubletype, d, DBL_MAX, DBL_MIN);
    LIMITS(longdoubletype, ld, LDBL_MAX, LDBL_MIN);
    LIMITS(booltype, i, 1, 0);
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
    *qty = *ty;
    switch (t) {
        case CONST:
            qty->qual_const = 1;
            break;
        case VOLATILE:
            qty->qual_volatile = 1;
            break;
        case RESTRICT:
            qty->qual_restrict = 1;
            break;
        case INLINE:
            qty->func_inline = 1;
            break;
        default:
            assert(0);
    }
    
    return qty;
}

struct type * unqual(int t, struct type *ty)
{
    assert(ty);
    struct type *qty = new_type();
    *qty = *ty;
    switch (t) {
        case CONST:
            qty->qual_const = 0;
            break;
        case VOLATILE:
            qty->qual_volatile = 0;
            break;
        case RESTRICT:
            qty->qual_restrict = 0;
            break;
        case INLINE:
            qty->func_inline = 0;
            break;
        default:
            assert(0);
    }
    
    return qty;
}

struct type * lookup_typedef_name(const char *id)
{
    if (!id)
        return NULL;
    
    struct symbol *sym = lookup_symbol(id, identifiers);
    
    if (sym && sym->sclass == TYPEDEF)
        return sym->type;
    else
        return NULL;
}

int is_typedef_name(const char *id)
{
    if (!id)
        return 0;
    struct symbol *sym = lookup_symbol(id, identifiers);
    return sym && sym->sclass == TYPEDEF;
}

struct type * array_type()
{
    struct type *ty = new_type();
    ty->op = ARRAY;
    
    return ty;
}

struct type * pointer_type(struct type *type)
{
    struct type *ty = new_type();
    ty->op = POINTER;
    ty->type = type;
    
    return ty;
}

struct type * function_type()
{
    struct type *ty = new_type();
    ty->op = FUNCTION;
    
    return ty;
}

struct type * enum_type(const char *tag)
{
    struct type *ty = new_type();
    ty->op = ENUM;
    ty->name = tag;
    ty->type = inttype;		// aka int
    
    return ty;
}

struct type * tag_type(int op, const char *tag, struct source src)
{
    struct type *ty = new_type();
    ty->op = op;
    ty->name = tag;
    // TODO
    if (op == ENUM)
        ty->type = inttype;
    if (tag) {
        struct symbol *sym = lookup_symbol(tag, tags);
        if ((sym && sym->scope == SCOPE) ||
            (sym && sym->scope == PARAM && SCOPE == LOCAL)) {
            if (sym->type->op == op && !sym->defined)
                return sym->type;
            
            redefinition_error(src, sym);
        }

        sym = install_symbol(tag, &tags, SCOPE);
        sym->type = ty;
        sym->src = src;
        ty->u.s.symbol = sym;
    } else {
        struct symbol *sym = anonymous_symbol(&tags, SCOPE);
        sym->type = ty;
        sym->src = src;
        ty->u.s.symbol = sym;
    }
    
    return ty;
}

const char * pname(struct type *type)
{
    switch (type->op) {
        case POINTER:
            return "pointer";
        case ARRAY:
            return "array";
        case FUNCTION:
            return "function";
        case ENUM:
            return "enum";
        case STRUCT:
            return "struct";
        case UNION:
            return "union";
        case VOID:
        case CHAR:
        case INT:
        case UNSIGNED:
        case FLOAT:
        case DOUBLE:
            return type->name;
        default:
            return "unknown";
    }
}

static int eqparams(struct symbol **params1, struct symbol **params2)
{
    if (params1 == params2) {
        return 1;
    } else if (params1 == NULL || params2 == NULL) {
        return 0;
    } else {
        int len1 = array_len((void **)params1);
        int len2 = array_len((void **)params2);
        if (len1 != len2)
            return 0;
        for (int i=0; i < len1; i++) {
            struct symbol *sym1 = params1[i];
            struct symbol *sym2 = params2[i];
            if (sym1 == sym2)
                continue;
            else if (sym1 == NULL || sym2 == NULL)
                return 0;
            else if (eqtype(sym1->type, sym2->type))
                continue;
            else
                return 0;
        }
        
        return 1;
    }
}

int eqtype(struct type *ty1, struct type *ty2)
{
    if (ty1 == ty2)
        return 1;
    else if (ty1 == NULL || ty2 == NULL)
        return 0;
    else if (ty1->op != ty2->op)
        return 0;
    else if (ty1->qual_const != ty2->qual_const ||
             ty1->qual_volatile != ty2->qual_volatile ||
             ty1->qual_restrict != ty2->qual_restrict)
        return 0;
    
    switch (ty1->op) {
        case ENUM:
        case UNION:
        case STRUCT:
            return 0;
            
        case CHAR:
        case INT:
        case UNSIGNED:
        case FLOAT:
        case DOUBLE:
        case VOID:
            return 1;
            
        case POINTER:
        case ARRAY:
            return eqtype(ty1->type, ty2->type);
            
        case FUNCTION:
            if (!eqtype(ty1->type, ty2->type))
                return 0;
            if (ty1->u.f.oldstyle && ty2->u.f.oldstyle) {
                // both oldstyle
                return 1;
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
                            struct type *ty = sym->type;
                            if (ty->op == CHAR)
                                return 0;
                            else if (ty->op == INT && ty->size == sizeof(short))
                                return 0;
                            else if (ty->op == UNSIGNED && (ty->size == sizeof(unsigned short)
                                                            || ty->size == sizeof(unsigned char)))
                                return 0;
                            else if (ty->op == FLOAT)
                                return 0;
                            else if (ty->op == ELLIPSIS)
                                return 0;
                        }
                    }
                }
                
                if (oldty->u.f.params == NULL)
                    return 1;
                
                return eqparams(oldty->u.f.params, newty->u.f.params);
            }
            
        default:
            assert(0);
            return 0;
    }
}
