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

void init_type()
{
#define INSTALL_TYPE(ty, name, op, size)      install_type(&ty, name, op, size)
    // char
    INSTALL_TYPE(chartype, "char",  CHAR, sizeof(char));
    INSTALL_TYPE(unsignedchartype, "unsigned char", UNSIGNED, sizeof(unsigned char));
    INSTALL_TYPE(signedchartype, "signed char", CHAR, sizeof(signed char));
    // wchar_t
    INSTALL_TYPE(wchartype, "wchar_t", UNSIGNED, sizeof(wchar_t));
    // short
    INSTALL_TYPE(shorttype, "short", INT, sizeof(short));
    INSTALL_TYPE(unsignedshorttype, "unsigned short", UNSIGNED, sizeof(unsigned short));
    // int
    INSTALL_TYPE(inttype, "int", INT, sizeof(int));
    INSTALL_TYPE(unsignedinttype, "unsigned int", UNSIGNED, sizeof(unsigned));
    // long
    INSTALL_TYPE(longtype, "long", INT, sizeof(long));
    INSTALL_TYPE(unsignedlongtype, "unsigned long", UNSIGNED, sizeof(unsigned long));
    // long long
    INSTALL_TYPE(longlongtype, "long long", INT, sizeof(long long));
    INSTALL_TYPE(unsignedlonglongtype, "unsigned long long", UNSIGNED, sizeof(unsigned long long));
    // float
    INSTALL_TYPE(floattype, "float", FLOAT, sizeof(float));
    // double
    INSTALL_TYPE(doubletype, "double", DOUBLE, sizeof(double));
    INSTALL_TYPE(longdoubletype, "long double", DOUBLE, sizeof(long double));
    // void
    INSTALL_TYPE(voidtype, "void", VOID, 0);
    // bool
    INSTALL_TYPE(booltype, "_Bool", INT, sizeof(int));

    // variable
    INSTALL_TYPE(vartype, "vartype", ELLIPSIS, 0);
#undef INSTALL_TYPE

    chartype->limits.max.i = CHAR_MAX;
    chartype->limits.min.i = CHAR_MIN;
    
    unsignedchartype->limits.max.u = UCHAR_MAX;
    
    signedchartype->limits.max.i = SCHAR_MAX;
    signedchartype->limits.min.i = SCHAR_MIN;

    wchartype->limits.max.u = WCHAR_MAX;

    shorttype->limits.max.i = SHRT_MAX;
    shorttype->limits.min.i = SHRT_MIN;

    unsignedshorttype->limits.max.u = USHRT_MAX;
    
    inttype->limits.max.i = INT_MAX;
    inttype->limits.min.i = INT_MIN;
    
    unsignedinttype->limits.max.u = UINT_MAX;

    longtype->limits.max.i = LONG_MAX;
    longtype->limits.min.i = LONG_MIN;

    unsignedlongtype->limits.max.u = ULONG_MAX;

    longlongtype->limits.max.i = LLONG_MAX;
    longlongtype->limits.min.i = LLONG_MIN;

    unsignedlonglongtype->limits.max.u = ULLONG_MAX;

    floattype->limits.max.d = FLT_MAX;
    floattype->limits.min.d = FLT_MIN;

    doubletype->limits.max.d = DBL_MAX;
    doubletype->limits.min.d = DBL_MIN;

    longdoubletype->limits.max.ld = LDBL_MAX;
    longdoubletype->limits.min.ld = LDBL_MIN;

    booltype->limits.max.i = 1;
    booltype->limits.min.i = 0;
}

struct type * new_type()
{
    return alloc_type_node();
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

struct type * scls(int t, struct type *ty)
{
    struct type *sty = new_type();
    switch (t) {
    case AUTO:
    case REGISTER:
    case STATIC:
    case EXTERN:
	*sty = *ty;
	sty->sclass = t;
	break;
	
    case TYPEDEF:
	sty->sclass = t;
	sty->type = ty;
	break;
	
    default:
	assert(0);
    }
    return sty;
}

struct type * qual(int t, struct type *ty)
{
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
    
    if (sym && sym->type && sym->type->sclass == TYPEDEF)
	return sym->type;
    else
	return NULL;
}

int is_typedef_name(const char *id)
{
    if (!id)
	return 0;
    struct symbol *sym = lookup_symbol(id, identifiers);
    return sym && sym->type && sym->type->sclass == TYPEDEF;
}

struct type * array_type()
{
    struct type *ty = new_type();
    ty->op = ARRAY;

    return ty;
}

struct type * pointer_type()
{
    struct type *ty = new_type();
    ty->op = POINTER;

    return ty;
}

struct type * pointer(struct type *ty)
{
    struct type *pty = pointer_type();
    pty->type = ty;
    
    return pty;
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

struct type * record_type(int t, const char *tag)
{
    struct type *ty = new_type();
    ty->op = t;
    ty->name = tag;

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
    case TYPEDEF:
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

int isftype(struct type *type)
{
    if (type == NULL)
	return 0;
    else if (type->op == FUNCTION)
	return 1;
    else if (type->sclass == TYPEDEF)
	return isftype(type->type);
    else
	return 0;
}

int isatype(struct type *type)
{
    if (type == NULL)
	return 0;
    else if (type->op == ARRAY)
	return 1;
    else if (type->sclass == TYPEDEF)
	return isatype(type->type);
    else
	return 0;
}

static int eqproto(struct node **proto1, struct node **proto2)
{
    if (proto1 == proto2) {
	return 1;
    } else if (proto1 == NULL || proto2 == NULL) {
	return 0;
    } else {
	int len1 = array_length((void **)proto1);
	int len2 = array_length((void **)proto2);
	if (len1 != len2)
	    return 0;
	for (int i=0; i < len1; i++) {
	    struct node *decl1 = proto1[i];
	    struct node *decl2 = proto2[i];
	    if (decl1 == decl2)
		continue;
	    
	    struct symbol *sym1 = decl1->symbol;
	    struct symbol *sym2 = decl2->symbol;
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
    else if (ty1->sclass == TYPEDEF && ty2->sclass == TYPEDEF)
	return eqtype(ty1->type, ty2->type);
    else if (ty1->sclass == TYPEDEF)
	return eqtype(ty1->type, ty2);
    else if (ty2->sclass == TYPEDEF)
	return eqtype(ty1, ty2->type);
    else if (ty1->op != ty2->op)
        return 0;
    else if (ty1->qual_const != ty2->qual_const ||
	       ty1->qual_volatile != ty2->qual_volatile ||
	       ty1->qual_restrict != ty2->qual_restrict)
	return 0;
    else if (ty1->func_inline != ty2->func_inline)
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
	if (ty1->f.oldstyle && ty2->f.oldstyle) {
	    // both oldstyle
	    return 1;
	} else if (!ty1->f.oldstyle && !ty2->f.oldstyle) {
	    // both prototype
	    return eqproto(ty1->f.proto, ty2->f.proto);
	} else {
	    // one oldstyle, the other prototype
	    struct type *oldty = ty1->f.oldstyle ? ty1 : ty2;
	    struct type *newty = ty1->f.oldstyle ? ty2 : ty1;
	    if (newty->f.proto) {
		for (int i=0; newty->f.proto[i]; i++) {
		    struct node *decl = newty->f.proto[i];
		    if (decl && decl->symbol && decl->symbol->type) {
			struct type *ty = decl->symbol->type;
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

	    if (oldty->f.proto == NULL)
		return 1;

	    return eqproto(oldty->f.proto, newty->f.proto);
	}
	
    default:
	assert(0);
	return 0;
    }
}
