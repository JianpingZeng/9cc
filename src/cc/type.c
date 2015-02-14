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

// static void printspec(struct type *type)
// {
//     if (type->sclass == STATIC) {
//         printf("static ");
//     }
//     else if (type->sclass == REGISTER) {
//         printf("register ");
//     }
//     if (isconst(type)) {
//         printf("const ");
//     }
//     if (isvolatile(type)) {
//         printf("volatile ");
//     }
//     if (isrestrict(type)) {
//         printf("restrict ");
//     }
//     if (isinline(type)) {
//         printf("inline ");
//     }
// }

// static void printtype1(struct type *type);

// static void printfparams(struct type *ftype)
// {
// 	printf("( ");
// 	if (ftype->u.f.proto) {
// 		for (int i = 0; ftype->u.f.proto[i]; i++) {
// 			printtype1(ftype->u.f.proto[i]->type);
//             if (ftype->u.f.proto[i+1]) {
//                 printf(", ");
//             }
// 		}
// 	}
// 	printf(")");
// }

// static void printtype1(struct type *type)
// {
//     if (type) {
//         if (isfunction(type)) {
//             printspec(type);
//             printf("%s ", tname(type->op));
//             printfparams(type);
//             printf(" returning ");
//         }
//         else if (ispointer(type)) {
//             printspec(type);
//             printf("%s to ", tname(type->op));
//         }
//         else if (isarray(type)) {
//             printspec(type);
//             printf("%s %d of ", tname(type->op), type->size);
//         }
//         else {
//             printspec(type);
//             printf("%s ", type->u.sym->lex.name);
//         }
//         printtype1(type->type);
//     }
// }

void printtype(struct type *type)
{
    // printtype1(type);
    // printf("\n");
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
    if (t > 0) {
        struct type *sty = new_type();
        *sty = *ty;
        sty->sclass = t;
        if (!ty->reserved) {
            // delete(ty);
        }
        return sty;
    }
    else {
        return ty;
    }
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
	qty->func_spec = 1;
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
	qty->func_spec = 0;
	break;
    default:
	assert(0);
    }
    
    return qty;
}

int equal_type(struct type *ty1, struct type *ty2)
{
    if (ty1 == ty2) {
        return 1;
    }
    if (ty1 == NULL || ty2 == NULL) {
        return 0;
    }
    if (ty1->op != ty2->op) {
        return 0;
    }
    return 1;
}

int is_typedef_name(const char *id)
{
    
    return 0;
}

struct type * typename()
{
    
    return NULL;
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

struct type * function_type()
{
    struct type *ty = new_type();
    ty->op = FUNCTION;

    return ty;
}

const char * type_print_function(void *data)
{
    struct type *p = data;
    return p->name;
}
