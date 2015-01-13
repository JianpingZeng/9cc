#include "c.h"

// predefined types
Type    chartype;               // char
Type    unsignedchartype;       // unsigned char
Type    signedchartype;         // signed char
Type    shorttype;              // short (int)
Type    unsignedshorttype;      // unsigned short (int)
Type    inttype;                // int
Type    unsignedinttype;        // unsigned (int)
Type    longtype;               // long
Type    unsignedlongtype;       // unsigned long (int)
Type    longlongtype;           // long long (int)
Type    unsignedlonglongtype;   // unsigned long long (int)
Type    floattype;              // float
Type    doubletype;             // double
Type    longdoubletype;         // long double
Type    voidtype;               // void

static void install_type(Type *type, const char *name, int op)
{
    Symbol sym;
    Type ty;
    NEW(sym);
    NEW(ty);
    
    sym->lex.name = string(name);
    sym->type = ty;
    ty->op = op;
    ty->u.sym = sym;
    ty->reserved = 1;
    *type = ty;
}

void init_type()
{
#define INSTALL_TYPE(ty, name, op)      install_type(&ty, name, op)
    // char
    INSTALL_TYPE(chartype, "char",  CHAR);
    INSTALL_TYPE(unsignedchartype, "unsigned char", CHAR);
    INSTALL_TYPE(signedchartype, "signed char", CHAR);
    // short
    INSTALL_TYPE(shorttype, "short", INT);
    INSTALL_TYPE(unsignedshorttype, "unsigned short", INT);
    // int
    INSTALL_TYPE(inttype, "int", INT);
    INSTALL_TYPE(unsignedinttype, "unsigned int", INT);
    // long
    INSTALL_TYPE(longtype, "long", INT);
    INSTALL_TYPE(unsignedlongtype, "unsigned long", INT);
    // long long
    INSTALL_TYPE(longlongtype, "long long", INT);
    INSTALL_TYPE(unsignedlonglongtype, "unsigned long long", INT);
    // float
    INSTALL_TYPE(floattype, "float", FLOAT);
    // double
    INSTALL_TYPE(doubletype, "double", DOUBLE);
    INSTALL_TYPE(longdoubletype, "long double", DOUBLE);
    // void
    INSTALL_TYPE(voidtype, "void", VOID);
#undef INSTALL_TYPE
}

static void printspec(Type type)
{
    if (type->sclass == STATIC) {
        printf("static ");
    }
    else if (type->sclass == REGISTER) {
        printf("register ");
    }
    if (isconst(type)) {
        printf("const ");
    }
    if (isvolatile(type)) {
        printf("volatile ");
    }
    if (isrestrict(type)) {
        printf("restrict ");
    }
    if (isinline(type)) {
        printf("inline ");
    }
}

static void printtype1(Type type);

static void printfparams(Type ftype)
{
	printf("( ");
	if (ftype->u.f.proto) {
		for (int i = 0; ftype->u.f.proto[i]; i++) {
			printtype1(ftype->u.f.proto[i]->type);
            if (ftype->u.f.proto[i+1]) {
                printf(", ");
            }
		}
	}
	printf(")");
}

static void printtype1(Type type)
{
    if (type) {
        if (isfunction(type)) {
            printspec(type);
            printf("%s ", tname(type->op));
            printfparams(type);
            printf(" returning ");
        }
        else if (ispointer(type)) {
            printspec(type);
            printf("%s to ", tname(type->op));
        }
        else if (isarray(type)) {
            printspec(type);
            printf("%s %d of ", tname(type->op), type->size);
        }
        else {
            printspec(type);
            printf("%s ", type->u.sym->lex.name);
        }
        printtype1(type->type);
    }
}

void printtype(Type type)
{
    printtype1(type);
    printf("\n");
}

void prepend_type(Type *typelist, Type type)
{
    attach_type(&type, *typelist);
    *typelist = type;
}

void attach_type(Type *typelist, Type type)
{
    if (*typelist) {
        Type tp = *typelist;
        while (tp && tp->type) {
            tp = tp->type;
        }
        tp->type = type;
    }
    else {
        *typelist = type;
    }
}

Type pretype(int tok)
{
    switch (tok) {
        case INT:
            return inttype;
        case CHAR:
            return chartype;
        case FLOAT:
            return floattype;
        case DOUBLE:
            return doubletype;
        case VOID:
            return voidtype;
        default:
            return NULL;
    }
}

Type scls(int t, Type ty)
{
    if (t > 0) {
        Type sty;
        NEW(sty);
        *sty = *ty;
        sty->sclass = t;
        if (!ty->reserved) {
            FREE(ty);
        }
        return sty;
    }
    else {
        return ty;
    }
}

Type qual(int t, Type ty)
{
    Type qty;
    NEW(qty);
    *qty = *ty;
    if (t == CONST) {
        qty->qual_const = 1;
    }
    else if (t == VOLATILE) {
        qty->qual_volatile = 1;
    }
    else if (t == RESTRICT) {
        qty->qual_restrict = 1;
    }
    else if (t == INLINE) {
        qty->func_spec = 1;
    }
    if (!ty->reserved) {
        FREE(ty);
    }
    return qty;
}

Type unqual(int t, Type ty)
{
    Type qty;
    NEW(qty);
    *qty = *ty;
    if (t == CONST) {
        qty->qual_const = 0;
    }
    else if (t == VOLATILE) {
        qty->qual_volatile = 0;
    }
    else if (t == RESTRICT) {
        qty->qual_restrict = 0;
    }
    else if (t == INLINE) {
        qty->func_spec = 0;
    }
    if (!ty->reserved) {
        FREE(ty);
    }
    return qty;
}

int equal_type(Type ty1, Type ty2)
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

int istypedefname(const char *id)
{
    
    return 0;
}

Type typename()
{
    
    return NULL;
}

