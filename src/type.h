#ifndef _type_h
#define _type_h

struct type {
    int op;
    const char *name;
    int size;
    unsigned qual_const : 1;
    unsigned qual_volatile : 1;
    unsigned qual_restrict : 1;
    unsigned func_inline : 1;
    unsigned reserved : 1;
    struct type *type;
    union {
        // function
        struct {
            struct symbol **params;
            unsigned oldstyle : 1;
        }f;
        // array
        struct {
            struct expr *assign;
            unsigned qual_const : 1;
            unsigned qual_volatile : 1;
            unsigned qual_restrict : 1;
            unsigned sclass_static : 1;
            unsigned wildcard : 1;
        }a;
        // enum/struct/union
        struct {
            struct symbol *symbol;
            struct symbol **ids;
            struct field **fields;
        }s;
    }u;
    struct {
        union value max;
        union value min;
    }limits;
};

extern const char * pname(struct type *type);
extern void type_init();
extern void prepend_type(struct type **typelist, struct type *type);
extern void attach_type(struct type **typelist, struct type *type);
extern struct type * qual(int t, struct type *ty);
extern struct type * unqual(int t, struct type *ty);
extern int eqtype(struct type *ty1, struct type *ty2);
extern struct type * lookup_typedef_name(const char *id);
extern int is_typedef_name(const char *id);
extern struct type * array_type();
extern struct type * pointer_type(struct type *ty);
extern struct type * function_type();
extern struct type * tag_type(int op, const char *tag, struct source src);

extern struct type    *chartype;               // char
extern struct type    *unsignedchartype;       // unsigned char
extern struct type    *signedchartype;         // signed char
extern struct type    *wchartype;              // wchar_t
extern struct type    *shorttype;              // short (int)
extern struct type    *unsignedshorttype;      // unsigned short (int)
extern struct type    *inttype;                // int
extern struct type    *unsignedinttype;        // unsigned (int)
extern struct type    *longtype;               // long
extern struct type    *unsignedlongtype;       // unsigned long (int)
extern struct type    *longlongtype;           // long long (int)
extern struct type    *unsignedlonglongtype;   // unsigned long long (int)
extern struct type    *floattype;              // float
extern struct type    *doubletype;             // double
extern struct type    *longdoubletype;         // long double
extern struct type    *voidtype;               // void
extern struct type    *booltype;	       // bool
extern struct type    *vartype;		       // variable type

#define isfunction(type)    ((type) && (type)->op == FUNCTION)
#define isarray(type)       ((type) && (type)->op == ARRAY)
#define ispointer(type)     ((type) && (type)->op == POINTER)
#define isconst(type)       ((type) && (type)->qual_const)
#define isvolatile(type)    ((type) && (type)->qual_volatile)
#define isrestrict(type)    ((type) && (type)->qual_restrict)
#define isqual(type)        (isconst(type) || isvolatile(type) || isrestrict(type))
#define isinline(type)      ((type) && (type)->func_inline)
#define isvoid(type)        ((type) && (type)->op == VOID)
#define isenum(type)        ((type) && (type)->op == ENUM)
#define isstruct(type)      ((type) && (type)->op == STRUCT)
#define isunion(type)       ((type) && (type)->op == UNION)

#endif
