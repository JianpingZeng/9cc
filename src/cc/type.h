#ifndef cc_type_h
#define cc_type_h

typedef struct type     Type;

union value {
    long long i;
    unsigned long long u;
    double d;
    long double ld;
    void *p;
    void (*g) ();
};

struct type {
    int op;
    const char *name;
    int size;
    int sclass;
    unsigned qual_const : 1;
    unsigned qual_volatile : 1;
    unsigned qual_restrict : 1;
    unsigned func_spec : 1;
    unsigned reserved : 1;
    Type *type;
    union {
	struct {
	    void **proto;
	    unsigned oldstyle : 1;
	}f;
    }u;
    struct {
	union value max;
	union value min;
    }limits;
};

// type
extern const char * type_print_function(void *data);

extern void init_type();
extern Type *pretype(int tok);
extern void printtype(Type *type);
extern void prepend_type(Type **typelist, Type *type);
extern void attach_type(Type **typelist, Type *type);
extern Type *scls(int t, Type *ty);
extern Type *qual(int t, Type *ty);
extern Type *unqual(int t, Type *ty);
extern int equal_type(Type *ty1, Type *ty2);
extern int istypedefname(const char *id);
extern Type *typename();
extern Type * arraytype(Type *basety, size_t n, void *p);

extern Type    *chartype;               // char
extern Type    *unsignedchartype;       // unsigned char
extern Type    *signedchartype;         // signed char
extern Type    *wchartype;              // wchar_t
extern Type    *shorttype;              // short (int)
extern Type    *unsignedshorttype;      // unsigned short (int)
extern Type    *inttype;                // int
extern Type    *unsignedinttype;        // unsigned (int)
extern Type    *longtype;               // long
extern Type    *unsignedlongtype;       // unsigned long (int)
extern Type    *longlongtype;           // long long (int)
extern Type    *unsignedlonglongtype;   // unsigned long long (int)
extern Type    *floattype;              // float
extern Type    *doubletype;             // double
extern Type    *longdoubletype;         // long double
extern Type    *voidtype;               // void

#define isfunction(type)    ((type) && (type)->op == FUNCTION)
#define isarray(type)       ((type) && (type)->op == ARRAY)
#define ispointer(type)     ((type) && (type)->op == POINTER)
#define isconst(type)       ((type) && (type)->qual_const)
#define isvolatile(type)    ((type) && (type)->qual_volatile)
#define isrestrict(type)    ((type) && (type)->qual_restrict)
#define isqual(type)        (isconst(type) || isvolatile(type) || isrestrict(type))
#define isinline(type)      ((type) && (type)->func_spec)
#define isvoid(type)        ((type) && (type)->op == VOID)


#endif
