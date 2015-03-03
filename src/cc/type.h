#ifndef cc_type_h
#define cc_type_h

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
    struct type *type;
    union {
	struct {
	    struct decl *proto;
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
extern struct type * new_type();
extern void prepend_type(struct type **typelist, struct type *type);
extern void attach_type(struct type **typelist, struct type *type);
extern struct type * scls(int t, struct type *ty);
extern struct type * qual(int t, struct type *ty);
extern struct type * unqual(int t, struct type *ty);
extern int equal_type(struct type *ty1, struct type *ty2);
extern struct type * lookup_typedef_name(const char *id);
extern int is_typedef_name(const char *id);
extern struct type * array_type();
extern struct type * pointer_type();
extern struct type * function_type();
extern struct type * enum_type(const char *tag);

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

#define isfunction(type)    ((type) && (type)->op == FUNCTION)
#define isarray(type)       ((type) && (type)->op == ARRAY)
#define ispointer(type)     ((type) && (type)->op == POINTER)
#define isconst(type)       ((type) && (type)->qual_const)
#define isvolatile(type)    ((type) && (type)->qual_volatile)
#define isrestrict(type)    ((type) && (type)->qual_restrict)
#define isqual(type)        (isconst(type) || isvolatile(type) || isrestrict(type))
#define isinline(type)      ((type) && (type)->func_spec)
#define isvoid(type)        ((type) && (type)->op == VOID)
#define isenum(type)        ((type) && (type)->op == ENUM)
#define isstruct(type)      ((type) && (type)->op == STRUCT)
#define isunion(type)       ((type) && (type)->op == UNION)
#define isrecord(type)      (isstruct(type) || isunion(type))
#define istypedef(type)     ((type) && (type)->op == TYPEDEF)

#endif
