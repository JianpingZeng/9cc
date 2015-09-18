#include "cc.h"

// predefined types
union node   *chartype;               // char
union node   *unsignedchartype;       // unsigned char
union node   *signedchartype;         // signed char
union node   *wchartype;              // wchar_t
union node   *shorttype;              // short (int)
union node   *unsignedshorttype;      // unsigned short (int)
union node   *inttype;                // int
union node   *unsignedinttype;        // unsigned (int)
union node   *longtype;               // long
union node   *unsignedlongtype;       // unsigned long (int)
union node   *longlongtype;           // long long (int)
union node   *unsignedlonglongtype;   // unsigned long long (int)
union node   *floattype;              // float
union node   *doubletype;             // double
union node   *longdoubletype;         // long double
union node   *voidtype;               // void
union node   *booltype;	       // bool
union node   *vartype;		       // variable type

struct metrics {
    size_t size;
    unsigned rank;
}
                       // size   rank
#ifdef X32
    boolmetrics         = { 1,  10},
    charmetrics         = { 1,  20},
    shortmetrics        = { 2,  30},
    wcharmetrics        = { 4,  40},
    intmetrics          = { 4,  40},
    longmetrics         = { 4,  50},
    longlongmetrics     = { 8,  60},
    floatmetrics        = { 4,  70},
    doublemetrics       = { 8,  80},
    longdoublemetrics   = { 8,  90},
    ptrmetrics          = { 4 },
#elif defined X64
    boolmetrics         = { 1,  10},
    charmetrics         = { 1,  20},
    shortmetrics        = { 2,  30},
    wcharmetrics        = { 4,  40},
    intmetrics          = { 4,  40},
    longmetrics         = { 8,  50},
    longlongmetrics     = { 8,  60},
    floatmetrics        = { 4,  70},
    doublemetrics       = { 8,  80},
    longdoublemetrics   = { 16, 90},
    ptrmetrics          = { 8 },
#else
    #error "architecture not defined."
#endif
    zerometrics         = { 0 };

static inline union node * new_type()
{
    return alloc_type();
}

static union node * install_type(const char *name, int kind, struct metrics m)
{
    union node *ty = new_type();
    
    TYPE_NAME(ty) = strs(name);
    TYPE_KIND(ty) = kind;
    TYPE_SIZE(ty) = m.size;
    TYPE_RANK(ty) = m.rank;
    switch (op(ty)) {
        case INT:
            TYPE_LIMITS_MAX(ty).i = TWOS(TYPE_SIZE(ty)) >> 1;
            TYPE_LIMITS_MIN(ty).i = -TYPE_LIMITS_MAX(ty).i - 1;
            break;
            
        case UNSIGNED:
            TYPE_LIMITS_MAX(ty).u = TWOS(TYPE_SIZE(ty));
            TYPE_LIMITS_MIN(ty).u = 0;
            break;
            
        case FLOAT:
            if (TYPE_SIZE(ty) == sizeof (float)) {
                TYPE_LIMITS_MAX(ty).d = FLT_MAX;
                TYPE_LIMITS_MIN(ty).d = FLT_MIN;
            } else if (TYPE_SIZE(ty) == sizeof (double)) {
                TYPE_LIMITS_MAX(ty).d = DBL_MAX;
                TYPE_LIMITS_MIN(ty).d = DBL_MIN;
            } else {
                TYPE_LIMITS_MAX(ty).ld = LDBL_MAX;
                TYPE_LIMITS_MIN(ty).ld = LDBL_MIN;
            }
            break;
            
        default:
            break;
    }
    
    return ty;
}

void type_init()
{
#define INSTALL(type, name, kind, metrics, op)    type = install_type(name, kind, metrics)
    
    // type                     name                    kind            metrics            op
    
    // bool
    INSTALL(booltype,           "_Bool",                _BOOL,          boolmetrics,       UNSIGNED);
    // char
    INSTALL(chartype,           "char",                 CHAR,           charmetrics,       INT);
    INSTALL(unsignedchartype,   "unsigned char",        CHAR,           charmetrics,       UNSIGNED);
    INSTALL(signedchartype,     "signed char",          CHAR,           charmetrics,       INT);
    // wchar_t
    INSTALL(wchartype,          "wchar_t",              UNSIGNED,       wcharmetrics,      UNSIGNED);
    // short
    INSTALL(shorttype,          "short",                SHORT,          shortmetrics,      INT);
    INSTALL(unsignedshorttype,  "unsigned short",       SHORT,          shortmetrics,      UNSIGNED);
    // int
    INSTALL(inttype,            "int",                  INT,            intmetrics,        INT);
    INSTALL(unsignedinttype,    "unsigned int",         UNSIGNED,       intmetrics,        UNSIGNED);
    // long
    INSTALL(longtype,           "long",                 LONG,           longmetrics,       INT);
    INSTALL(unsignedlongtype,   "unsigned long",        LONG,           longmetrics,       UNSIGNED);
    // long long
    INSTALL(longlongtype,       "long long",            LONG+LONG,      longlongmetrics,   INT);
    INSTALL(unsignedlonglongtype, "unsigned long long", LONG+LONG,      longlongmetrics,   UNSIGNED);
    // float
    INSTALL(floattype,          "float",                FLOAT,          floatmetrics,      FLOAT);
    // double
    INSTALL(doubletype,         "double",               DOUBLE,         doublemetrics,     FLOAT);
    INSTALL(longdoubletype,     "long double",          LONG+DOUBLE,    longdoublemetrics, FLOAT);
    // void
    INSTALL(voidtype,           "void",                 VOID,           zerometrics,       VOID);
    // variable
    INSTALL(vartype,            "...",                  ELLIPSIS,       zerometrics,       ELLIPSIS);

#undef INSTALL
}

int op(union node *type)
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

void prepend_type(union node **typelist, union node *type)
{
    attach_type(&type, *typelist);
    *typelist = type;
}

void attach_type(union node **typelist, union node *type)
{
    if (*typelist) {
        union node *tp = *typelist;
        while (tp && TYPE_TYPE(tp)) {
            tp = TYPE_TYPE(tp);
        }
        TYPE_TYPE(tp) = type;
    }
    else {
        *typelist = type;
    }
}

static bool isconst1(int kind)
{
    return  kind == CONST ||
            kind == CONST + VOLATILE ||
            kind == CONST + RESTRICT ||
            kind == CONST + VOLATILE + RESTRICT;
}

static bool isvolatile1(int kind)
{
    return  kind == VOLATILE ||
            kind == VOLATILE + CONST ||
            kind == VOLATILE + RESTRICT ||
            kind == CONST + VOLATILE + RESTRICT;
}

static bool isrestrict1(int kind)
{
    return  kind == RESTRICT ||
            kind == RESTRICT + CONST ||
            kind == RESTRICT + VOLATILE ||
            kind == CONST + VOLATILE + RESTRICT;
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

bool contains(int qual1, int qual2)
{
    if (isconst1(qual2) && !isconst1(qual1))
	return false;
    if (isvolatile1(qual2) && !isvolatile1(qual1))
	return false;
    if (isrestrict1(qual2) && !isrestrict1(qual1))
	return false;
    return true;
}

union node * qual(int t, union node *ty)
{
    CCAssert(ty);
    union node *qty = new_type();
    if (isqual(ty))
        TYPE_KIND(qty) = combine(t, TYPE_KIND(ty));
    else
        TYPE_KIND(qty) = t;
    TYPE_TYPE(qty) = unqual(ty);
    return qty;
}

union node * lookup_typedef_name(const char *id)
{
    if (!id)
        return NULL;
    
    union node *sym = lookup(id, identifiers);
    
    if (sym && SYM_SCLASS(sym) == TYPEDEF)
        return SYM_TYPE(sym);
    else
        return NULL;
}

bool is_typedef_name(const char *id)
{
    union node *ty = lookup_typedef_name(id);
    return ty != NULL;
}

union node * new_field(char *id)
{
    union node *field = alloc_field();
    FIELD_NAME(field) = id;
    return field;
}

union node * array_type()
{
    union node *ty = new_type();
    TYPE_KIND(ty) = ARRAY;
    TYPE_NAME(ty) = "array";
    
    return ty;
}

union node * ptr_type(union node *type)
{
    union node *ty = new_type();
    TYPE_KIND(ty) = POINTER;
    TYPE_TYPE(ty) = type;
    TYPE_NAME(ty) = "pointer";
    
    return ty;
}

union node * func_type()
{
    union node *ty = new_type();
    TYPE_KIND(ty) = FUNCTION;
    TYPE_NAME(ty) = "function";
    
    return ty;
}

union node * tag_type(int t, const char *tag, struct source src)
{
    union node *ty = new_type();
    TYPE_KIND(ty) = t;
    TYPE_TAG(ty) = tag;
    TYPE_NAME(ty) = tname(t);
    if (t == ENUM)
        TYPE_TYPE(ty) = inttype;
    
    union node *sym = NULL;
    if (tag) {
        sym = lookup(tag, tags);
        if (sym && currentscope(sym)) {
            if (op(SYM_TYPE(sym)) == t && !SYM_DEFINED(sym))
                return sym;
            
            redefinition_error(src, sym);
        }

        sym = install(tag, &tags, SCOPE);
    } else {
        sym = anonymous(&tags, SCOPE);
        TYPE_TAG(ty) = SYM_NAME(sym);
    }

    SYM_TYPE(sym) = ty;
    SYM_SRC(sym) = src;
    TYPE_TSYM(ty) = sym;
    
    return sym;
}

static bool eqparams(union node **params1, union node **params2)
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
            union node *sym1 = params1[i];
            union node *sym2 = params2[i];
            if (sym1 == sym2)
                continue;
            else if (sym1 == NULL || sym2 == NULL)
                return false;
            else if (eqtype(SYM_TYPE(sym1), SYM_TYPE(sym2)))
                continue;
            else
                return false;
        }
        
        return true;
    }
}

bool eqtype(union node *ty1, union node *ty2)
{
    if (ty1 == ty2)
        return true;
    else if (TYPE_KIND(ty1) != TYPE_KIND(ty2))
        return false;
    
    switch (TYPE_KIND(ty1)) {
        case CONST:
        case VOLATILE:
        case RESTRICT:
        case CONST+VOLATILE:
        case CONST+RESTRICT:
        case VOLATILE+RESTRICT:
        case CONST+VOLATILE+RESTRICT:
            return eqtype(TYPE_TYPE(ty1), TYPE_TYPE(ty2));
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
            return eqtype(TYPE_TYPE(ty1), TYPE_TYPE(ty2));
        case FUNCTION:
            if (!eqtype(TYPE_TYPE(ty1), TYPE_TYPE(ty2)))
                return false;
            if (TYPE_OLDSTYLE(ty1) && TYPE_OLDSTYLE(ty2)) {
                // both oldstyle
                return true;
            } else if (!TYPE_OLDSTYLE(ty1) && !TYPE_OLDSTYLE(ty2)) {
                // both prototype
                return eqparams(TYPE_PARAMS(ty1), TYPE_PARAMS(ty2));
            } else {
                // one oldstyle, the other prototype
                union node *oldty = TYPE_OLDSTYLE(ty1) ? ty1 : ty2;
                union node *newty = TYPE_OLDSTYLE(ty1) ? ty2 : ty1;
                if (TYPE_PARAMS(newty)) {
                    for (int i=0; TYPE_PARAMS(newty)[i]; i++) {
                        union node *sym = TYPE_PARAMS(newty)[i];
                        if (SYM_TYPE(sym)) {
                            union node *ty = unqual(SYM_TYPE(sym));
                            if (kind(ty) == CHAR || kind(ty) == SHORT)
                                return false;
                            else if (op(ty) == FLOAT)
                                return false;
                            else if (op(ty) == ELLIPSIS)
                                return false;
                        }
                    }
                }
                
                if (TYPE_PARAMS(oldty) == NULL)
                    return true;
                
                return eqparams(TYPE_PARAMS(oldty), TYPE_PARAMS(newty));
            }
            
        default:
            CCAssert(0);
            return false;
    }
}

union node * find_field(union node *sty, const char *name)
{
    int i;
    union node *ty = unqual(sty);
    int len = array_len((void **)TYPE_FIELDS(ty));
    union node *ret = NULL;

    if (name == NULL)
	return NULL;
    for (i = 0; i < len; i++) {
        union node *field = TYPE_FIELDS(ty)[i];
	if (FIELD_NAME(field) && !strcmp(name, FIELD_NAME(field)))
	    break;
    }
    if (i < len)
	ret = TYPE_FIELDS(ty)[i];

    return ret;
}

int indexof_field(union node *ty, union node *field)
{
    for (int i = 0; i < array_len((void **)TYPE_FIELDS(ty)); i++) {
	union node *f = TYPE_FIELDS(ty)[i];
	if (field == f)
	    return i;
    }
    CCAssert(0);
    return -1;
}

// TODO: 
static unsigned struct_size(union node *ty)
{
    return 0;
}

unsigned typesize(union node *ty)
{
    if (ty == NULL)
	return 0;
    else if (isfunc(ty) || unqual(ty) == vartype)
	return 0;
    else if (isstruct(ty) || isunion(ty))
	return struct_size(ty);
    else if (isarray(ty))
	return TYPE_SIZE(ty) * typesize(rtype(ty));
    else if (isptr(ty))
	return ptrmetrics.size;
    else
	return TYPE_SIZE(ty);
}

union node * compose(union node *ty1, union node *ty2)
{
    if (isqual(ty1) && isqual(ty2)) {
	int kind = combine(TYPE_KIND(ty1), TYPE_KIND(ty2));
	return qual(kind, unqual(ty1));
    } else if (isqual(ty2)) {
	return qual(TYPE_KIND(ty2), ty1);
    } else {
	return ty1;
    }
}

bool isconst(union node *ty)
{
    return isconst1(TYPE_KIND(ty));
}

bool isvolatile(union node *ty)
{
    return isvolatile1(TYPE_KIND(ty));
}

bool isrestrict(union node *ty)
{
    return isrestrict1(TYPE_KIND(ty));
}

bool eqarith(union node *ty1, union node *ty2)
{
    return kind(ty1) == kind(ty2) && op(ty1) == op(ty2);
}

bool isfunc(union node *ty)
{
    return op(ty) == FUNCTION;
}

bool isarray(union node *ty)
{
    return op(ty) == ARRAY;
}

bool isptr(union node *ty)
{
    return op(ty) == POINTER;
}

bool isvoid(union node *ty)
{
    return op(ty) == VOID;
}

bool isenum(union node *ty)
{
    return op(ty) == ENUM;
}

bool isstruct(union node *ty)
{
    return op(ty) == STRUCT;
}

bool isunion(union node *ty)
{
    return op(ty) == UNION;
}

bool isrecord(union node *type)
{
    return isstruct(type) || isunion(type);
}

bool istag(union node *type)
{
    return isstruct(type) || isunion(type) || isenum(type);
}

bool isint(union node *ty)
{
    return op(ty) == INT || op(ty) == UNSIGNED || isenum(ty);
}

bool isfloat(union node *ty)
{
    return op(ty) == FLOAT;
}

bool isarith(union node *ty)
{
    return isint(ty) || isfloat(ty);
}

bool isscalar(union node *ty)
{
    return isarith(ty) || isptr(ty);
}

bool isptrto(union node *ty, int kind)
{
    return isptr(ty) && kind(rtype(ty)) == kind;
}

#define LPAREN  1
#define RPAREN  2
#define FCOMMA  3
#define FSPACE  4
struct type2s {
    int id;
    int qual;
    union node *type;
};
static struct vector *type2s1(union node *ty);

static struct type2s * paren(int id, union node *ty)
{
    struct type2s *s = zmalloc(sizeof (struct type2s));
    s->id = id;
    s->type = ty;
    return s;
}

static void dotype2s(struct vector *l, struct vector *r)
{
    struct type2s *s;
    int k;

    if (vec_len(l) == 0)
	return;

    s = vec_tail(l);
    k = kind(s->type);
    switch (k) {
        case POINTER:
        {
	    struct vector *v = vec_new();
	    for (int i = vec_len(l) - 1; i >= 0; i--) {
		struct type2s *s = vec_at(l, i);
		if (!isptr(s->type))
		    break;
		vec_push(v, s);
		vec_pop(l);
	    }
	    s = vec_tail(l);
	    if (isfunc(s->type) || isarray(s->type)) {
		struct type2s *s2 = vec_head(r);
		bool rfunc = s2 && s2->type && isfunc(s2->type);
		if (rfunc)
		    vec_push_front(r, paren(LPAREN, s2->type));
		for (int i = 0; i < vec_len(v); i++)
		    vec_push_front(r, vec_at(v, i));
		vec_push_front(r, paren(LPAREN, s->type));
		vec_push_front(r, paren(FSPACE, NULL));
		if (rfunc)
		    vec_push(r, paren(RPAREN, s2->type));
		vec_push(r, paren(RPAREN, s->type));
	    } else {
		for (int i = 0; i < vec_len(v); i++)
		    vec_push_front(r, vec_at(v, i));
		vec_push_front(r, paren(FSPACE, NULL));
	    }
        }
            break;
        case FUNCTION:
        {
            union node **params = TYPE_PARAMS(s->type);
	    int len = array_len((void **)params);
	    vec_push(r, paren(FSPACE, NULL));
	    vec_push(r, paren(LPAREN, s->type));
	    for (int i=0; params && params[i]; i++) {
		union node *ty = SYM_TYPE(params[i]);
		struct vector *v = type2s1(ty);
		vec_add(r, v);
		vec_free(v);
		if (i < len - 1)
		    vec_push(r, paren(FCOMMA, NULL));
	    }
	    vec_push(r, paren(RPAREN, s->type));
	    vec_pop(l);
        }
            break;
        case ARRAY:
        {
            vec_push(r, s);
	    vec_pop(l);
        }
            break;
        default:
        {
            vec_push_front(r, s);
	    vec_pop(l);
        }
            break;
    }

    dotype2s(l, r);
}

static struct vector *type2s1(union node *ty)
{
    struct vector *l, *r, *v;

    v = vec_new();
    while (ty) {
	struct type2s *s = zmalloc(sizeof (struct type2s));
	if (isqual(ty)) {
	    s->qual = TYPE_KIND(ty);
	    s->type = unqual(ty);
	} else {
	    s->type = ty;
	}
	vec_push(v, s);
	ty = TYPE_TYPE(s->type);
    }
    
    l = vec_reverse(v);
    r = vec_new();
    vec_free(v);

    dotype2s(l, r);
    vec_free(l);
    return r;
}

static void qualstr(struct strbuf *s, int q)
{
    if (isconst1(q))
	strbuf_cats(s, "const ");
    if (isvolatile1(q))
	strbuf_cats(s, "volatile ");
    if (isrestrict1(q))
	strbuf_cats(s, "restrict ");
}

const char *type2s(union node *ty)
{
    const char *ret;
    struct strbuf *buf = strbuf_new();
    struct vector *v = type2s1(ty);
    for (int i = 0; i < vec_len(v); i++) {
	struct type2s *s = vec_at(v, i);
	if (s->id == LPAREN) {
	    strbuf_cats(buf, "(");
	} else if (s->id == RPAREN) {
	    strbuf_cats(buf, ")");
	} else if (s->id == FCOMMA) {
	    strbuf_cats(buf, ",");
	} else if (s->id == FSPACE) {
	    strbuf_cats(buf, " ");
	} else if (isptr(s->type)) {
	    strbuf_cats(buf, "*");
	    qualstr(buf, s->qual);
	} else if (isarray(s->type)) {
	    if (TYPE_SIZE(s->type) > 0) {
		strbuf_cats(buf, "[");
		strbuf_catd(buf, TYPE_SIZE(s->type));
		strbuf_cats(buf, "]");
	    } else {
		strbuf_cats(buf, "[]");
	    }
	} else if (isenum(s->type) || isstruct(s->type) || isunion(s->type)) {
	    qualstr(buf, s->qual);
	    strbuf_cats(buf, TYPE_NAME(s->type));
	    if (TYPE_TAG(s->type)) {
		strbuf_cats(buf, " ");
		strbuf_cats(buf, TYPE_TAG(s->type));
	    }
	} else {
	    qualstr(buf, s->qual);
	    strbuf_cats(buf, TYPE_NAME(s->type));
	}
    }

    ret = strs(strbuf_strip(buf)->str);
    strbuf_free(buf);
    vec_purge(v);
    return ret;
}
