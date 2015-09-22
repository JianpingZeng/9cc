#include "cc.h"

// predefined types
node_t   *chartype;               // char
node_t   *unsignedchartype;       // unsigned char
node_t   *signedchartype;         // signed char
node_t   *wchartype;              // wchar_t
node_t   *shorttype;              // short (int)
node_t   *unsignedshorttype;      // unsigned short (int)
node_t   *inttype;                // int
node_t   *unsignedinttype;        // unsigned (int)
node_t   *longtype;               // long
node_t   *unsignedlongtype;       // unsigned long (int)
node_t   *longlongtype;           // long long (int)
node_t   *unsignedlonglongtype;   // unsigned long long (int)
node_t   *floattype;              // float
node_t   *doubletype;             // double
node_t   *longdoubletype;         // long double
node_t   *voidtype;               // void
node_t   *booltype;	       // bool
node_t   *vartype;		       // variable type

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

static inline node_t * new_type()
{
    return alloc_type();
}

static node_t * install_type(const char *name, int kind, struct metrics m)
{
    node_t *ty = new_type();
    
    TYPE_NAME(ty) = strs(name);
    TYPE_KIND(ty) = kind;
    TYPE_SIZE(ty) = m.size;
    TYPE_RANK(ty) = m.rank;
    switch (op(ty)) {
        case INT:
            TYPE_LIMITS_MAX(ty).u = ONES(TYPE_SIZE(ty)) >> 1;
            TYPE_LIMITS_MIN(ty).u = -TYPE_LIMITS_MAX(ty).u - 1;
            break;
            
        case UNSIGNED:
            TYPE_LIMITS_MAX(ty).u = ONES(TYPE_SIZE(ty));
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
                TYPE_LIMITS_MAX(ty).d = LDBL_MAX;
                TYPE_LIMITS_MIN(ty).d = LDBL_MIN;
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

int op(node_t *type)
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

void prepend_type(node_t **typelist, node_t *type)
{
    attach_type(&type, *typelist);
    *typelist = type;
}

void attach_type(node_t **typelist, node_t *type)
{
    if (*typelist) {
        node_t *tp = *typelist;
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

bool qual_contains(node_t *ty1, node_t *ty2)
{
    int qual1 = isqual(ty1) ? TYPE_KIND(ty1) : 0;
    int qual2 = isqual(ty2) ? TYPE_KIND(ty2) : 0;
    if (isconst1(qual2) && !isconst1(qual1))
	return false;
    if (isvolatile1(qual2) && !isvolatile1(qual1))
	return false;
    if (isrestrict1(qual2) && !isrestrict1(qual1))
	return false;
    return true;
}

node_t * qual(int t, node_t *ty)
{
    CCAssert(ty);
    node_t *qty = new_type();
    if (isqual(ty))
        TYPE_KIND(qty) = combine(t, TYPE_KIND(ty));
    else
        TYPE_KIND(qty) = t;
    TYPE_TYPE(qty) = unqual(ty);
    return qty;
}

node_t * lookup_typedef_name(const char *id)
{
    if (!id)
        return NULL;
    
    node_t *sym = lookup(id, identifiers);
    
    if (sym && SYM_SCLASS(sym) == TYPEDEF)
        return SYM_TYPE(sym);
    else
        return NULL;
}

bool is_typedef_name(const char *id)
{
    node_t *ty = lookup_typedef_name(id);
    return ty != NULL;
}

node_t * new_field(char *id)
{
    node_t *field = alloc_field();
    FIELD_NAME(field) = id;
    return field;
}

node_t * array_type()
{
    node_t *ty = new_type();
    TYPE_KIND(ty) = ARRAY;
    TYPE_NAME(ty) = "array";
    
    return ty;
}

node_t * ptr_type(node_t *type)
{
    node_t *ty = new_type();
    TYPE_KIND(ty) = POINTER;
    TYPE_TYPE(ty) = type;
    TYPE_NAME(ty) = "pointer";
    
    return ty;
}

node_t * func_type()
{
    node_t *ty = new_type();
    TYPE_KIND(ty) = FUNCTION;
    TYPE_NAME(ty) = "function";
    
    return ty;
}

node_t * tag_type(int t, const char *tag, struct source src)
{
    node_t *ty = new_type();
    TYPE_KIND(ty) = t;
    TYPE_TAG(ty) = tag;
    TYPE_NAME(ty) = tname(t);
    if (t == ENUM)
        TYPE_TYPE(ty) = inttype;
    
    node_t *sym = NULL;
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

static bool eqparams(node_t **params1, node_t **params2)
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
            node_t *sym1 = params1[i];
            node_t *sym2 = params2[i];
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

bool eqtype(node_t *ty1, node_t *ty2)
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
                node_t *oldty = TYPE_OLDSTYLE(ty1) ? ty1 : ty2;
                node_t *newty = TYPE_OLDSTYLE(ty1) ? ty2 : ty1;
                if (TYPE_PARAMS(newty)) {
                    for (int i=0; TYPE_PARAMS(newty)[i]; i++) {
                        node_t *sym = TYPE_PARAMS(newty)[i];
                        if (SYM_TYPE(sym)) {
                            node_t *ty = unqual(SYM_TYPE(sym));
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

node_t * find_field(node_t *sty, const char *name)
{
    int i;
    node_t *ty = unqual(sty);
    int len = array_len((void **)TYPE_FIELDS(ty));
    node_t *ret = NULL;

    if (name == NULL)
	return NULL;
    for (i = 0; i < len; i++) {
        node_t *field = TYPE_FIELDS(ty)[i];
	if (FIELD_NAME(field) && !strcmp(name, FIELD_NAME(field)))
	    break;
    }
    if (i < len)
	ret = TYPE_FIELDS(ty)[i];

    return ret;
}

int indexof_field(node_t *ty, node_t *field)
{
    for (int i = 0; i < array_len((void **)TYPE_FIELDS(ty)); i++) {
	node_t *f = TYPE_FIELDS(ty)[i];
	if (field == f)
	    return i;
    }
    CCAssert(0);
    return -1;
}

// TODO: 
static unsigned struct_size(node_t *ty)
{
    return 0;
}

unsigned typesize(node_t *ty)
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

bool isincomplete(node_t *ty)
{
    return isarray(ty) && TYPE_A_ASSIGN(ty) == NULL && TYPE_A_WILDCARD(ty) == 0;
}

node_t * compose(node_t *ty1, node_t *ty2)
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

bool isconst(node_t *ty)
{
    return isconst1(TYPE_KIND(ty));
}

bool isvolatile(node_t *ty)
{
    return isvolatile1(TYPE_KIND(ty));
}

bool isrestrict(node_t *ty)
{
    return isrestrict1(TYPE_KIND(ty));
}

bool eqarith(node_t *ty1, node_t *ty2)
{
    return kind(ty1) == kind(ty2) && op(ty1) == op(ty2);
}

bool isfunc(node_t *ty)
{
    return op(ty) == FUNCTION;
}

bool isarray(node_t *ty)
{
    return op(ty) == ARRAY;
}

bool isptr(node_t *ty)
{
    return op(ty) == POINTER;
}

bool isvoid(node_t *ty)
{
    return op(ty) == VOID;
}

bool isenum(node_t *ty)
{
    return op(ty) == ENUM;
}

bool isstruct(node_t *ty)
{
    return op(ty) == STRUCT;
}

bool isunion(node_t *ty)
{
    return op(ty) == UNION;
}

bool isrecord(node_t *type)
{
    return isstruct(type) || isunion(type);
}

bool istag(node_t *type)
{
    return isstruct(type) || isunion(type) || isenum(type);
}

bool isint(node_t *ty)
{
    return op(ty) == INT || op(ty) == UNSIGNED || isenum(ty);
}

bool isfloat(node_t *ty)
{
    return op(ty) == FLOAT;
}

bool isarith(node_t *ty)
{
    return isint(ty) || isfloat(ty);
}

bool isscalar(node_t *ty)
{
    return isarith(ty) || isptr(ty);
}

bool isptrto(node_t *ty, int kind)
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
    node_t *type;
};
static struct vector *type2s1(node_t *ty);

static struct type2s * paren(int id, node_t *ty)
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
            node_t **params = TYPE_PARAMS(s->type);
	    int len = array_len((void **)params);
	    vec_push(r, paren(FSPACE, NULL));
	    vec_push(r, paren(LPAREN, s->type));
	    for (int i=0; params && params[i]; i++) {
		node_t *ty = SYM_TYPE(params[i]);
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

static struct vector *type2s1(node_t *ty)
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

const char *type2s(node_t *ty)
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
