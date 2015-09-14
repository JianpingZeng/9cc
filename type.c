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
ptrmetrics          = { sizeof(void *) },
zerometrics         = { 0 };

static struct type * new_type()
{
    return NEWS(type);
}

static struct type * install_type(const char *name, int kind, struct metrics m)
{
    struct type *ty = new_type();
    
    ty->name = strs(name);
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
    INSTALL(vartype,            "...",                  ELLIPSIS,       zerometrics);       // ELLIPSIS

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

static int combine(int t, int op)
{
    int r = CONST + VOLATILE + RESTRICT - t;
    int x = op - t;
    
    if (op == t || r == x)
        return op;
    else if (op == r)
        return t + op;
    else if (x == CONST || x == VOLATILE || x == RESTRICT)
        return op;
    else
        return t + op;
}

struct type * qual(int t, struct type *ty)
{
    assert(ty);
    struct type *qty = new_type();
    if (isqual(ty))
        qty->kind = combine(t, ty->kind);
    else
        qty->kind = t;
    qty->type = unqual(ty);
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
    } else {
        sym = anonymous(&tags, SCOPE);
        ty->tag = sym->name;
    }

    sym->type = ty;
    sym->src = src;
    ty->u.s.tsym = sym;
    
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

bool eqtype(struct type *ty1, struct type *ty2)
{
    if (ty1 == ty2)
        return true;
    else if (ty1->kind != ty2->kind)
        return false;
    
    switch (ty1->kind) {
        case CONST:
        case VOLATILE:
        case RESTRICT:
        case CONST+VOLATILE:
        case CONST+RESTRICT:
        case VOLATILE+RESTRICT:
        case CONST+VOLATILE+RESTRICT:
            return eqtype(ty1->type, ty2->type);
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

struct field * find_field(struct type *ty, const char *name)
{
    int i;
    int len = array_len((void **)ty->u.s.fields);
    struct field *ret = NULL;

    if (name == NULL)
	return NULL;
    for (i = 0; i < len; i++) {
	struct field *field = ty->u.s.fields[i];
	if (field->name && !strcmp(name, field->name))
	    break;
    }
    if (i < len)
	ret = ty->u.s.fields[i];

    return ret;
}

int indexof_field(struct type *ty, struct field *field)
{
    for (int i = 0; i < array_len((void **)ty->u.s.fields); i++) {
	struct field *f = ty->u.s.fields[i];
	if (field == f)
	    return i;
    }
    assert(0);
    return -1;
}

// TODO: 
static unsigned struct_size(struct type *ty)
{
    return 0;
}

unsigned typesize(struct type *ty)
{
    if (ty == NULL)
	return 0;
    else if (isfunc(ty) || unqual(ty) == vartype)
	return 0;
    else if (isstruct(ty) || isunion(ty))
	return struct_size(ty);
    else if (isarray(ty))
	return ty->size * typesize(rtype(ty));
    else if (isptr(ty))
	return ptrmetrics.size;
    else
	return ty->size;
}

//TODO
struct type * compose(struct type *ty1, struct type *ty2)
{
    return NULL;
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

bool isconst(struct type *ty)
{
    return isconst1(ty->kind);
}

bool isvolatile(struct type *ty)
{
    return isvolatile1(ty->kind);
}

bool isrestrict(struct type *ty)
{
    return isrestrict1(ty->kind);
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

bool isrecord(struct type *type)
{
    return isstruct(type) || isunion(type);
}

bool istag(struct type *type)
{
    return isstruct(type) || isunion(type) || isenum(type);
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

#define LPAREN  1
#define RPAREN  2
#define FCOMMA  3
#define FSPACE  4
struct type2s {
    int id;
    int qual;
    struct type *type;
};
static struct vector *type2s1(struct type *ty);

static struct type2s * paren(int id, struct type *ty)
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
            struct symbol **params = s->type->u.f.params;
	    int len = array_len((void **)params);
	    vec_push(r, paren(FSPACE, NULL));
	    vec_push(r, paren(LPAREN, s->type));
	    for (int i=0; params && params[i]; i++) {
		struct type *ty = params[i]->type;
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

static struct vector *type2s1(struct type *ty)
{
    struct vector *l, *r, *v;

    v = vec_new();
    while (ty) {
	struct type2s *s = zmalloc(sizeof (struct type2s));
	if (isqual(ty)) {
	    s->qual = ty->kind;
	    s->type = unqual(ty);
	} else {
	    s->type = ty;
	}
	vec_push(v, s);
	ty = s->type->type;
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

const char *type2s(struct type *ty)
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
	    if (s->type->size > 0) {
		strbuf_cats(buf, "[");
		strbuf_catd(buf, s->type->size);
		strbuf_cats(buf, "]");
	    } else {
		strbuf_cats(buf, "[]");
	    }
	} else if (isenum(s->type) || isstruct(s->type) || isunion(s->type)) {
	    qualstr(buf, s->qual);
	    strbuf_cats(buf, s->type->name);
	    if (s->type->tag) {
		strbuf_cats(buf, " ");
		strbuf_cats(buf, s->type->tag);
	    }
	} else {
	    qualstr(buf, s->qual);
	    strbuf_cats(buf, s->type->name);
	}
    }
    
    ret = strs(buf->str);
    strbuf_free(buf);
    vec_purge(v);
    return ret;
}
