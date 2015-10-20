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
node_t   *booltype;	          // bool
node_t   *vartype;		  // variable type

struct metrics {
    size_t size;
    int align;
    unsigned rank;
}
                       // size  align  rank
#ifdef CONFIG_X32
    boolmetrics         = { 1,  1,  10},
    charmetrics         = { 1,  1,  20},
    shortmetrics        = { 2,  2,  30},
    wcharmetrics        = { 4,  4,  40},
    intmetrics          = { 4,  4,  40},
    longmetrics         = { 4,  4,  50},
    longlongmetrics     = { 8,  8,  60},
    floatmetrics        = { 4,  4,  70},
    doublemetrics       = { 8,  8,  80},
    longdoublemetrics   = { 8,  8,  90},
    ptrmetrics          = { 4,  4},
#elif defined CONFIG_X64
    boolmetrics         = { 1,  1,  10},
    charmetrics         = { 1,  1,  20},
    shortmetrics        = { 2,  2,  30},
    wcharmetrics        = { 4,  4,  40},
    intmetrics          = { 4,  4,  40},
    longmetrics         = { 8,  8,  50},
    longlongmetrics     = { 8,  8,  60},
    floatmetrics        = { 4,  4,  70},
    doublemetrics       = { 8,  8,  80},
    longdoublemetrics   = { 16, 16, 90},
    ptrmetrics          = { 8,  8},
#else
    #error "architecture not defined."
#endif
    zerometrics         = { 0,  1};

static inline node_t * new_type(void)
{
    return alloc_type();
}

static node_t * install_type(const char *name, int kind, struct metrics m)
{
    node_t *ty = new_type();
    
    _TYPE_NAME(ty) = strs(name);
    _TYPE_KIND(ty) = kind;
    _TYPE_SIZE(ty) = m.size;
    _TYPE_ALIGN(ty) = m.align;
    _TYPE_RANK(ty) = m.rank;
    switch (TYPE_OP(ty)) {
        case INT:
            VALUE_I(_TYPE_LIMITS_MAX(ty)) = ONES(_TYPE_SIZE(ty)) >> 1;
            VALUE_I(_TYPE_LIMITS_MIN(ty)) = -VALUE_I(_TYPE_LIMITS_MAX(ty)) - 1;
            break;
            
        case UNSIGNED:
            VALUE_U(_TYPE_LIMITS_MAX(ty)) = ONES(_TYPE_SIZE(ty));
            VALUE_U(_TYPE_LIMITS_MIN(ty)) = 0;
            break;
            
        case FLOAT:
            if (_TYPE_SIZE(ty) == floatmetrics.size) {
                VALUE_D(_TYPE_LIMITS_MAX(ty)) = FLT_MAX;
                VALUE_D(_TYPE_LIMITS_MIN(ty)) = FLT_MIN;
            } else if (_TYPE_SIZE(ty) == doublemetrics.size) {
                VALUE_D(_TYPE_LIMITS_MAX(ty)) = DBL_MAX;
                VALUE_D(_TYPE_LIMITS_MIN(ty)) = DBL_MIN;
            } else {
                VALUE_D(_TYPE_LIMITS_MAX(ty)) = LDBL_MAX;
                VALUE_D(_TYPE_LIMITS_MIN(ty)) = LDBL_MIN;
            }
            break;
            
        default:
            break;
    }
    
    return ty;
}

void type_init(void)
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

int type_op(node_t *type)
{
    int kind = TYPE_KIND(type);
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
        while (tp && _TYPE_TYPE(tp)) {
            tp = _TYPE_TYPE(tp);
        }
        _TYPE_TYPE(tp) = type;
    }
    else {
        *typelist = type;
    }
}

bool isconst1(int kind)
{
    return  kind == CONST ||
            kind == CONST + VOLATILE ||
            kind == CONST + RESTRICT ||
            kind == CONST + VOLATILE + RESTRICT;
}

bool isvolatile1(int kind)
{
    return  kind == VOLATILE ||
            kind == VOLATILE + CONST ||
            kind == VOLATILE + RESTRICT ||
            kind == CONST + VOLATILE + RESTRICT;
}

bool isrestrict1(int kind)
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
    int qual1 = isqual(ty1) ? _TYPE_KIND(ty1) : 0;
    int qual2 = isqual(ty2) ? _TYPE_KIND(ty2) : 0;
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
    cc_assert(ty);
    node_t *qty = new_type();
    if (isqual(ty))
        _TYPE_KIND(qty) = combine(t, _TYPE_KIND(ty));
    else
        _TYPE_KIND(qty) = t;
    _TYPE_TYPE(qty) = unqual(ty);
    return qty;
}

node_t * unqual(node_t *ty)
{
    return isqual(ty) ? _TYPE_TYPE(ty) : ty;
}

node_t * lookup_typedef(const char *id)
{
    if (!id)
        return NULL;
    
    node_t *sym = lookup(id, identifiers);
    
    if (sym && SYM_SCLASS(sym) == TYPEDEF)
        return SYM_TYPE(sym);
    else
        return NULL;
}

bool istypedef(const char *id)
{
    node_t *ty = lookup_typedef(id);
    return ty != NULL;
}

node_t * new_field(char *id)
{
    node_t *field = alloc_field();
    FIELD_NAME(field) = id;
    return field;
}

node_t * array_type(node_t *type)
{
    node_t *ty = new_type();
    _TYPE_KIND(ty) = ARRAY;
    _TYPE_NAME(ty) = "array";
    _TYPE_TYPE(ty) = type;
    
    return ty;
}

node_t * ptr_type(node_t *type)
{
    node_t *ty = new_type();
    _TYPE_KIND(ty) = POINTER;
    _TYPE_NAME(ty) = "pointer";
    _TYPE_TYPE(ty) = type;
    _TYPE_SIZE(ty) = ptrmetrics.size;
    _TYPE_ALIGN(ty) = ptrmetrics.align;
    
    return ty;
}

node_t * func_type(void)
{
    node_t *ty = new_type();
    _TYPE_KIND(ty) = FUNCTION;
    _TYPE_NAME(ty) = "function";
    _TYPE_ALIGN(ty) = ptrmetrics.align;
    
    return ty;
}

node_t * tag_type(int t, const char *tag, struct source src)
{
    node_t *ty = new_type();
    _TYPE_KIND(ty) = t;
    _TYPE_TAG(ty) = tag;
    _TYPE_NAME(ty) = id2s(t);
    if (t == ENUM)
        _TYPE_TYPE(ty) = inttype;
    
    node_t *sym = NULL;
    if (tag) {
        sym = lookup(tag, tags);
        if (sym && currentscope(sym)) {
            if (TYPE_OP(SYM_TYPE(sym)) == t && !SYM_DEFINED(sym))
                return sym;
            
            redefinition_error(src, sym);
        }

        sym = install(tag, &tags, SCOPE);
    } else {
        sym = anonymous(&tags, SCOPE);
        _TYPE_TAG(ty) = SYM_NAME(sym);
    }

    SYM_TYPE(sym) = ty;
    AST_SRC(sym) = src;
    _TYPE_TSYM(ty) = sym;
    
    return sym;
}

static bool eqparams(node_t **params1, node_t **params2)
{
    if (params1 == params2) {
        return true;
    } else if (params1 == NULL || params2 == NULL) {
        return false;
    } else {
        int len1 = LIST_LEN(params1);
        int len2 = LIST_LEN(params2);
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
    else if (_TYPE_KIND(ty1) != _TYPE_KIND(ty2))
        return false;
    
    switch (_TYPE_KIND(ty1)) {
        case CONST:
        case VOLATILE:
        case RESTRICT:
        case CONST+VOLATILE:
        case CONST+RESTRICT:
        case VOLATILE+RESTRICT:
        case CONST+VOLATILE+RESTRICT:
            return eqtype(_TYPE_TYPE(ty1), _TYPE_TYPE(ty2));
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
            return eqtype(_TYPE_TYPE(ty1), _TYPE_TYPE(ty2));
        case FUNCTION:
            if (!eqtype(_TYPE_TYPE(ty1), _TYPE_TYPE(ty2)))
                return false;
            if (_TYPE_OLDSTYLE(ty1) && _TYPE_OLDSTYLE(ty2)) {
                // both oldstyle
                return true;
            } else if (!_TYPE_OLDSTYLE(ty1) && !_TYPE_OLDSTYLE(ty2)) {
                // both prototype
                return eqparams(_TYPE_PARAMS(ty1), _TYPE_PARAMS(ty2));
            } else {
                // one oldstyle, the other prototype
                node_t *oldty = _TYPE_OLDSTYLE(ty1) ? ty1 : ty2;
                node_t *newty = _TYPE_OLDSTYLE(ty1) ? ty2 : ty1;
                if (_TYPE_PARAMS(newty)) {
                    for (int i=0; _TYPE_PARAMS(newty)[i]; i++) {
                        node_t *sym = _TYPE_PARAMS(newty)[i];
                        if (SYM_TYPE(sym)) {
                            node_t *ty = unqual(SYM_TYPE(sym));
                            if (TYPE_KIND(ty) == CHAR || TYPE_KIND(ty) == SHORT)
                                return false;
                            else if (TYPE_OP(ty) == FLOAT)
                                return false;
                            else if (TYPE_OP(ty) == ELLIPSIS)
                                return false;
                        }
                    }
                }
                
                if (_TYPE_PARAMS(oldty) == NULL)
                    return true;
                
                return eqparams(_TYPE_PARAMS(oldty), _TYPE_PARAMS(newty));
            }
            
        default:
            cc_assert(0);
            return false;
    }
}

node_t * find_field(node_t *sty, const char *name)
{
    int i;
    node_t *ty = unqual(sty);
    int len = LIST_LEN(TYPE_FIELDS(ty));

    if (name == NULL)
	return NULL;
    for (i = 0; i < len; i++) {
        node_t *field = TYPE_FIELDS(ty)[i];
	if (FIELD_NAME(field) && !strcmp(name, FIELD_NAME(field)))
	    return field;
    }
    
    return NULL;
}

int indexof_field(node_t *ty, node_t *field)
{
    for (int i = 0; i < LIST_LEN(TYPE_FIELDS(ty)); i++) {
	node_t *f = TYPE_FIELDS(ty)[i];
	if (field == f)
	    return i;
    }
    cc_assert(0);
    return -1;
}

/* Structure alignment requirements
 *
 * The rule is that the structure will be padded out
 * to the size the type would occupy as an element
 * of an array of such types.
 *
 * The bitfields must be packed as tightly as possible.
 */
static unsigned struct_size(node_t *ty)
{
    int offset = 0;
    int bsize = 0;
    int max = 1;
    int maxbits = 0;
    node_t **fields = TYPE_FIELDS(ty);

    for (int i = 0; i < LIST_LEN(fields); i++) {
    	node_t *field = fields[i];
    	node_t *ty = FIELD_TYPE(field);
	
    	if (FIELD_ISBIT(field)) {
	    int bitsize = FIELD_BITSIZE(field);
	    
	    if (!isint(ty))
		continue;
	    if (bitsize < 0 || (FIELD_NAME(field) && bitsize == 0))
		continue;

	    if (bitsize > BITS(TYPE_SIZE(ty)))
		bitsize = BITS(TYPE_SIZE(ty));

	    if (bitsize == 0) {
		offset = ROUNDUP(offset, TYPE_SIZE(ty));
		bsize = ROUNDUP(bsize, BITS(TYPE_SIZE(ty)));
		continue;
	    }

	    maxbits = MAX(maxbits, BITS(TYPE_SIZE(ty)));

	    if (bsize == 0 || bitsize + bsize > maxbits) {
		FIELD_OFFSET(field) = offset = ROUNDUP(offset, TYPE_SIZE(ty));
		offset += TYPE_SIZE(ty);
		bsize = FIELD_BITSIZE(field);
	    } else {
		FIELD_OFFSET(field) = FIELD_OFFSET(fields[i-1]);
		bsize += bitsize;
		offset = ROUNDUP(offset, TYPE_SIZE(ty));
	    }
	    
    	} else {
	    int align = TYPE_ALIGN(ty);
	    int bitsize = BITS(TYPE_SIZE(ty));
	    int bits = BITS(ROUNDUP(BYTES(bsize), align));

	    if (bitsize == 0)
		goto clear;

	    if (bsize == 0 || bits + bitsize > maxbits) {
	        FIELD_OFFSET(field) = offset = ROUNDUP(offset, align);
		offset += TYPE_SIZE(ty);
	    } else {
		FIELD_OFFSET(field) = FIELD_OFFSET(fields[i-1]);
	        bsize = bits + bitsize;
	    }

	clear:
	    bsize = 0;
	    maxbits = 0;
    	}
	
	max = MAX(max, TYPE_ALIGN(ty));
    }

    TYPE_ALIGN(ty) = max;
    
    return ROUNDUP(offset, max);
}

static unsigned union_size(node_t *ty)
{
    int max = 1;
    int size = 0;
    node_t **fields = TYPE_FIELDS(ty);

    for (int i = 0; i < LIST_LEN(fields); i++) {
	node_t *field = fields[i];
    	node_t *ty = FIELD_TYPE(field);
	int tysize = TYPE_SIZE(ty);

	if (tysize == 0)
	    continue;
	
	if (FIELD_ISBIT(field)) {
	    int bitsize = FIELD_BITSIZE(field);
	    
	    if (!isint(ty))
		continue;
	    if (bitsize <= 0)
		continue;
	}

	size = MAX(size, tysize);
	max = MAX(max, TYPE_ALIGN(ty));
    }

    TYPE_ALIGN(ty) = max;

    return ROUNDUP(size, max);
}

static unsigned array_size(node_t *ty)
{
    unsigned size = 1;
    node_t *rty = ty;
    
    do {
	size *= TYPE_LEN(rty);
	rty = rtype(rty);
    } while (isarray(rty));

    size *= TYPE_SIZE(rty);

    // set align
    do {
	TYPE_ALIGN(ty) = TYPE_ALIGN(rty);
	ty = rtype(ty);
    } while (isarray(ty));
    
    return size;
}

void set_typesize(node_t *ty)
{
    if (isarray(ty))
	TYPE_SIZE(ty) = array_size(ty);
    else if (isstruct(ty))
	TYPE_SIZE(ty) = struct_size(ty);
    else if (isunion(ty))
	TYPE_SIZE(ty) = union_size(ty);
}

bool isincomplete(node_t *ty)
{
    if (isvoid(ty))
	return true;
    else if (isarray(ty))
	return !TYPE_A_HASEXPR(ty) && !TYPE_A_WILDCARD(ty) && TYPE_LEN(ty) == 0;
    else if (isenum(ty) || isstruct(ty) || isunion(ty))
	return !SYM_DEFINED(TYPE_TSYM(ty));
    return false;
}

bool isvarray(node_t *ty)
{
    if (!isarray(ty))
	return false;
    if (isincomplete(ty))
	return false;
    if (TYPE_LEN(ty) == 0)
	return true;
    if (isarray(rtype(ty)))
	return isvarray(rtype(ty));
    else
	return false;
}

node_t * unpack(node_t *ty)
{
    if (isenum(ty))
	return _TYPE_TYPE(unqual(ty));
    else
	return ty;
}

node_t * compose(node_t *ty1, node_t *ty2)
{
    if (isqual(ty1) && isqual(ty2)) {
	int kind = combine(_TYPE_KIND(ty1), _TYPE_KIND(ty2));
	return qual(kind, unqual(ty1));
    } else if (isqual(ty2)) {
	return qual(_TYPE_KIND(ty2), ty1);
    } else {
	return ty1;
    }
}

bool isconst(node_t *ty)
{
    return isconst1(_TYPE_KIND(ty));
}

bool isvolatile(node_t *ty)
{
    return isvolatile1(_TYPE_KIND(ty));
}

bool isrestrict(node_t *ty)
{
    return isrestrict1(_TYPE_KIND(ty));
}

bool eqarith(node_t *ty1, node_t *ty2)
{
    return TYPE_KIND(ty1) == TYPE_KIND(ty2) && TYPE_OP(ty1) == TYPE_OP(ty2);
}

bool isfunc(node_t *ty)
{
    return TYPE_OP(ty) == FUNCTION;
}

bool isarray(node_t *ty)
{
    return TYPE_OP(ty) == ARRAY;
}

bool isptr(node_t *ty)
{
    return TYPE_OP(ty) == POINTER;
}

bool isvoid(node_t *ty)
{
    return TYPE_OP(ty) == VOID;
}

bool isenum(node_t *ty)
{
    return TYPE_OP(ty) == ENUM;
}

bool isstruct(node_t *ty)
{
    return TYPE_OP(ty) == STRUCT;
}

bool isunion(node_t *ty)
{
    return TYPE_OP(ty) == UNION;
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
    return TYPE_OP(ty) == INT || TYPE_OP(ty) == UNSIGNED || isenum(ty);
}

bool isfloat(node_t *ty)
{
    return TYPE_OP(ty) == FLOAT;
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
    return isptr(ty) && TYPE_KIND(rtype(ty)) == kind;
}
