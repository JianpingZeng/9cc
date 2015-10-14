#include "cc.h"

static void abstract_declarator(node_t **ty);
static void declarator(node_t **ty, const char **id, int *params);
static void param_declarator(node_t **ty, const char **id);
static node_t * ptr_decl(void);
static node_t * enum_decl(void);
static node_t * struct_decl(void);
static struct vector * decls(node_t * (*)(const char *id, node_t *ftype, int sclass,  struct source src));
static node_t * paramdecl(const char *id, node_t *ty, int sclass,  struct source src);
static node_t * globaldecl(const char *id, node_t *ty, int sclass, struct source src);
static node_t * localdecl(const char *id, node_t *ty, int sclass, struct source src);
static node_t * funcdef(const char *id, node_t *ftype, int sclass,  struct source src);
static node_t * initializer(node_t *ty);
static void fields(node_t *sty);

#define FIRST_INIT(t) (t->id == '[' || t->id == '.' || t->id == '{' || firstexpr(t))
static void eat_initlist(void);
static void struct_init(node_t *ty, bool brace, struct vector *v);
static void array_init(node_t *ty, bool brace, struct vector *v);
static void scalar_init(node_t *ty, struct vector *v);
static void elem_init(node_t *sty, node_t *ty, bool designated, struct vector *v, int i);
static void decl_initializer(node_t *decl, node_t *sym, int sclass, int kind);

static void post_decl(node_t *decl, node_t *sym, int sclass, int kind);
static void post_funcdef(node_t *fty, node_t *stmt);
static void ensure_array(node_t *atype, struct source src, int level);
static void ensure_func(node_t *ftype, struct source src);

#define PACK_PARAM(prototype, first, fvoid, sclass)	\
    (((prototype) & 0x01) << 30) |			\
    (((first) & 0x01) << 29) |				\
    (((fvoid) & 0x01) << 28) |				\
    ((sclass) & 0xffffff)

#define PARAM_STYLE(i)   (((i) & 0x40000000) >> 30)
#define PARAM_FIRST(i)   (((i) & 0x20000000) >> 29)
#define PARAM_FVOID(i)   (((i) & 0x10000000) >> 28)
#define PARAM_SCLASS(i)  ((i) & 0x0fffffff)

struct vector *gotos;
struct map *labels;
node_t *current_ftype;
const char *current_fname;

#define SET_FUNCDEF_CONTEXT(fty, id)		\
    gotos = vec_new();				\
    labels = map_new();				\
    current_ftype = fty;			\
    current_fname = id

#define RESTORE_FUNCDEF_CONTEXT()		\
    vec_free(gotos);				\
    gotos = NULL;				\
    map_free(labels);				\
    labels = NULL;				\
    current_ftype = NULL;			\
    current_fname = NULL

static node_t * specifiers(int *sclass)
{
    int cls, sign, size, type;
    int cons, vol, res, inl;
    node_t *basety;
    int ci;			// _Complex, _Imaginary
    node_t *tydefty = NULL;
    
    basety = NULL;
    cls = sign = size = type = 0;
    cons = vol = res = inl = 0;
    ci = 0;
    if (sclass == NULL)
        cls = AUTO;
    
    for (;;) {
        int *p, t = token->id;
        const char *name = token->name;
        struct source src = source;
        switch (token->id) {
	case AUTO:
	case EXTERN:
	case REGISTER:
	case STATIC:
	case TYPEDEF:
	    p = &cls;
	    gettok();
	    break;
                
	case CONST:
	    p = &cons;
	    gettok();
	    break;
                
	case VOLATILE:
	    p = &vol;
	    gettok();
	    break;
                
	case RESTRICT:
	    p = &res;
	    gettok();
	    break;
                
	case INLINE:
	    p = &inl;
	    gettok();
	    break;
                
	case ENUM:
	    p = &type;
	    basety = enum_decl();
	    break;
                
	case STRUCT:
	case UNION:
	    p = &type;
	    basety = struct_decl();
	    break;
                
	case LONG:
	    if (size == LONG) {
		t = LONG + LONG;
		size = 0;	// clear
	    }
	    // go through
	case SHORT:
	    p = &size;
	    gettok();
	    break;
                
	case FLOAT:
	    p = &type;
	    basety = floattype;
	    gettok();
	    break;
                
	case DOUBLE:
	    p = &type;
	    basety = doubletype;
	    gettok();
	    break;
                
	case VOID:
	    p = &type;
	    basety = voidtype;
	    gettok();
	    break;
                
	case CHAR:
	    p = &type;
	    basety = chartype;
	    gettok();
	    break;
                
	case INT:
	    p = &type;
	    basety = inttype;
	    gettok();
	    break;
                
	case _BOOL:
	    p = &type;
	    basety = booltype;
	    gettok();
	    break;
                
	case SIGNED:
	case UNSIGNED:
	    p = &sign;
	    gettok();
	    break;
                
	case _COMPLEX:
	case _IMAGINARY:
	    p = &ci;
	    gettok();
	    break;
                
	case ID:
	    if (istypedef(token->name)) {
		tydefty = lookup_typedef(token->name);
		p = &type;
		gettok();
	    } else {
		p = NULL;
	    }
	    break;
                
	default:
	    p = NULL;
	    break;
        }
        
        if (p == NULL)
            break;
        
        if (*p != 0) {
            if (p == &cls)
                errorf(src, "type name does not allow storage class to be specified at '%s'", name);
            else if (p == &inl && !sclass)
                errorf(src, "invalid function specifier");
            else if (p == &cons || p == &res || p == &vol || p == &inl)
                warningf(src, "duplicate '%s' declaration specifier", name);
            else if (p == &ci)
                errorf(src, "duplicate _Complex/_Imaginary specifier '%s'", name);
            else if (p == &sign)
                errorf(src, "duplicate signed/unsigned speficier '%s'", name);
            else if (p == &type || p == &size)
                errorf(src, "duplicate type specifier '%s'", name);
            else
                CCAssert(0);
        }
        
        *p = t;
    }
    
    // default is int
    if (type == 0) {
        if (sign == 0 && size == 0)
            error("missing type specifier");
        type = INT;
        basety = inttype;
    }
    
    // type check
    if ((size == SHORT && type != INT) ||
        (size == LONG + LONG && type != INT) ||
        (size == LONG && type != INT && type != DOUBLE)) {
        if (size == LONG + LONG)
            error("%s %s %s is invalid", id2s(size/2), id2s(size/2), id2s(type));
        else
            error("%s %s is invalid", id2s(size), id2s(type));
    } else if (sign && type != INT && type != CHAR) {
        error("'%s' cannot be signed or unsigned", id2s(type));
    } else if (ci && type != DOUBLE && type != FLOAT) {
        error("'%s' cannot be %s", id2s(type), id2s(ci));
    }
    
    if (tydefty)
        basety = tydefty;
    else if (type == CHAR && sign)
        basety = sign == UNSIGNED ? unsignedchartype : signedchartype;
    else if (size == SHORT)
        basety = sign == UNSIGNED ? unsignedshorttype : shorttype;
    else if (type == INT && size == LONG)
        basety = sign == UNSIGNED ? unsignedlongtype : longtype;
    else if (size == LONG + LONG)
        basety = sign == UNSIGNED ? unsignedlonglongtype : longlongtype;
    else if (type == DOUBLE && size == LONG)
        basety = longdoubletype;
    else if (sign == UNSIGNED)
        basety = unsignedinttype;
    
    // qulifier
    if (cons)
        basety = qual(CONST, basety);
    if (vol)
        basety = qual(VOLATILE, basety);
    if (res)
        basety = qual(RESTRICT, basety);
    if (inl)
        basety = qual(INLINE, basety);
    
    if (sclass)
        *sclass = cls;

    return basety;
}

static void array_qualifiers(node_t *atype)
{
    int cons, vol, res;
    int *p;
    cons = vol = res = 0;
    while (token->kind == CONST) {
        int t = token->id;
        switch (t) {
	case CONST:
	    p = &cons;
	    gettok();
	    break;
                
	case VOLATILE:
	    p = &vol;
	    gettok();
	    break;
                
	case RESTRICT:
	    p = &res;
	    gettok();
	    break;
                
	default:
	    CCAssert(0);
        }
        
        if (*p != 0)
            warning("duplicate type qualifier '%s'", id2s(*p));
        
        *p = t;
    }
    
    if (cons)
	TYPE_A_CONST(atype) = 1;
    if (vol)
	TYPE_A_VOLATILE(atype) = 1;
    if (res)
	TYPE_A_RESTRICT(atype) = 1;
}

static node_t ** parameters(node_t *ftype, int *params)
{
    node_t **ret = NULL;
    
    /**
     * To make it easy to distinguish between 'paramaters in parameter' and
     * 'compound statement of function definition', they both may be at scope
     * LOCAL (aka PARAM+1), so enter scope again to make things easy.
     */
    enter_scope();
    if (SCOPE > PARAM)
        enter_scope();
    
    if (firstdecl(token)) {
        // prototype
        struct vector *v = vec_new();
	bool first_void = false;
        for (int i=0;;i++) {
            node_t *basety = NULL;
            int sclass;
            node_t *ty = NULL;
            const char *id = NULL;
            struct source src = source;
	    node_t *sym;
            
            basety = specifiers(&sclass);
            if (token->id == '*' || token->id == '(' || token->id == '[' || token->id == ID)
                param_declarator(&ty, &id);
            
            attach_type(&ty, basety);
            if (isinline(basety))
                error("'inline' can only appear on functions");
	    if (i == 0 && isvoid(ty))
		first_void = true;
		
	    SAVE_ERRORS;
	    sym = paramdecl(id, ty, PACK_PARAM(1, i == 0, first_void, sclass), src);
	    if (NO_ERROR && !first_void)
		vec_push(v, sym);
            if (token->id != ',')
                break;
            
            expect(',');
            if (token->id == ELLIPSIS) {
		if (!first_void)
		    vec_push(v, paramdecl(token->name, vartype, PACK_PARAM(1, 0, first_void, 0), source));
                expect(ELLIPSIS);
                break;
            }
        }
        
        ret = (node_t **)vtoa(v);
    } else if (token->id == ID) {
        // oldstyle
	TYPE_OLDSTYLE(ftype) = 1;
        struct vector *v = vec_new();
        for (;;) {
            if (token->id == ID)
                vec_push(v, paramdecl(token->name, inttype, 0, source));
            expect(ID);
            if (token->id != ',')
                break;
            expect(',');
        }
        
        if (SCOPE > PARAM)
            error("a parameter list without types is only allowed in a function definition");
        ret = (node_t **) vtoa(v);
    } else if (token->id == ')') {
        TYPE_OLDSTYLE(ftype) = 1;
    } else {
	TYPE_OLDSTYLE(ftype) = 1;
        if (token->id == ELLIPSIS)
            error("ISO C requires a named parameter before '...'");
        else
            error("expect parameter declarator at '%s'", token->name);        
	gettok();
    }
    
    if (params && *params == 0) {
        *params = 1;
    } else {
        if (SCOPE > PARAM)
            exit_scope();
        
        exit_scope();
    }
    
    return ret;
}

static inline void parse_assign(node_t *atype)
{
    TYPE_A_ASSIGN(atype) = assign_expr();
    TYPE_A_HASEXPR(atype) = 1;
}

static node_t * arrays(bool abstract)
{
    node_t *atype = array_type(NULL);
    
    if (abstract) {
	if (token->id == '*') {
	    if (lookahead()->id != ']') {
	        parse_assign(atype);
	    } else {
		expect('*');
		TYPE_A_WILDCARD(atype) = 1;
	    }
	} else if (firstexpr(token)) {
	    parse_assign(atype);
	}
    } else {
	if (token->id == STATIC) {
	    expect(STATIC);
	    TYPE_A_STATIC(atype) = 1;
	    if (token->kind == CONST)
		array_qualifiers(atype);
	    parse_assign(atype);
	} else if (token->kind == CONST) {
	    if (token->kind == CONST)
		array_qualifiers(atype);
	    if (token->id == STATIC) {
		expect(STATIC);
		TYPE_A_STATIC(atype) = 1;
	        parse_assign(atype);
	    } else if (token->id == '*') {
		if (lookahead()->id != ']') {
		    parse_assign(atype);
		} else {
		    expect('*');
		    TYPE_A_WILDCARD(atype) = 1;
		}
	    } else if (firstexpr(token)) {
	        parse_assign(atype);
	    }
	} else if (token->id == '*') {
	    if (lookahead()->id != ']') {
	        parse_assign(atype);
	    } else {
		expect('*');
		TYPE_A_WILDCARD(atype) = 1;
	    }
	} else if (firstexpr(token)) {
	    parse_assign(atype);
	}
    }

    // try evaluate the length
    if (TYPE_A_ASSIGN(atype)) {
	node_t *ret = eval(TYPE_A_ASSIGN(atype), inttype);
	if (ret) {
	    if (isiliteral(ret))
		TYPE_LEN(atype) = ILITERAL_VALUE(ret);
	    else if (isfliteral(ret))
		TYPE_LEN(atype) = (int)FLITERAL_VALUE(ret);
	}
    }

    return atype;
}

static node_t * func_or_array(int *params)
{
    node_t *ty = NULL;
    int follow[] = {'[', ID, IF, 0};
    
    for (; token->id == '(' || token->id == '['; ) {
        if (token->id == '[') {
            node_t *atype;
            expect('[');
            atype = arrays(false);
            match(']', follow);
            attach_type(&ty, atype);
        } else {
            node_t *ftype = func_type();
            expect('(');
            TYPE_PARAMS(ftype) = parameters(ftype, params);
            match(')', follow);
            attach_type(&ty, ftype);
        }
    }
    
    return ty;
}

static node_t * abstract_func_or_array(void)
{
    node_t *ty = NULL;
    
    for (; token->id == '(' || token->id == '['; ) {
        if (token->id == '[') {
            node_t *atype;
            expect('[');
            atype = arrays(true);
            expect(']');
            attach_type(&ty, atype);
        } else {
            node_t *ftype = func_type();
            expect('(');
            TYPE_PARAMS(ftype) = parameters(ftype, NULL);
            expect(')');
            attach_type(&ty, ftype);
        }
    }
    
    return ty;
}

static node_t * enum_decl(void)
{
    node_t *sym = NULL;
    const char *id = NULL;
    struct source src = source;
    int follow[] = {INT, CONST, STATIC, IF, 0};
    
    expect(ENUM);
    if (token->id == ID) {
        id = token->name;
        expect(ID);
    }
    if (token->id == '{') {
        int val = 0;
        struct vector *v = vec_new();
        expect('{');
        sym = tag_type(ENUM, id, src);
        if (token->id != ID)
            error("expect identifier");
        while (token->id == ID) {
            node_t *s = lookup(token->name, identifiers);
            if (s && currentscope(s))
                redefinition_error(source, s);
            
            s = install(token->name, &identifiers, SCOPE);
            SYM_TYPE(s) = SYM_TYPE(sym);
            SYM_SRC(s) = source;
            SYM_SCLASS(s) = ENUM;
            expect(ID);
            if (token->id == '=') {
                expect('=');
                val = intexpr();
            }
            SYM_VALUE_U(s) = val++;
            vec_push(v, s);
            if (token->id != ',')
                break;
            expect(',');
        }
        match('}', follow);
        TYPE_IDS(SYM_TYPE(sym)) = (node_t **)vtoa(v);
        SYM_DEFINED(sym) = true;
    } else if (id) {
        sym = lookup(id, tags);
        if (sym) {
            if (currentscope(sym) && !isenum(SYM_TYPE(sym)))
                errorf(src, "use of '%s' with tag type that does not match previous declaration '%s %s' at %s:%u",
                       id2s(ENUM), id, type2s(SYM_TYPE(sym)),  SYM_SRC(sym).file, SYM_SRC(sym).line);
        } else {
            sym = tag_type(ENUM, id, src);
        }
    } else {
        error("expected identifier or '{'");
        sym = tag_type(ENUM, NULL, src);
    }
    
    return SYM_TYPE(sym);
}

static node_t * struct_decl(void)
{
    int t = token->id;
    const char *id = NULL;
    node_t *sym = NULL;
    struct source src = source;
    int follow[] = {INT, CONST, STATIC, IF, '[', 0};
    
    expect(t);
    if (token->id == ID) {
        id = token->name;
        expect(ID);
    }
    if (token->id == '{') {
        expect('{');
        sym = tag_type(t, id, src);
        fields(SYM_TYPE(sym));
	SYM_DEFINED(sym) = true;
	set_typesize(SYM_TYPE(sym));
        match('}', follow);
    } else if (id) {
        sym = lookup(id, tags);
        if (sym) {
            if (currentscope(sym) && TYPE_OP(SYM_TYPE(sym)) != t)
                errorf(src, "use of '%s' with tag type that does not match previous declaration '%s %s' at %s:%u",
                       id2s(t), id, type2s(SYM_TYPE(sym)), SYM_SRC(sym).file, SYM_SRC(sym).line);
        } else {
            sym = tag_type(t, id, src);
        }
    } else {
        error("expected identifier or '{'");
        sym = tag_type(t, NULL, src);
    }

    return SYM_TYPE(sym);
}

static void ensure_field(node_t *field, node_t *sty, struct vector *v, bool last)
{
    const char *name = FIELD_NAME(field);
    node_t *ty = FIELD_TYPE(field);
    
    if (FIELD_ISBIT(field)) {
	int bitsize = FIELD_BITSIZE(field);
	int bits = BITS(TYPE_SIZE(ty));
	
	if (!isint(ty)) {
	    if (name)
		error("bit-field '%s' has non-integral type '%s'", name, type2s(ty));
	    else
		error("anonymous bit-field has non-integral type '%s'", type2s(ty));
	}
	
	if (bitsize < 0) {
	    if (name)
		error("bit-field '%s' has negative width '%d'", name, bitsize);
	    else
		error("anonymous bit-field has negative width '%d'", bitsize);
	}
	
	if (bitsize == 0 && name)
	    error("named bit-field '%s' has zero width", name);
	
	if (bitsize > bits) {
	    if (name)
		error("size of bit-field '%s' (%d bits) exceeds size of its type (%d bits)", name, bitsize, bits);
	    else
		error("anonymous bit-field (%d bits) exceeds size of its type (%d bits)", bitsize, bits);
	}
    }

    if (isarray(ty)) {
	ensure_array(ty, source, CONSTANT);
	if (isstruct(sty) && last && isincomplete(ty)) {
	    if (vec_len(v) == 1)
		error("flexible array cannot be the only member");
	} else {
	    if (TYPE_LEN(ty) == 0) {
		if (isincomplete(ty))
		    error("field has incomplete type '%s'", type2s(ty));
		else
		    error("array has variable or zero length");
	    }
	}
    } else if (isincomplete(ty)) {
	error("field has incomplete type '%s'", type2s(ty));
    } else if (isfunc(ty)) {
	error("field has invalid type '%s'", TYPE_NAME(ty));
    }
}

static void fields(node_t *sty)
{
    int follow[] = {INT, CONST, '}', IF, 0};

#define FIRST_FIELD(t)    (istypename(t) || (t)->kind == STATIC)
    
    struct vector *v = vec_new();
    while (FIRST_FIELD(token)) {
        node_t *basety = specifiers(NULL);
        
        for (;;) {
            node_t *field = new_field(NULL);
            if (token->id == ':') {
                expect(':');
                FIELD_BITSIZE(field) = intexpr();
                FIELD_TYPE(field) = basety;
                FIELD_ISBIT(field) = true;
            } else {
                node_t *ty = NULL;
                const char *id = NULL;
                declarator(&ty, &id, NULL);
                attach_type(&ty, basety);
                if (token->id == ':') {
                    expect(':');
                    FIELD_BITSIZE(field) = intexpr();
                    FIELD_ISBIT(field) = true;
                }
                FIELD_TYPE(field) = ty;
                if (id) {
                    for (int i=0; i < vec_len(v); i++) {
                        node_t *f = vec_at(v, i);
                        if (FIELD_NAME(f) && !strcmp(FIELD_NAME(f), id)) {
                            error("redefinition of '%s'", id);
                            break;
                        }
                    }
                    FIELD_NAME(field) = id;
                }
            }
            
	    vec_push(v, field);
            if (token->id != ',')
                break;
            expect(',');
	    ensure_field(field, sty, v, false);
        }
        
        match(';', follow);
	ensure_field(vec_tail(v), sty, v, !FIRST_FIELD(token));
    }
    TYPE_FIELDS(sty) = (node_t **)vtoa(v);
}

static node_t * ptr_decl(void)
{
    node_t *ret = NULL;
    int con, vol, res, type;
    
    CCAssert(token->id == '*');
    
    for (;;) {
        int *p, t = token->id;
        switch (token->id) {
	case CONST:
	    p = &con;
	    break;
                
	case VOLATILE:
	    p = &vol;
	    break;
                
	case RESTRICT:
	    p = &res;
	    break;
                
	case '*':
            {
                node_t *pty = ptr_type(NULL);
                con = vol = res = type = 0;
                p = &type;
                prepend_type(&ret, pty);
            }
	    break;
                
	default:
	    p = NULL;
	    break;
        }
        
        if (p == NULL)
            break;
        
        if (*p != 0)
            warning("duplicate type qulifier '%s'", token->name);
        
        *p = t;
        
        if (t == CONST || t == VOLATILE || t == RESTRICT)
            ret = qual(t, ret);
        
        gettok();
    }
    
    return ret;
}

static void param_declarator(node_t **ty, const char **id)
{
    if (token->id == '*') {
        node_t *pty = ptr_decl();
        prepend_type(ty, pty);
    }
    
    if (token->id == '(') {
        if (firstdecl(lookahead())) {
            abstract_declarator(ty);
        } else {
            node_t *type1 = *ty;
            node_t *rtype = NULL;
            expect('(');
            param_declarator(&rtype, id);
            expect(')');
            if (token->id == '(' || token->id == '[') {
                node_t *faty;
                CCAssert(id);
                if (*id) {
                    faty = func_or_array(NULL);
                } else {
                    faty = abstract_func_or_array();
                }
                attach_type(&faty, type1);
                attach_type(&rtype, faty);
            }
            *ty = rtype;
        }
    } else if (token->id == '[') {
        abstract_declarator(ty);
    } else if (token->id == ID) {
        declarator(ty, id, NULL);
    }
}

static void abstract_declarator(node_t **ty)
{
    CCAssert(ty);
    
    if (token->id == '*' || token->id == '(' || token->id == '[') {
        if (token->id == '*') {
            node_t *pty = ptr_decl();
            prepend_type(ty, pty);
        }
        
        if (token->id == '(') {
            if (firstdecl(lookahead())) {
                node_t *faty = abstract_func_or_array();
                prepend_type(ty, faty);
            } else {
                expect('(');
                abstract_declarator(ty);
                expect(')');
            }
        } else if (token->id == '[') {
            node_t *faty = abstract_func_or_array();
            prepend_type(ty, faty);
        }
    } else {
        error("expect '(' or '[' at '%s'", token->name);
    }
}

static void declarator(node_t **ty, const char **id, int *params)
{
    CCAssert(ty && id);
    int follow[] = {',', '=', IF, 0};
    
    if (token->id == '*') {
        node_t *pty = ptr_decl();
        prepend_type(ty, pty);
    }
    
    if (token->id == ID) {
        *id = token->name;
        expect(ID);
        if (token->id == '[' || token->id == '(') {
            node_t *faty = func_or_array(params);
            prepend_type(ty, faty);
        }
    } else if (token->id == '(') {
        node_t *type1 = *ty;
        node_t *rtype = NULL;
        expect('(');
        declarator(&rtype, id, params);
        match(')', follow);
        if (token->id == '[' || token->id == '(') {
            node_t *faty = func_or_array(params);
            attach_type(&faty, type1);
            attach_type(&rtype, faty);
        } else {
	    attach_type(&rtype, type1);
	}
        *ty = rtype;
    } else {
        error("expect identifier or '(' before '%s'", token->name);
    }
}

static bool firstfuncdef(node_t *ty)
{
    bool prototype = token->id == '{';
    bool oldstyle = (istypename(token) || token->kind == STATIC) && TYPE_OLDSTYLE(ty) && TYPE_PARAMS(ty);
    
    return isfunc(ty) && (prototype || oldstyle);
}

static struct vector * decls(node_t * (*dcl)(const char *id, node_t *ftype, int sclass,  struct source src))
{
    struct vector *v = vec_new();
    node_t *basety;
    int sclass;
    struct source src = source;
    int level = SCOPE;
    int follow[] = {STATIC, INT, CONST, IF, '}', 0};
    
    basety = specifiers(&sclass);
    if (token->id == ID || token->id == '*' || token->id == '(') {
        const char *id = NULL;
        node_t *ty = NULL;
        int params = 0;		// for functioness
        src = source;
        
        // declarator
        declarator(&ty, &id, &params);
        attach_type(&ty, basety);
	
        if (level == GLOBAL) {
            if (params) {
                if (firstfuncdef(ty)) {
                    vec_push(v, funcdef(id, ty, sclass, src));
                    return v;
                } else {
                    if (SCOPE > PARAM)
                        exit_scope();
                    
                    exit_scope();
                }
            }
        }
        
        for (;;) {
            if (id) {
                node_t *decl;
		int kind;
                node_t *sym = dcl(id, ty, sclass, src);
                if (sclass == TYPEDEF)
                    decl = ast_decl(TYPEDEF_DECL, SCOPE);
                else if (isfunc(ty))
		    decl = ast_decl(FUNC_DECL, SCOPE);
		else
                    decl = ast_decl(VAR_DECL, SCOPE);
                
                DECL_SYM(decl) = sym;
		AST_TYPE(decl) = SYM_TYPE(sym);

		if (dcl == globaldecl)
		    kind = GLOBAL;
		else if (dcl == localdecl)
		    kind = LOCAL;
		else if (dcl == paramdecl)
		    kind = PARAM;
		else
		    CCAssert(0);
		
                if (token->id == '=')
		    decl_initializer(decl, sym, sclass, kind);

		post_decl(decl, sym, sclass, kind);
                vec_push(v, decl);
            }
            
            if (token->id != ',')
                break;
            
            expect(',');
            id = NULL;
            ty = NULL;
            src = source;
            // declarator
            declarator(&ty, &id, NULL);
            attach_type(&ty, basety);
        }
    } else if (isenum(basety) || isstruct(basety) || isunion(basety)) {
        // struct/union/enum
        int node_id;
        node_t *decl;
        if (isstruct(basety))
            node_id = STRUCT_DECL;
        else if (isunion(basety))
            node_id = UNION_DECL;
        else
            node_id = ENUM_DECL;
        
        decl = ast_decl(node_id, SCOPE);
        DECL_SYM(decl) = TYPE_TSYM(basety);
	AST_TYPE(decl) = SYM_TYPE(DECL_SYM(decl));
        vec_push(v, decl);
    } else {
        error("invalid token '%s' in declaration", token->name);
    }
    match(';', follow);
    
    return v;
}

static node_t * initializer(node_t *ty)
{
    if (token->id == '{') {
        return initializer_list(ty);
    } else if (firstexpr(token)) {
        return assign_expr();
    } else {
        error("expect '{' or assignment expression");
        return NULL;
    }
}

node_t * initializer_list(node_t *ty)
{
    int follow[] = {',', IF, '[', ID, '.', DEREF, 0};
    node_t *ret = ast_inits();
    struct vector *v = vec_new();
    
    expect('{');
    if (FIRST_INIT(token)) {
	if (ty) {
	    if (isstruct(ty) || isunion(ty))
		struct_init(ty, true, v);
	    else if (isarray(ty))
		array_init(ty, true, v);
	    else
		scalar_init(ty, v);

	    if (token->id == ',')
		expect(',');

	    if (FIRST_INIT(token)) {
		warning("excess elements in %s initializer at '%s'", TYPE_NAME(ty), token->name);
		eat_initlist();
	    }
	} else {
	    eat_initlist();
	}
    } else {
	// inhibit redundant errors
	if (ty)
	    error("expect initializer at '%s'", token->name);
    }
    
    match('}', follow);
    EXPR_INITS(ret) = (node_t **)vtoa(v);
    AST_TYPE(ret) = ty;
    return ret;
}

int firstdecl(struct token *t)
{
    return t->kind == STATIC || t->kind == INT || t->kind == CONST || (t->id == ID && istypedef(t->name));
}

int firststmt(struct token *t)
{
    return  t->kind == IF || firstexpr(t);
}

int firstexpr(struct token *t)
{
    return t->kind == ID;
}

bool istypename(struct token *t)
{
    return (t->kind == INT || t->kind == CONST) ||
	(t->id == ID && istypedef(t->name));
}

node_t * typename(void)
{
    node_t *basety;
    node_t *ty = NULL;
    
    basety = specifiers(NULL);
    if (token->id == '*' || token->id == '(' || token->id == '[')
        abstract_declarator(&ty);
    
    attach_type(&ty, basety);
    
    return ty;
}

node_t ** declaration(void)
{
    CCAssert(SCOPE >= LOCAL);
    return (node_t **)vtoa(decls(localdecl));
}

node_t * translation_unit(void)
{
    node_t *ret = ast_decl(TU_DECL, GLOBAL);
    struct vector *v = vec_new();

    for (gettok(); token->id != EOI; ) {
        if (firstdecl(token)) {
            CCAssert(SCOPE == GLOBAL);
            vec_add(v, decls(globaldecl));
        } else {
            if (token->id != ';')
                error("invalid token '%s' in declaration", token->name);
            gettok();
        }
    }
    
    DECL_EXTS(ret) = (node_t **)vtoa(v);
    return ret;
}

static node_t * paramdecl(const char *id, node_t *ty, int sclass,  struct source src)
{
    node_t *sym = NULL;
    bool prototype = PARAM_STYLE(sclass);
    bool first = PARAM_FIRST(sclass);
    bool fvoid = PARAM_FVOID(sclass);
    sclass = PARAM_SCLASS(sclass);
    
    if (sclass && sclass != REGISTER) {
        error("invalid storage class specifier '%s' in function declarator", id2s(sclass));
        sclass = 0;
    }
    
    if (isfunc(ty)) {
        ensure_func(ty, src);
        ty = ptr_type(ty);
    } else if (isarray(ty)) {
        ensure_array(ty, src, PARAM);
        ty = ptr_type(rtype(ty));
    } else if (isenum(ty) || isstruct(ty) || isunion(ty)) {
        if (!SYM_DEFINED(TYPE_TSYM(ty)) ||
	    SYM_SCOPE(TYPE_TSYM(ty)) == SCOPE)
            warningf(src, "declaration of '%s' will not be visible outside of this function", type2s(ty));
    } else if (isvoid(ty)) {
	if (prototype) {
	    if (first) {
		if (id)
		    errorf(src, "argument may not have 'void' type");
		else if (isqual(ty))
		    errorf(src, "'void' as parameter must not have type qualifiers");
	    }
	} else {
	    errorf(src, "argument may not have 'void' type");
	}
    }

    if (prototype && fvoid && !first)
	errorf(src, "'void' must be the first and only parameter if specified");
    
    if (id) {
        sym = lookup(id, identifiers);
        if (sym && SYM_SCOPE(sym) == SCOPE)
            redefinition_error(source, sym);
        sym = install(id, &identifiers, SCOPE);
    } else {
        sym = anonymous(&identifiers, SCOPE);
    }
    
    SYM_TYPE(sym) = ty;
    SYM_SRC(sym) = src;
    SYM_SCLASS(sym) = sclass;
    
    return sym;
}

static node_t * localdecl(const char *id, node_t *ty, int sclass, struct source src)
{
    node_t *sym = NULL;
    
    CCAssert(id);
    CCAssert(SCOPE >= LOCAL);
    
    if (isfunc(ty)){
        if (TYPE_PARAMS(ty) && TYPE_OLDSTYLE(ty))
            error("a parameter list without types is only allowed in a function definition");
	ensure_func(ty, src);
    } else if (isarray(ty)) {
        ensure_array(ty, src, LOCAL);
    }
    
    sym = lookup(id, identifiers);
    if (sym && currentscope(sym)) {
        redefinition_error(src, sym);
    } else {
        sym = install(id, &identifiers, SCOPE);
        SYM_TYPE(sym) = ty;
        SYM_SRC(sym) = src;
        SYM_DEFINED(sym) = true;
        SYM_SCLASS(sym) = sclass;
    }

    return sym;
}

static node_t * globaldecl(const char *id, node_t *ty, int sclass, struct source src)
{
    node_t *sym = NULL;
    
    CCAssert(id);
    CCAssert(SCOPE == GLOBAL);
    
    if (sclass == AUTO || sclass == REGISTER) {
        errorf(src, "illegal storage class on file-scoped variable");
        sclass = 0;
    }
    
    if (isfunc(ty)) {
        if (TYPE_PARAMS(ty) && TYPE_OLDSTYLE(ty))
            error("a parameter list without types is only allowed in a function definition");
	ensure_func(ty, src);
    } else if (isarray(ty)) {
        ensure_array(ty, src, GLOBAL);
    }
    
    sym = lookup(id, identifiers);
    if (!sym || SYM_SCOPE(sym) != SCOPE) {
        sym = install(id, &identifiers, SCOPE);
        SYM_TYPE(sym) = ty;
        SYM_SRC(sym) = src;
        SYM_SCLASS(sym) = sclass;
    } else if (sclass != TYPEDEF && eqtype(ty, SYM_TYPE(sym))) {
        if (sclass == STATIC && SYM_SCLASS(sym) != STATIC)
            errorf(src, "static declaration of '%s' follows non-static declaration", id);
        else if (SYM_SCLASS(sym) == STATIC && sclass != STATIC)
            errorf(src, "non-static declaration of '%s' follows static declaration", id);
    } else {
        conflicting_types_error(src, sym);
    }
    
    return sym;
}

static node_t * funcdef(const char *id, node_t *ftype, int sclass,  struct source src)
{
    node_t *decl = ast_decl(FUNC_DECL, SCOPE);
    
    CCAssert(SCOPE == PARAM);
    
    if (sclass && sclass != EXTERN && sclass != STATIC) {
        error("invalid storage class specifier '%s'", id2s(sclass));
        sclass = 0;
    }
    
    if (id) {
        node_t *sym = lookup(id, identifiers);
        if (!sym) {
            sym = install(id, &identifiers, GLOBAL);
            SYM_TYPE(sym) = ftype;
            SYM_SRC(sym) = src;
            SYM_DEFINED(sym) = true;
            SYM_SCLASS(sym) = sclass;
            DECL_SYM(decl) = sym;
        } else if (eqtype(ftype, SYM_TYPE(sym)) && !SYM_DEFINED(sym)) {
            if (sclass == STATIC && SYM_SCLASS(sym) != STATIC) {
                errorf(src, "static declaaration of '%s' follows non-static declaration", id);
            } else {
                SYM_TYPE(sym) = ftype;
                SYM_SRC(sym) = src;
                SYM_DEFINED(sym) = true;
                DECL_SYM(decl) = sym;
            }
        } else {
            redefinition_error(src, sym);
        }
    }
    
    if (TYPE_PARAMS(ftype)) {
        for (int i=0; TYPE_PARAMS(ftype)[i]; i++) {
            node_t *sym = TYPE_PARAMS(ftype)[i];
            SYM_DEFINED(sym) = true;
            // params id is required in prototype
            if (isanonymous(SYM_NAME(sym)))
                errorf(SYM_SRC(sym), "parameter name omitted");
            if (isenum(SYM_TYPE(sym)) || isstruct(SYM_TYPE(sym)) || isunion(SYM_TYPE(sym))) {
                if (!SYM_DEFINED(TYPE_TSYM(SYM_TYPE(sym))))
                    errorf(SYM_SRC(sym), "variable has incomplete type '%s'", type2s(SYM_TYPE(sym)));
            }
        }
    }
    
    if (firstdecl(token)) {
        // old style function definition
        struct vector *v = vec_new();
        enter_scope();
        while (firstdecl(token))
            vec_add(v, decls(paramdecl));
        
        for (int i=0; i < vec_len(v); i++) {
            node_t *decl = (node_t *)vec_at(v, i);
            node_t *sym = DECL_SYM(decl);
            
            CCAssert(SYM_NAME(sym));
            if (!isvardecl(decl)) {
                warningf(SYM_SRC(sym), "empty declaraion");
            } else if (TYPE_PARAMS(ftype)) {
                node_t *p = NULL;
                for (int i=0; TYPE_PARAMS(ftype)[i]; i++) {
                    node_t *s = TYPE_PARAMS(ftype)[i];
                    if (SYM_NAME(s) && !strcmp(SYM_NAME(s), SYM_NAME(sym))) {
                        p = s;
                        break;
                    }
                }
                if (p)
                    SYM_TYPE(p) = SYM_TYPE(sym);
                else
                    errorf(SYM_SRC(sym), "parameter named '%s' is missing", SYM_NAME(sym));
            }
        }
        exit_scope();
        if (token->id != '{') {
            error("expect function body after function declarator");
            // TODO
        }
    }

    if (token->id == '{') {
        // function definition
        // install symbol first for backward reference
        SET_FUNCDEF_CONTEXT(ftype, id);
        node_t *stmt = compound_stmt();
	post_funcdef(ftype, stmt);
        exit_scope();
	DECL_BODY(decl) = stmt;
        RESTORE_FUNCDEF_CONTEXT();
    }
    
    return decl;
}

static void eat_initializer(void)
{
    if (token->id == '[' || token->id == '.') {
	do {
	    if (token->id == '[') {
		expect('[');
		intexpr();
		expect(']');
	    } else {
		expect('.');
		expect(ID);
	    }
	} while (token->id == '[' || token->id == '.');
	expect('=');
    }

    initializer(NULL);
}

static void eat_initlist(void)
{
    do {
	eat_initializer();
	if (token->id != ',')
	    break;

	expect(',');
    } while (FIRST_INIT(token));
}

static bool is_string(node_t *ty)
{
    if (!isarray(ty))
	return false;
    
    node_t *rty = rtype(ty);
    return TYPE_KIND(rty) == CHAR || unqual(rty) == wchartype;
}

static node_t * find_elem(struct vector *v, int i)
{
    for (int j = vec_len(v); j <= i; j++)
	vec_push(v, ast_vinit());
    return vec_at(v, i);
}

static node_t * init_elem_conv(node_t *ty, node_t *node)
{
    node_t *ret = init_conv(ty, node);
    if (ret == NULL)
	error(INCOMPATIBLE_TYPES, type2s(AST_TYPE(node)), type2s(ty));

    return ret;
}

static inline node_t * do_init_elem_conv(node_t *ty, node_t *node)
{
    // cannot pass VINIT_EXPR to init_elem_conv
    if (AST_ID(node) == VINIT_EXPR)
	return NULL;		// init_elem_conv has failed
    return init_elem_conv(ty, node);
}

static void init_string(node_t *ty, node_t *node)
{
    int len1 = TYPE_LEN(ty);
    int len2 = TYPE_LEN(AST_TYPE(node));
    if (len1 > 0) {
	if (len1 < len2-1)
	    warning("initializer-string for char array is too long");
    } else if (isincomplete(ty)) {
	TYPE_LEN(ty) = len2;
	set_typesize(ty);
    }
}

static void aggregate_set(node_t *ty, struct vector *v, int i, node_t *node)
{
    if (!node)
	return;
    
    node_t *n = find_elem(v, i);
    if (AST_ID(n) != VINIT_EXPR)
	warning("initializer overrides prior initialization");
    
    if (AST_ID(node) == INITS_EXPR) {
	vec_set(v, i, node);
    } else if (is_string(ty) && issliteral(node)) {
        init_string(ty, node);
	vec_set(v, i, node);
    } else {
	node_t *rty = NULL;
	if (isarray(ty)) {
	    rty = rtype(ty);
	} else {
	    if (TYPE_FIELDS(ty))
		rty = FIELD_TYPE(TYPE_FIELDS(ty)[0]);
	}

	if (rty) {
	    node_t *n1 = ast_inits();
	    struct vector *v1 = vec_new();
	    vec_set(v, i, n1);

	    if (isarray(rty) || isstruct(rty) || isunion(rty))
		aggregate_set(rty, v1, 0, node);
	    else
		vec_push_safe(v1, do_init_elem_conv(rty, node));
	
	    EXPR_INITS(n1) = (node_t **)vtoa(v1);
	}
    }
}

static void scalar_set(node_t *ty, struct vector *v, int i, node_t *node)
{
    if (!node)
	return;
    
    node_t *n = find_elem(v, i);
    if (AST_ID(n) != VINIT_EXPR)
	warning("initializer overrides prior initialization");

    if (AST_ID(node) == INITS_EXPR) {	
	node_t **inits;
    loop:
	inits = EXPR_INITS(node);
	if (inits) {
	    node = inits[0];
	    if (AST_ID(node) == INITS_EXPR)
		goto loop;
	    vec_set_safe(v, i, do_init_elem_conv(ty, node));
	}
    } else {
	vec_set_safe(v, i, do_init_elem_conv(ty, node));
    }
}

static void struct_init(node_t *ty, bool brace, struct vector *v)
{
    bool designated = false;
    int len = LIST_LEN(TYPE_FIELDS(ty));

    for (int i = 0; ; i++) {
	node_t *fieldty = NULL;

	if (token->id == '.') {
	    const char *name = NULL;
	    expect('.');
	    if (token->id == ID)
		name = token->name;
	    expect(ID);
	    node_t *field = find_field(ty, name);
	    if (field) {
		i = indexof_field(ty, field);
		fieldty = FIELD_TYPE(field);
	    } else {
		i--;
		if (name)
		    field_not_found_error(ty, name);
	    }
	    designated = true;
	}

	if (i >= len)
	    break;

	if (!designated)
	    fieldty = FIELD_TYPE(TYPE_FIELDS(ty)[i]);
	elem_init(ty, fieldty, designated, v, i);
	designated = false;
	
	struct token *ahead = lookahead();
	if (token->id == '}' || (token->id == ',' && ahead->id == '}'))
	    break;
	if ((ahead->id == '.' || ahead->id == '[') && !brace)
	    break;
	if (brace || i < len - 1)
	    expect(',');
    }
}

static void array_init(node_t *ty, bool brace, struct vector *v)
{
    bool designated = false;
    int c = 0;
    int len = TYPE_LEN(ty);

    if (is_string(ty) && token->id == SCONSTANT) {
	node_t *expr = assign_expr();
	if (vec_len(v)) {
	    warning("initializer overrides prior initialization");
	    vec_clear(v);
	}
	aggregate_set(ty, v, 0, expr);
	return;
    }
    
    for (int i = 0; ; i++) {
	node_t *rty = NULL;
	
	if (token->id == '[') {
	    expect('[');
	    i = intexpr();
	    expect(']');
	    designated = true;
	}

	if (len > 0 && i >= len && !designated)
	    break;
	
	c = MAX(c, i);
	if (len > 0 && i >= len)
	    error("array designator index [%d] exceeds array bounds (%d)", i, len);
	else
	    rty = rtype(ty);
	elem_init(ty, rty, designated, v, i);
	designated = false;

	struct token *ahead = lookahead();
	if (token->id == '}' || (token->id == ',' && ahead->id == '}'))
	    break;
	if ((ahead->id == '.' || ahead->id == '[') && !brace)
	    break;
	if (brace || (len > 0 && i < len - 1) || len == 0)
	    expect(',');
    }

    if (isincomplete(ty)) {
	TYPE_LEN(ty) = c + 1;
	set_typesize(ty);
    }
}

static void scalar_init(node_t *ty, struct vector *v)
{
    if (token->id == '.' || token->id == '[') {
	error("designator in initializer for scalar type '%s'", type2s(ty));
	eat_initializer();
    } else if (token->id == '{') {
	static int braces;
	if (braces++ == 0)
	    warning("too many braces around scalar initializer");
	scalar_set(ty, v, 0, initializer_list(ty));
	braces--;
    } else {
	scalar_set(ty, v, 0, initializer(ty));
    }
}

static void elem_init(node_t *sty, node_t *ty, bool designated, struct vector *v, int i)
{
#define IS_STRING_VEC(ty, v)			\
    (is_string(ty) &&				\
     vec_len(v) == 1 &&				\
     issliteral((node_t *)vec_head(v)))
    
    
    if (ty == NULL) {
        if (token->id == '.' || token->id == '[') {
	    eat_initializer();
	} else {
	    if (token->id == '=')
		expect('=');
	    initializer(ty);
	}
    } else if (isstruct(ty) || isunion(ty) || isarray(ty)) {
	if (token->id == '=') {
	    if (!designated)
		error("expect designator before '='");
	    expect('=');
	    aggregate_set(ty, v, i, initializer(ty));
	} else if (token->id == '{') {
	    if (designated)
		error("expect '=' or another designator at '%s'", token->name);
	    aggregate_set(ty, v, i, initializer_list(ty));
	} else if ((token->id == '.' && isarray(ty)) ||
		   (token->id == '[' && !isarray(ty))) {
	    SAVE_ERRORS;
	    eat_initializer();
	    // inhibit redundant errors
	    if (NO_ERROR)
		error("%s designator cannot initialize non-%s type '%s'", TYPE_NAME(ty), TYPE_NAME(ty), type2s(ty));
	} else {
	    node_t *n = find_elem(v, i);
	    struct vector *v1 = vec_new();
	    if (AST_ID(n) == INITS_EXPR) {
		vec_add_array(v1, (void **)EXPR_INITS(n));
	    } else if (AST_ID(n) == STRING_LITERAL) {
		vec_push(v1, n);
	    }
	    
	    if (isarray(ty))
		array_init(ty, false, v1);
	    else
		struct_init(ty, false, v1);

	    if (IS_STRING_VEC(ty, v1)) {
		// string literal
		vec_set(v, i, (node_t *)vec_head(v1));
	    } else {
		if (AST_ID(n) != INITS_EXPR) {
		    n = ast_inits();
		    vec_set(v, i, n);
		}
		EXPR_INITS(n) = (node_t **)vtoa(v1);
	    }
	}
    } else {
	if (designated)
	    expect('=');
	if (IS_STRING_VEC(sty, v)) {
	    warning("initializer overrides prior initialization");
	    vec_clear(v);
	}
	scalar_set(ty, v, i, initializer(ty));
    }

#undef IS_STRING_VEC
}

bool has_static_extent(node_t *sym)
{
    return SYM_SCLASS(sym) == EXTERN ||
	SYM_SCLASS(sym) == STATIC ||
	SYM_SCOPE(sym) == GLOBAL;
}

static void decl_initializer(node_t *decl, node_t *sym, int sclass, int kind)
{
    node_t *ty = SYM_TYPE(sym);
    struct source src = SYM_SRC(sym);
    node_t *init;

    expect('=');

    if (kind == PARAM) {
	error("C does not support default arguments");
	initializer(NULL);
	return;
    } else if (!(isscalar(ty) || isarray(ty) || isrecord(ty))) {
	error("'%s' cannot have an initializer", TYPE_NAME(ty));
	initializer(NULL);
	return;
    }

    init = initializer(ty);
    if (init == NULL)
	return;
    
    if (sclass == EXTERN) {
	if (kind == GLOBAL) {
	    warningf(src, "'extern' variable has an initializer");
	} else {
	    errorf(src, "'extern' variable cannot have an initializer");
	    return;
	}
    } else if (sclass == TYPEDEF) {
	errorf(src, "illegal initializer (only variable can be initialized)");
	return;
    }

    if (kind == GLOBAL) {
	if (SYM_DEFINED(sym))
	    redefinition_error(src, sym);
	SYM_DEFINED(sym) = true;
    }

    if (istag(ty) && isincomplete(ty)) {
	error("variable has incomplete type '%s'", type2s(ty));
	return;
    } else if (isvarray(ty)) {
	error("variable-sized object may not be initialized");
	return;
    }

    if (AST_ID(init) != INITS_EXPR) {
	if (isarray(ty)) {
	    if (is_string(ty) && issliteral(init))
		init_string(ty, init);
	    else
		error("array initializer must be an initializer list or string literal");
	} else if (isstruct(ty) || isunion(ty)) {
	    if (!eqtype(ty, AST_TYPE(init)))
		error("initialzing '%s' with an expression of imcompatible type '%s'",
		      type2s(ty), type2s(AST_TYPE(init)));
	} else {
	    init = init_elem_conv(ty, init);
	}
    }

    if (init && has_static_extent(sym)) {
        init = eval(init, ty);
	if (init == NULL)
	    error("initializer element is not a compile-time constant");
    }

    DECL_BODY(decl) = init;
}

static void post_decl(node_t *decl, node_t *sym, int sclass, int kind)
{
    node_t *ty = SYM_TYPE(sym);
    if (isarray(ty)) {
	
    }
}

static void post_funcdef(node_t *fty, node_t *stmt)
{
    // check goto labels
    backfill_labels();
    // TODO: check control flow and return stmt
    
}

static void ensure_func(node_t *ftype, struct source src)
{
    if (isarray(rtype(ftype)))
        errorf(src, "function cannot return array type");
    else if (isfunc(rtype(ftype)))
        errorf(src, "function cannot return function type");
}

static void ensure_array(node_t *atype, struct source src, int level)
{
    node_t *rty = atype;
    do {
	node_t *assign = TYPE_A_ASSIGN(rty);
	if (assign) {
	    if (!isint(AST_TYPE(assign)))
		error("size of array has non-integer type '%s'", type2s(AST_TYPE(assign)));
	}
	
	if (TYPE_A_WILDCARD(rty) && level != PARAM)
	    error("star modifier used outside of function prototype");
	
	if ((TYPE_A_CONST(rty) || TYPE_A_RESTRICT(rty) || TYPE_A_VOLATILE(rty) || TYPE_A_STATIC(rty)) &&
	    level != PARAM)
	    error("type qualifier used in array declarator outside of function prototype");
	
	rty = rtype(rty);
	if (isfunc(rty))
	    errorf(src, "array of function is invalid");
	else if (isincomplete(rty))
	    errorf(src, "array has incomplete element type '%s'", type2s(rty));
	
    } while (isarray(rty));

    set_typesize(atype);
}
