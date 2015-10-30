#include "cc.h"

static void abstract_declarator(node_t **ty);
static void declarator(node_t **ty, struct token **id, int *params);
static void param_declarator(node_t **ty, struct token **id);
static node_t * ptr_decl(void);
static node_t * enum_decl(void);
static node_t * struct_decl(void);

typedef node_t * declfun_p (struct token *id, node_t *ty, int sclass);
static struct vector * decls(declfun_p *dcl);
static node_t * paramdecl(struct token *id, node_t *ty, int sclass);
static node_t * globaldecl(struct token *id, node_t *ty, int sclass);
static node_t * localdecl(struct token *id, node_t *ty, int sclass);
static node_t * funcdef(struct token *id, node_t *ty, int sclass);
static void fields(node_t *sty);

static void ensure_decl(node_t *decl, int sclass, int kind);
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
struct vector *localvars;
struct vector *staticvars;

#define SET_FUNCDEF_CONTEXT(fty, id)		\
    gotos = vec_new();				\
    labels = map_new();				\
    current_ftype = fty;			\
    current_fname = id;				\
    localvars = vec_new()

#define RESTORE_FUNCDEF_CONTEXT()		\
    vec_free(gotos);				\
    gotos = NULL;				\
    map_free(labels);				\
    labels = NULL;				\
    current_ftype = NULL;			\
    current_fname = NULL;			\
    vec_free(localvars);			\
    localvars = NULL

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
                cc_assert(0);
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
	    cc_assert(0);
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
            struct token *id = NULL;
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
	    sym = paramdecl(id, ty, PACK_PARAM(1, i == 0, first_void, sclass));
	    if (NO_ERROR && !first_void)
		vec_push(v, sym);
            if (token->id != ',')
                break;
            
            expect(',');
            if (token->id == ELLIPSIS) {
		if (!first_void)
		    vec_push(v, paramdecl(token, vartype, PACK_PARAM(1, 0, first_void, 0)));
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
                vec_push(v, paramdecl(token, inttype, 0));
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

static node_t * arrays(bool abstract)
{
    node_t *atype = array_type(NULL);
    
    if (abstract) {
	if (token->id == '*') {
	    if (lookahead()->id != ']') {
	        TYPE_A_ASSIGN(atype) = assign_expr();
	    } else {
		expect('*');
		TYPE_A_STAR(atype) = 1;
	    }
	} else if (firstexpr(token)) {
	    TYPE_A_ASSIGN(atype) = assign_expr();
	}
    } else {
	if (token->id == STATIC) {
	    expect(STATIC);
	    TYPE_A_STATIC(atype) = 1;
	    if (token->kind == CONST)
		array_qualifiers(atype);
	    TYPE_A_ASSIGN(atype) = assign_expr();
	} else if (token->kind == CONST) {
	    if (token->kind == CONST)
		array_qualifiers(atype);
	    if (token->id == STATIC) {
		expect(STATIC);
		TYPE_A_STATIC(atype) = 1;
	        TYPE_A_ASSIGN(atype) = assign_expr();
	    } else if (token->id == '*') {
		if (lookahead()->id != ']') {
		    TYPE_A_ASSIGN(atype) = assign_expr();
		} else {
		    expect('*');
		    TYPE_A_STAR(atype) = 1;
		}
	    } else if (firstexpr(token)) {
	        TYPE_A_ASSIGN(atype) = assign_expr();
	    }
	} else if (token->id == '*') {
	    if (lookahead()->id != ']') {
	        TYPE_A_ASSIGN(atype) = assign_expr();
	    } else {
		expect('*');
		TYPE_A_STAR(atype) = 1;
	    }
	} else if (firstexpr(token)) {
	    TYPE_A_ASSIGN(atype) = assign_expr();
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
            AST_SRC(s) = source;
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
                errorf(src,
		       "use of '%s' with tag type that does not match previous declaration '%s %s' at %s:%u:%u",
                       id2s(ENUM), id, type2s(SYM_TYPE(sym)),  AST_SRC(sym).file, AST_SRC(sym).line, AST_SRC(sym).column);
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
                errorf(src, "use of '%s' with tag type that does not match previous declaration '%s %s' at %s:%u:%u",
                       id2s(t), id, type2s(SYM_TYPE(sym)), AST_SRC(sym).file, AST_SRC(sym).line, AST_SRC(sym).column);
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
    struct source src = AST_SRC(field);
    
    if (FIELD_ISBIT(field)) {
	int bitsize = FIELD_BITSIZE(field);
	int bits = BITS(TYPE_SIZE(ty));
	
	if (!isint(ty)) {
	    if (name)
		errorf(src, "bit-field '%s' has non-integral type '%s'", name, type2s(ty));
	    else
		errorf(src, "anonymous bit-field has non-integral type '%s'", type2s(ty));
	}
	
	if (bitsize < 0) {
	    if (name)
		errorf(src, "bit-field '%s' has negative width '%d'", name, bitsize);
	    else
		errorf(src, "anonymous bit-field has negative width '%d'", bitsize);
	}
	
	if (bitsize == 0 && name)
	    errorf(src, "named bit-field '%s' has zero width", name);
	
	if (bitsize > bits) {
	    if (name)
		errorf(src, "size of bit-field '%s' (%d bits) exceeds size of its type (%d bits)", name, bitsize, bits);
	    else
		errorf(src, "anonymous bit-field (%d bits) exceeds size of its type (%d bits)", bitsize, bits);
	}
    }

    if (isarray(ty)) {
	ensure_array(ty, source, CONSTANT);
	if (isstruct(sty) && last && isincomplete(ty)) {
	    if (vec_len(v) == 1)
		errorf(src, "flexible array cannot be the only member");
	} else {
	    if (TYPE_LEN(ty) == 0) {
		if (isincomplete(ty))
		    errorf(src, "field has incomplete type '%s'", type2s(ty));
		else
		    errorf(src, "array has variable or zero length");
	    }
	}
    } else if (isincomplete(ty)) {
	errorf(src, "field has incomplete type '%s'", type2s(ty));
    } else if (isfunc(ty)) {
	errorf(src, "field has invalid type '%s'", TYPE_NAME(ty));
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
		AST_SRC(field) = source;
                expect(':');
                FIELD_BITSIZE(field) = intexpr();
                FIELD_TYPE(field) = basety;
                FIELD_ISBIT(field) = true;
            } else {
                node_t *ty = NULL;
                struct token *id = NULL;
                declarator(&ty, &id, NULL);
                attach_type(&ty, basety);
                if (token->id == ':') {
		    AST_SRC(field) = source;
                    expect(':');
                    FIELD_BITSIZE(field) = intexpr();
                    FIELD_ISBIT(field) = true;
                }
                FIELD_TYPE(field) = ty;
                if (id) {
                    for (int i=0; i < vec_len(v); i++) {
                        node_t *f = vec_at(v, i);
                        if (FIELD_NAME(f) && !strcmp(FIELD_NAME(f), id->name)) {
                            errorf(id->src, "redefinition of '%s'", id->name);
                            break;
                        }
                    }
                    FIELD_NAME(field) = id->name;
		    AST_SRC(field) = id->src;
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
    
    cc_assert(token->id == '*');
    
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

static void param_declarator(node_t **ty, struct token **id)
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
                cc_assert(id);
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
    cc_assert(ty);
    
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

static void declarator(node_t **ty, struct token **id, int *params)
{
    cc_assert(ty && id);
    int follow[] = {',', '=', IF, 0};
    
    if (token->id == '*') {
        node_t *pty = ptr_decl();
        prepend_type(ty, pty);
    }
    
    if (token->id == ID) {
        *id = token;
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

static node_t * make_decl(struct token *id, node_t *ty, int sclass, declfun_p *dcl)
{
    node_t *decl;
    node_t *sym = dcl(id, ty, sclass);
    if (sclass == TYPEDEF)
	decl = ast_decl(TYPEDEF_DECL);
    else if (isfunc(ty))
	decl = ast_decl(FUNC_DECL);
    else
	decl = ast_decl(VAR_DECL);

    DECL_SYM(decl) = sym;
    return decl;
}

static struct vector * decls(declfun_p *dcl)
{
    struct vector *v = vec_new();
    node_t *basety;
    int sclass;
    int level = SCOPE;
    int follow[] = {STATIC, INT, CONST, IF, '}', 0};
    
    basety = specifiers(&sclass);
    if (token->id == ID || token->id == '*' || token->id == '(') {
        struct token *id = NULL;
        node_t *ty = NULL;
        int params = 0;		// for functioness
        
        // declarator
        declarator(&ty, &id, &params);
        attach_type(&ty, basety);
	
        if (level == GLOBAL) {
            if (params) {
                if (firstfuncdef(ty)) {
                    vec_push(v, funcdef(id, ty, sclass));
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
		int kind;
		if (dcl == globaldecl)
		    kind = GLOBAL;
		else if (dcl == localdecl)
		    kind = LOCAL;
		else if (dcl == paramdecl)
		    kind = PARAM;
		else
		    cc_assert(0);
		node_t *decl = make_decl(id, ty, sclass, dcl);
		if (token->id == '=')
		    decl_initializer(decl, sclass, kind);
		ensure_decl(decl, sclass, kind);
                vec_push(v, decl);
	    }
            
            if (token->id != ',')
                break;
            
            expect(',');
            id = NULL;
            ty = NULL;
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
        
        decl = ast_decl(node_id);
        DECL_SYM(decl) = TYPE_TSYM(basety);
        vec_push(v, decl);
    } else {
        error("invalid token '%s' in declaration", token->name);
    }
    match(';', follow);
    
    return v;
}

static struct vector * filter(struct vector *v)
{
    for (int i = 0; i < vec_len(v); i++) {
	node_t *decl = vec_at(v, i);
	node_t *sym = DECL_SYM(decl);
	int sclass = SYM_SCLASS(sym);
	if (!isvardecl(decl))
	    continue;
        // local variables
	if (sclass == STATIC) {
	    SYM_LABEL(sym) = gen_static_label(SYM_NAME(sym));
	    vec_push(STATICVARS, decl);
	} else if (sclass != EXTERN) {
	    vec_push(LOCALVARS, decl);
	}
    }
    return v;
}

static int compare(const void *val1, const void *val2)
{
    node_t *decl1 = (node_t *)(*((void **)val1));
    node_t *decl2 = (node_t *)(*((void **)val2));
    int id1 = AST_ID(decl1);
    int id2 = AST_ID(decl2);
    if (id1 != id2)
	return id1 < id2;
    return 0;
}

static void ** sorts(void **exts)
{
    qsort(exts, LIST_LEN(exts), sizeof(node_t *), compare);
    return exts;
}

node_t * define_localvar(const char *name, node_t *ty, int sclass)
{
    struct token *id = new_token(&(struct token){.id = ID, .name = name, .kind = ID, .src = source});
    node_t *decl = make_decl(id, ty, sclass, localdecl);
    return decl;
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
    cc_assert(SCOPE >= LOCAL);
    return (node_t **)vtoa(filter(decls(localdecl)));
}

node_t * translation_unit(void)
{
    node_t *ret = ast_decl(TU_DECL);
    struct vector *v = vec_new();
    staticvars = vec_new();

    for (gettok(); token->id != EOI; ) {
        if (firstdecl(token)) {
            cc_assert(SCOPE == GLOBAL);
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

static void ensure_decl(node_t *decl, int sclass, int kind)
{
    if (kind == PARAM)
	return;

    node_t *sym = DECL_SYM(decl);
    node_t *ty = SYM_TYPE(sym);
    struct source src = AST_SRC(sym);
    if (isvardecl(decl)) {
	if (isincomplete(ty) && SYM_DEFINED(sym))
	    errorf(src, "variable has incomplete type '%s'", type2s(ty));
    }
}

static void ensure_func(node_t *ftype, struct source src)
{
    node_t *rty = rtype(ftype);
    if (isarray(rty))
        errorf(src, "function cannot return array type '%s'", type2s(rty));
    else if (isfunc(rty))
        errorf(src, "function cannot return function type '%s'", type2s(rty));
}

/**
 *  1. Array qualifiers may appear only when in a function parameter.
 *  2. Array qualifiers 'const', 'volatile', 'restrict', 'static' may appear
 *     within the _outermost_ brackets.
 *  3. 'static' is an optimization hint, asserting that the actual array
 *     argument will be non-null and will have the declared size and type upon
 *     entry to the function.
 *  4. The star modifier '*' or non-constant expression describe a variable
 *     length array. The '*' can only appear in array parameter declarations
 *     within function prototypes that are not part of a function definition.
 */
static void ensure_array(node_t *atype, struct source src, int level)
{
    node_t *rty = atype;
    do {
	node_t *assign = TYPE_A_ASSIGN(rty);
	if (assign) {
	    if (isint(AST_TYPE(assign))) {
		// try evaluate the length
	        node_t *ret = eval(assign, inttype);
		if (ret) {
		    cc_assert(isiliteral(ret));
		    TYPE_LEN(rty) = ILITERAL_VALUE(ret);
		}
	    } else {
		error("size of array has non-integer type '%s'", type2s(AST_TYPE(assign)));
	    }
	}
	
	if (TYPE_A_STAR(rty) && level != PARAM)
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

static node_t * paramdecl(struct token *t, node_t *ty, int sclass)
{
    node_t *sym = NULL;
    bool prototype = PARAM_STYLE(sclass);
    bool first = PARAM_FIRST(sclass);
    bool fvoid = PARAM_FVOID(sclass);
    const char *id = NULL;
    struct source src = source;
    sclass = PARAM_SCLASS(sclass);

    if (t) {
	id = t->name;
	src = t->src;
    }
    
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
    AST_SRC(sym) = src;
    SYM_SCLASS(sym) = sclass;
    
    return sym;
}

static node_t * localdecl(struct token *t, node_t *ty, int sclass)
{
    node_t *sym = NULL;
    const char *id = t->name;
    struct source src = t->src;
    
    cc_assert(id);
    cc_assert(SCOPE >= LOCAL);
    
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
        AST_SRC(sym) = src;
        SYM_DEFINED(sym) = true;
        SYM_SCLASS(sym) = sclass;
    }

    return sym;
}

static node_t * globaldecl(struct token *t, node_t *ty, int sclass)
{
    node_t *sym = NULL;
    const char *id = t->name;
    struct source src = t->src;
    
    cc_assert(id);
    cc_assert(SCOPE == GLOBAL);
    
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
        AST_SRC(sym) = src;
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

static node_t * funcdef(struct token *t, node_t *ftype, int sclass)
{
    node_t *decl = ast_decl(FUNC_DECL);
    const char *id = t->name;
    struct source src = t->src;
    
    cc_assert(SCOPE == PARAM);
    
    ensure_func(ftype, src);
    if (sclass && sclass != EXTERN && sclass != STATIC) {
        error("invalid storage class specifier '%s'", id2s(sclass));
        sclass = 0;
    }
    
    if (id) {
        node_t *sym = lookup(id, identifiers);
        if (!sym) {
            sym = install(id, &identifiers, GLOBAL);
            SYM_TYPE(sym) = ftype;
            AST_SRC(sym) = src;
            SYM_DEFINED(sym) = true;
            SYM_SCLASS(sym) = sclass;
            DECL_SYM(decl) = sym;
        } else if (eqtype(ftype, SYM_TYPE(sym)) && !SYM_DEFINED(sym)) {
            if (sclass == STATIC && SYM_SCLASS(sym) != STATIC) {
                errorf(src, "static declaaration of '%s' follows non-static declaration", id);
            } else {
                SYM_TYPE(sym) = ftype;
                AST_SRC(sym) = src;
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
            if (isanonymous(SYM_NAME(sym)) && !isvartype(SYM_TYPE(sym)))
                errorf(AST_SRC(sym), "parameter name omitted");
            if (isenum(SYM_TYPE(sym)) || isstruct(SYM_TYPE(sym)) || isunion(SYM_TYPE(sym))) {
                if (!SYM_DEFINED(TYPE_TSYM(SYM_TYPE(sym))))
                    errorf(AST_SRC(sym), "variable has incomplete type '%s'", type2s(SYM_TYPE(sym)));
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
            
            cc_assert(SYM_NAME(sym));
            if (!isvardecl(decl)) {
                warningf(AST_SRC(sym), "empty declaraion");
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
                    errorf(AST_SRC(sym), "parameter named '%s' is missing", SYM_NAME(sym));
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
	// check goto labels
	backfill_labels();
	// TODO: check control flow and return stmt
	DECL_BODY(decl) = stmt;
	DECL_VARS(decl) = (node_t **)vtoa(LOCALVARS);
        RESTORE_FUNCDEF_CONTEXT();
	exit_scope();
    }
    
    return decl;
}
