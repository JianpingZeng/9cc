#include "cc.h"

static void abstract_declarator(struct type **ty);
static void declarator(struct type **ty, const char **id, int *params);
static void param_declarator(struct type **ty, const char **id);
static struct type * ptr_decl();
static struct type * enum_decl();
static struct type * struct_decl();
static struct vector * decls(struct symbol * (*)(const char *id, struct type *ftype, int sclass,  struct source src));
static struct symbol * paramdecl(const char *id, struct type *ty, int sclass,  struct source src);
static struct symbol * globaldecl(const char *id, struct type *ty, int sclass, struct source src);
static struct symbol * localdecl(const char *id, struct type *ty, int sclass, struct source src);
static union node * funcdef(const char *id, struct type *ftype, int sclass,  struct source src);
static union node * initializer(struct type *ty);
static void fields(struct type *sty);
static void decl_initializer(union node *decl, struct symbol *sym, int sclass, int kind);

static struct type * specifiers(int *sclass)
{
    int cls, sign, size, type;
    int cons, vol, res, inl;
    struct type *basety;
    int ci;			// _Complex, _Imaginary
    struct type *tydefty = NULL;
    
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
                if (is_typedef_name(token->name)) {
                    tydefty = lookup_typedef_name(token->name);
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
                errorf(src, "invalid storage class specifier at '%s'", name);
            else if (p == &inl && !sclass)
                errorf(src, "invalid function specifier");
            else if (p == &cons || p == &res || p == &vol || p == &inl)
                warningf(src, "duplicate '%s' declaration specifier", name);
            else if (p == &ci)
                errorf(src, "duplicate _Complex/_Imaginary specifier at '%s'", name);
            else if (p == &sign)
                errorf(src, "duplicate signed/unsigned speficier at '%s'", name);
            else if (p == &type || p == &size)
                errorf(src, "duplicate type specifier at '%s'", name);
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
            error("%s %s %s is invalid", tname(size/2), tname(size/2), tname(type));
        else
            error("%s %s is invalid", tname(size), tname(type));
    } else if (sign && type != INT && type != CHAR) {
        error("'%s' cannot be signed or unsigned", tname(type));
    } else if (ci && type != DOUBLE && type != FLOAT) {
        error("'%s' cannot be %s", tname(type), tname(ci));
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

static void qualifiers(struct type *atype)
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
            warning("duplicate type qualifier '%s'", tname(*p));
        
        *p = t;
    }
    
    if (cons)
        atype->u.a.is_const = 1;
    if (vol)
        atype->u.a.is_volatile = 1;
    if (res)
        atype->u.a.is_restrict = 1;
}

static struct symbol ** parameters(struct type *ftype, int *params)
{
    struct symbol **ret = NULL;
    
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
        for (int i=0;;i++) {
            struct type *basety = NULL;
            int sclass;
            struct type *ty = NULL;
            const char *id = NULL;
            struct source src = source;
            
            basety = specifiers(&sclass);
            if (token->id == '*' || token->id == '(' || token->id == '[' || token->id == ID)
                param_declarator(&ty, &id);
            
            attach_type(&ty, basety);
            
            if (isinline(basety))
                error("'inline' can only appear on functions");
            if (isvoid(ty)) {
                if (i == 0 && token->id == ')') {
                    if (id)
                        error("argument may not have 'void' type");
                    else if (isqual(ty))
                        error("'void' as parameter must not have type qualifiers");
                } else {
                    error("'void' must be the first and only parameter if specified");
                }
            }
            
            vec_push(v, paramdecl(id, ty, sclass, src));
            if (token->id != ',')
                break;
            
            expect(',');
            if (token->id == ELLIPSIS) {
                vec_push(v, paramdecl(token->name, vartype, 0, source));
                expect(ELLIPSIS);
                break;
            }
        }
        
        ret = (struct symbol **)vtoa(v);
    } else if (token->id == ID) {
        // oldstyle
        OLDSTYLE(ftype) = 1;
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
        ret = (struct symbol **) vtoa(v);
    } else if (token->id == ')') {
        OLDSTYLE(ftype) = 1;
    } else {
        int follow[] = {')', IF, 0};
        if (token->id == ELLIPSIS)
            error("ISO C requires a named parameter before '...'");
        else
            error("expect parameter declarator at '%s'", token->name);
        
        match(')', follow);
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

static struct type * func_or_array(int *params)
{
    struct type *ty = NULL;
    int follow[] = {'[', ID, IF, 0};
    
    for (; token->id == '(' || token->id == '['; ) {
        if (token->id == '[') {
            struct type *atype = array_type();
            expect('[');
            if (token->id == STATIC) {
                expect(STATIC);
                atype->u.a.is_static = 1;
                if (token->kind == CONST)
                    qualifiers(atype);
                atype->u.a.assign = assign_expr();
            } else if (token->kind == CONST) {
                if (token->kind == CONST)
                    qualifiers(atype);
                if (token->id == STATIC) {
                    expect(STATIC);
                    atype->u.a.is_static = 1;
                    atype->u.a.assign = assign_expr();
                } else if (token->id == '*') {
                    if (lookahead()->id != ']') {
                        atype->u.a.assign = assign_expr();
                    } else {
                        expect('*');
                        atype->u.a.wildcard = 1;
                    }
                } else if (firstexpr(token)) {
                    atype->u.a.assign = assign_expr();
                }
            } else if (token->id == '*') {
                if (lookahead()->id != ']') {
                    atype->u.a.assign = assign_expr();
                } else {
                    expect('*');
                    atype->u.a.wildcard = 1;
                }
            } else if (firstexpr(token)) {
                atype->u.a.assign = assign_expr();
            }
            match(']', follow);
            attach_type(&ty, atype);
        } else {
            struct type *ftype = func_type();
            expect('(');
            PARAMS(ftype) = parameters(ftype, params);
            match(')', follow);
            attach_type(&ty, ftype);
        }
    }
    
    return ty;
}

static struct type * abstract_func_or_array()
{
    struct type *ty = NULL;
    
    for (; token->id == '(' || token->id == '['; ) {
        if (token->id == '[') {
            struct type *atype = array_type();
            expect('[');
            if (token->id == '*') {
                if (lookahead()->id != ']') {
                    atype->u.a.assign = assign_expr();
                } else {
                    expect('*');
                    atype->u.a.wildcard = 1;
                }
            } else if (firstexpr(token)) {
                atype->u.a.assign = assign_expr();
            }
            expect(']');
            attach_type(&ty, atype);
        } else {
            struct type *ftype = func_type();
            expect('(');
            PARAMS(ftype) = parameters(ftype, NULL);
            expect(')');
            attach_type(&ty, ftype);
        }
    }
    
    return ty;
}

static struct type * ptr_decl()
{
    struct type *ret = NULL;
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
                struct type *pty = ptr_type(NULL);
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

static void param_declarator(struct type **ty, const char **id)
{
    if (token->id == '*') {
        struct type *pty = ptr_decl();
        prepend_type(ty, pty);
    }
    
    if (token->id == '(') {
        if (firstdecl(lookahead())) {
            abstract_declarator(ty);
        } else {
            struct type *type1 = *ty;
            struct type *rtype = NULL;
            expect('(');
            param_declarator(&rtype, id);
            expect(')');
            if (token->id == '(' || token->id == '[') {
                struct type *faty;
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

static void abstract_declarator(struct type **ty)
{
    CCAssert(ty);
    
    if (token->id == '*' || token->id == '(' || token->id == '[') {
        if (token->id == '*') {
            struct type *pty = ptr_decl();
            prepend_type(ty, pty);
        }
        
        if (token->id == '(') {
            if (firstdecl(lookahead())) {
                struct type *faty = abstract_func_or_array();
                prepend_type(ty, faty);
            } else {
                expect('(');
                abstract_declarator(ty);
                expect(')');
            }
        } else if (token->id == '[') {
            struct type *faty = abstract_func_or_array();
            prepend_type(ty, faty);
        }
    } else {
        error("expect '(' or '[' at '%s'", token->name);
    }
}

static void declarator(struct type **ty, const char **id, int *params)
{
    CCAssert(ty && id);
    int follow[] = {',', '=', IF, 0};
    
    if (token->id == '*') {
        struct type *pty = ptr_decl();
        prepend_type(ty, pty);
    }
    
    if (token->id == ID) {
        *id = token->name;
        expect(ID);
        if (token->id == '[' || token->id == '(') {
            struct type *faty = func_or_array(params);
            prepend_type(ty, faty);
        }
    } else if (token->id == '(') {
        struct type *type1 = *ty;
        struct type *rtype = NULL;
        expect('(');
        declarator(&rtype, id, params);
        match(')', follow);
        if (token->id == '[' || token->id == '(') {
            struct type *faty = func_or_array(params);
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

static bool firstfuncdef(struct type *ty)
{
    bool prototype = token->id == '{';
    bool oldstyle = (istypename(token) || token->kind == STATIC) && OLDSTYLE(ty) && PARAMS(ty);
    
    return isfunc(ty) && (prototype || oldstyle);
}

static struct vector * decls(struct symbol * (*dcl)(const char *id, struct type *ftype, int sclass,  struct source src))
{
    struct vector *v = vec_new();
    struct type *basety;
    int sclass;
    struct source src = source;
    int level = SCOPE;
    int follow[] = {STATIC, INT, CONST, IF, '}', 0};
    
    basety = specifiers(&sclass);
    if (token->id == ID || token->id == '*' || token->id == '(') {
        const char *id = NULL;
        struct type *ty = NULL;
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
                union node *decl;
                struct symbol *sym = dcl(id, ty, sclass, src);
                if (sclass == TYPEDEF)
                    decl = ast_decl(TYPEDEF_DECL, SCOPE);
                else if (isfunc(ty))
		    decl = ast_decl(FUNC_DECL, SCOPE);
		else
                    decl = ast_decl(VAR_DECL, SCOPE);
                
                DECL_SYM(decl) = sym;
		AST_TYPE(decl) = sym->type;
		
                if (token->id == '=' && (dcl == globaldecl || dcl == localdecl))
		    decl_initializer(decl, sym, sclass, dcl == globaldecl ? GLOBAL : LOCAL);
                
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
        union node *decl;
        if (isstruct(basety))
            node_id = STRUCT_DECL;
        else if (isunion(basety))
            node_id = UNION_DECL;
        else
            node_id = ENUM_DECL;
        
        decl = ast_decl(node_id, SCOPE);
        DECL_SYM(decl) = TSYM(basety);
        vec_push(v, decl);
    } else {
        error("invalid token '%s' in declaration", token->name);
    }
    match(';', follow);
    
    return v;
}

static union node * initializer(struct type *ty)
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

static void elem_init(struct type *ty, bool designated, struct vector *v, int i);
#define FIRST_INIT(t) (t->id == '[' || t->id == '.' || t->id == '{' || firstexpr(t))

static void eat_initializer()
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

static void eat_initlist()
{
    do {
	eat_initializer();
	if (token->id != ',')
	    break;

	expect(',');
    } while (FIRST_INIT(token));
}

static void init_check(struct type *ty1, struct type *ty2)
{
    // TODO: 
}

static union node * find_elem(struct vector *v, int i)
{
    for (int j = vec_len(v); j <= i; j++)
	vec_push(v, ast_vinit());
    return vec_at(v, i);
}

static void aggregate_set(struct type *ty, struct vector *v, int i, union node *node)
{
    if (!node)
	return;
    
    union node *n = find_elem(v, i);
    if (AST_ID(n) != VINIT_EXPR)
	warning("initializer overrides prior initialization");
    
    if (AST_ID(node) == INITS_EXPR) {
	vec_set(v, i, node);
    } else {
	struct type *rty = NULL;
	if (isarray(ty)) {
	    rty = rtype(ty);
	} else {
	    if (FIELDS(ty))
		rty = FIELDS(ty)[0]->type;
	}

	if (rty) {
	    union node *n1 = ast_inits();
	    struct vector *v1 = vec_new();
	    vec_set(v, i, n1);

	    if (isarray(rty) || isstruct(rty) || isunion(rty)) {
		aggregate_set(rty, v1, 0, node);
	    } else {
		init_check(rty, AST_TYPE(node));
		vec_push(v1, node);
	    }
	
	    EXPR_INITS(n1) = (union node **)vtoa(v1);
	}
    }
}

static void scalar_set(struct type *ty, struct vector *v, int i, union node *node)
{
    if (!node)
	return;
    
    union node *n = find_elem(v, i);
    if (AST_ID(n) != VINIT_EXPR)
	warning("initializer overrides prior initialization");

    if (AST_ID(node) == INITS_EXPR) {	
	union node **inits;
    loop:
	    inits = EXPR_INITS(node);
	    if (inits) {
		node = inits[0];
		if (AST_ID(node) == INITS_EXPR)
		    goto loop;
		init_check(ty, AST_TYPE(node));
		vec_set(v, i, node);
	    }
    } else {
	init_check(ty, AST_TYPE(node));
	vec_set(v, i, node);
    }
}

static void struct_init(struct type *ty, bool brace, struct vector *v)
{
    bool designated = false;
    int len = array_len((void **)FIELDS(ty));

    for (int i = 0; ; i++) {
	struct type *fieldty = NULL;

	if (token->id == '.') {
	    const char *name = NULL;
	    expect('.');
	    if (token->id == ID)
		name = token->name;
	    expect(ID);
	    struct field *field = find_field(ty, name);
	    if (field) {
		i = indexof_field(ty, field);
		fieldty = field->type;
	    } else {
		i--;
		if (name)
		    error("'%s' has no field named '%s'", type2s(ty), name);
	    }
	    designated = true;
	}

	if (i >= len)
	    break;

	if (!designated)
	    fieldty = FIELDS(ty)[i]->type;
	elem_init(fieldty, designated, v, i);
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

static void array_init(struct type *ty, bool brace, struct vector *v)
{
    bool designated = false;
    int c = 0;
    
    for (int i = 0; ; i++) {
	struct type *rty = NULL;
	
	if (token->id == '[') {
	    expect('[');
	    i = intexpr();
	    expect(']');
	    designated = true;
	}

	if (ty->size > 0 && i >= ty->size && !designated)
	    break;
	
	c = MAX(c, i);
	if (ty->size > 0 && i >= ty->size)
	    error("array designator index [%d] exceeds array bounds (%d)", i, ty->size);
	else
	    rty = rtype(ty);
	elem_init(rty, designated, v, i);
	designated = false;

	struct token *ahead = lookahead();
	if (token->id == '}' || (token->id == ',' && ahead->id == '}'))
	    break;
	if ((ahead->id == '.' || ahead->id == '[') && !brace)
	    break;
	if (brace || (ty->size > 0 && i < ty->size - 1) || ty->size == 0)
	    expect(',');
    }

    if (ty->size == 0)
	ty->size = c + 1;
}

static void scalar_init(struct type *ty, struct vector *v)
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

static void elem_init(struct type *ty, bool designated, struct vector *v, int i)
{
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
	    unsigned errs = errors;
	    eat_initializer();
	    // inhibit redundant errors
	    if (errs == errors)
		error("%s designator cannot initialize non-%s type '%s'", unqual(ty)->name, unqual(ty)->name, type2s(ty));
	} else {
	    union node *n = find_elem(v, i);
	    struct vector *v1 = vec_new();
	    if (AST_ID(n) == INITS_EXPR) {
		vec_add_array(v1, (void **)EXPR_INITS(n));
	    } else {
		n = ast_inits();
		vec_set(v, i, n);
	    }
	    if (isarray(ty))
		array_init(ty, false, v1);
	    else
		struct_init(ty, false, v1);
	    EXPR_INITS(n) = (union node **)vtoa(v1);
	}
    } else {
	if (designated)
	    expect('=');
	scalar_set(ty, v, i, initializer(ty));
    }
}

union node * initializer_list(struct type *ty)
{
    int follow[] = {',', IF, '[', ID, '.', DEREF, 0};
    union node *ret = ast_inits();
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
		warning("excess elements in %s initializer at '%s'", unqual(ty)->name, token->name);
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
    EXPR_INITS(ret) = (union node **)vtoa(v);
    AST_TYPE(ret) = ty;
    return ret;
}

int firstdecl(struct token *t)
{
    return t->kind == STATIC || t->kind == INT || t->kind == CONST || (t->id == ID && is_typedef_name(t->name));
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
    (t->id == ID && is_typedef_name(t->name));
}

struct type * typename()
{
    struct type *basety;
    struct type *ty = NULL;
    
    basety = specifiers(NULL);
    if (token->id == '*' || token->id == '(' || token->id == '[')
        abstract_declarator(&ty);
    
    attach_type(&ty, basety);
    
    return ty;
}

union node ** declaration()
{
    CCAssert(SCOPE >= LOCAL);
    return (union node **)vtoa(decls(localdecl));
}

union node * translation_unit()
{
    union node *ret = ast_decl(TU_DECL, GLOBAL);
    struct vector *v = vec_new();
    
    for (; token->id != EOI; ) {
        if (firstdecl(token)) {
            CCAssert(SCOPE == GLOBAL);
            vec_add(v, decls(globaldecl));
        } else {
            if (token->id != ';')
                error("invalid token '%s'", token->name);
            gettok();
        }
    }
    
    DECL_EXTS(ret) = (union node **)vtoa(v);
    return ret;
}

static void ensure_func(struct type *ftype, struct source src)
{
    if (isarray(rtype(ftype)))
        errorf(src, "function cannot return array type");
    else if (isfunc(rtype(ftype)))
        errorf(src, "function cannot return function type");
}

static void ensure_array(struct type *atype, struct source src, int level)
{
    if (isfunc(rtype(atype)))
        errorf(src, "array of function is invalid");
    else if (isvoid(rtype(atype)))
        errorf(src, "array has incomplete element type '%s'", type2s(rtype(atype)));
}

static void ensure_nonvoid(struct type *ty, struct source src, int level)
{
    if (isvoid(ty))
        errorf(src, "%s may not have 'void' type", level == PARAM ? "argument" : "variable");
}

static struct type * enum_decl()
{
    struct symbol *sym = NULL;
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
            struct symbol *s = lookup(token->name, identifiers);
            if (s && currentscope(s))
                redefinition_error(source, s);
            
            s = install(token->name, &identifiers, SCOPE);
            s->type = sym->type;
            s->src = source;
            s->sclass = ENUM;
            expect(ID);
            if (token->id == '=') {
                expect('=');
                val = intexpr();
            }
            s->value.i = val++;
            vec_push(v, s);
            if (token->id != ',')
                break;
            expect(',');
        }
        match('}', follow);
        IDS(sym->type) = (struct symbol **)vtoa(v);
        sym->defined = true;
    } else if (id) {
        sym = lookup(id, tags);
        if (sym) {
            if (currentscope(sym) && !isenum(sym->type))
                errorf(src, "use of '%s' with tag type that does not match previous declaration '%s %s' at %s:%u",
                       tname(ENUM), id, type2s(sym->type),  sym->src.file, sym->src.line);
        } else {
            sym = tag_type(ENUM, id, src);
        }
    } else {
        error("expected identifier or '{'");
        sym = tag_type(ENUM, NULL, src);
    }
    
    return sym->type;
}

static struct type * struct_decl()
{
    int t = token->id;
    const char *id = NULL;
    struct symbol *sym = NULL;
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
        sym->defined = true;
        fields(sym->type);
        match('}', follow);
    } else if (id) {
        sym = lookup(id, tags);
        if (sym) {
            if (currentscope(sym) && op(sym->type) != t)
                errorf(src, "use of '%s' with tag type that does not match previous declaration '%s %s' at %s:%u",
                       tname(t), id, type2s(sym->type), sym->src.file, sym->src.line);
        } else {
            sym = tag_type(t, id, src);
        }
    } else {
        error("expected identifier or '{'");
        sym = tag_type(t, NULL, src);
    }

    return sym->type;
}

// TODO: not finished yet
static void fields(struct type *sty)
{
    int follow[] = {INT, CONST, '}', IF, 0};
    
    struct vector *v = vec_new();
    while (istypename(token)) {
        struct type *basety = specifiers(NULL);
        
        for (;;) {
            struct field *field = new_field(NULL);
            int hasbit = 0;
            if (token->id == ':') {
                expect(':');
                field->bitsize = intexpr();
                field->type = basety;
                hasbit = 1;
            } else {
                struct type *ty = NULL;
                const char *id = NULL;
                declarator(&ty, &id, NULL);
                attach_type(&ty, basety);
                if (token->id == ':') {
                    expect(':');
                    field->bitsize = intexpr();
                    hasbit = 1;
                }
                field->type = ty;
                if (id) {
                    for (int i=0; i < vec_len(v); i++) {
                        struct field *f = vec_at(v, i);
                        if (f->name && !strcmp(f->name, id)) {
                            error("redefinition of '%s'", id);
                            break;
                        }
                    }
                    field->name = id;
                }
            }
            
            if (hasbit) {
                if (!isint(field->type)) {
                    if (field->name)
                        error("bit-field '%s' has non-integral type '%s'", field->name, type2s(field->type));
                    else
                        error("anonymous bit-field has non-integral type '%s'", type2s(field->type));
                }
                if (field->bitsize < 0) {
                    if (field->name)
                        error("bit-field '%s' has negative width '%d'", field->name, field->bitsize);
                    else
                        error("anonymous bit-field has negative width '%d'", field->bitsize);
                }
                if (field->bitsize == 0 && field->name)
                    error("named bit-field '%s' has zero width", field->name);
                if (field->bitsize > BITS(field->type)) {
                    if (field->name)
                        error("size of bit-field '%s' (%d bits) exceeds size of its type (%d bits)", field->name, field->bitsize, BITS(field->type));
                    else
                        error("anonymous bit-field (%d bits) exceeds size of its type (%d bits)", field->bitsize, BITS(field->type));
                }
            }
            
            vec_push(v, field);
            
            if (token->id != ',')
                break;
            expect(',');
        }
        
        match(';', follow);
    }
    FIELDS(sty) = (struct field **)vtoa(v);
}

static struct symbol * paramdecl(const char *id, struct type *ty, int sclass,  struct source src)
{
    struct symbol *sym = NULL;
    if (sclass && sclass != REGISTER) {
        error("invalid storage class specifier '%s' in function declarator", tname(sclass));
        sclass = 0;
    }
    
    if (isfunc(ty)) {
        ensure_func(ty, src);
        ty = ptr_type(ty);
    } else if (isarray(ty)) {
        ensure_array(ty, src, PARAM);
        ty = ptr_type(rtype(ty));
    } else if (isenum(ty) || isstruct(ty) || isunion(ty)) {
        if (!TSYM(ty)->defined)
            warningf(src, "declaration of '%s' will not be visible outside of this function", type2s(ty));
    }
    
    if (id) {
        sym = lookup(id, identifiers);
        if (sym && sym->scope == SCOPE)
            redefinition_error(source, sym);
        sym = install(id, &identifiers, SCOPE);
    } else {
        sym = anonymous(&identifiers, SCOPE);
    }
    
    sym->type = ty;
    sym->src = src;
    sym->sclass = sclass;

    if (token->id == '=') {
	error("C does not support default arguments");
	expect('=');
	initializer(NULL);
    }
    
    return sym;
}

static struct symbol * localdecl(const char *id, struct type *ty, int sclass, struct source src)
{
    struct symbol *sym = NULL;
    
    CCAssert(id);
    CCAssert(SCOPE >= LOCAL);
    
    if (isfunc(ty)){
        if (PARAMS(ty) && OLDSTYLE(ty))
            error("a parameter list without types is only allowed in a function definition");
    } else if (isarray(ty)) {
        // TODO: convert to poniter
    }
    
    sym = lookup(id, identifiers);
    if (sym && currentscope(sym)) {
        redefinition_error(src, sym);
    } else {
        sym = install(id, &identifiers, SCOPE);
        sym->type = ty;
        sym->src = src;
        sym->defined = true;
        sym->sclass = sclass;
    }

    return sym;
}

static struct symbol * globaldecl(const char *id, struct type *ty, int sclass, struct source src)
{
    struct symbol *sym = NULL;
    
    CCAssert(id);
    CCAssert(SCOPE == GLOBAL);
    
    if (sclass == AUTO || sclass == REGISTER) {
        errorf(src, "illegal storage class on file-scoped variable");
        sclass = 0;
    }
    
    if (isfunc(ty)) {
        if (PARAMS(ty) && OLDSTYLE(ty))
            error("a parameter list without types is only allowed in a function definition");
    } else if (isarray(ty)) {
        // TODO: convert to poniter
    }
    
    sym = lookup(id, identifiers);
    if (!sym || sym->scope != SCOPE) {
        sym = install(id, &identifiers, SCOPE);
        sym->type = ty;
        sym->src = src;
        sym->sclass = sclass;
    } else if (sclass != TYPEDEF && eqtype(ty, sym->type)) {
        if (sclass == STATIC && sym->sclass != STATIC)
            errorf(src, "static declaration of '%s' follows non-static declaration", id);
        else if (sym->sclass == STATIC && sclass != STATIC)
            errorf(src, "non-static declaration of '%s' follows static declaration", id);
    } else {
        conflicting_types_error(src, sym);
    }
    
    return sym;
}

static union node * funcdef(const char *id, struct type *ftype, int sclass,  struct source src)
{
    union node *decl = ast_decl(FUNC_DECL, SCOPE);
    
    CCAssert(SCOPE == PARAM);
    
    if (sclass && sclass != EXTERN && sclass != STATIC) {
        error("invalid storage class specifier '%s'", tname(sclass));
        sclass = 0;
    }
    
    if (id) {
        struct symbol *sym = lookup(id, identifiers);
        if (!sym) {
            sym = install(id, &identifiers, GLOBAL);
            sym->type = ftype;
            sym->src = src;
            sym->defined = true;
            sym->sclass = sclass;
            DECL_SYM(decl) = sym;
        } else if (eqtype(ftype, sym->type) && !sym->defined) {
            if (sclass == STATIC && sym->sclass != STATIC) {
                errorf(src, "static declaaration of '%s' follows non-static declaration", id);
            } else {
                sym->type = ftype;
                sym->src = src;
                sym->defined = true;
                DECL_SYM(decl) = sym;
            }
        } else {
            redefinition_error(src, sym);
        }
    }
    
    if (PARAMS(ftype)) {
        for (int i=0; PARAMS(ftype)[i]; i++) {
            struct symbol *sym = PARAMS(ftype)[i];
            sym->defined = true;
            // params id is required in prototype
            if (issymnamed(sym))
                errorf(sym->src, "parameter name omitted");
            if (isenum(sym->type) || isstruct(sym->type) || isunion(sym->type)) {
                if (!TSYM(sym->type)->defined)
                    errorf(sym->src, "variable has incomplete type '%s'", type2s(sym->type));
                else if (sym->type->tag)
                    warningf(sym->src, "declaration of '%s' will not be visible outside of this function", type2s(sym->type));
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
            union node *decl = (union node *)vec_at(v, i);
            struct symbol *sym = DECL_SYM(decl);
            
            CCAssert(sym->name);
            if (AST_ID(decl) != VAR_DECL) {
                warningf(sym->src, "empty declaraion");
            } else if (PARAMS(ftype)) {
                struct symbol *p = NULL;
                for (int i=0; PARAMS(ftype)[i]; i++) {
                    struct symbol *s = PARAMS(ftype)[i];
                    if (s->name && !strcmp(s->name, sym->name)) {
                        p = s;
                        break;
                    }
                }
                if (p)
                    p->type = sym->type;
                else
                    errorf(sym->src, "parameter named '%s' is missing", sym->name);
            }
	    ensure_nonvoid(AST_TYPE(decl), sym->src, PARAM);
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
        union node *stmt = compound_stmt();
        exit_scope();
        AST_KID(decl, 0) = stmt;
    }
    
    return decl;
}

static void decl_initializer(union node *decl, struct symbol *sym, int sclass, int kind)
{
    struct type *ty = sym->type;
    struct source src = sym->src;
    union node *init = NULL;

    expect('=');
    init = initializer(ty);
    if (init) {
	if (sclass == EXTERN)
	    warningf(src, "'extern' variable has an initializer");
	else if (sclass == TYPEDEF)
	    errorf(src, "illegal initializer (only variable can be initialized)");
    }

    if (isenum(ty) || isstruct(ty) || isunion(ty)) {
	if (kind == LOCAL) {
	    if (!TSYM(ty)->defined)
		error("variable has incomplete type '%s'", type2s(ty));
	} else if (kind == GLOBAL) {
	    if (init && !TSYM(ty)->defined && sclass != TYPEDEF)
		error("variable has incomplete type '%s'", type2s(ty));
	} 
    }
                    
    if (SCOPE == GLOBAL) {
	if (sym->defined && init)
	    redefinition_error(src, sym);
	sym->defined = init ? true : false;
    }
                    
    DECL_BODY(decl) = init;
}
