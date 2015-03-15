#include "cc.h"

static void abstract_declarator(struct type **ty);
static void declarator(struct type **ty, const char **id, int *params);
static void param_declarator(struct type **ty, const char **id);
static struct type * pointer_decl();
static struct type * enum_decl();
static struct type * struct_decl();
typedef struct symbol * (*DeclFunc)(const char *id, struct type *ftype, int sclass,  struct source src);
static struct node ** decls(DeclFunc declfunc);
static struct symbol * paramdecl(const char *id, struct type *ty, int sclass,  struct source src);
static struct symbol * globaldecl(const char *id, struct type *ty, int sclass, struct source src);
static struct symbol * localdecl(const char *id, struct type *ty, int sclass, struct source src);

static int kinds[] = {
#define _a(a, b, c, d)  d,
#define _x(a, b, c, d)  c,
#define _t(a, b, c)     c,
#include "token.h"
};

int kind(int t)
{
    if (t < 0)
	return 0;
    else if (t < 128)
	return kinds[t];
    else if (t >= ID && t < TOKEND)
        return kinds[128+t-ID];
    else
        return 0;
}

static void conflicting_types_error(struct source src, struct symbol *sym)
{
    errorf(src, "conflicting types for '%s', previous at %s line %u",
	  sym->name, sym->src.file, sym->src.line);
}

static void validate_func_or_array(struct type *ty)
{
    if (isfunction(ty)) {
	if (isfunction(ty->type))
	    error("function cannot return function type");
	else if (isarray(ty->type))
	    error("function cannot return array type");
    } else if (isarray(ty) && isfunction(ty->type)) {
	error("array of function is invalid");
    }
}

static struct type * specifiers(int *sclass)
{
    int cls, sign, size, type;
    int cons, vol, res, inl;
    struct type *basety;
    int ci;			// _Complex, _Imaginary
    struct type *tydefty = NULL;
        
    basety = NULL;
    sign = size = type = 0;
    cons = vol = res = inl = 0;
    ci = 0;
    if (sclass)
	cls = 0;
    else
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
		errorf(src, "invalid storage class specifier at '%s'", tname(t));
	    else if (p == &inl && !sclass)
		errorf(src, "invalid function specifier");
	    else if (p == &cons || p == &res || p == &vol || p == &inl)
		warningf(src, "duplicate '%s' declaration specifier", tname(t));
	    else if (p == &ci)
		errorf(src, "duplicate _Complex/_Imaginary specifier at '%s'", tname(t));
	    else if (p == &sign)
		errorf(src, "duplicate signed/unsigned speficier at '%s'", tname(t));
	    else if (p == &type || p == &size)
		errorf(src, "duplicate type specifier at '%s'", name);
	    else
		assert(0);
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

static void atype_qualifiers(struct type *atype)
{
    int cons, vol, res;
    int *p;
    cons = vol = res = 0;
    while (kind(token->id) & TYPE_QUAL) {
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
	    assert(0);
	}

	if (*p != 0)
	    warning("duplicate type qualifier '%s'", tname(*p));
			
	*p = t;
    }

    if (cons)
	atype->a.qual_const = 1;
    if (vol)
	atype->a.qual_volatile = 1;
    if (res)
	atype->a.qual_restrict = 1;
}

static struct symbol ** func_proto(struct type *ftype)
{    
    struct symbol **ret = NULL;

    enter_scope();
    
    if ((token->id != ID && kind(token->id) & FIRST_DECL) ||
	(token->id == ID && is_typedef_name(token->name))) {
	// prototype
	struct vector *v = new_vector();
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
		error("invalid function specifier 'inline' in parameter list");
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
	    
	    vector_push(v, paramdecl(id, ty, sclass, src));
	    if (token->id != ',')
		break;

	    match(',');
	    if (token->id == ELLIPSIS) {
		vector_push(v, paramdecl(token->name, vartype, 0, source));
		match(ELLIPSIS);
		break;
	    }
	}

	ret = (struct symbol **)vector_to_array(v);
    } else if (token->id == ID) {
	// oldstyle
	ftype->f.oldstyle = 1;
	struct vector *v = new_vector();
	for (;;) {
	    if (token->id == ID)
		vector_push(v, paramdecl(token->name, inttype, 0, source));
	    match(ID);
	    if (token->id != ',')
		break;
	    match(',');
	}

	if (SCOPE > PARAM)
	    error("a parameter list without types is only allowed in a function definition");
	ret = (struct symbol **) vector_to_array(v);
    } else if (token->id == ')') {
	ftype->f.oldstyle = 1;
    } else {
	error("invalid token '%s' in parameter list", token->name);
	gettok();
    }

    if (SCOPE > PARAM)
	exit_scope();

    return ret;
}

static struct type * func_or_array(int *params)
{    
    struct type *ty = NULL;
    
    for (; token->id == '(' || token->id == '['; ) {
        if (token->id == '[') {
            struct type *atype = array_type();
            match('[');
	    if (token->id == STATIC) {
		match(STATIC);
		atype->a.sclass_static = 1;
		if (kind(token->id) & TYPE_QUAL)
		    atype_qualifiers(atype);
		atype->a.assign = assign_expression();
	    } else if (kind(token->id) & TYPE_QUAL) {
	        if (kind(token->id) & TYPE_QUAL)
		    atype_qualifiers(atype);
		if (token->id == STATIC) {
		    match(STATIC);
		    atype->a.sclass_static = 1;
		    atype->a.assign = assign_expression();
		} else if (token->id == '*') {
		    if (lookahead()->id != ']') {
			atype->a.assign = assign_expression();
		    } else {
			match('*');
			atype->a.wildcard = 1;
		    }
		} else if (kind(token->id) & FIRST_ASSIGN_EXPR) {
		    atype->a.assign = assign_expression();
		}
	    } else if (token->id == '*') {
		if (lookahead()->id != ']') {
		    atype->a.assign = assign_expression();
		} else {
		    match('*');
		    atype->a.wildcard = 1;
		}
	    } else if (kind(token->id) & FIRST_ASSIGN_EXPR) {
		atype->a.assign = assign_expression();
	    } 
	    skipto(']');
            attach_type(&ty, atype);
        } else {
	    struct type *ftype = function_type();
            match('(');
            ftype->f.proto = func_proto(ftype);
	    skipto(')');
            attach_type(&ty, ftype);
	    if (params)
		*params = 1;
        }
    }

    // TODO:
    validate_func_or_array(ty);

    return ty;
}

static struct type * abstract_func_or_array()
{
    struct type *ty = NULL;

    for (; token->id == '(' || token->id == '['; ) {
	if (token->id == '[') {
	    struct type *atype = array_type();
	    match('[');
	    if (token->id == '*') {
		if (lookahead()->id != ']') {
		    atype->a.assign = assign_expression();
		} else {
		    match('*');
		    atype->a.wildcard = 1;
		}
	    } else if (kind(token->id) & FIRST_ASSIGN_EXPR) {
		atype->a.assign = assign_expression();
	    }
	    skipto(']');
	    attach_type(&ty, atype);
	} else {
	    struct type *ftype = function_type();
	    match('(');
	    ftype->f.proto = func_proto(ftype);
	    skipto(')');
	    attach_type(&ty, ftype);
	}
    }

    // TODO
    validate_func_or_array(ty);

    return ty;
}

static struct type * pointer_decl()
{
    struct type *ret = NULL;
    int con, vol, res, type;
    
    assert(token->id == '*');
    
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
                struct type *pty = pointer_type(NULL);
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

static void abstract_declarator(struct type **ty)
{
    assert(ty);

    if (token->id == '*' || token->id == '(' || token->id == '[') {
	if (token->id == '*') {
	    struct type *pty = pointer_decl();
	    prepend_type(ty, pty);
	}

	if (token->id == '(') {
	    if (kind(lookahead()->id) & FIRST_DECL) {
		struct type *faty = abstract_func_or_array();
		prepend_type(ty, faty);
	    } else {
		match('(');
		abstract_declarator(ty);
		match(')');
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
    assert(ty && id);
    
    if (token->id == '*') {
	struct type *pty = pointer_decl();
	prepend_type(ty, pty);
    }

    if (token->id == ID) {
	*id = token->name;
	match(ID);
	if (token->id == '[' || token->id == '(') {
	    struct type *faty = func_or_array(params);
	    prepend_type(ty, faty);
	}
    } else if (token->id == '(') {
	struct type *type1 = *ty;
	struct type *rtype = NULL;
	match('(');
	declarator(&rtype, id, params);
	skipto(')');
	if (token->id == '[' || token->id == '(') {
	    struct type *faty = func_or_array(params);
	    attach_type(&faty, type1);
	    attach_type(&rtype, faty);
	}
	*ty = rtype;
    } else {
	error("expect identifier or '(' before '%s'", token->name);
    }
}

static void param_declarator(struct type **ty, const char **id)
{    
    if (token->id == '*') {
	struct type *pty = pointer_decl();
	prepend_type(ty, pty);
    }

    if (token->id == '(') {
	if (kind(lookahead()->id) & FIRST_DECL) {
	    abstract_declarator(ty);
	} else {
	    struct type *type1 = *ty;
	    struct type *rtype = NULL;
	    match('(');
	    param_declarator(&rtype, id);
	    match(')');
	    if (token->id == '(' || token->id == '[') {
		struct type *faty;
		assert(id);
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

static struct type * enum_decl()
{
    struct type *ret = NULL;
    const char *id = NULL;
    struct source src = source;

    match(ENUM);
    if (token->id == ID) {
	id = token->name;
	match(ID);
    }
    if (token->id == '{') {
	struct type *ety;
	long long val = 0;
	struct vector *v = new_vector();
	match('{');
	ety = record_type(ENUM, id, src);
	if (token->id != ID)
	    error("expect identifier");
	while (token->id == ID) {
	    struct symbol *sym = locate_symbol(token->name, identifiers, SCOPE);
	    if (sym)
		redefinition_error(source, sym);
	    
	    sym = install_symbol(token->name, &identifiers, SCOPE);
	    sym->type = ety;
	    sym->src = source;
	    match(ID);
	    if (token->id == '=') {
		match('=');
	        // TODO
		val = intexpr();
	    }
	    sym->value.i = val++;
	    vector_push(v, sym);
	    if (token->id != ',')
		break;
	    match(',');
	}
	skipto('}');	
	ety->s.ids = (struct symbol **)vector_to_array(v);	
	ety->s.symbol->defined = 1;
	ret = ety;
    } else if (id) {
	struct symbol *sym = lookup_symbol(id, records);
	if (sym)
	    ret = sym->type;
	else
	    ret = record_type(ENUM, id, src);
    } else {
	error("expected identifier or '{'");
    }

    return ret;
}

static struct type * struct_decl()
{
    int t = token->id;
    const char *id = NULL;
    struct type *ret = NULL;
    struct source src = source;
    
    match(t);
    if (token->id == ID) {
    	id = token->name;
    	match(ID);
    }
    if (token->id == '{') {
    	match('{');
    	enter_scope();
    	do {
    	    struct type *basety = specifiers(NULL);
    	    if (token->id == ':') {
    		match(':');
    		constant_expression();
    	    } else {
    		struct type *ty = NULL;
    		const char *name = NULL;
    		declarator(&ty, &name, NULL);
    		attach_type(&ty, basety);
    	    }
    	    match(';');
    	    if (token->id == '}')
    		break;
    	} while (token->id != EOI);
    	match('}');
    	exit_scope();

    	if (id) {
    	    struct symbol *sym = locate_symbol(id, records, SCOPE);
    	    if (!sym) {
    		ret = record_type(t, id);
    		sym = install_symbol(id, &records, SCOPE);
    		sym->src = src;
    		sym->type = ret;
    	    } else {
    		redefinition_error(source, sym);
    	    }
    	} else {
    	    ret = record_type(t, id);
    	}
    } else if (id) {
    	struct symbol *sym = lookup_symbol(id, records);
    	if (sym)
    	    ret = sym->type;
    	else
    	    ret = record_type(t, id, src);
    } else {
        error("expected identifier or '{'");
    }
    
    return ret;
}

static struct node * initializer()
{
    if (token->id == '{') {
	// initializer list
	return NODE(initializer_list());
    } else if (kind(token->id) & FIRST_ASSIGN_EXPR) {
	// assign expr
	return NODE(assign_expression());
    } else {
	error("expect '{' or assignment expression");
	return NULL;
    }
}

static void update_params(void *elem, void *context)
{
    struct node *decl = (struct node *)elem;
    struct type *ftype = (struct type *)context;
    struct symbol *sym = decl->symbol;

    assert(sym->name);
    if (decl->id != VAR_DECL) {
	warningf(sym->src, "empty declaraion");
    } else if (ftype->f.proto) {
	struct symbol *p = NULL;
	for (int i=0; ftype->f.proto[i]; i++) {
	    struct symbol *s = ftype->f.proto[i];
	    if (!strcmp(s->name, sym->name)) {
		p = s;
		break;
	    }
	}
	if (p)
	    p->type = sym->type;
	else
	    errorf(sym->src, "parameter named '%s' is missing", sym->name);
    }
}

static struct symbol * paramdecl(const char *id, struct type *ty, int sclass,  struct source src)
{
    struct symbol *sym = NULL;
    if (sclass && sclass != REGISTER) {
	error("invalid storage class specifier '%s' in parameter list", tname(sclass));
	sclass = 0;
    }

    if (isfunction(ty)) {
	ty = pointer_type(ty);
    } else if (isarray(ty)) {
	// TODO: convert to poniter
    } else if (isenum(ty) || isstruct(ty) || isunion(ty)) {
	if (!ty->s.symbol->defined)
	    warningf(src, "declaration '%s %s' in parameter list", tname(ty->op), ty->name);
    }

    if (id) {
	sym = locate_symbol(id, identifiers, SCOPE);
	if (sym)
	    redefinition_error(source, sym);
	sym = install_symbol(id, &identifiers, SCOPE);
	sym->type = ty;
	sym->src = src;
	sym->sclass = sclass;
    } else {
	sym = anonymous_symbol(&identifiers, SCOPE);
	sym->type = ty;
	sym->src = src;
	sym->sclass = sclass;
    }
    return sym;
}

static struct symbol * localdecl(const char *id, struct type *ty, int sclass, struct source src)
{
    struct symbol *sym = NULL;
    struct node *init_node = NULL;

    assert(id);
    assert(SCOPE >= LOCAL);
    
    if (token->id == '=') {
	// initializer
	match('=');
	init_node = initializer();
    }

    if (init_node) {
	if (sclass == EXTERN)
	    errorf(src, "'extern' variable cannot have an initializer");
	else if (sclass == TYPEDEF)
	    errorf(src, "illegal initializer (only variable can be initialized)");
    }
    
    if (isfunction(ty)){
	if (ty->f.proto && ty->f.oldstyle)
	    error("a parameter list without types is only allowed in a function definition");
    } else if (isarray(ty)) {
	// TODO: convert to poniter
    } else if (isenum(ty) || isstruct(ty) || isunion(ty)) {
	if (!ty->s.symbol->defined)
	    error("incomplete type '%s %s'", tname(ty->op), ty->name);
    }
    validate_func_or_array(ty);

    if (SCOPE == LOCAL)
	sym = find_symbol(id, identifiers, PARAM);
    else
	sym = locate_symbol(id, identifiers, SCOPE);

    if (!sym) {
	sym = install_symbol(id, &identifiers, SCOPE);
	sym->type = ty;
	sym->src = src;
	sym->defined = 1;
	sym->sclass = sclass;
    } else {
	redefinition_error(src, sym);
    }

    return sym;
}

static struct symbol * globaldecl(const char *id, struct type *ty, int sclass, struct source src)
{
    struct symbol *sym = NULL;
    struct node *init_node = NULL;

    assert(id);
    assert(SCOPE == GLOBAL);
    
    if (token->id == '=') {
	// initializer
	match('=');
	init_node = initializer();
    }

    if (sclass == AUTO || sclass == REGISTER) {
	errorf(src, "illegal storage class on file-scoped variable");
	sclass = 0;
    }

    if (init_node) {
	if (sclass == EXTERN)
	    warningf(src, "'extern' variable has an initializer");
	else if (sclass == TYPEDEF)
	    errorf(src, "illegal initializer (only variable can be initialized)");
    }
    
    if (isfunction(ty)) {
	if (ty->f.proto && ty->f.oldstyle)
	    error("a parameter list without types is only allowed in a function definition");
    } else if (isarray(ty)) {
	// TODO: convert to poniter
    } else if (isenum(ty) || isstruct(ty) || isunion(ty)) {
	if (!ty->s.symbol->defined)
	    error("incomplete type '%s %s'", tname(ty->op), ty->name);
    }
    validate_func_or_array(ty);

    sym = locate_symbol(id, identifiers, SCOPE);
    if (!sym) {
	sym = install_symbol(id, &identifiers, SCOPE);
	sym->type = ty;
	sym->src = src;
	sym->defined = init_node ? 1 : 0;
	sym->sclass = sclass;
    } else if (sclass != TYPEDEF && eqtype(ty, sym->type)) {
	if (sym->defined && init_node)
	    redefinition_error(src, sym);
	else if (sclass == STATIC && sym->sclass != STATIC)
	    errorf(src, "static declaration of '%s' follows non-static declaration", id);
	else if (sym->sclass == STATIC && sclass != STATIC)
	    errorf(src, "non-static declaration of '%s' follows static declaration", id);
    } else {
	conflicting_types_error(src, sym);
    }

    return sym;
}

static struct decl * funcdef(const char *id, struct type *ftype, int sclass,  struct source src)
{
    struct decl *decl = decl_node(FUNC_DECL, SCOPE);

    assert(SCOPE == PARAM);

    validate_func_or_array(ftype);
    if (sclass && sclass != EXTERN && sclass != STATIC) {
	error("invalid storage class specifier '%s'", tname(sclass));
	sclass = 0;
    }

    if (id) {
	struct symbol *sym = locate_symbol(id, identifiers, GLOBAL);
	if (!sym) {
	    sym = install_symbol(id, &identifiers, GLOBAL);
	    sym->type = ftype;
	    sym->src = src;
	    sym->defined = 1;
	    sym->sclass = sclass;
	    decl->node.symbol = sym;
	} else if (eqtype(ftype, sym->type) && !sym->defined) {
	    if (sclass == STATIC && sym->sclass != STATIC) {
		errorf(src, "static declaaration of '%s' follows non-static declaration", id);
	    } else {
		sym->type = ftype;
		sym->src = src;
		sym->defined = 1;
		decl->node.symbol = sym;
	    }
	} else {
	    redefinition_error(src, sym);
	}
    } else {
	error("missing identifier in function definition");
    }

    if (ftype->f.proto) {
	// params id is required in prototype
	for (int i=0; ftype->f.proto[i]; i++) {
	    struct symbol *sym = ftype->f.proto[i];
	    sym->defined = 1;
	    if (sym->name == NULL)
		errorf(sym->src, "parameter name omitted");
	    if (isenum(sym->type) || isstruct(sym->type) || isunion(sym->type)) {
		if (!sym->type->s.symbol->defined)
		    errorf(sym->src, "incomplete type '%s %s'",
			   tname(sym->type->op), sym->type->name);
	    }
	}
    }

    if (kind(token->id) & FIRST_DECL) {
	// old style function definition
	struct vector *v = new_vector();
	enter_scope();
	while (kind(token->id) & FIRST_DECL)
	    vector_add_from_array(v, (void **)decls(paramdecl));

	vector_foreach(v, update_params, ftype);
	free_vector(v);
	exit_scope();
	if (token->id != '{') {
	    error("expect function body after function declarator");
	    // TODO
	}
    }
    
    if (token->id == '{') {
	// function definition
	// install symbol first for backward reference
	struct stmt *stmt = compound_statement(NULL);
	exit_scope();
	decl->node.kids[0] = NODE(stmt);
    }

    return decl;
}

static struct node ** decls(DeclFunc declfunc)
{
    struct vector *v = new_vector();
    struct type *basety;
    int sclass;
    struct source src = source;
    int level = SCOPE;
 
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
	    if (params && isfunction(ty) &&
		(token->id == '{' ||
		 ((istypename(token) || token->id & SCLASS_SPEC) &&
		  ty->f.oldstyle && ty->f.proto))) {
		vector_push(v, funcdef(id, ty, sclass, src));
		return (struct node **) vector_to_array(v);
	    } else if (SCOPE == PARAM) {
		exit_scope();
	    }
	}

	for (;;) {
	    if (id) {
		struct decl *decl;
		struct symbol *sym = declfunc(id, ty, sclass, src);
		if (sclass == TYPEDEF)
		    decl = decl_node(TYPEDEF_DECL, SCOPE);
		else
		    decl = decl_node(VAR_DECL, SCOPE);

		decl->node.symbol = sym;
		vector_push(v, decl);
	    } else {
		errorf(src, "missing identifier in declaration");
	    }

	    if (token->id != ',')
		break;

	    match(',');
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
	struct decl *decl;
	if (isstruct(basety))
	    node_id = STRUCT_DECL;
	else if (isunion(basety))
	    node_id = UNION_DECL;
	else
	    node_id = ENUM_DECL;
		
	decl = decl_node(node_id, SCOPE);
	decl->node.symbol = basety->s.symbol;
	vector_push(v, decl);
    } else {
	error("invalid token '%s' in declaration", token->name);
    }
    skipto(';');
    return (struct node **)vector_to_array(v);
}

struct decl * initializer_list()
{
    match('{');
    // TODO
    
    if (token->id == ',')
	match(',');
    match('}');
    return NULL;
}

int istypename(struct token *t)
{
    return kind(t->id) & (TYPE_SPEC|TYPE_QUAL) ||
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

struct node ** declaration()
{
    assert(SCOPE >= LOCAL);
    return decls(localdecl);
}

struct decl * translation_unit()
{
    struct decl *ret = decl_node(TU_DECL, GLOBAL);
    struct vector *v = new_vector();
    
    for (; token->id != EOI; ) {
	if (kind(token->id) & FIRST_DECL) {
	    assert(SCOPE == GLOBAL);
	    vector_add_from_array(v, (void **)decls(globaldecl));
	} else {
	    error("invalid token '%s'", token->name);
	    gettok();
	}
    }

    ret->exts = (struct node **)vector_to_array(v);

    return ret;
}
