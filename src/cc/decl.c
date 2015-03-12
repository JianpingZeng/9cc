#include "cc.h"

static void abstract_declarator(struct type **ty);
static void declarator(struct type **ty, const char **id);
static void param_declarator(struct type **ty, const char **id);
static struct type * pointer_decl();
static struct type * enum_decl();
static struct type * record_decl();

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

static void redefinition_error(struct source src, struct symbol *sym)
{
    errorf(src, "redefinition of '%s', previous definition at %s line %u",
	  sym->name, sym->src.file, sym->src.line);
}

static void conflicting_types_error(struct source src, struct symbol *sym)
{
    errorf(src, "conflicting types for '%s', previous at %s line %u",
	  sym->name, sym->src.file, sym->src.line);
}

static void validate_func_or_array(struct type *ty)
{
    if (isfunction(ty)) {
	if (isftype(ty->type))
	    error("function cannot return function type");
	else if (isatype(ty->type))
	    error("function cannot return array type");
    } else if (isarray(ty) && isftype(ty->type)) {
	error("array of function is invalid");
    }
}

static int is_void_qual(struct type *ty)
{
    if (ty == NULL)
	return 0;
    else if (isqual(ty))
	return 1;
    else if (ty->op == TYPEDEF)
	return is_void_qual(ty->type);
    else
	return 0;
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
	    basety = record_decl();
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

static struct symbol * lookup_ids(struct type *ftype, const char *id)
{
    if (ftype->f.proto && id) {
	for (int i=0; ftype->f.proto[i]; i++) {
	    struct node *p = ftype->f.proto[i];
	    if (p->symbol && !strcmp(p->symbol->name, id))
		return p->symbol;
	}
    }

    return NULL;
}

// oldstyle
static void parameter_decl_list(struct type *ftype)
{
    enter_scope();
    
    for (; kind(token->id) & FIRST_DECL;) {
	int sclass;
	struct type *basety;
	    
	basety = specifiers(&sclass);
	if (token->id == ID || token->id == '*' || token->id == '(') {
	    if (sclass && sclass != REGISTER)
		error("invalid storage class specifier '%s' in parameter list", tname(sclass));
	    else if (isinline(basety))
		error("invalid function specifier 'inline' in parameter list");
		
	    for (;;) {
		const char *id = NULL;
		struct type *ty = NULL;
		struct source src = source;

		// declarator
		declarator(&ty, &id);
		attach_type(&ty, basety);
		
		if (isvtype(ty))
		    error("argument may not have 'void' type");
		else if (isftype(ty))
		    ty = pointer(ty);

		if (id) {
		    struct symbol *sym = locate_symbol(id, identifiers, SCOPE);
		    if (!sym) {
			sym = lookup_ids(ftype, id);
			if (sym) {
			    sym->type = ty;
			    // new symbol
			    sym = install_symbol(id, &identifiers, SCOPE);
			    sym->src = src;
			    sym->type = ty;
			    sym->sclass = sclass == REGISTER ? REGISTER : 0;
			} else {
			    error("parameter named '%s' is missing", id);
			}
		    } else {
			redefinition_error(source, sym);
		    }
		} else {
		    error("missing identifier");
		}

		if (token->id != ',')
		    break;

		match(',');
	    }
	    
	    skipto(';');
	} else if (token->id == ';'){
	    match(';');
	    if (basety)
		error("missing declarator");
	} else {
	    error("invalid token '%s'", token);
	}
    }

    exit_scope();
}

// prototype
static struct node ** parameter_type_list()
{    
    struct vector *v = new_vector();

    for (int i=0;;i++) {
	struct type *basety = NULL;
	int sclass;
	struct type *ty = NULL;
	const char *id = NULL;
	struct source src = source;
	struct decl *decl = decl_node(VAR_DECL, SCOPE);

	basety = specifiers(&sclass);
	if (sclass && sclass != REGISTER)
	    error("invalid storage class specifier '%s' in parameter list",
		  tname(sclass));
	else if (isinline(basety))
	    error("invalid function specifier 'inline' in parameter list");

        if (token->id == '*' || token->id == '(' || token->id == '[' || token->id == ID)
	    param_declarator(&ty, &id);
	
	attach_type(&ty, basety);
	
	if (isvtype(ty)) {
	    if (i == 0 && token->id == ')') {
	        if (id)
		    error("argument may not have 'void' type");
		else if (is_void_qual(ty))
		    error("'void' as parameter must not have type qualifiers");
	    } else {
		error("'void' must be the first and only parameter if specified");
	    }
	} else if (isftype(ty)) {
	    // convert to pointer to function
	    ty = pointer(ty);
	}

	if (id) {
	    struct symbol *sym = locate_symbol(id, identifiers, SCOPE);
	    if (!sym) {
		sym = install_symbol(id, &identifiers, SCOPE);
		sym->type = ty;
		sym->src = src;
	        sym->sclass = sclass == REGISTER ? REGISTER : 0;
		decl->node.symbol = sym;
	    } else {
		redefinition_error(source, sym);
	    }
	} else {
	    struct symbol *sym = anonymous_symbol(&identifiers, SCOPE);
	    sym->type = ty;
	    sym->src = src;
	    sym->sclass = sclass == REGISTER ? REGISTER : 0;
	    decl->node.symbol = sym;
	}
	vector_push(v, decl);
	
	if (token->id != ',')
	    break;

	match(',');
	if (token->id == ELLIPSIS) {
	    struct symbol *sym = install_symbol(token->name, &identifiers, SCOPE);
	    sym->src = source;
	    sym->type = vartype;
	    decl = decl_node(VAR_DECL, SCOPE);
	    decl->node.symbol = sym;
	    vector_push(v, decl);
	    match(ELLIPSIS);
	    break;
	}
    }

    return (struct node **) vector_to_array(v);
}

static struct node ** func_proto(struct type *ftype)
{    
    struct node **ret = NULL;

    enter_scope();
    
    if ((token->id != ID && kind(token->id) & FIRST_DECL) ||
	(token->id == ID && is_typedef_name(token->name))) {
	// prototype
	ret = parameter_type_list();
    } else if (token->id == ID) {
	// oldstyle
	struct vector *v = new_vector();
	for (;;) {
	    if (token->id == ID) {
		struct symbol *sym = locate_symbol(token->name, identifiers, SCOPE);
		if (!sym) {
		    struct decl *decl = decl_node(VAR_DECL, SCOPE);
		    sym = install_symbol(token->name, &identifiers, SCOPE);
		    sym->type = inttype;
		    sym->src = source;
		    decl->node.symbol = sym;
		    vector_push(v, decl);
		} else {
		    redefinition_error(source, sym);
		}
	    }
	    match(ID);
	    if (token->id != ',')
		break;
	    match(',');
	}

	if (SCOPE > PARAM)
	    error("a parameter list without types is only allowed in a function definition");
	ret = (struct node **) vector_to_array(v);
	ftype->f.oldstyle = 1;
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

static struct type * func_or_array()
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
        }
    }

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
                struct type *pty = pointer_type();
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
	struct type *ety = enum_type(id);
	int val = 0;
	match('{');
	do {
	    if (token->id == ID) {
		struct symbol *sym = locate_symbol(token->name, identifiers, SCOPE);
		if (!sym) {
		    sym = install_symbol(token->name, &identifiers, SCOPE);
		    sym->type = ety;
		    sym->src = source;
		} else {
		    redefinition_error(source, sym);
		}

		match(ID);
		if (token->id == '=') {
		    struct expr *expr;
		    match('=');
		    expr = constant_expression();
		    if (expr) {
		        //TODO evaluate
			union value value;
			eval_constexpr(expr, &value);
		    } else {
			error("enum constant is not a compile-time constant");
		    }
		}

		val++;
	    } else {
		error("expect identifier");
		gettok();
	    }
	    
	    if (token->id == ',')
		match(',');
	    if (token->id == '}')
		break;
	} while(token->id != EOI);
	match('}');	
	
	if (id) {
	    struct symbol *sym = locate_symbol(id, records, SCOPE);
	    if (!sym) {
		sym = install_symbol(id, &records, SCOPE);
		sym->type = ety;
		sym->src = src;
		ret = ety;
	    } else {
		redefinition_error(source, sym);
	    }
	} else {
	    ret = ety;
	}
	
    } else if (id) {
	struct symbol *sym = lookup_symbol(id, records);
	if (sym)
	    ret = sym->type;
	else
	    error("undefined enum type '%s'", id);
    } else {
	error("missing identifier after 'enum'");
    }
	
    return ret;
}

static struct type * record_decl()
{
    assert(token->id == STRUCT || token->id == UNION);
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
		declarator(&ty, &name);
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
	    error("undefined %s type '%s'", tname(t), id);
    } else {
	error("missing identifier after '%s'", tname(t));
    }
    
    return ret;
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

struct node * initializer()
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

static void declarator(struct type **ty, const char **id)
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
	    struct type *faty = func_or_array();
	    prepend_type(ty, faty);
	}
    } else if (token->id == '(') {
	struct type *type1 = *ty;
	struct type *rtype = NULL;
	match('(');
	declarator(&rtype, id);
	skipto(')');
	if (token->id == '[' || token->id == '(') {
	    struct type *faty = func_or_array();
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
		    faty = func_or_array();
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
	declarator(ty, id);
    }
}

static struct decl * funcdef(const char *id, struct type *ftype, int sclass,  struct source src)
{
    struct decl *decl = decl_node(FUNC_DECL, SCOPE);

    assert(SCOPE == PARAM);

    if (id == NULL)
	error("missing identifier in function definition");

    if (isftype(ftype->type))
	error("function cannot return function type");
    else if (isatype(ftype->type))
	error("function cannot return array type");

    if (sclass && sclass != EXTERN && sclass != STATIC) {
	error("invalid storage class specifier '%s'", tname(sclass));
	sclass = 0;
    }

    if (!ftype->f.oldstyle && ftype->f.proto) {
	// params id is required in prototype
	for (int i=0; ftype->f.proto[i]; i++) {
	    struct node *decl = ftype->f.proto[i];
	    struct symbol *sym = decl->symbol;
	    if (sym && sym->name == NULL)
		errorf(sym->src, "parameter name omitted");
	}
    }

    if (kind(token->id) & FIRST_DECL) {
	// old style function definition
	parameter_decl_list(ftype);
	if (token->id != '{') {
	    error("expect function body after function declarator");
	    stopat('{');
	}
    }
    
    if (token->id == '{') {
	// function definition
	// install symbol first for backward reference
	struct stmt *stmt;
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
	}
	stmt = compound_statement(NULL);
	exit_scope();
	assert(SCOPE == GLOBAL);
	decl->node.kids[0] = NODE(stmt);
    }

    return decl;
}

static struct decl * vardecl(const char *id, struct type *ty, int sclass, struct source src)
{
    struct decl *decl = NULL;
    int node_id = VAR_DECL;
    struct node *init_node = NULL;

    assert(id);
    assert(SCOPE == GLOBAL || SCOPE >= LOCAL);
    
    if (token->id == '=') {
	// initializer
	match('=');
	init_node = initializer();
    }

    if (SCOPE == GLOBAL && (sclass == AUTO || sclass == REGISTER)) {
	errorf(src, "illegal storage class on file-scoped variable");
	sclass = 0;
    }

    if (sclass == TYPEDEF) {
	// typedef decl
	node_id = TYPEDEF_DECL;
	ty = typedef_type(id, ty);
    } else if (isfunction(ty)){
	node_id = FUNC_DECL;
	if (ty->f.proto && ty->f.oldstyle)
	    error("a parameter list without types is only allowed in a function definition");
	if (isftype(ty->type))
	    error("function cannot return function type");
	else if (isatype(ty->type))
	    error("function cannot return array type");
    } else if (isarray(ty) && isftype(ty->type)) {
	error("array of function is invalid");
    }

    if (SCOPE == GLOBAL) {
	struct symbol *sym = locate_symbol(id, identifiers, SCOPE);
	if (!sym) {
	    sym = install_symbol(id, &identifiers, SCOPE);
	    sym->type = ty;
	    sym->src = src;
	    sym->defined = init_node ? 1 : 0;
	    sym->sclass = sclass;
	    decl = decl_node(node_id, SCOPE);
	    decl->node.kids[0] = init_node;
	    decl->node.symbol = sym;
	} else if (sclass != TYPEDEF && eqtype(ty, sym->type)) {
	    if (sym->defined && init_node)
		redefinition_error(src, sym);
	} else {
	    conflicting_types_error(src, sym);
	}
    } else {
	struct symbol *sym;
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
	    decl = decl_node(node_id, SCOPE);
	    decl->node.kids[0] = init_node;
	    decl->node.symbol = sym;
	} else {
	    redefinition_error(src, sym);
	}
    }

    return decl;
}

static struct decl * typedecl(struct type *ty, struct source src)
{
    int node_id;
    struct decl *decl;

    if (isstruct(ty))
	node_id = STRUCT_DECL;
    else if (isunion(ty))
	node_id = UNION_DECL;
    else
	node_id = ENUM_DECL;
		
    decl = decl_node(node_id, SCOPE);
    if (ty->name) {
	decl->node.symbol = locate_symbol(ty->name, records, SCOPE);
    } else {
	struct symbol *sym = anonymous_symbol(&records, SCOPE);
	sym->type = ty;
	sym->src = src;
	decl->node.symbol = sym;
    }

    return decl;
}

static struct node ** decls()
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
	src = source;

	// declarator
	declarator(&ty, &id);
	attach_type(&ty, basety);
	
	if (level == GLOBAL) {
	    if (isfunction(ty) &&
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
	    if (id == NULL)
		errorf(src, "missing identifier in declaration");
	    else
		vector_push(v, vardecl(id, ty, sclass, src));

	    if (token->id != ',')
		break;

	    match(',');
	    id = NULL;
	    ty = NULL;
	    src = source;
	    // declarator
	    declarator(&ty, &id);
	    attach_type(&ty, basety);
	}

	skipto(';');
    } else if (token->id == ';') {
	// struct/union/enum
	if (basety) {
	    if (isenum(basety) || isrecord(basety))
		vector_push(v, typedecl(basety, src));
	    else
		error("expect enum/struct/union type before ';'");
	}   
	match(';');
    } else {
	error("invalid token '%s' in declaration", token->name);
	skipto(';');
    }

    return (struct node **)vector_to_array(v);
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
    return decls();
}

struct decl * translation_unit()
{
    struct decl *ret = decl_node(TU_DECL, GLOBAL);
    struct vector *v = new_vector();
    
    for (; token->id != EOI; ) {
	if (kind(token->id) & FIRST_DECL) {
	    assert(SCOPE == GLOBAL);
	    vector_add_from_array(v, (void **)decls());
	} else {
	    error("invalid token '%s'", token->name);
	    gettok();
	}
    }

    ret->exts = (struct node **)vector_to_array(v);

    return ret;
}
