#include "cc.h"

static struct type * specifiers(int *sclass);
static void abstract_declarator(struct type **ty);
static void declarator(struct type **ty, const char **id);
static struct type * pointer();
static void param_declarator(struct type **ty, const char **id);

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

int is_typename(struct token *t)
{
    return kind(t->id) & (TYPE_SPEC|TYPE_QUAL) || is_typedef_name(t->name);
}

static struct decl * parameter_type_list()
{    
    struct decl *ret;
    struct node *node = NULL;

    enter_scope();

    ret = decl_node(PARAM_DECL, SCOPE);

    for (int i=0;;i++) {
	struct type *basety = NULL;
	int sclass;
	struct type *ty = NULL;
	const char *id = NULL;
	struct source src = source;
	struct node *node1 = concat_node(NULL, NULL);
	struct decl *paramval_decl = decl_node(VAR_DECL, SCOPE);
	node1->kids[0] = NODE(paramval_decl);

	basety = specifiers(&sclass);

	if (sclass && sclass != REGISTER)
	    error("invalid storage class specifier '%s' at parameter list",
		  tname(sclass));
	else if (isinline(basety))
	    error("invalid function specifier 'inline' at parameter list");

	basety = scls(sclass == REGISTER ? REGISTER : 0, basety);

        if (token->id == '*' || token->id == '(' || token->id == '[' || token->id == ID)
	    param_declarator(&ty, &id);
	
	attach_type(&ty, basety);

	if (id) {
	    struct symbol *sym = locate_symbol(id, identifiers);
	    if (!sym) {
		sym = install_symbol(id, &identifiers, SCOPE);
		sym->type = ty;
		sym->src = src;
		paramval_decl->node.symbol = sym;
	    } else {
		error("redeclaration '%s', previous declaration at %s line %u",
		      id, sym->src.file, sym->src.line);
	    }
	} else {
	    struct symbol *sym = anonymous_symbol(&identifiers, SCOPE);
	    sym->type = ty;
	    sym->src = src;
	    paramval_decl->node.symbol = sym;
	}
	
        if (isvoid(ty)) {
	    if (i > 0)
		error("invalid parameter type 'void'");
	    else if (isqual(ty))
		error("invalid type qualifier for type 'void'");
	    else if (sclass)
		error("invalid storage class specifier '%s' for type 'void'",
		      tname(sclass));
	    else if (token->id != ')')
		error("'void' must be the first and only parameter if specified");
	}

	if (node)
	    node->kids[1] = node1;
	else
	    ret->node.kids[0] = node1;

	node = node1;

	if (token->id == ',') {
	    match(',');
	    if (token->id == ELLIPSIS) {
		match(ELLIPSIS);
		// TODO
		break;
	    }
	}
	else if (token->id == ')') {
	    break;
	}
    }

    if (SCOPE > PARAM)
	exit_scope();

    return ret;
}

static struct decl * func_proto(struct type *ftype)
{    
    struct decl *ret = NULL;

    if ((token->id != ID && kind(token->id) & FIRST_DECL) ||
	(token->id == ID && is_typedef_name(token->name))) {

	ret = parameter_type_list();
	    
    } else if (token->id == ID) {
	enter_scope();

	for (;;) {
	    match(ID);
	    if (token->id == ')')
		break;
	    else
		match(',');
	}

	if (SCOPE > PARAM)
	    exit_scope();

	ftype->u.f.oldstyle = 1;
    } else if (token->id != ')') {
	error("invalid token '%k' in parameter list", token);
    }
    
    return ret;
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
		
	    } else if (kind(token->id) & TYPE_QUAL) {
		
	    } else if (kind(token->id) & FIRST_ASSIGN_EXPR) {
		assign_expression();
	    } else if (token->id == '*') {
		match('*');
	    }
	    skipto(']');
            attach_type(&ty, atype);
        } else {
	    struct type *ftype = function_type();
            match('(');
            ftype->u.f.proto = func_proto(ftype);
	    skipto(')');
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
	    match('[');
	    if (token->id == '*') {
		if (lookahead()->id != ']') 
		    assign_expression();
		else
		    match('*');
	    } else if (kind(token->id) & FIRST_ASSIGN_EXPR) {
		assign_expression();
	    }
	    skipto(']');
	    attach_type(&ty, atype);
	} else {
	    struct type *ftype = function_type();
	    match('(');
	    ftype->u.f.proto = parameter_type_list();
	    skipto(')');
	    attach_type(&ty, ftype);
	}
    }
    
    return ty;
}

static struct type * pointer()
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
            warning("duplicate type qulifier '%k'", token);
        
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
	if (id) {
	    struct symbol *sym = locate_symbol(id, records);
	    if (!sym)
		sym = install_symbol(id, &records, SCOPE);
	    else
		error("redefinition symbol '%s', previous definition at %s line %u",
		      id, sym->src.file, sym->src.line);
	}
	match('{');
	do {
	    if (token->id == ID) {
		struct symbol *sym = locate_symbol(token->name, identifiers);
		if (!sym) {
		    sym = install_symbol(token->name, &identifiers, SCOPE);
		    sym->type = ety;
		    sym->src = source;
		} else {
		    error("redeclaration symbol '%s', previous declaration at %s line %u",
			  token->name, sym->src.file, sym->src.line);
		}
	    } else {
		error("expect identifier");
	    }
	    if (token->id == ',')
		match(',');
	} while(token->id != '}' && token->id != EOI);
	match('}');
    } else if (id) {
	struct symbol *sym = lookup_symbol(id, records);
	if (sym)
	    ret = sym->type;
	else
	    error("undeclared symbol '%s'", id);
    } else {
	error("missing identifier after 'enum'");
    }
	
    return ret;
}

static struct type * record_decl()
{
    return NULL;
}

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

    for (;;) {
        int *p, t = token->id;
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
		errorf(src, "duplicate storage class specifier at '%s'", tname(t));
	    else if (p == &cons || p == &res || p == &vol || p == &inl)
		warningf(src, "duplicate '%s' declaration specifier", tname(t));
	    else if (p == &ci)
		errorf(src, "duplicate _Complex/_Imaginary specifier at '%s'", tname(t));
	    else if (p == &sign)
		errorf(src, "duplicate signed/unsigned speficier at '%s'", tname(t));
	    else if (p == &type || p == &size)
		errorf(src, "duplicate type specifier at '%s'", tname(t));
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
    
    *sclass = cls;
        
    return basety;
}

struct decl * initializer_list()
{
    return NULL;
}

static void abstract_declarator(struct type **ty)
{
    assert(ty);

    if (token->id == '*') {
	struct type *pty = pointer();
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
    } else {
	error("expect '(' or '[' at '%k'", token);
    }
}

static void declarator(struct type **ty, const char **id)
{    
    assert(ty && id);
    
    if (token->id == '*') {
	struct type *pty = pointer();
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
	match(')');
	if (token->id == '[' || token->id == '(') {
	    struct type *faty = func_or_array();
	    attach_type(&faty, type1);
	    attach_type(&rtype, faty);
	}
	*ty = rtype;
    } else {
	error("expect identifier or '(' at '%k'", token);
    }
}

static void param_declarator(struct type **ty, const char **id)
{    
    if (token->id == '*') {
	struct type *pty = pointer();
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

#define MODE_BOTH  0
#define MODE_DECL  1

static struct node * external_decl(int mode)
{    
    struct node *ret = concat_node(NULL, NULL);
    struct type *basety;
    int sclass;

    basety = specifiers(&sclass);
    if (token->id == ID || token->id == '*' || token->id == '(') {
	struct node *node = NULL;
	
	do {
	    const char *id = NULL;
	    struct type *ty = NULL;
	    struct source src = source;
	    
	    // declarator
	    declarator(&ty, &id);
	    attach_type(&ty, basety);
	    if (!id)
		errorf(src, "missing identifier in declarator");
	    if (mode == MODE_BOTH && (token->id == '{' || kind(token->id) & FIRST_DECL)) {
		if (token->id == '{') {
		    // function definition
		    compound_statement(NULL);
		    break;
		} else if (kind(token->id) & FIRST_DECL) {
		    // old style function definition
		
		    break;
		}
	    } else {
		struct decl *decl = decl_node(VAR_DECL, SCOPE);
		if (SCOPE == PARAM)
		    exit_scope();
		assert(SCOPE == GLOBAL);
		if (token->id == '=') {
		    // initializer
		}

		if (sclass == TYPEDEF) {
		    // typedef decl
		    decl->node.id = TYPEDEF_DECL;
		    struct type *type = new_type();
		    type->name = id;
		    type->op = TYPEDEF;
		    type->type = ty;
		    ty = type;
		} else if (isfunction(ty)){
		    decl->node.id = FUNC_DECL;
		}

	        if (id) {
		    struct symbol *sym = locate_symbol(id, identifiers);
		    if (!sym) {
			sym = install_symbol(id, &identifiers, SCOPE);
			sym->type = ty;
			sym->src = src;
			decl->node.symbol = sym;
		    } else {
			errorf(src, "redeclaration symbol '%s', previous declaration at %s line %u",
			       id, sym->src.file, sym->src.line);
		    }
		}

		if (node) {
		    node->kids[1] = concat_node(NODE(decl), NULL);
		    node = node->kids[1];
		}
		else {
		    ret->kids[0] = NODE(decl);
		    node = ret;
		}

		if (token->id == ',')
		    match(',');
	    }

	} while (token->id != ';' && token->id != EOI);

	if (!isfuncdef(ret->kids[0]))
	    match(';');
    } else if (token->id == ';') {
	// struct/union/enum
    } else {
	error("invalid token '%k' in declaration", token);
    }
    
    return ret;
}

struct node * declaration()
{
    return external_decl(MODE_DECL);
}

struct decl * translation_unit()
{
    struct decl *ret = decl_node(TU_DECL, GLOBAL);
    struct node *node = NULL;
    
    for (; token->id != EOI; ) {
	if (kind(token->id) & FIRST_DECL) {
	    struct node *node1 = external_decl(MODE_BOTH);
	    if (node)
	        node->kids[1] = node1;
	    else
		ret->node.kids[0] = node1;

	    node = node1;
	} else {
	    error("invalid token '%k'", token);
	    gettok();
	}
    }
    return ret;
}

