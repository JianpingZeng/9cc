#include "cc.h"

static struct type * specifiers(int *sclass);

// // abstract
// enum {
//     NON_ABSTRACT,
//     ABSTRACT,
//     COMPAT, // both
// };

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

static struct node * parameter_type_list()
{
    struct node *ret = NULL;

    enterscope();

    for (;;) {
	struct type *basety = NULL;
	int sclass;

	basety = specifiers(&sclass);
    }
    
    exitscope();

    return ret;
//     BEGIN_CALL(parameter_type_list);

//     Vector *v = new_vector();
//     enterscope();
//     if (isdeclspec(token->id) || (token->id == ID && istypedefname(token->name))) {
//         int n = 0;
//         for (;;) {
//             struct type *basety = NULL;
//             struct type *type = NULL;
//             const char *id = NULL;
//             int sclass;

//             n++;
//             basety = declaration_specifier(&sclass);
//             declarator(&type, &id, COMPAT);
//             attach_type(&type, basety);
//             type = scls(sclass, type);
//             if (isvoid(type)) {
//                 if (n > 1 || id || type->sclass || isqual(type)) {
//                     error("invalid void type as parameter");
//                 }
//             }
//             if (id == NULL) {
//                 id = stringd(n);
//             }
//             vector_push(v, declparam(id, type));
//             if (token->id == ')') {
//                 break;
//             }
//             match(',');
//             if (token->id == ELLIPSIS) {
//                 match(ELLIPSIS);
//                 break;
//             }

//             if (!(isdeclspec(token->id) || (token->id == ID && istypedefname(token->name)))) {
//                 error("invalid token '%k', missing type name", token);
//                 break;
//             }
//         }

//         ftype->u.f.oldstyle = 0;
//     }
//     exitscope();

//     END_CALL(parameter_type_list);

//     return (struct symbol **)vector_to_array(v);
}

static struct node * func_proto(struct type *ftype)
{
    struct node *ret = NULL;

    if ((token->id != ID && kind(token->id) & FIRST_DECL) ||
	(token->id == ID && is_typedef_name(token->name))) {

	ret = parameter_type_list();
	    
    } else if (token->id == ID) {
	enterscope();
	
	exitscope();
    } else if (token->id != ')') {
	error("invalid token '%k' in parameter list", token);
    }
    
    return ret;
//     struct symbol ** syms;
    
//     if (isdeclspec(token->id) || (token->id == ID && istypedefname(token->name))) {
//         // prototype
//         syms = parameter_type_list(ftype);
//         ftype->u.f.proto = syms;
//     }
//     else {
//         Vector *v = new_vector();
//         if (token->id == ID) {
//             // old style
//             enterscope();
//             for ( ; ; ) {
//                 struct symbol *s;
//                 if (token->id != ID) {
//                     error("expect identifier at '%k'", token);
//                     break;
//                 }
//                 s = declparam(token->name, inttype);
//                 vector_push(v, s);
//                 match(ID);
//                 if (token->id != ',') {
//                     break;
//                 }
//                 match(',');
//             }
//             ftype->u.f.proto = NULL;
//             ftype->u.f.oldstyle = 1;
//             exitscope();
//         }
//         syms = vector_to_array(v);
//     }

//     END_CALL(funcparams);
    
//     return syms;
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

// static struct type * abstract_func_or_array()
// {
//     struct type *ty = NULL;
//     BEGIN_CALL(abstract_func_or_array);
    
//     for (; token->id == '(' || token->id == '['; ) {
//         if (token->id == '[') {
//             match('[');
//             if (token->id == ']') {
//                 match(']');
//             }
//             else if (token->id == '*') {
//                 match('*');
//                 match(']');
//             }
//             else {
//                 assignment_expr();
//                 match(']');
//             }
//             struct type *atype = new_type();
//             atype->op = ARRAY;
//             attach_type(&ty, atype);
//         }
//         else {
//             match('(');
//             struct type *ftype = new_type();
//             ftype->op = FUNCTION;
//             ftype->u.f.proto = parameter_type_list(ftype);
//             attach_type(&ty, ftype);
//             match(')');
//         }
//     }
    
//     END_CALL(abstract_func_or_array);
    
//     return ty;
// }

// static void direct_abstract_declarator(struct type **type, const char **id)
// {
//     BEGIN_CALL(direct_abstract_declarator);
    
//     assert(token->id == '(' || token->id == '[');
    
//     if (token->id == '[') {
//         struct type *type2 = abstract_func_or_array();
//         prepend_type(type, type2);
//     }
//     else if (token->id == '(') {
//         int t = lookahead();
//         if (t == '*' || t == '(' || t == '[') {
//             struct type *type1 = *type;
//             match('(');
//             declarator(type, id, ABSTRACT);
//             match(')');
//             if (token->id == '(' || token->id == '[') {
//                 struct type *type2;
//                 struct type *rtype = *type;
//                 type2 = abstract_func_or_array();
//                 attach_type(&type2, type1);
//                 // deref list
//                 while (rtype && rtype->type != type1) {
//                     rtype = rtype->type;
//                 }
//                 assert(rtype);
//                 if (type1 == rtype->type) {
//                     rtype->type = NULL;
//                 }
//                 attach_type(type, type2);
//             }
//         }
//         else {
//             struct type *type2 = abstract_func_or_array();
//             prepend_type(type, type2);
//         }
//     }
    
//     END_CALL(direct_abstract_declarator);
// }

// static void direct_declarator(struct type **type, const char **id)
// {
//     BEGIN_CALL(direct_declarator);
    
//     assert(token->id == ID || token->id == '(');
    
//     if (token->id == ID) {
//         if (id) {
//             *id = token->name;
//         }
//         match(ID);
//         if (token->id == '(' || token->id == '[') {
//             struct type *type2 = func_or_array();
//             prepend_type(type, type2);
//         }
//     }
//     else if (token->id == '(') {
//         struct type *type1 = *type;
//         match('(');
//         declarator(type, id, NON_ABSTRACT);
//         match(')');
//         if (token->id == '(' || token->id == '[') {
//             struct type *type2 = NULL;
//             struct type *rtype = *type;
//             type2 = func_or_array();
//             attach_type(&type2, type1);
//             // deref list
//             while (rtype && rtype->type != type1) {
//                 rtype = rtype->type;
//             }
//             assert(rtype);
//             if (type1 == rtype->type) {
//                 rtype->type = NULL;
//             }
//             attach_type(type, type2);
//         }
//     }
    
//     END_CALL(direct_declarator);
// }

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

// // abstract = NON_ABSTRACT  : only direct_declarator
// //          = ABSTRACT      : only direct_abstract_declarator
// //          = COMPAT        : both
// static void declarator(struct type **type, const char **id, int abstract)
// {
//     BEGIN_CALL(declarator);
    
//     if (token->id == '*') {
//         struct type *ptype = pointer();
//         prepend_type(type, ptype);
//     }
    
//     if (abstract == NON_ABSTRACT) {
//         // direct declarator
//         if (token->id == ID || token->id == '(') {
//             direct_declarator(type, id);
//         }
//         else {
//             error("expect identifier or '('");
//         }
//     }
//     else if (abstract == ABSTRACT) {
//         // direct abstract declarators
//         if (token->id == '(' || token->id == '[') {
//             direct_abstract_declarator(type, id);
//         }
//         // can be pointer only
//     }
//     else if (abstract == COMPAT) {
//         if (token->id == ID) {
//             direct_declarator(type, id);
//         }
//         else if (token->id == '[') {
//             direct_abstract_declarator(type, id);
//         }
//         else if (token->id == '(') {
// 	    int t = lookahead();
// 	    if (t == '*' || t == ID || t == '(' || t == '[') {
// 		struct type *type1 = *type;
// 		match('(');
// 		declarator(type, id, abstract);
// 		match(')');
// 		if (token->id == '(' || token->id == '[') {
// 		    struct type *type2;
// 		    assert(id);
// 		    if (*id) {
// 			type2 =  func_or_array();
// 		    }
// 		    else {
// 			type2 = abstract_func_or_array();
// 		    }
// 		    struct type *rtype = *type;
// 		    attach_type(&type2, type1);
// 		    // deref list
// 		    while (rtype && rtype->type != type1) {
// 			rtype = rtype->type;
// 		    }
// 		    assert(rtype);
// 		    if (type1 == rtype->type) {
// 			rtype->type = NULL;
// 		    }
// 		    attach_type(type, type2);
// 		}
// 	    }
// 	    else {
//                 match('(');
// 		struct type *ftype = new_type();
//                 ftype->op = FUNCTION;
// 		ftype->u.f.proto = parameter_type_list(ftype);
//                 attach_type(type, ftype);
//                 match(')');
// 		if (token->id == '(' || token->id == '[') {
// 		    abstract_func_or_array();
// 		}
// 	    }
//         }
//     }
    
//     END_CALL(declarator);
// }

static struct decl * enum_decl()
{
    return NULL;
}

static struct decl * record_decl()
{
    return NULL;
}

static struct type * specifiers(int *sclass)
{
    int cls, sign, size, type;
    int cons, vol, res, inl;
    struct type *basety;
    int ci;			// _Complex, _Imaginary
        
    basety = NULL;
    cls = sign = size = type = 0;
    cons = vol = res = inl = 0;
    ci = 0;

    for (;;) {
        int *p, t = token->id;
	unsigned line = src.line;
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
	    basety = enum_decl()->node.symbol->type;
	    break;
                
	case STRUCT:
	case UNION:
	    p = &type;
	    basety = record_decl()->node.symbol->type;
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
		errorf(line, "duplicate storage class specifier at '%s'", tname(t));
	    else if (p == &cons || p == &res || p == &vol || p == &inl)
		warningf(line, "duplicate '%s' declaration specifier", tname(t));
	    else if (p == &ci)
		errorf(line, "duplicate _Complex/_Imaginary specifier at '%s'", tname(t));
	    else if (p == &sign)
		errorf(line, "duplicate signed/unsigned speficier at '%s'", tname(t));
	    else if (p == &type || p == &size)
		errorf(line, "duplicate type specifier at '%s'", tname(t));
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
    } else if ((sign && type != INT && type != CHAR)) {
        error("'%s' cannot be signed or unsigned", tname(type));
    } else if ((ci && type != DOUBLE && type != FLOAT)) {
	error("'%s' cannot be %s", tname(type), tname(ci));
    }
    
    if (type == CHAR && sign)
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

// static struct node * declare(const char *id, struct type *ty)
// {
//     struct node *n = NULL;
//     struct symbol *sym = lookupsym(id, identifiers);
//     if (sym == NULL || equal_type(sym->type, ty)) {
//         if (sym == NULL) {
//             sym = installsym(id, &identifiers, scopelevel);
//             sym->type = ty;
//         }
//         // new node for symbol
//         if (isfunction(ty)) {
//             struct decl *fdecl = funcdecl_node(scopelevel);
//             fdecl->d.n.s = sym;
//             n = NODE(fdecl);
//         }
//         else {
//             struct decl *vdecl = decl_node(VAR_DECL, scopelevel);
//             vdecl->n.s = sym;
//             n = NODE(vdecl);
//         }
//     }
//     else {
//         error("Confliting types for %s", id);
//     }
    
//     return n;
// }

// static struct node * declglobal(const char *id, struct type *ty)
// {
//     struct symbol *s;
//     assert(scopelevel == GLOBAL);
    
//     if (ty->sclass == 0) {
//         ty = scls(EXTERN, ty);
//     }
//     else if (ty->sclass == AUTO || ty->sclass == REGISTER) {
//         error("invalid storage class specifier '%s' for '%s'", tname(ty->sclass), id);
//         ty = scls(EXTERN, ty);
//     }
//     s = lookupsym(id, identifiers);
//     if (s && s->scope == GLOBAL) {
//         error("redeclaratoin of '%s'", id);
//     }
//     if (s == NULL || s->scope != GLOBAL) {
//         s = installsym(id, &identifiers, scopelevel);
//     }
//     s->type = ty;
    
//     return NULL;
// }

// static struct symbol * declparam(const char *id, struct type *ty)
// {
//     struct symbol *sym;
//     assert(scopelevel > GLOBAL);
    
//     if (ty->sclass != 0 && ty->sclass != REGISTER) {
//         error("invalid storage class specifier '%s' for '%s' in parameter list", tname(ty->sclass), id);
//     }
    
//     sym = lookupsym(id, identifiers);
//     if (sym && sym->scope == scopelevel) {
//         error("redeclaration '%s' in parameter list", id);
//     }
//     else {
//         sym = installsym(id, &identifiers, scopelevel);
//     }
//     sym->type = ty;
    
//     return sym;
// }

// static struct node * declocal(const char *id, struct type *ty)
// {
//     struct node *n = NULL;
//     assert(scopelevel > PARAM);
    
//     return n;
// }

// static void initializer()
// {
    
// }

struct decl * initializer_list()
{
    return NULL;
}

struct decl * declaration()
{
    BEGIN_CALL(declaration);
    Log.v("%d %k", token->id, token);
    END_CALL(declaration);
    return NULL;
//     Vector *v = new_vector();
//     const char *id = NULL;
//     struct type *basety, *type = NULL;
//     int sclass;
    
//     BEGIN_CALL(declaration);
    
//     basety = declaration_specifier(&sclass);
//     if (token->id == ID || token->id == '*' || token->id == '(') {
//         declarator(&type, &id, NON_ABSTRACT);
//         attach_type(&type, basety);
//         type = scls(sclass, type);
//         if (token->id == ';' || token->id == '=' || token->id == ',') {
//             struct node *n;
//             // initializer
//             if (token->id == '=') {
//                 match('=');
//                 initializer();
//             }
//             // next declarator
//             n = declare(id, type);
//             if (n) {
//                 vector_push(v, n);
//             }
//             while (token->id == ',') {
//                 match(',');
//                 id = NULL;
//                 type = NULL;
//                 if (token->id == ID || token->id == '*' || token->id == '(') {
//                     declarator(&type, &id, NON_ABSTRACT);
//                     attach_type(&type, basety);
//                     // initializer
//                     if (token->id == '=') {
//                         match('=');
//                         initializer();
//                     }
//                     n = declare(id, type);
//                     if (n) {
//                         vector_push(v, n);
//                     }
//                 }
//                 else {
//                     error("invalid token '%k' in declaration", token);
//                 }
//             }
//             match(';');
//         }
//         else if (token->id == '{' && isfunction(type)) {
//             // function definiction
//             if (scopelevel == GLOBAL) {
//                 struct stmt *cs = compound_stmt();
//                 struct decl *fdecl = declglobal(id, type);
//                 if (fdecl) {
//                     Vector *pv = new_vector();
//                     for (int i=0; type->u.f.proto[i]; i++) {
//                         struct decl *param_decl = decl_node(PARAMVAL_DECL, PARAM);
//                         param_decl->n.s = type->u.f.proto[i];
//                         vector_push(pv, param_decl);
//                     }
//                     fdecl->params = vector_to_array(pv);
//                     fdecl->cs = cs;
//                     vector_push(v, fdecl);
//                 }
//             }
//             else {
//                 error("function definition can only in global scope");
//             }
//         }
//         else if ((kind(token->id) & (SCLASS_SPEC|TYPE_QUAL|TYPE_SPEC) || token->id == '(') &&
//                  isfunction(type) && type->u.f.oldstyle) {
//             // old style function
            
//         }
//         else {
            
//         }
//     }
//     else if (token->id == ';') {
//         // struct/union/enum
//         match(';');
//     }
//     else {
//         error("invalid token '%k' in declaration", token);
//     }
    
//     END_CALL(declaration);
    
//     return (struct node **) vector_to_array(v);
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

    } else {
	error("expect identifier or '(' at '%k'", token);
    }
}

static struct decl * external_decl()
{
    struct decl *ret = NULL;
    struct type *basety;
    int sclass;

    basety = specifiers(&sclass);
    if (token->id == ID || token->id == '*' || token->id == '(') {
	const char *id = NULL;
	struct type *ty = NULL;

	// declarator
	declarator(&ty, &id);
	
    } else if (token->id == ';') {
	// struct/union/enum
    } else {
	error("invalid token '%k' in declaration", token);
    }

    return ret;
}

struct decl * translation_unit()
{
    struct decl *ret = decl_node(TU_DECL, GLOBAL);
    struct node *node = NULL;
    
    for (; token->id != EOI; ) {
	if (kind(token->id) & FIRST_DECL) {
	    struct node *node1 = concat_node(NODE(external_decl()), NULL);
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

