#include "c.h"

#define isdeclspec(t) (kind(t) & (SCLASS_SPEC|TYPE_QUAL|TYPE_SPEC|FUNC_SPEC))

int scopelevel;

static Type * declaration_specifier(int *sclass);
static void declarator(Type **, const char **, int abstract);
static Type * pointer();

static Symbol * declparam(const char *id, Type *ty);

// abstract
enum {
    NON_ABSTRACT,
    ABSTRACT,
    COMPAT, // both
};

static unsigned char kinds[] = {
#define _a(x, y, z)
#define _x(a, b, c, d)      d,
#define _t(a, b, c, d)      d,
#include "token.h"
};

unsigned char kind(int t)
{
    if (t >= ID && t < TOKEND) {
        return kinds[t-ID];
    }
    else {
        return 0;
    }
}

static Symbol ** parameter_type_list(Type *ftype)
{
    BEGIN_CALL(parameter_type_list);

    Vector *v = new_vector();
    enterscope();
    if (isdeclspec(token->id) || (token->id == ID && istypedefname(token->name))) {
        int n = 0;
        for (;;) {
            Type *basety = NULL;
            Type *type = NULL;
            const char *id = NULL;
            int sclass;

            n++;
            basety = declaration_specifier(&sclass);
            declarator(&type, &id, COMPAT);
            attach_type(&type, basety);
            type = scls(sclass, type);
            if (isvoid(type)) {
                if (n > 1 || id || type->sclass || isqual(type)) {
                    error("invalid void type as parameter");
                }
            }
            if (id == NULL) {
                id = stringd(n);
            }
            vector_push(v, declparam(id, type));
            if (token->id == ')') {
                break;
            }
            match(',');
            if (token->id == ELLIPSIS) {
                match(ELLIPSIS);
                break;
            }

            if (!(isdeclspec(token->id) || (token->id == ID && istypedefname(token->name)))) {
                error("invalid token '%k', missing type name", token);
                break;
            }
        }

        ftype->u.f.oldstyle = 0;
    }
    exitscope();

    END_CALL(parameter_type_list);

    return (Symbol **)vector_to_array(v);
}

static Symbol ** funcparams(Type *ftype)
{
    BEGIN_CALL(funcparams);

    Symbol ** syms;
    
    if (isdeclspec(token->id) || (token->id == ID && istypedefname(token->name))) {
        // prototype
        syms = parameter_type_list(ftype);
        ftype->u.f.proto = syms;
    }
    else {
        Vector *v = new_vector();
        if (token->id == ID) {
            // old style
            enterscope();
            for ( ; ; ) {
                Symbol *s;
                if (token->id != ID) {
                    error("expect identifier at '%k'", token);
                    break;
                }
                s = declparam(token->name, inttype);
                vector_push(v, s);
                match(ID);
                if (token->id != ',') {
                    break;
                }
                match(',');
            }
            ftype->u.f.proto = NULL;
            ftype->u.f.oldstyle = 1;
            exitscope();
        }
        syms = vector_to_array(v);
    }

    END_CALL(funcparams);
    
    return syms;
}

static Type * func_or_array()
{
    Type *ty = NULL;
    BEGIN_CALL(func_or_array);
    
    for (; token->id == '(' || token->id == '['; ) {
        if (token->id == '[') {
            Type *atype = new(Type);
            match('[');
            if (token->id == ICONSTANT) {
                match(ICONSTANT);
            }
            match(']');
            atype->op = ARRAY;
            atype->size = token->v.i;
            attach_type(&ty, atype);
        }
        else {
            match('(');
            Type *ftype = new(Type);
            ftype->op = FUNCTION;
            ftype->u.f.proto = funcparams(ftype);
            attach_type(&ty, ftype);
            match(')');
        }
    }
    
    END_CALL(func_or_array);
    
    return ty;
}

static Type * abstract_func_or_array()
{
    Type *ty = NULL;
    BEGIN_CALL(abstract_func_or_array);
    
    for (; token->id == '(' || token->id == '['; ) {
        if (token->id == '[') {
            match('[');
            if (token->id == ']') {
                match(']');
            }
            else if (token->id == '*') {
                match('*');
                match(']');
            }
            else {
                assignment_expr();
                match(']');
            }
            Type *atype = new(Type);
            atype->op = ARRAY;
            attach_type(&ty, atype);
        }
        else {
            match('(');
            Type *ftype = new(Type);
            ftype->op = FUNCTION;
            ftype->u.f.proto = parameter_type_list(ftype);
            attach_type(&ty, ftype);
            match(')');
        }
    }
    
    END_CALL(abstract_func_or_array);
    
    return ty;
}

static void direct_abstract_declarator(Type **type, const char **id)
{
    BEGIN_CALL(direct_abstract_declarator);
    
    assert(token->id == '(' || token->id == '[');
    
    if (token->id == '[') {
        Type *type2 = abstract_func_or_array();
        prepend_type(type, type2);
    }
    else if (token->id == '(') {
        int t = lookahead();
        if (t == '*' || t == '(' || t == '[') {
            Type *type1 = *type;
            match('(');
            declarator(type, id, ABSTRACT);
            match(')');
            if (token->id == '(' || token->id == '[') {
                Type *type2;
                Type *rtype = *type;
                type2 = abstract_func_or_array();
                attach_type(&type2, type1);
                // deref list
                while (rtype && rtype->type != type1) {
                    rtype = rtype->type;
                }
                assert(rtype);
                if (type1 == rtype->type) {
                    rtype->type = NULL;
                }
                attach_type(type, type2);
            }
        }
        else {
            Type *type2 = abstract_func_or_array();
            prepend_type(type, type2);
        }
    }
    
    END_CALL(direct_abstract_declarator);
}

static void direct_declarator(Type **type, const char **id)
{
    BEGIN_CALL(direct_declarator);
    
    assert(token->id == ID || token->id == '(');
    
    if (token->id == ID) {
        if (id) {
            *id = token->name;
        }
        match(ID);
        if (token->id == '(' || token->id == '[') {
            Type *type2 = func_or_array();
            prepend_type(type, type2);
        }
    }
    else if (token->id == '(') {
        Type *type1 = *type;
        match('(');
        declarator(type, id, NON_ABSTRACT);
        match(')');
        if (token->id == '(' || token->id == '[') {
            Type *type2 = NULL;
            Type *rtype = *type;
            type2 = func_or_array();
            attach_type(&type2, type1);
            // deref list
            while (rtype && rtype->type != type1) {
                rtype = rtype->type;
            }
            assert(rtype);
            if (type1 == rtype->type) {
                rtype->type = NULL;
            }
            attach_type(type, type2);
        }
    }
    
    END_CALL(direct_declarator);
}

static Type * pointer()
{
    Type *ty = NULL;
    int con, vol, res, type;
    assert(token->id == '*');
    
    BEGIN_CALL(pointer);
    
    con = vol = res = type = 0;
    
    for (; ; ) {
        int *p, t = token->id;
        switch (token->id) {
            case CONST:
                p = &con;
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
                
            case '*':
            {
                Type *ptype = new(Type);
                ptype->op = POINTER;
                con = vol = res = type = 0;
                p = &type;
                prepend_type(&ty, ptype);
                gettok();
            }
                break;
                
            default:
                p = NULL;
                break;
        }
        
        if (p == NULL) {
            break;
        }
        
        if (*p != 0) {
            error("duplicate type qulifier '%k'", token);
        }
        
        *p = t;
        
        if (t == CONST || t == VOLATILE || t == RESTRICT) {
            ty = qual(t, ty);
        }
    }
    
    END_CALL(pointer);
    
    return ty;
}

// abstract = NON_ABSTRACT  : only direct_declarator
//          = ABSTRACT      : only direct_abstract_declarator
//          = COMPAT        : both
static void declarator(Type **type, const char **id, int abstract)
{
    BEGIN_CALL(declarator);
    
    if (token->id == '*') {
        Type *ptype = pointer();
        prepend_type(type, ptype);
    }
    
    if (abstract == NON_ABSTRACT) {
        // direct declarator
        if (token->id == ID || token->id == '(') {
            direct_declarator(type, id);
        }
        else {
            error("expect identifier or '('");
        }
    }
    else if (abstract == ABSTRACT) {
        // direct abstract declarators
        if (token->id == '(' || token->id == '[') {
            direct_abstract_declarator(type, id);
        }
        // can be pointer only
    }
    else if (abstract == COMPAT) {
        if (token->id == ID) {
            direct_declarator(type, id);
        }
        else if (token->id == '[') {
            direct_abstract_declarator(type, id);
        }
        else if (token->id == '(') {
	    int t = lookahead();
	    if (t == '*' || t == ID || t == '(' || t == '[') {
		Type *type1 = *type;
		match('(');
		declarator(type, id, abstract);
		match(')');
		if (token->id == '(' || token->id == '[') {
		    Type *type2;
		    assert(id);
		    if (*id) {
			type2 =  func_or_array();
		    }
		    else {
			type2 = abstract_func_or_array();
		    }
		    Type *rtype = *type;
		    attach_type(&type2, type1);
		    // deref list
		    while (rtype && rtype->type != type1) {
			rtype = rtype->type;
		    }
		    assert(rtype);
		    if (type1 == rtype->type) {
			rtype->type = NULL;
		    }
		    attach_type(type, type2);
		}
	    }
	    else {
                match('(');
		Type *ftype = new(Type);
                ftype->op = FUNCTION;
		ftype->u.f.proto = parameter_type_list(ftype);
                attach_type(type, ftype);
                match(')');
		if (token->id == '(' || token->id == '[') {
		    abstract_func_or_array();
		}
	    }
        }
    }
    
    END_CALL(declarator);
}

static Type * enum_decl()
{
    return NULL;
}

static Type * record_decl()
{
    return NULL;
}

static Type * declaration_specifier(int *sclass)
{
    int cls, sign, size, type;
    int cons, vol, res, inl;
    Type *basety = NULL;
    
    BEGIN_CALL(decl_spec);
    
    cls = sign = size = type = 0;
    cons = vol = res = inl = 0;

    for (;;) {
        int *p, t = token->id;
        switch (token->id) {
            case AUTO: case EXTERN: case REGISTER:
            case STATIC: case TYPEDEF:
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
                gettok();
                break;
                
            case STRUCT:
            case UNION:
                p = &type;
                basety = record_decl();
                gettok();
                break;
             
            case LONG:
                if (size == LONG) {
                    t = LONG + LONG;
                    size = 0;   // clear
                }
                // go through
            case SHORT:
                p = &size;
                gettok();
                break;
            
            case FLOAT:
            case DOUBLE:
            case VOID:
            case CHAR:
            case INT:
                p = &type;
                basety = pretype(token->id);
                gettok();
                break;
                
            case SIGNED:
            case UNSIGNED:
                p = &sign;
                gettok();
                break;
                
            case ID:
                // typedef name
            default:
                p = NULL;
                break;
        }
        
        if (p == NULL) {
            break;
        }
        
        if (*p != 0) {
            error("invalid syntax at '%k'", token);
        }
        
        *p = t;
    }
    
    // default is int
    if (type == 0) {
        if (sign == 0 && size == 0) {
            if (stdc99) {
                error("missing type specifier");
            }
            else {
                error("missing type specifier, default is int");
            }
        }
        type = INT;
        basety = inttype;
    }
    
    // type check
    if ((size == SHORT && type != INT) ||
        (size == LONG + LONG && type != INT) ||
        (size == LONG && type != INT && type != DOUBLE)) {
        if (size == LONG + LONG) {
            error("%s %s %s is invalid", tname(size/2), tname(size/2), tname(type));
        }
        else {
            error("%s %s is invalid", tname(size), tname(type));
        }
    }
    else if ((sign && type != INT && type != CHAR)) {
        error("'%s' cannot be signed or unsigned", tname(type));
    }
    
    if (type == CHAR && sign) {
        basety = sign == UNSIGNED ? unsignedchartype : signedchartype;
    }
    else if (size == SHORT) {
        basety = sign == UNSIGNED ? unsignedshorttype : shorttype;
    }
    else if (type == INT && size == LONG) {
        basety = sign == UNSIGNED ? unsignedlongtype : longtype;
    }
    else if (size == LONG + LONG) {
        basety = sign == UNSIGNED ? unsignedlonglongtype : longlongtype;
    }
    else if (type == DOUBLE && size == LONG) {
        basety = longdoubletype;
    }
    else if (sign == UNSIGNED) {
        basety = unsignedinttype;
    }
    
    // qulifier
    if (cons) {
        basety = qual(CONST, basety);
    }
    if (vol) {
        basety = qual(VOLATILE, basety);
    }
    if (res) {
        basety = qual(RESTRICT, basety);
    }
    if (inl) {
        basety = qual(INLINE, basety);
    }
    
    *sclass = cls;
    
    END_CALL(decl_spec);
    
    return basety;
}

static Node * declare(const char *id, Type *ty)
{
    Node *n = NULL;
    Symbol *sym = lookupsym(id, identifiers);
    if (sym == NULL || equal_type(sym->type, ty)) {
        if (sym == NULL) {
            sym = installsym(id, &identifiers, scopelevel);
            sym->type = ty;
        }
        // new node for symbol
        if (isfunction(ty)) {
            FuncDecl *fdecl = funcdecl_node(scopelevel);
            fdecl->d.n.s = sym;
            n = NODE(fdecl);
        }
        else {
            Decl *vdecl = decl_node(VAR_DECL, scopelevel);
            vdecl->n.s = sym;
            n = NODE(vdecl);
        }
    }
    else {
        error("Confliting types for %s", id);
    }
    
    return n;
}

static Node * declglobal(const char *id, Type *ty)
{
    Symbol *s;
    assert(scopelevel == GLOBAL);
    
    if (ty->sclass == 0) {
        ty = scls(EXTERN, ty);
    }
    else if (ty->sclass == AUTO || ty->sclass == REGISTER) {
        error("invalid storage class specifier '%s' for '%s'", tname(ty->sclass), id);
        ty = scls(EXTERN, ty);
    }
    s = lookupsym(id, identifiers);
    if (s && s->scope == GLOBAL) {
        error("redeclaratoin of '%s'", id);
    }
    if (s == NULL || s->scope != GLOBAL) {
        s = installsym(id, &identifiers, scopelevel);
    }
    s->type = ty;
    
    return NULL;
}

static Symbol * declparam(const char *id, Type *ty)
{
    Symbol *sym;
    assert(scopelevel > GLOBAL);
    
    if (ty->sclass != 0 && ty->sclass != REGISTER) {
        error("invalid storage class specifier '%s' for '%s' in parameter list", tname(ty->sclass), id);
    }
    
    sym = lookupsym(id, identifiers);
    if (sym && sym->scope == scopelevel) {
        error("redeclaration '%s' in parameter list", id);
    }
    else {
        sym = installsym(id, &identifiers, scopelevel);
    }
    sym->type = ty;
    
    return sym;
}

static Node * declocal(const char *id, Type *ty)
{
    Node *n = NULL;
    assert(scopelevel > PARAM);
    
    return n;
}

static void initializer()
{
    
}

void initializer_list()
{
    
}

Node ** declaration()
{
    Vector *v = new_vector();
    const char *id = NULL;
    Type *basety, *type = NULL;
    int sclass;
    
    BEGIN_CALL(declaration);
    
    basety = declaration_specifier(&sclass);
    if (token->id == ID || token->id == '*' || token->id == '(') {
        declarator(&type, &id, NON_ABSTRACT);
        attach_type(&type, basety);
        type = scls(sclass, type);
        if (token->id == ';' || token->id == '=' || token->id == ',') {
            Node *n;
            // initializer
            if (token->id == '=') {
                match('=');
                initializer();
            }
            // next declarator
            n = declare(id, type);
            if (n) {
                vector_push(v, n);
            }
            while (token->id == ',') {
                match(',');
                id = NULL;
                type = NULL;
                if (token->id == ID || token->id == '*' || token->id == '(') {
                    declarator(&type, &id, NON_ABSTRACT);
                    attach_type(&type, basety);
                    // initializer
                    if (token->id == '=') {
                        match('=');
                        initializer();
                    }
                    n = declare(id, type);
                    if (n) {
                        vector_push(v, n);
                    }
                }
                else {
                    error("invalid token '%k' in declaration", token);
                }
            }
            match(';');
        }
        else if (token->id == '{' && isfunction(type)) {
            // function definiction
            if (scopelevel == GLOBAL) {
                CompoundStmt *cs = compound_stmt();
                FuncDecl *fdecl = declglobal(id, type);
                if (fdecl) {
                    Vector *pv = new_vector();
                    for (int i=0; type->u.f.proto[i]; i++) {
                        Decl *param_decl = decl_node(PARAMVAL_DECL, PARAM);
                        param_decl->n.s = type->u.f.proto[i];
                        vector_push(pv, param_decl);
                    }
                    fdecl->params = vector_to_array(pv);
                    fdecl->cs = cs;
                    vector_push(v, fdecl);
                }
            }
            else {
                error("function definition can only in global scope");
            }
        }
        else if ((kind(token->id) & (SCLASS_SPEC|TYPE_QUAL|TYPE_SPEC) || token->id == '(') &&
                 isfunction(type) && type->u.f.oldstyle) {
            // old style function
            
        }
        else {
            
        }
    }
    else if (token->id == ';') {
        // struct/union/enum
        match(';');
    }
    else {
        error("invalid token '%k' in declaration", token);
    }
    
    END_CALL(declaration);
    
    return (Node **) vector_to_array(v);
}

TranslationUnitDecl * translation_unit()
{
    TranslationUnitDecl *tudecl = tudecl_node();
    Vector *v = new_vector();
    identifiers = table(NULL, GLOBAL);
    gettok();
    for (; token->id != EOI; ) {
        if (isdeclspec(token->id) || token->id == ID || token->id == '(') {
            vector_add_from_array(v, (void *) declaration());
        }
        else if (token->id == ';') {
            warning("empty declaration");
            match(';');
        }
        else {
            error("invalid token '%k'", token);
            gettok();
        }
    }
    tudecl->exts = vector_to_array(v);
    return tudecl;
}

