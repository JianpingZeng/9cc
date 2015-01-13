#include "c.h"

#define isdeclspec(t) (kind(t) & (SCLASS_SPEC|TYPE_QUAL|TYPE_SPEC|FUNC_SPEC))

int scopelevel;

static Type declaration_specifier(int *sclass);
static void declarator(Type *, const char **, int abstract);
static Type pointer();

static Symbol declparam(const char *id, Type ty);

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

static Symbol * parameter_type_list(Type ftype)
{
    BEGIN_CALL(parameter_type_list);

    Vector v = new_vector();
    enterscope();
    if (isdeclspec(tok) || (tok == ID && istypedefname(toklex.name))) {
        int n = 0;
        for (;;) {
            Type basety = NULL;
            Type type = NULL;
            const char *id = NULL;
            int sclass;

            n++;
            basety = declaration_specifier(&sclass);
            declarator(&type, &id, COMPAT);
            attach_type(&type, basety);
            type = scls(sclass, type);
            if (isvoid(type)) {
                if (n > 1 || id || type->sclass || isqual(type)) {
                    ERROR("invalid void type as parameter");
                }
            }
            if (id == NULL) {
                id = stringd(n);
            }
            vector_push_back(v, declparam(id, type));
            printf("%s: ", id);
            printtype(type);
            if (tok == ')') {
                break;
            }
            match(',');
            if (tok == ELLIPSIS) {
                match(ELLIPSIS);
                break;
            }

            if (!(isdeclspec(tok) || (tok == ID && istypedefname(toklex.name)))) {
                ERROR("invalid token '%s', missing type name", tname(tok));
                break;
            }
        }

        ftype->u.f.oldstyle = 0;
    }
    exitscope();

    END_CALL(parameter_type_list);

    return (Symbol *)vector_to_array(v);
}

static Symbol * funcparams(Type ftype)
{
	BEGIN_CALL(funcparams);

    Symbol * syms;
    
    if (isdeclspec(tok) || (tok == ID && istypedefname(toklex.name))) {
        // prototype
        syms = parameter_type_list(ftype);
        ftype->u.f.proto = syms;
    }
    else {
        Vector v = new_vector();
        if (tok == ID) {
            // old style
            enterscope();
            for ( ; ; ) {
                Symbol s;
                if (tok != ID) {
                    ERROR("expect identifier at '%s'", tname(tok));
                    break;
                }
                s = declparam(toklex.name, inttype);
                vector_push_back(v, s);
                match(ID);
                if (tok != ',') {
                    break;
                }
                match(',');
            }
            ftype->u.f.proto = NULL;
            ftype->u.f.oldstyle = 1;
            exitscope();
        }
        syms = (Symbol *)vector_to_array(v);
    }

	END_CALL(funcparams);
    
    return syms;
}

static Type func_or_array()
{
    Type ty = NULL;
    BEGIN_CALL(func_or_array);
    
    for (; tok == '(' || tok == '['; ) {
        if (tok == '[') {
            Type atype;
            match('[');
            if (tok == ICONSTANT) {
                match(ICONSTANT);
            }
            match(']');
            NEW(atype);
            atype->op = ARRAY;
            atype->size = toklex.u.i;
            attach_type(&ty, atype);
        }
        else {
            match('(');
            Type ftype;
            NEW(ftype);
            ftype->op = FUNCTION;
            ftype->u.f.proto = funcparams(ftype);
            attach_type(&ty, ftype);
            match(')');
        }
    }
    
    END_CALL(func_or_array);
    
    return ty;
}

static Type abstract_func_or_array()
{
    Type ty = NULL;
    BEGIN_CALL(abstract_func_or_array);
    
    for (; tok == '(' || tok == '['; ) {
        if (tok == '[') {
            match('[');
            if (tok == ']') {
                match(']');
            }
            else if (tok == '*') {
                match('*');
                match(']');
            }
            else {
                assignment_expr();
                match(']');
            }
            Type atype;
            NEW(atype);
            atype->op = ARRAY;
            attach_type(&ty, atype);
        }
        else {
            match('(');
            Type ftype;
            NEW(ftype);
            ftype->op = FUNCTION;
            ftype->u.f.proto = parameter_type_list(ftype);
            attach_type(&ty, ftype);
            match(')');
        }
    }
    
    END_CALL(abstract_func_or_array);
    
    return ty;
}

static void direct_abstract_declarator(Type *type, const char **id)
{
    BEGIN_CALL(direct_abstract_declarator);
    
    assert(tok == '(' || tok == '[');
    
    if (tok == '[') {
        Type type2 = abstract_func_or_array();
        prepend_type(type, type2);
    }
    else if (tok == '(') {
        int t = lookahead();
        if (t == '*' || t == '(' || t == '[') {
            Type type1 = *type;
            match('(');
            declarator(type, id, ABSTRACT);
            match(')');
            if (tok == '(' || tok == '[') {
                Type type2;
                Type rtype = *type;
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
            Type type2 = abstract_func_or_array();
            prepend_type(type, type2);
        }
    }
    
    END_CALL(direct_abstract_declarator);
}

static void direct_declarator(Type *type, const char **id)
{
    BEGIN_CALL(direct_declarator);
    
    assert(tok == ID || tok == '(');
    
    if (tok == ID) {
        if (id) {
            *id = toklex.name;
        }
        match(ID);
        if (tok == '(' || tok == '[') {
            Type type2 = func_or_array();
            prepend_type(type, type2);
        }
    }
    else if (tok == '(') {
        Type type1 = *type;
        match('(');
        declarator(type, id, NON_ABSTRACT);
        match(')');
        if (tok == '(' || tok == '[') {
            Type type2 = NULL;
            Type rtype = *type;
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

static Type pointer()
{
    Type ty = NULL;
    int con, vol, res, type;
    assert(tok == '*');
    
    BEGIN_CALL(pointer);
    
    con = vol = res = type = 0;
    
    for (; ; ) {
        int *p, t = tok;
        switch (tok) {
            case CONST:
                p = &con;
                tok = gettok();
                break;
                
            case VOLATILE:
                p = &vol;
                tok = gettok();
                break;
                
            case RESTRICT:
                p = &res;
                tok = gettok();
                break;
                
            case '*':
            {
                Type ptype;
                NEW(ptype);
                ptype->op = POINTER;
                con = vol = res = type = 0;
                p = &type;
                prepend_type(&ty, ptype);
                tok = gettok();
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
            ERROR("duplicate type qulifier '%s'", tname(t));
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
static void declarator(Type *type, const char **id, int abstract)
{
    BEGIN_CALL(declarator);
    
    if (tok == '*') {
        Type ptype = pointer();
        prepend_type(type, ptype);
    }
    
    if (abstract == NON_ABSTRACT) {
        // direct declarator
        if (tok == ID || tok == '(') {
            direct_declarator(type, id);
        }
        else {
            ERROR("expect identifier or '('");
        }
    }
    else if (abstract == ABSTRACT) {
        // direct abstract declarators
        if (tok == '(' || tok == '[') {
            direct_abstract_declarator(type, id);
        }
        // can be pointer only
    }
    else if (abstract == COMPAT) {
        if (tok == ID) {
            direct_declarator(type, id);
        }
        else if (tok == '[') {
            direct_abstract_declarator(type, id);
        }
        else if (tok == '(') {
			int t = lookahead();
			if (t == '*' || t == ID || t == '(' || t == '[') {
				Type type1 = *type;
				match('(');
				declarator(type, id, abstract);
				match(')');
				if (tok == '(' || tok == '[') {
					Type type2;
					assert(id);
					if (*id) {
						type2 =  func_or_array();
					}
					else {
						type2 = abstract_func_or_array();
					}
					Type rtype = *type;
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
				Type ftype;
				NEW(ftype);
                ftype->op = FUNCTION;
				ftype->u.f.proto = parameter_type_list(ftype);
                attach_type(type, ftype);
                match(')');
				if (tok == '(' || tok == '[') {
					abstract_func_or_array();
				}
			}
        }
    }
    
    END_CALL(declarator);
}

static Type enum_decl()
{
    return NULL;
}

static Type record_decl()
{
    return NULL;
}

static Type declaration_specifier(int *sclass)
{
    int cls, sign, size, type;
    int cons, vol, res, inl;
    Type basety = NULL;
    
    BEGIN_CALL(decl_spec);
    
    cls = sign = size = type = 0;
    cons = vol = res = inl = 0;

    for (;;) {
        int *p, t = tok;
        switch (tok) {
            case AUTO: case EXTERN: case REGISTER:
            case STATIC: case TYPEDEF:
                p = &cls;
                tok = gettok();
                break;
                
            case CONST:
                p = &cons;
                tok = gettok();
                break;
                
            case VOLATILE:
                p = &vol;
                tok = gettok();
                break;
                
            case RESTRICT:
                p = &res;
                tok = gettok();
                break;
                
            case INLINE:
                p = &inl;
                tok = gettok();
                break;
                
            case ENUM:
                p = &type;
                basety = enum_decl();
                tok = gettok();
                break;
                
            case STRUCT:
            case UNION:
                p = &type;
                basety = record_decl();
                tok = gettok();
                break;
             
            case LONG:
                if (size == LONG) {
                    t = LONG + LONG;
                    size = 0;   // clear
                }
                // go through
            case SHORT:
                p = &size;
                tok = gettok();
                break;
            
            case FLOAT:
            case DOUBLE:
            case VOID:
            case CHAR:
            case INT:
                p = &type;
                basety = pretype(tok);
                tok = gettok();
                break;
                
            case SIGNED:
            case UNSIGNED:
                p = &sign;
                tok = gettok();
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
            ERROR("invalid syntax '%s' at '%s'", tname(*p), tname(t));
        }
        
        *p = t;
    }
    
    // default is int
    if (type == 0) {
        if (sign == 0 && size == 0) {
            if (stdc99) {
                ERROR("missing type specifier");
            }
            else {
                WARNING("missing type specifier, default is int");
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
            ERROR("%s %s %s is invalid", tname(size/2), tname(size/2), tname(type));
        }
        else {
            ERROR("%s %s is invalid", tname(size), tname(type));
        }
    }
    else if ((sign && type != INT && type != CHAR)) {
        ERROR("'%s' cannot be signed or unsigned", tname(type));
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

static Node declare(const char *id, Type ty)
{
    Node n = NULL;
    Symbol sym = lookupsym(id, identifiers);
    if (sym == NULL || equal_type(sym->type, ty)) {
        if (sym == NULL) {
            sym = installsym(id, &identifiers, scopelevel);
            sym->type = ty;
        }
        // new node for symbol
        if (isfunction(ty)) {
            FuncDecl fdecl = funcdecl_node(scopelevel);
            fdecl->d.n.s = sym;
            n = NODE(fdecl);
        }
        else {
            Decl vdecl = decl_node(VAR_DECL, scopelevel);
            vdecl->n.s = sym;
            n = NODE(vdecl);
        }
    }
    else {
        ERROR("Confliting types for %s", id);
    }
    
    printf("declare %s as ", id);
    printtype(ty);
    
    return n;
}

static Node declglobal(const char *id, Type ty)
{
    Symbol s;
    assert(scopelevel == GLOBAL);
    
    if (ty->sclass == 0) {
        ty = scls(EXTERN, ty);
    }
    else if (ty->sclass == AUTO || ty->sclass == REGISTER) {
        ERROR("invalid storage class specifier '%s' for '%s'", tname(ty->sclass), id);
        ty = scls(EXTERN, ty);
    }
    s = lookupsym(id, identifiers);
    if (s && s->scope == GLOBAL) {
        ERROR("redeclaratoin of '%s'", id);
    }
    if (s == NULL || s->scope != GLOBAL) {
        s = installsym(id, &identifiers, scopelevel);
    }
    s->type = ty;
    
    
    
    return NULL;
}

static Symbol declparam(const char *id, Type ty)
{
    Symbol sym;
    assert(scopelevel > GLOBAL);
    
    if (ty->sclass != 0 && ty->sclass != REGISTER) {
        ERROR("invalid storage class specifier '%s' for '%s' in parameter list", tname(ty->sclass), id);
    }
    
    sym = lookupsym(id, identifiers);
    if (sym && sym->scope == scopelevel) {
        ERROR("redeclaration '%s' in parameter list", id);
    }
    else {
        sym = installsym(id, &identifiers, scopelevel);
    }
    sym->type = ty;
    
    return sym;
}

static Node declocal(const char *id, Type ty)
{
    Node n = NULL;
    assert(scopelevel > PARAM);
    
    return n;
}

static void initializer()
{
    
}

void initializer_list()
{
    
}

Node * declaration()
{
    Vector v = new_vector();
    const char *id = NULL;
    Type basety, type = NULL;
    int sclass;
    
    BEGIN_CALL(declaration);
    
    basety = declaration_specifier(&sclass);
    if (tok == ID || tok == '*' || tok == '(') {
        declarator(&type, &id, NON_ABSTRACT);
        attach_type(&type, basety);
        type = scls(sclass, type);
        if (tok == ';' || tok == '=' || tok == ',') {
            Node n;
            // initializer
            if (tok == '=') {
                match('=');
                initializer();
            }
            // next declarator
            n = declare(id, type);
            if (n) {
                vector_push_back(v, n);
            }
            while (tok == ',') {
                match(',');
                id = NULL;
                type = NULL;
                if (tok == ID || tok == '*' || tok == '(') {
                    declarator(&type, &id, NON_ABSTRACT);
                    attach_type(&type, basety);
                    // initializer
                    if (tok == '=') {
                        match('=');
                        initializer();
                    }
                    n = declare(id, type);
                    if (n) {
                        vector_push_back(v, n);
                    }
                }
                else {
                    ERROR("invalid token '%s' in declaration", tname(tok));
                }
            }
            match(';');
        }
        else if (tok == '{' && isfunction(type)) {
            // function definiction
            if (scopelevel == GLOBAL) {
                CompoundStmt cs = compound_stmt();
                FuncDecl fdecl = (FuncDecl) declglobal(id, type);
                if (fdecl) {
                    Vector pv = new_vector();
                    for (int i=0; type->u.f.proto[i]; i++) {
                        Decl param_decl = decl_node(PARAMVAL_DECL, PARAM);
                        param_decl->n.s = type->u.f.proto[i];
                        vector_push_back(pv, param_decl);
                    }
                    fdecl->params = (Node *) vector_to_array(pv);
                    fdecl->cs = cs;
                    vector_push_back(v, fdecl);
                }
            }
            else {
                ERROR("function definition can only in global scope");
            }
        }
        else if ((kind(tok) & (SCLASS_SPEC|TYPE_QUAL|TYPE_SPEC) || tok == '(') &&
                 isfunction(type) && type->u.f.oldstyle) {
            // old style function
            
        }
        else {
            
        }
    }
    else if (tok == ';') {
        // struct/union/enum
        match(';');
    }
    else {
        ERROR("invalid token '%s' in declaration", tname(tok));
    }
    
    END_CALL(declaration);
    
    return (Node *) vector_to_array(v);
}

TranslationUnitDecl translation_unit()
{
    TranslationUnitDecl tudecl = tudecl_node();
    Vector v = new_vector();
    identifiers = table(NULL, GLOBAL);
    tok = gettok();
    for (; tok != EOI; ) {
        if (isdeclspec(tok) || tok == ID || tok == '(') {
            vector_add_from_array(v, (void *) declaration());
        }
        else if (tok == ';') {
            WARNING("empty declaration");
            match(';');
        }
        else {
            ERROR("invalid token '%s'", tname(tok));
            tok = gettok();
        }
    }
    tudecl->exts = (Node *) vector_to_array(v);
    printnode(NODE(tudecl));
    printreport();
    return tudecl;
}

