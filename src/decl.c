#include "cc.h"

static void abstract_declarator(struct type **ty);
static void declarator(struct type **ty, const char **id, int *params);
static void param_declarator(struct type **ty, const char **id);
static struct type * ptr_decl();
static struct type * enum_decl();
static struct type * struct_decl();
typedef struct symbol * (*DeclFunc)(const char *id, struct type *ftype, int sclass,  struct source src);
static struct vector * decls(DeclFunc declfunc);
static struct symbol * paramdecl(const char *id, struct type *ty, int sclass,  struct source src);
static struct symbol * globaldecl(const char *id, struct type *ty, int sclass, struct source src);
static struct symbol * localdecl(const char *id, struct type *ty, int sclass, struct source src);
static struct node * funcdef(const char *id, struct type *ftype, int sclass,  struct source src);
static struct node * initializer();

static void conflicting_types_error(struct source src, struct symbol *sym)
{
    errorf(src, "conflicting types for '%s', previous at %s line %u",
           sym->name, sym->src.file, sym->src.line);
}

static void validate_func_or_array(struct type *ty)
{
    //TODO: ty->type may be null
    if (isfunc(ty) && ty->type) {
        if (isfunc(ty->type))
            error("function cannot return function type");
        else if (isarray(ty->type))
            error("function cannot return array type");
    } else if (isarray(ty) && ty->type && isfunc(ty->type)) {
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
                assert(0);
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

static struct symbol ** func_params(struct type *ftype, int *params)
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
        ftype->u.f.oldstyle = 1;
        struct vector *v = new_vector();
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
        ftype->u.f.oldstyle = 1;
    } else {
        error("invalid token '%s' in parameter list", token->name);
        gettok();
    }
    
    if (params) {
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
    
    for (; token->id == '(' || token->id == '['; ) {
        if (token->id == '[') {
            struct type *atype = array_type();
            expect('[');
            if (token->id == STATIC) {
                expect(STATIC);
                atype->u.a.is_static = 1;
                if (token->kind == CONST)
                    atype_qualifiers(atype);
                atype->u.a.assign = assign_expr();
            } else if (token->kind == CONST) {
                if (token->kind == CONST)
                    atype_qualifiers(atype);
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
            expect(']');
            attach_type(&ty, atype);
        } else {
            struct type *ftype = func_type();
            expect('(');
            ftype->u.f.params = func_params(ftype, params);
            expect(')');
            attach_type(&ty, ftype);
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
            ftype->u.f.params = func_params(ftype, NULL);
            expect(')');
            attach_type(&ty, ftype);
        }
    }
    
    // TODO
    validate_func_or_array(ty);
    
    return ty;
}

static struct type * ptr_decl()
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

static void abstract_declarator(struct type **ty)
{
    assert(ty);
    
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
    assert(ty && id);
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
        }
        *ty = rtype;
    } else {
        error("expect identifier or '(' before '%s'", token->name);
    }
}

static bool firstfuncdef(struct type *ty)
{
    bool f = isfunc(ty);
    bool prototype = token->id == '{';
    bool oldstyle = (istypename(token) || token->kind == STATIC) && ty->u.f.oldstyle && ty->u.f.params;
    
    return f && (prototype || oldstyle);
}

static struct vector * decls(DeclFunc dclf)
{
    struct vector *v = new_vector();
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
                struct node *decl;
                struct symbol *sym = dclf(id, ty, sclass, src);
                if (sclass == TYPEDEF)
                    decl = ast_decl(TYPEDEF_DECL, SCOPE);
                else
                    decl = ast_decl(VAR_DECL, SCOPE);
                
                decl->sym = sym;
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
        struct node *decl;
        if (isstruct(basety))
            node_id = STRUCT_DECL;
        else if (isunion(basety))
            node_id = UNION_DECL;
        else
            node_id = ENUM_DECL;
        
        decl = ast_decl(node_id, SCOPE);
        decl->sym = tag_sym(basety);
        vec_push(v, decl);
    } else {
        error("invalid token '%s' in declaration", token->name);
    }
    match(';', follow);
    
    return v;
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
        struct vector *v = new_vector();
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
        sym->type->u.s.ids = (struct symbol **)vtoa(v);
        sym->defined = true;
    } else if (id) {
        sym = lookup(id, tags);
        if (sym && currentscope(sym)) {
            if (!isenum(sym->type))
                errorf(src, "use of '%s %s' with tag type that does not match previous declaration '%s %s' at %s:%u",
                       tname(ENUM), id, sym->type->name, sym->type->tag,  sym->src.file, sym->src.line);
        } else {
            sym = tag_type(ENUM, id, src);
        }
    } else {
        error("expected identifier or '{'");
        sym = tag_type(ENUM, NULL, src);
    }
    
    return sym->type;
}

// TODO: not finished yet
static void fields(struct type *sty)
{
    int follow[] = {INT, CONST, '}', IF, 0};
    
    struct vector *v = new_vector();
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
                        error("bit-field '%s' has non-integral type '%s'", field->name, field->type->name);
                    else
                        error("anonymous bit-field has non-integral type '%s'", field->type->name);
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
    sty->u.s.fields = (struct field **)vtoa(v);
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
        if (sym && currentscope(sym)) {
            if (op(sym->type) != t)
                errorf(src, "use of '%s %s' with tag type that does not match previous declaration '%s %s' at %s:%u",
                       tname(t), id, sym->type->name, sym->type->tag, sym->src.file, sym->src.line);
        } else {
            sym = tag_type(t, id, src);
        }
    } else {
        error("expected identifier or '{'");
        sym = tag_type(t, NULL, src);
    }
    
    return sym->type;
}

static void update_params(void *elem, void *context)
{
    struct node *decl = (struct node *)elem;
    struct type *ftype = (struct type *)context;
    struct symbol *sym = decl->sym;
    
    assert(sym->name);
    if (decl->id != VAR_DECL) {
        warningf(sym->src, "empty declaraion");
    } else if (ftype->u.f.params) {
        struct symbol *p = NULL;
        for (int i=0; ftype->u.f.params[i]; i++) {
            struct symbol *s = ftype->u.f.params[i];
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
}

static struct symbol * paramdecl(const char *id, struct type *ty, int sclass,  struct source src)
{
    struct symbol *sym = NULL;
    if (sclass && sclass != REGISTER) {
        error("invalid storage class specifier '%s' in parameter list", tname(sclass));
        sclass = 0;
    }
    
    if (isfunc(ty)) {
        ty = ptr_type(ty);
    } else if (isarray(ty)) {
        // TODO: convert to poniter
    } else if (isenum(ty) || isstruct(ty) || isunion(ty)) {
        if (!tag_sym(ty)->defined)
            warningf(src, "declaration of '%s %s' will not be visible outside of this function",
                     ty->name, ty->tag);
    }
    
    if (id) {
        sym = lookup(id, identifiers);
        if (sym && sym->scope == SCOPE)
            redefinition_error(source, sym);
        sym = install(id, &identifiers, SCOPE);
        sym->type = ty;
        sym->src = src;
        sym->sclass = sclass;
    } else {
        sym = anonymous(&identifiers, SCOPE);
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
        expect('=');
        init_node = initializer();
    }
    
    if (init_node) {
        if (sclass == EXTERN)
            errorf(src, "'extern' variable cannot have an initializer");
        else if (sclass == TYPEDEF)
            errorf(src, "illegal initializer (only variable can be initialized)");
    }
    
    if (isfunc(ty)){
        if (ty->u.f.params && ty->u.f.oldstyle)
            error("a parameter list without types is only allowed in a function definition");
    } else if (isarray(ty)) {
        // TODO: convert to poniter
    } else if (isenum(ty) || isstruct(ty) || isunion(ty)) {
        if (!tag_sym(ty)->defined)
            error("incomplete type '%s %s'", ty->name, ty->tag);
    }
    validate_func_or_array(ty);
    
    sym = lookup(id, identifiers);
    if ((sym && sym->scope >= SCOPE) ||
        (sym && sym->scope == PARAM && SCOPE == LOCAL)) {
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
    struct node *init_node = NULL;
    
    assert(id);
    assert(SCOPE == GLOBAL);
    
    if (token->id == '=') {
        // initializer
        expect('=');
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
    
    if (isfunc(ty)) {
        if (ty->u.f.params && ty->u.f.oldstyle)
            error("a parameter list without types is only allowed in a function definition");
    } else if (isarray(ty)) {
        // TODO: convert to poniter
    } else if (isenum(ty) || isstruct(ty) || isunion(ty)) {
        if (!tag_sym(ty)->defined && sclass != TYPEDEF)
            error("incomplete type '%s %s'", ty->name, ty->tag);
    }
    validate_func_or_array(ty);
    
    sym = lookup(id, identifiers);
    if (!sym || sym->scope != SCOPE) {
        sym = install(id, &identifiers, SCOPE);
        sym->type = ty;
        sym->src = src;
        sym->defined = init_node ? true : false;
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

static struct node * funcdef(const char *id, struct type *ftype, int sclass,  struct source src)
{
    struct node *decl = ast_decl(FUNC_DECL, SCOPE);
    
    assert(SCOPE == PARAM);
    
    validate_func_or_array(ftype);
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
            decl->sym = sym;
        } else if (eqtype(ftype, sym->type) && !sym->defined) {
            if (sclass == STATIC && sym->sclass != STATIC) {
                errorf(src, "static declaaration of '%s' follows non-static declaration", id);
            } else {
                sym->type = ftype;
                sym->src = src;
                sym->defined = true;
                decl->sym = sym;
            }
        } else {
            redefinition_error(src, sym);
        }
    }
    
    if (ftype->u.f.params) {
        for (int i=0; ftype->u.f.params[i]; i++) {
            struct symbol *sym = ftype->u.f.params[i];
            sym->defined = true;
            // params id is required in prototype
            if (sym->name == NULL)
                errorf(sym->src, "parameter name omitted");
            if (isenum(sym->type) || isstruct(sym->type) || isunion(sym->type)) {
                if (!tag_sym(sym->type)->defined)
                    errorf(sym->src, "incomplete type '%s %s'", sym->type->name, sym->type->tag);
                else if (sym->type->tag)
                    warningf(sym->src, "declaration of '%s %s' will not be visible outside of this function", sym->type->name, sym->type->tag);
            }
        }
    }
    
    if (firstdecl(token)) {
        // old style function definition
        struct vector *v = new_vector();
        enter_scope();
        while (firstdecl(token))
            vec_add_from_array(v, (void **)decls(paramdecl));
        
        vec_foreach(v, update_params, ftype);
        exit_scope();
        if (token->id != '{') {
            error("expect function body after function declarator");
            // TODO
        }
    }
    
    if (token->id == '{') {
        // function definition
        // install symbol first for backward reference
        struct node *stmt = compound_stmt();
        exit_scope();
        LEFT(decl) = stmt;
    }
    
    return decl;
}

static struct node * initializer()
{
    if (token->id == '{') {
        // initializer list
        return initializer_list();
    } else if (firstexpr(token)) {
        // assign expr
        return assign_expr();
    } else {
        error("expect '{' or assignment expression");
        return NULL;
    }
}

struct node * initializer_list()
{
    int follow[] = {',', IF, '[', ID, '.', DEREF, 0};
    
    struct node *ret = ast_expr(INITS_EXPR, '{', NULL, NULL);
    struct vector *v = new_vector();
    expect('{');
    for (; token->id == '[' || token->id == '.' || token->id == '{'
         || firstexpr(token);) {
        struct node *lnode = NULL;
        struct node *rnode;
        
        if (token->id == '[' || token->id == '.') {
            for (; token->id == '[' || token->id == '.'; ) {
                if (token->id == '[') {
                    expect('[');
                    int i = intexpr();
                    expect(']');
                } else {
                    expect('.');
                    if (token->id == ID) {
                        
                    }
                    expect(ID);
                }
            }
            expect('=');
        }
        
        rnode = initializer();
        if (lnode) {
            struct node *assign_node = ast_bop('=', lnode, rnode);
            vec_push(v, assign_node);
        } else {
            vec_push(v, rnode);
        }
    }
    
    if (token->id == ',')
        expect(',');
    
    match('}', follow);
    ret->u.e.inits = (struct node **)vtoa(v);
    
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

struct node ** declaration()
{
    assert(SCOPE >= LOCAL);
    return (struct node **)vtoa(decls(localdecl));
}

struct node * translation_unit()
{
    struct node *ret = ast_decl(TU_DECL, GLOBAL);
    struct vector *v = new_vector();
    
    for (; token->id != EOI; ) {
        if (firstdecl(token)) {
            assert(SCOPE == GLOBAL);
            vec_add_from_vector(v, decls(globaldecl));
        } else if (token->id == ';') {
            gettok();
        } else {
            error("invalid token '%s'", token->name);
            gettok();
        }
    }
    
    ret->u.d.exts = (struct node **)vtoa(v);
    
    return ret;
}
