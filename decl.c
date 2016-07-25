#include "cc.h"

static void abstract_declarator(node_t ** ty);
static void declarator(node_t ** ty, struct token **id, int *params);
static void param_declarator(node_t ** ty, struct token **id);
static node_t *ptr_decl(void);
static node_t *tag_decl(void);
static void ids(node_t *sym);
static void fields(node_t * sym);

typedef node_t *declfun_p(struct token *id, node_t * ty, int sclass, int fspec);
static node_t *paramdecl(struct token *id, node_t * ty, int sclass, int fspec);
static node_t *globaldecl(struct token *id, node_t * ty, int sclass, int fspec);
static node_t *localdecl(struct token *id, node_t * ty, int sclass, int fspec);
static node_t *funcdef(struct token *id, node_t * ty, int sclass, int fspec);
static node_t *typedefdecl(struct token *id, node_t * ty, int fspec, int kind);
static struct vector *decls(declfun_p * dcl);

static void ensure_field(node_t * field, size_t total, bool last);
static void ensure_decl(node_t * decl, int sclass, int kind);
static void ensure_array(node_t * atype, struct source src, int level);
static void ensure_func(node_t * ftype, struct source src);
static void ensure_main(node_t *ftype, const char *name, struct source src);

static void decl_initializer(node_t * decl, int sclass, int kind);

#define PACK_PARAM(prototype, first, fvoid, sclass)     \
    (((prototype) & 0x01) << 30) |                      \
    (((first) & 0x01) << 29) |                          \
    (((fvoid) & 0x01) << 28) |                          \
    ((sclass) & 0xffffff)

#define PARAM_STYLE(i)   (((i) & 0x40000000) >> 30)
#define PARAM_FIRST(i)   (((i) & 0x20000000) >> 29)
#define PARAM_FVOID(i)   (((i) & 0x10000000) >> 28)
#define PARAM_SCLASS(i)  ((i) & 0x0fffffff)

static node_t *specifiers(int *sclass, int *fspec)
{
    int cls, sign, size, type;
    int cons, vol, res, inl;
    node_t *basety;
    int ci;                        // _Complex, _Imaginary
    node_t *tydefty = NULL;

    basety = NULL;
    cls = sign = size = type = 0;
    cons = vol = res = inl = 0;
    ci = 0;
    if (sclass == NULL)
        cls = AUTO;

    for (;;) {
        int *p, t = token->id;
        struct token *tok = token;
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
        case STRUCT:
        case UNION:
            p = &type;
            basety = tag_decl();
            break;

        case LONG:
            if (size == LONG) {
                t = LONG + LONG;
                size = 0;        // clear
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
            if (istypedef(TOK_IDENT_STR(token))) {
                tydefty = lookup_typedef(TOK_IDENT_STR(token));
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
            if (p == &cls) {
                if (sclass)
                    errorf(src,
                           "duplicate storage class '%s'",
                           tok2s(tok));
                else
                    errorf(src,
                           "type name does not allow storage class to be specified",
                           tok2s(tok));
            } else if (p == &inl) {
                if (fspec)
                    warningf(src,
                             "duplicate '%s' declaration specifier",
                             tok2s(tok));
                else
                    errorf(src, "function specifier not allowed");
            } else if (p == &cons || p == &res || p == &vol) {
                warningf(src,
                         "duplicate '%s' declaration specifier",
                         tok2s(tok));
            } else if (p == &ci) {
                errorf(src,
                       "duplicate _Complex/_Imaginary specifier '%s'",
                       tok2s(tok));
            } else if (p == &sign) {
                errorf(src,
                       "duplicate signed/unsigned speficier '%s'",
                       tok2s(tok));
            } else if (p == &type || p == &size) {
                errorf(src,
                       "duplicate type specifier '%s'",
                       tok2s(tok));
            } else {
                assert(0);
            }
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
            error("%s %s %s is invalid",
                  id2s(size / 2), id2s(size / 2), id2s(type));
        else
            error("%s %s is invalid", id2s(size), id2s(type));
    } else if (sign && type != INT && type != CHAR) {
        error("'%s' cannot be signed or unsigned", id2s(type));
    } else if (ci && type != DOUBLE && type != FLOAT) {
        error("'%s' cannot be %s", id2s(type), id2s(ci));
    }

    if (type == ID)
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
    if (fspec)
        *fspec = inl;

    return basety;
}

static void array_qualifiers(node_t * atype)
{
    int cons, vol, res;
    int *p;
    cons = vol = res = 0;
    while (token->kind == CONST) {
        int t = token->id;
        struct source src = source;
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
            warningf(src, "duplicate type qualifier '%s'", id2s(*p));

        *p = t;
    }

    if (cons)
        TYPE_A_CONST(atype) = 1;
    if (vol)
        TYPE_A_VOLATILE(atype) = 1;
    if (res)
        TYPE_A_RESTRICT(atype) = 1;
}

static void enter_params(void)
{
    /**
     * To make it easy to distinguish between 'paramaters in parameter'
     * and 'compound statement of function definition', they both may be
     * at scope LOCAL (aka PARAM+1), so enter scope again to make things
     * easy.
     */
    enter_scope();
    if (SCOPE > PARAM)
        enter_scope();
}

static void exit_params(void)
{
    if (SCOPE > PARAM)
        exit_scope();

    exit_scope();
}

static struct vector *prototype(node_t *ftype)
{
    struct vector *v = vec_new();
    TYPE_OLDSTYLE(ftype) = 0;
    bool first_void = false;
    
    for (int i = 0;; i++) {
        node_t *basety = NULL;
        int sclass, fspec;
        node_t *ty = NULL;
        struct token *id = NULL;
        node_t *sym;

        basety = specifiers(&sclass, &fspec);
        param_declarator(&ty, &id);
        attach_type(&ty, basety);

        if (i == 0 && isvoid(ty))
            first_void = true;

        SAVE_ERRORS;
        sym = paramdecl(id, ty,
                        PACK_PARAM(1, i == 0, first_void, sclass),
                        fspec);
        if (NO_ERROR && !first_void)
            vec_push(v, sym);
        
        if (token->id != ',')
            break;

        expect(',');
        if (token->id == ELLIPSIS) {
            if (!first_void)
                TYPE_VARG(ftype) = 1;
            else
                error("'void' must be the first and only parameter if specified");
            expect(ELLIPSIS);
            break;
        }
    }
    return v;
}

static struct vector *oldstyle(node_t *ftype)
{
    struct vector *v = vec_new();
    TYPE_OLDSTYLE(ftype) = 1;
    
    for (;;) {
        if (token->id == ID)
            vec_push(v, paramdecl(token, inttype, 0, 0));
        expect(ID);
        if (token->id != ',')
            break;
        expect(',');
    }

    if (SCOPE > PARAM)
        error("a parameter list without types is only allowed in a function definition");
    return v;
}

static struct vector *parameters(node_t * ftype, int *params)
{
    struct vector *ret = NULL;

    enter_params();

    if (first_decl(token)) {
        // prototype
        ret = prototype(ftype);
    } else if (token->id == ID) {
        // oldstyle
        ret = oldstyle(ftype);
    } else if (token->id == ')') {
        TYPE_OLDSTYLE(ftype) = 1;
    } else {
        TYPE_OLDSTYLE(ftype) = 1;
        if (token->id == ELLIPSIS)
            error("ISO C requires a named parameter before '...'");
        else
            error("expect parameter declarator at '%s'", tok2s(token));
        gettok();
    }

    if (params && *params == 0)
        *params = 1;
    else
        exit_params();

    return ret;
}

static void parse_assign(node_t *atype)
{
    node_t *assign = assign_expr();
    TYPE_A_ASSIGN(atype) =assign;

    if (!assign)
        return;

    if (isint(AST_TYPE(assign))) {
        // try evaluate the length
        node_t *ret = eval(assign, longtype);
        if (ret) {
            assert(isiliteral(ret));
            TYPE_LEN(atype) = ILITERAL_VALUE(ret);
            if ((long)ILITERAL_VALUE(ret) < 0)
                error("array has negative size");
        } else {
            error("expect constant expression");
        }
    } else {
        error("size of array has non-integer type '%s'",
              type2s(AST_TYPE(assign)));
    }
}

static node_t *arrays(bool abstract)
{
    node_t *atype = array_type(NULL);

    if (abstract) {
        if (token->id == '*') {
            if (lookahead()->id != ']') {
                parse_assign(atype);
            } else {
                expect('*');
                TYPE_A_STAR(atype) = 1;
            }
        } else if (first_expr(token)) {
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
                    TYPE_A_STAR(atype) = 1;
                }
            } else if (first_expr(token)) {
                parse_assign(atype);
            }
        } else if (token->id == '*') {
            if (lookahead()->id != ']') {
                parse_assign(atype);
            } else {
                expect('*');
                TYPE_A_STAR(atype) = 1;
            }
        } else if (first_expr(token)) {
            parse_assign(atype);
        }
    }

    return atype;
}

static node_t *func_or_array(bool abstract, int *params)
{
    node_t *ty = NULL;
    int follow[] = { '[', ID, IF, 0 };

    for (; token->id == '(' || token->id == '[';) {
        if (token->id == '[') {
            node_t *atype;
            expect('[');
            atype = arrays(abstract);
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

static node_t *tag_decl(void)
{
    int t = token->id;
    const char *id = NULL;
    node_t *sym = NULL;
    struct source src = source;
    int follow[] = {INT, CONST, STATIC, IF, 0};

    expect(t);
    if (token->id == ID) {
        id = TOK_IDENT_STR(token);
        expect(ID);
    }
    if (token->id == '{') {
        expect('{');
        sym = tag_type(t, id, src);
        if (t == ENUM)
            ids(sym);
        else
            fields(sym);
        match('}', follow);
        SYM_DEFINED(sym) = true;
    } else if (id) {
        sym = lookup(id, tags);
        if (sym) {
            if (is_current_scope(sym) && TYPE_OP(SYM_TYPE(sym)) != t)
                errorf(src,
                       "use of '%s' with tag type that does not match previous declaration '%s' at %s:%u:%u",
                       id2s(t), type2s(SYM_TYPE(sym)),
                       AST_SRC(sym).file,
                       AST_SRC(sym).line,
                       AST_SRC(sym).column);
        } else {
            sym = tag_type(t, id, src);
        }
    } else {
        error("expected identifier or '{'");
        sym = tag_type(t, NULL, src);
    }

    return SYM_TYPE(sym);
}

static void ids(node_t *sym)
{
    if (token->id == ID) {
        int val = 0;
        do {
            const char *name = TOK_IDENT_STR(token);
            node_t *s = lookup(name, identifiers);
            if (s && is_current_scope(s))
                redefinition_error(source, s);

            s = install(name, &identifiers, SCOPE);
            SYM_TYPE(s) = SYM_TYPE(sym);
            AST_SRC(s) = source;
            SYM_SCLASS(s) = ENUM;
            expect(ID);
            if (token->id == '=') {
                expect('=');
                val = intexpr();
            }
            SYM_VALUE_U(s) = val++;
            if (token->id != ',')
                break;
            expect(',');
        } while (token->id == ID);
    } else {
        error("expect identifier");
    }
}

static void bitfield(node_t *field)
{
    AST_SRC(field) = source;
    expect(':');
    FIELD_BITSIZE(field) = intexpr();
    FIELD_ISBIT(field) = true;
}

static void fields(node_t * sym)
{
    int follow[] = {INT, CONST, '}', IF, 0};
    node_t *sty = SYM_TYPE(sym);

    if (!first_decl(token)) {
        // supports empty record
        if (token->id != '}')
            error("expect type name or qualifiers");
        return;
    }
    
    struct vector *v = vec_new();
    do {
        node_t *basety = specifiers(NULL, NULL);

        for (;;) {
            node_t *field = new_field();
            if (token->id == ':') {
                bitfield(field);
                FIELD_TYPE(field) = basety;
            } else if (token->id == ';' &&
                       isrecord(basety) &&
                       is_anonymous(TYPE_TAG(basety))) {
                //C11: anonymous record
                size_t len = vec_len(TYPE_FIELDS(basety));
                for (int i = 0; i < len; i++) {
                    node_t *field = vec_at(TYPE_FIELDS(basety), i);
                    vec_push(v, field);
                    if (i < len - 1)
                        ensure_field(field, vec_len(v), false);
                }
                goto next;
            } else {
                node_t *ty = NULL;
                struct token *id = NULL;
                declarator(&ty, &id, NULL);
                attach_type(&ty, basety);
                if (token->id == ':')
                    bitfield(field);
                FIELD_TYPE(field) = ty;
                if (id) {
                    const char *name = TOK_IDENT_STR(id);
                    for (int i = 0; i < vec_len(v); i++) {
                        node_t *f = vec_at(v, i);
                        if (FIELD_NAME(f) &&
                            !strcmp(FIELD_NAME(f), name)) {
                            errorf(id->src,
                                   "redefinition of '%s'",
                                   name);
                            break;
                        }
                    }
                    FIELD_NAME(field) = name;
                    AST_SRC(field) = id->src;
                }
            }

            vec_push(v, field);
            if (token->id != ',')
                break;
            expect(',');
            ensure_field(field, vec_len(v), false);
        }
    next:
        match(';', follow);
        ensure_field(vec_tail(v), vec_len(v), isstruct(sty) && !first_decl(token));
    } while (first_decl(token));

    TYPE_FIELDS(sty) = v;
    set_typesize(sty);
}

static node_t *ptr_decl(void)
{
    node_t *ret = NULL;
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
            warning("duplicate type qulifier '%s'", tok2s(token));

        *p = t;

        if (t == CONST || t == VOLATILE || t == RESTRICT)
            ret = qual(t, ret);

        gettok();
    }

    return ret;
}

static void param_declarator(node_t ** ty, struct token **id)
{
    if (token->id == '*') {
        node_t *pty = ptr_decl();
        prepend_type(ty, pty);
    }

    if (token->id == '(') {
        if (first_decl(lookahead())) {
            abstract_declarator(ty);
        } else {
            node_t *type1 = *ty;
            node_t *rtype = NULL;
            expect('(');
            param_declarator(&rtype, id);
            expect(')');
            if (token->id == '(' || token->id == '[') {
                node_t *faty;
                assert(id);
                if (*id) {
                    faty = func_or_array(false, NULL);
                } else {
                    faty = func_or_array(true, NULL);
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

static void abstract_declarator(node_t ** ty)
{
    assert(ty);

    if (token->id == '*' || token->id == '(' || token->id == '[') {
        if (token->id == '*') {
            node_t *pty = ptr_decl();
            prepend_type(ty, pty);
        }

        if (token->id == '(') {
            if (first_decl(lookahead())) {
                node_t *faty = func_or_array(true, NULL);
                prepend_type(ty, faty);
            } else {
                node_t *type1 = *ty;
                node_t *rtype = NULL;
                expect('(');
                abstract_declarator(&rtype);
                expect(')');
                if (token->id == '[' || token->id == '(') {
                    node_t *faty = func_or_array(true, NULL);
                    attach_type(&faty, type1);
                    attach_type(&rtype, faty);
                }
                *ty = rtype;
            }
        } else if (token->id == '[') {
            node_t *faty = func_or_array(true, NULL);
            prepend_type(ty, faty);
        }
    } else {
        error("expect '(' or '[' at '%s'", tok2s(token));
    }
}

static void declarator(node_t ** ty, struct token **id, int *params)
{
    int follow[] = { ',', '=', IF, 0 };

    assert(ty && id);
    
    if (token->id == '*') {
        node_t *pty = ptr_decl();
        prepend_type(ty, pty);
    }

    if (token->id == ID) {
        *id = token;
        expect(ID);
        if (token->id == '[' || token->id == '(') {
            node_t *faty = func_or_array(false, params);
            prepend_type(ty, faty);
        }
    } else if (token->id == '(') {
        node_t *type1 = *ty;
        node_t *rtype = NULL;
        expect('(');
        declarator(&rtype, id, params);
        match(')', follow);
        if (token->id == '[' || token->id == '(') {
            node_t *faty = func_or_array(false, params);
            attach_type(&faty, type1);
            attach_type(&rtype, faty);
        } else {
            attach_type(&rtype, type1);
        }
        *ty = rtype;
    } else {
        error("expect identifier or '('");
    }
}

static bool first_funcdef(node_t * ty)
{
    bool prototype = token->id == '{';
    bool oldstyle =  first_decl(token) && TYPE_OLDSTYLE(ty);

    return isfunc(ty) && (prototype || oldstyle);
}

int first_decl(struct token *t)
{
    return t->kind == STATIC || first_typename(t);
}

int first_stmt(struct token *t)
{
    return t->kind == IF || first_expr(t);
}

int first_expr(struct token *t)
{
    return t->kind == ID;
}

bool first_typename(struct token * t)
{
    return t->kind == INT || t->kind == CONST ||
        (t->id == ID && istypedef(TOK_IDENT_STR(t)));
}

static node_t *make_decl(struct token *id, node_t * ty, int sclass,
                         int fspec, declfun_p * dcl)
{
    node_t *decl;
    if (sclass == TYPEDEF)
        decl = ast_decl(TYPEDEF_DECL);
    else if (isfunc(ty))
        decl = ast_decl(FUNC_DECL);
    else
        decl = ast_decl(VAR_DECL);
    node_t *sym = dcl(id, ty, sclass, fspec);

    DECL_SYM(decl) = sym;
    return decl;
}

static struct vector *decls(declfun_p * dcl)
{
    struct vector *v = vec_new();
    node_t *basety;
    int sclass, fspec;
    int level = SCOPE;
    int follow[] = {STATIC, INT, CONST, IF, '}', 0};

    basety = specifiers(&sclass, &fspec);
    if (token->id == ID || token->id == '*' || token->id == '(') {
        struct token *id = NULL;
        node_t *ty = NULL;
        int params = 0;        // for functioness

        // declarator
        if (level == GLOBAL)
            declarator(&ty, &id, &params);
        else
            declarator(&ty, &id, NULL);
        attach_type(&ty, basety);

        if (level == GLOBAL && params) {
            if (first_funcdef(ty)) {
                vec_push(v, funcdef(id, ty, sclass, fspec));
                return v;
            } else {
                exit_params();
            }
        }

        for (;;) {
            if (id) {
                int kind;
                if (dcl == globaldecl)
                    kind = GLOBAL;
                else if (dcl == paramdecl)
                    kind = PARAM;
                else
                    kind = LOCAL;
                node_t *decl = make_decl(id, ty, sclass, fspec, dcl);
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
        error("invalid token '%s' in declaration", tok2s(token));
    }
    match(';', follow);

    return v;
}

node_t *typename(void)
{
    node_t *basety;
    node_t *ty = NULL;

    basety = specifiers(NULL, NULL);
    if (token->id == '*' || token->id == '(' || token->id == '[')
        abstract_declarator(&ty);

    attach_type(&ty, basety);

    return ty;
}

struct vector *declaration(void)
{
    assert(SCOPE >= LOCAL);
    return decls(localdecl);
}

node_t *translation_unit(void)
{
    node_t *ret = ast_decl(TU_DECL);
    struct vector *v = vec_new();

    for (gettok(); token->id != EOI;) {
        if (first_decl(token)) {
            assert(SCOPE == GLOBAL);
            vec_add(v, decls(globaldecl));
        } else {
            if (token->id == ';')
                // empty declaration
                gettok();
            else
                skipto(FARRAY(first_decl));
        }
    }

    DECL_EXTS(ret) = v;
    return ret;
}

static void ensure_bitfield(node_t *field)
{
    const char *name = FIELD_NAME(field);
    node_t *ty = FIELD_TYPE(field);
    struct source src = AST_SRC(field);
    int bitsize = FIELD_BITSIZE(field);
    int bits = BITS(TYPE_SIZE(ty));

    if (!isint(ty)) {
        if (name)
            errorf(src,
                   "bit-field '%s' has non-integral type '%s'",
                   name, type2s(ty));
        else
            errorf(src,
                   "anonymous bit-field has non-integral type '%s'",
                   type2s(ty));
    }

    if (bitsize < 0) {
        if (name)
            errorf(src,
                   "bit-field '%s' has negative width '%d'",
                   name, bitsize);
        else
            errorf(src,
                   "anonymous bit-field has negative width '%d'",
                   bitsize);
    }

    if (bitsize == 0 && name)
        errorf(src,
               "named bit-field '%s' has zero width",
               name);

    if (bitsize > bits) {
        if (name)
            errorf(src,
                   "size of bit-field '%s' (%d bits) exceeds size of its type (%d bits)",
                   name, bitsize, bits);
        else
            errorf(src,
                   "anonymous bit-field (%d bits) exceeds size of its type (%d bits)",
                   bitsize, bits);
    }
}

static void ensure_nonbitfield(node_t * field, size_t total, bool last)
{
    node_t *ty = FIELD_TYPE(field);
    struct source src = AST_SRC(field);
        
    if (isarray(ty)) {
        ensure_array(ty, source, CONSTANT);
        if (isincomplete(ty)) {
            if (last) {
                if (total == 1)
                    errorf(src,
                           "flexible array cannot be the only member");
            } else {
                errorf(src,
                       "field has incomplete type '%s'",
                       type2s(ty));
            }
        }
    } else if (isfunc(ty)) {
        errorf(src, "field has invalid type '%s'", TYPE_NAME(ty));
    } else if (isincomplete(ty)) {
        errorf(src, "field has incomplete type '%s'", type2s(ty));
    }
}

static void ensure_field(node_t * field, size_t total, bool last)
{
    if (FIELD_ISBIT(field))
        ensure_bitfield(field);
    else
        ensure_nonbitfield(field, total, last);
}

static void ensure_decl(node_t * decl, int sclass, int kind)
{
    if (kind == PARAM)
        return;

    node_t *sym = DECL_SYM(decl);
    node_t *ty = SYM_TYPE(sym);
    struct source src = AST_SRC(sym);
    if (isvardecl(decl)) {
        if (isincomplete(ty) && SYM_DEFINED(sym))
            errorf(src, "variable has incomplete type '%s'",
                   type2s(ty));
    }
}

static void ensure_main(node_t *ftype, const char *name,
                        struct source src)
{
    if (!isfunc(ftype) || !name || strcmp(name, "main"))
        return;
    
    node_t *rty = rtype(ftype);
    struct vector *params = TYPE_PARAMS(ftype);
    size_t len = vec_len(params);
    if (rty != inttype)
        errorf(src, "return type of 'main' is not 'int'");
    for (int i = 0; i < MIN(3, len); i++) {
        node_t *param = vec_at(params, i);
        node_t *ty = SYM_TYPE(param);
        if (i == 0) {
            if (ty != inttype)
                errorf(src,
                       "first parameter of 'main' is not 'int'");
        } else if (i == 1 || i == 2) {
            if (!isptrto(ty, POINTER) ||
                !isptrto(rtype(ty), CHAR))
                errorf(src,
                       "%s parameter of 'main' is not 'char **'",
                       i == 1 ? "second" : "third");
        }
    }
    if (len == 1 || len > 3)
        errorf(src,
               "expect 0, 2 or 3 parameters for 'main', have %d",
               len);
}

static void ensure_func(node_t * ftype, struct source src)
{
    node_t *rty = rtype(ftype);
    if (isarray(rty))
        errorf(src, "function cannot return array type '%s'",
               type2s(rty));
    else if (isfunc(rty))
        errorf(src, "function cannot return function type '%s'",
               type2s(rty));
}

/**
 *  1. Array qualifiers may appear only when in a function parameter.
 *
 *  2. Array qualifiers 'const', 'volatile', 'restrict', 'static' may
 *     appear within the _outermost_ brackets.
 *
 *  3. 'static' is an optimization hint, asserting that the actual array
 *     argument will be non-null and will have the declared size and
 *     type upon entry to the function.
 *
 *  4. The star modifier '*' or non-constant expression describe a
 *     variable length array. The '*' can only appear in array parameter
 *     declarations within function prototypes that are not part of
 *     a function definition.
 */
static void ensure_array_sub(node_t *atype, struct source src, int level, bool outermost)
{
    if (TYPE_A_STAR(atype) && level != PARAM)
        errorf(src, "star modifier used outside of function prototype");
    
    if (TYPE_A_CONST(atype) || TYPE_A_RESTRICT(atype) ||
        TYPE_A_VOLATILE(atype) || TYPE_A_STATIC(atype)) {
        if (level != PARAM)
            errorf(src,
                   "type qualifier used in array declarator outside of function prototype");
        else if (!outermost)
            errorf(src,
                   "type qualifier used in non-outermost array type derivation");
    }
            

    node_t *rty = rtype(atype);
    if (isarray(rty))
        ensure_array_sub(rty, src, level, false);
    else if (isfunc(rty))
        errorf(src, "array of function is invalid");
    
    set_typesize(atype);
}

static void ensure_array(node_t * atype, struct source src, int level)
{
    ensure_array_sub(atype, src, level, true);

    node_t *rty = rtype(atype);
    if (isincomplete(rty))
        errorf(src,
               "array has incomplete element type '%s'",
               type2s(rty));
}

static void ensure_inline(node_t *ty, int fspec, struct source src)
{
    if (fspec == INLINE) {
        if (isfunc(ty))
            TYPE_INLINE(ty) = 1;
        else
            errorf(src, "'inline' can only appear on functions");
    }
}

static void check_oldstyle(node_t *ftype)
{
    assert(isfunc(ftype));
    
    if (TYPE_PARAMS(ftype) && TYPE_OLDSTYLE(ftype))
        error("a parameter list without types is only allowed in a function definition");
}

static node_t * typedefdecl(struct token *t, node_t * ty, int fspec, int kind)
{
    int sclass = TYPEDEF;

    assert(t);
    assert(kind != PARAM);
    
    const char *id = TOK_IDENT_STR(t);
    struct source src = t->src;

    if (isfunc(ty)) {
        if (kind == GLOBAL)
            check_oldstyle(ty);
        ensure_func(ty, src);
    } else if (isarray(ty)) {
        ensure_array(ty, src, kind);
    }

    ensure_inline(ty, fspec, src);

    node_t *sym = lookup(id, identifiers);
    if (sym && is_current_scope(sym))
        redefinition_error(src, sym);
    sym = install(id, &identifiers, SCOPE);
    SYM_TYPE(sym) = ty;
    AST_SRC(sym) = src;
    SYM_SCLASS(sym) = sclass;

    return sym;
}

static node_t *paramdecl(struct token *t, node_t * ty, int sclass,
                         int fspec)
{
    node_t *sym = NULL;
    bool prototype = PARAM_STYLE(sclass);
    bool first = PARAM_FIRST(sclass);
    bool fvoid = PARAM_FVOID(sclass);
    const char *id = NULL;
    struct source src = source;
    sclass = PARAM_SCLASS(sclass);

    if (t) {
        id = TOK_IDENT_STR(t);
        src = t->src;
    }

    if (sclass && sclass != REGISTER) {
        error("invalid storage class specifier '%s' in function declarator",
              id2s(sclass));
        sclass = 0;
    }

    if (isfunc(ty)) {
        ensure_func(ty, src);
        ty = ptr_type(ty);
    } else if (isarray(ty)) {
        ensure_array(ty, src, PARAM);
        node_t *aty = ty;
        ty = ptr_type(rtype(ty));
        if (TYPE_A_CONST(aty))
            ty = qual(CONST, ty);
        if (TYPE_A_VOLATILE(aty))
            ty = qual(RESTRICT, ty);
        if (TYPE_A_RESTRICT(aty))
            ty = qual(VOLATILE, ty);
    } else if (isenum(ty) || isstruct(ty) || isunion(ty)) {
        if (!SYM_DEFINED(TYPE_TSYM(ty)) ||
            SYM_SCOPE(TYPE_TSYM(ty)) == SCOPE)
            warningf(src,
                     "declaration of '%s' will not be visible outside of this function",
                     type2s(ty));
    } else if (isvoid(ty)) {
        if (prototype) {
            if (first) {
                if (id)
                    errorf(src,
                           "argument may not have 'void' type");
                else if (isqual(ty))
                    errorf(src,
                           "'void' as parameter must not have type qualifiers");
            }
        } else {
            errorf(src, "argument may not have 'void' type");
        }
    }

    if (prototype && fvoid && !first)
        errorf(src,
               "'void' must be the first and only parameter if specified");

    // check inline after conversion (decay)
    ensure_inline(ty, fspec, src);
        
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

static node_t *localdecl(struct token *t, node_t * ty, int sclass,
                         int fspec)
{
    node_t *sym = NULL;
    const char *id = TOK_IDENT_STR(t);
    struct source src = t->src;

    assert(id);
    assert(SCOPE >= LOCAL);

    // typedef
    if (sclass == TYPEDEF)
        return typedefdecl(t, ty, fspec, LOCAL);

    if (isfunc(ty)) {
        ensure_func(ty, src);
        ensure_main(ty, id, src);
        if (sclass && sclass != EXTERN) {
            errorf(src,
                   "function declared in block scope cannot have '%s' storage class",
                   id2s(sclass));
            sclass = 0;
        }
    } else if (isarray(ty)) {
        ensure_array(ty, src, LOCAL);
    }

    ensure_inline(ty, fspec, src);

    bool globl = isfunc(ty) || sclass == EXTERN;

    sym = lookup(id, identifiers);
    if (sym &&
        (is_current_scope(sym) ||
         (globl && (isfunc(SYM_TYPE(sym)) ||
                    SYM_SCLASS(sym) == EXTERN)))) {
        redefinition_error(src, sym);
    } else {
        sym = install(id, &identifiers, SCOPE);
        SYM_TYPE(sym) = ty;
        AST_SRC(sym) = src;
        SYM_SCLASS(sym) = sclass;
        if (!globl)
            SYM_DEFINED(sym) = true;
    }

    return sym;
}

static node_t *globaldecl(struct token *t, node_t * ty, int sclass,
                          int fspec)
{
    node_t *sym = NULL;
    const char *id = TOK_IDENT_STR(t);
    struct source src = t->src;

    assert(id);
    assert(SCOPE == GLOBAL);

    // typedef
    if (sclass == TYPEDEF)
        return typedefdecl(t, ty, fspec, GLOBAL);

    if (sclass == AUTO || sclass == REGISTER) {
        errorf(src, "illegal storage class on file-scoped variable");
        sclass = 0;
    }

    if (isfunc(ty)) {
        check_oldstyle(ty);
        ensure_func(ty, src);
        ensure_main(ty, id, src);
    } else if (isarray(ty)) {
        ensure_array(ty, src, GLOBAL);
    }

    ensure_inline(ty, fspec, src);

    sym = lookup(id, identifiers);
    if (!sym || SYM_SCOPE(sym) != SCOPE) {
        sym = install(id, &identifiers, SCOPE);
        SYM_TYPE(sym) = ty;
        AST_SRC(sym) = src;
        SYM_SCLASS(sym) = sclass;
    } else if (eqtype(ty, SYM_TYPE(sym))) {
        if (sclass == STATIC && SYM_SCLASS(sym) != STATIC)
            errorf(src,
                   "static declaration of '%s' follows "
                   "non-static declaration",
                   id);
        else if (SYM_SCLASS(sym) == STATIC && sclass != STATIC)
            errorf(src,
                   "non-static declaration of '%s' follows "
                   "static declaration",
                   id);
        if (sclass != EXTERN)
            SYM_SCLASS(sym) = sclass;
    } else {
        conflicting_types_error(src, sym);
    }

    return sym;
}

static void oldstyle_decls(node_t *ftype)
{
    struct vector *v = vec_new();
    enter_scope();
    while (first_decl(token))
        vec_add(v, decls(paramdecl));

    for (int i = 0; i < vec_len(v); i++) {
        node_t *decl = (node_t *) vec_at(v, i);
        node_t *sym = DECL_SYM(decl);

        assert(SYM_NAME(sym));
        if (!isvardecl(decl)) {
            warningf(AST_SRC(sym), "empty declaraion");
        } else if (TYPE_PARAMS(ftype)) {
            node_t *p = NULL;
            for (int i = 0; i < vec_len(TYPE_PARAMS(ftype)); i++) {
                node_t *s = vec_at(TYPE_PARAMS(ftype), i);
                if (SYM_NAME(s)
                    && !strcmp(SYM_NAME(s),
                               SYM_NAME(sym))) {
                    p = s;
                    break;
                }
            }
            if (p) {
                SYM_TYPE(p) = SYM_TYPE(sym);
                AST_SRC(p) = AST_SRC(sym);
            } else {
                errorf(AST_SRC(sym),
                       "parameter named '%s' is missing",
                       SYM_NAME(sym));
            }
        }
    }
    exit_scope();
    if (token->id != '{')
        error("expect function body after function declarator");
}

static void ensure_params(node_t *ftype)
{
    for (int i = 0; i < vec_len(TYPE_PARAMS(ftype)); i++) {
        node_t *sym = vec_at(TYPE_PARAMS(ftype), i);
        node_t *ty = SYM_TYPE(sym);
        SYM_DEFINED(sym) = true;
        // params id is required in prototype
        if (is_anonymous(SYM_NAME(sym)))
            errorf(AST_SRC(sym), "parameter name omitted");
        if (isenum(ty) || isstruct(ty) || isunion(ty)) {
            if (!SYM_DEFINED(TYPE_TSYM(ty)))
                errorf(AST_SRC(sym),
                       "variable has incomplete type '%s'",
                       type2s(ty));
        }
    }
}

static void make_funcdecl(node_t *sym, node_t *ty, int sclass, struct source src,
                          node_t *decl)
{
    SYM_TYPE(sym) = ty;
    AST_SRC(sym) = src;
    SYM_DEFINED(sym) = true;
    SYM_SCLASS(sym) = sclass;
    DECL_SYM(decl) = sym;
}

// token maybe NULL
static node_t *funcdef(struct token *t, node_t * ftype, int sclass,
                       int fspec)
{
    assert(SCOPE == PARAM);
    
    node_t *decl = ast_decl(FUNC_DECL);

    if (sclass && sclass != EXTERN && sclass != STATIC) {
        error("invalid storage class specifier '%s'", id2s(sclass));
        sclass = 0;
    }
    
    if (t) {
        const char *id = TOK_IDENT_STR(t);
        struct source src = t->src;
        node_t *sym = lookup(id, identifiers);
        if (!sym || SYM_SCOPE(sym) != GLOBAL) {
            sym = install(id, &identifiers, GLOBAL);
            make_funcdecl(sym, ftype, sclass, src, decl);
        } else if (eqtype(ftype, SYM_TYPE(sym)) && !SYM_DEFINED(sym)) {
            if (sclass == STATIC && SYM_SCLASS(sym) != STATIC)
                errorf(src,
                       "static declaaration of '%s' follows "
                       "non-static declaration",
                       id);
            else
                make_funcdecl(sym, ftype, sclass, src, decl);
        } else {
            redefinition_error(src, sym);
        }

        ensure_func(ftype, src);
        ensure_main(ftype, id, src);
        ensure_inline(ftype, fspec, src);
    } else {
        node_t *sym = anonymous(&identifiers, GLOBAL);
        make_funcdecl(sym, ftype, sclass, source, decl);
    }

    // old style function parameters declaration
    if (first_decl(token))
        oldstyle_decls(ftype);

    if (TYPE_PARAMS(ftype))
        ensure_params(ftype);

    if (token->id == '{') {
        // function definition
        func_body(decl);
        exit_scope();
    }

    return decl;
}

node_t *make_localdecl(const char *name, node_t * ty, int sclass)
{
    struct ident *ident = new_ident(cpp_file, name);
    struct token *id = new_token(&(struct token){
            .id = ID, .value.ident = ident, .kind = ID, .src = source});
    node_t *decl = make_decl(id, ty, sclass, 0, localdecl);
    return decl;
}

///
/// Initializer
///
static void struct_init(node_t * ty, bool brace, struct vector *v);
static void array_init(node_t * ty, bool brace, struct vector *v);
static void scalar_init(node_t * ty, struct vector *v);
static void elem_init(node_t * sty, node_t * ty, bool designated,
                      struct vector *v, int i);
static node_t *initializer(node_t * ty);

#define INIT_OVERRIDE    "initializer overrides prior initialization"

static bool first_init(struct token *t)
{
    return t->id == '[' || t->id == '.' || t->id == '{' || first_expr(t);
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
    } while (first_init(token));
}

static bool is_string(node_t * ty)
{
    if (!isarray(ty))
        return false;

    node_t *rty = rtype(ty);
    return TYPE_KIND(rty) == CHAR || unqual(rty) == wchartype;
}

static node_t *find_elem(struct vector *v, int i)
{
    for (int j = vec_len(v); j <= i; j++)
        vec_push(v, ast_vinit());
    return vec_at(v, i);
}

static node_t *init_elem_conv(node_t * ty, node_t * node)
{
    // VINIT_EXPR means failure.
    // cannot pass VINIT_EXPR to assignconv
    if (AST_ID(node) == VINIT_EXPR)
        return NULL;

    node_t *ret = assignconv(ty, node);
    if (ret == NULL)
        errorf(AST_SRC(node), INCOMPATIBLE_TYPES,
               type2s(AST_TYPE(node)), type2s(ty));

    return ret;
}

void init_string(node_t * ty, node_t * node)
{
    int len1 = TYPE_LEN(ty);
    int len2 = TYPE_LEN(AST_TYPE(node));
    if (len1 > 0) {
        if (len1 < len2 - 1)
            warning("initializer-string for char array is too long");
    } else if (isincomplete(ty)) {
        TYPE_LEN(ty) = len2;
        set_typesize(ty);
    }
}

static void aggregate_set(node_t * ty, struct vector *v, int i, node_t * node)
{
    if (!node)
        return;

    node_t *n = find_elem(v, i);
    if (AST_ID(n) != VINIT_EXPR)
        warningf(AST_SRC(node), INIT_OVERRIDE);

    if (AST_ID(node) == INITS_EXPR) {
        vec_set(v, i, node);
    } else if (is_string(ty) && issliteral(node)) {
        init_string(ty, node);
        vec_set(v, i, node);
    } else if (isrecord(ty) && isrecord(AST_TYPE(node))
               && eqtype(unqual(ty), unqual(AST_TYPE(node)))) {
        vec_set(v, i, node);
    } else {
        node_t *rty = NULL;
        if (isarray(ty)) {
            rty = rtype(ty);
        } else {
            if (vec_len(TYPE_FIELDS(ty))) {
                node_t *field = vec_head(TYPE_FIELDS(ty));
                rty = FIELD_TYPE(field);
            }
        }

        if (rty) {
            node_t *n1 = ast_inits(ty, source);
            struct vector *v1 = vec_new();
            vec_set(v, i, n1);

            if (isarray(rty) || isstruct(rty) || isunion(rty))
                aggregate_set(rty, v1, 0, node);
            else
                vec_push_safe(v1, init_elem_conv(rty, node));

            EXPR_INITS(n1) = v1;
        }
    }
}

static void scalar_set(node_t * ty, struct vector *v, int i, node_t * node)
{
    if (!node)
        return;

    node_t *n = find_elem(v, i);
    if (AST_ID(n) != VINIT_EXPR)
        warningf(AST_SRC(node), INIT_OVERRIDE);

    if (AST_ID(node) == INITS_EXPR) {
        struct vector *inits;
    loop:
        inits = EXPR_INITS(node);
        if (vec_len(inits)) {
            node = vec_head(inits);
            if (AST_ID(node) == INITS_EXPR)
                goto loop;
            vec_set_safe(v, i, init_elem_conv(ty, node));
        }
    } else {
        vec_set_safe(v, i, init_elem_conv(ty, node));
    }
}

static void struct_init(node_t * ty, bool brace, struct vector *v)
{
    bool designated = false;
    int len = vec_len(TYPE_FIELDS(ty));

    for (int i = 0;; i++) {
        node_t *fieldty = NULL;

        if (token->id == '.') {
            const char *name = NULL;
            expect('.');
            if (token->id == ID)
                name = TOK_IDENT_STR(token);
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

        if (!designated) {
            node_t *field = vec_at(TYPE_FIELDS(ty), i);
            fieldty = FIELD_TYPE(field);
        }
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

static void array_init(node_t * ty, bool brace, struct vector *v)
{
    bool designated = false;
    int c = 0;
    int len = TYPE_LEN(ty);

    if (is_string(ty) && token->id == SCONSTANT) {
        node_t *expr = assign_expr();
        if (vec_len(v)) {
            warningf(AST_SRC(expr), INIT_OVERRIDE);
            vec_clear(v);
        }
        aggregate_set(ty, v, 0, expr);
        return;
    }

    for (int i = 0;; i++) {
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
            error("array designator index [%d] exceeds array bounds (%d)",
                  i, len);
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

static void scalar_init(node_t * ty, struct vector *v)
{
    if (token->id == '.' || token->id == '[') {
        error("designator in initializer for scalar type '%s'",
              type2s(ty));
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

static bool is_string_vec(node_t *ty, struct vector *v)
{
    return is_string(ty) && vec_len(v) == 1 && issliteral((node_t *)vec_head(v));
}

static void elem_init(node_t * sty, node_t * ty, bool designated,
                      struct vector *v, int i)
{
    if (isunion(sty))
        i = 0;                // always set the first elem

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
                error("expect '=' or another designator at '%s'",
                      tok2s(token));
            aggregate_set(ty, v, i, initializer_list(ty));
        } else if ((token->id == '.' && isarray(ty)) ||
                   (token->id == '[' && !isarray(ty))) {
            SAVE_ERRORS;
            eat_initializer();
            // inhibit redundant errors
            if (NO_ERROR)
                error("%s designator cannot initialize non-%s type '%s'",
                      TYPE_NAME(ty), TYPE_NAME(ty), type2s(ty));
        } else {
            node_t *n = find_elem(v, i);
            struct vector *v1 = vec_new();
            if (AST_ID(n) == INITS_EXPR) {
                vec_add(v1, EXPR_INITS(n));
            } else if (AST_ID(n) == STRING_LITERAL) {
                vec_push(v1, n);
            }

            if (isarray(ty))
                array_init(ty, false, v1);
            else
                struct_init(ty, false, v1);

            if (is_string_vec(ty, v1)) {
                // string literal
                vec_set(v, i, (node_t *) vec_head(v1));
            } else {
                if (AST_ID(n) != INITS_EXPR) {
                    n = ast_inits(ty, source);
                    vec_set(v, i, n);
                }
                EXPR_INITS(n) = v1;
            }
        }
    } else {
        if (designated)
            expect('=');
        if (is_string_vec(sty, v)) {
            warning(INIT_OVERRIDE);
            vec_clear(v);
        }
        scalar_set(ty, v, i, initializer(ty));
    }
}

static node_t *initializer(node_t * ty)
{
    if (token->id == '{') {
        return initializer_list(ty);
    } else if (first_expr(token)) {
        return assign_expr();
    } else {
        error("expect '{' or assignment expression");
        return NULL;
    }
}

node_t *initializer_list(node_t * ty)
{
    int follow[] = { ',', IF, '[', ID, '.', DEREF, 0 };
    node_t *ret = ast_inits(ty, source);
    struct vector *v = vec_new();

    expect('{');
    if (first_init(token)) {
        if (ty) {
            if (isstruct(ty) || isunion(ty))
                struct_init(ty, true, v);
            else if (isarray(ty))
                array_init(ty, true, v);
            else
                scalar_init(ty, v);

            if (token->id == ',')
                expect(',');

            if (first_init(token)) {
                warning("excess elements in %s initializer at '%s'",
                        TYPE_NAME(ty), tok2s(token));
                eat_initlist();
            }
        } else {
            eat_initlist();
        }
    } else {
        // inhibit redundant errors
        if (ty)
            error("expect initializer at '%s'", tok2s(token));
    }

    match('}', follow);
    EXPR_INITS(ret) = v;
    return ret;
}

bool has_static_extent(node_t * sym)
{
    return SYM_SCLASS(sym) == EXTERN ||
        SYM_SCLASS(sym) == STATIC ||
        SYM_SCOPE(sym) == GLOBAL;
}

void decl_initializer(node_t * decl, int sclass, int kind)
{
    node_t *sym = DECL_SYM(decl);
    node_t *ty = SYM_TYPE(sym);
    struct source src = AST_SRC(sym);
    node_t *init;
    struct source init_src;

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

    init_src = AST_SRC(init);

    if (sclass == EXTERN) {
        if (kind == GLOBAL) {
            warningf(src, "'extern' variable has an initializer");
        } else {
            errorf(src,
                   "'extern' variable cannot have an initializer");
            return;
        }
    } else if (sclass == TYPEDEF) {
        errorf(src,
               "illegal initializer (only variable can be initialized)");
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
    }

    SAVE_ERRORS;
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

    if (NO_ERROR && init && has_static_extent(sym)) {
        init = eval(init, ty);
        if (init == NULL)
            errorf(init_src,
                   "initializer element is not a compile-time constant");
    }

    DECL_BODY(decl) = init;
}

void redefinition_error(struct source src, node_t * sym)
{
    errorf(src,
           "redefinition of '%s', previous definition at %s:%u:%u",
           SYM_NAME(sym),
           AST_SRC(sym).file,
           AST_SRC(sym).line,
           AST_SRC(sym).column);
}

void conflicting_types_error(struct source src, node_t * sym)
{
    errorf(src,
           "conflicting types for '%s', previous at %s:%u:%u",
           SYM_NAME(sym),
           AST_SRC(sym).file,
           AST_SRC(sym).line,
           AST_SRC(sym).column);
}

void field_not_found_error(node_t * ty, const char *name)
{
    if (isincomplete(ty))
        error("incomplete definition of type '%s'", type2s(ty));
    else
        error("'%s' has no field named '%s'", type2s(ty), name);
}
