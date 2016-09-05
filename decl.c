#include "cc.h"

static void abstract_declarator(node_t ** ty);
static void declarator(node_t ** ty, struct token **id, node_t ***params);
static void param_declarator(node_t ** ty, struct token **id);
static node_t *ptr_decl(void);
static node_t *tag_decl(void);
static void ids(node_t *sym);
static void fields(node_t * sym);

static node_t *funcdef(const char *id, node_t * ty, int sclass, int fspec,
                       node_t *params[], struct source src);
static node_t *typedefdecl(const char *id, node_t * ty, int fspec, int kind, struct source src);

typedef node_t *declfun_p(const char *id, node_t * ty, int sclass, int fspec, struct source src);
static node_t *paramdecl(const char *id, node_t * ty, int sclass, int fspec, struct source src);
static node_t *globaldecl(const char *id, node_t * ty, int sclass, int fspec, struct source src);
static node_t *localdecl(const char *id, node_t * ty, int sclass, int fspec, struct source src);
static struct vector *decls(declfun_p * dcl);

static void finalize(void);
static void func_body(node_t *decl);

struct funcinfo funcinfo;

#define PACK_PARAM(prototype, first, fvoid, sclass)     \
    (((prototype) & 0x01) << 30) |                      \
    (((first) & 0x01) << 29) |                          \
    (((fvoid) & 0x01) << 28) |                          \
    ((sclass) & 0xffffff)

#define PARAM_STYLE(i)   (((i) & 0x40000000) >> 30)
#define PARAM_FIRST(i)   (((i) & 0x20000000) >> 29)
#define PARAM_FVOID(i)   (((i) & 0x10000000) >> 28)
#define PARAM_SCLASS(i)  ((i) & 0x0fffffff)

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

/// declaration-specifier:
///   storage-class-specifier declaration-specifiers[opt]
///   type-specifier          declaration-specifiers[opt]
///   type-qualifier          declaration-specifiers[opt]
///   function-specifier      declaration-specifiers[opt]
///
/// storage-class-specifier:
///   'auto'
///   'extern'
///   'register'
///   'static'
///   'typedef'
///
/// type-qualifier:
///   'const'
///   'volatile'
///   'restrict'
///
/// function-specifier:
///   'inline'
///
/// type-specifier:
///   'void'
///   'char'
///   'short'
///   'int'
///   'long'
///   'float'
///   'double'
///   'signed'
///   'unsigned'
///   '_Bool'
///   '_Complex'
///   '_Imaginary'
///   enum-specifier
///   struct-or-union-specifier
///   typedef-name
///
/// typedef-name:
///   identifier
///
static node_t *specifiers(int *sclass, int *fspec)
{
    int cls, sign, size, type;
    int cons, vol, res, inl;
    node_t *basety, *tydefty;
    int ci;                        // _Complex, _Imaginary

    basety = tydefty = NULL;
    cls = sign = size = type = 0;
    cons = vol = res = inl = 0;
    ci = 0;
    if (sclass == NULL)
        cls = AUTO;

    for (;;) {
        int *p, t = token->id;
        struct token *tok = token;
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
                    error_at(tok->src,
                             "duplicate storage class '%s'",
                             tok2s(tok));
                else
                    error_at(tok->src,
                             "type name does not allow storage class to be specified",
                             tok2s(tok));
            } else if (p == &inl) {
                if (fspec)
                    warning_at(tok->src,
                               "duplicate '%s' declaration specifier",
                               tok2s(tok));
                else
                    error_at(tok->src, "function specifier not allowed");
            } else if (p == &cons || p == &res || p == &vol) {
                warning_at(tok->src,
                           "duplicate '%s' declaration specifier",
                           tok2s(tok));
            } else if (p == &ci) {
                error_at(tok->src,
                         "duplicate _Complex/_Imaginary specifier '%s'",
                         tok2s(tok));
            } else if (p == &sign) {
                error_at(tok->src,
                         "duplicate signed/unsigned speficier '%s'",
                         tok2s(tok));
            } else if (p == &type || p == &size) {
                error_at(tok->src,
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
            warning_at(src, "duplicate type qualifier '%s'", id2s(*p));

        *p = t;
    }

    if (cons)
        TYPE_A_CONST(atype) = 1;
    if (vol)
        TYPE_A_VOLATILE(atype) = 1;
    if (res)
        TYPE_A_RESTRICT(atype) = 1;
}

static void exit_params(node_t *params[])
{
    assert(params);
    if (params[0] && !SYM_DEFINED(params[0]))
        error_at(AST_SRC(params[0]),
                 "a parameter list without types is only allowed in a function definition");
    
    if (SCOPE > PARAM)
        exit_scope();

    exit_scope();
}

/// parameter-type-list:
///   parameter-list
///   parameter-list ',' '...'
///
/// parameter-list:
///   parameter-declaration
///   parameter-list parameter-declaration
///
/// parameter-declaration:
///   declaration-specifier declarator
///   declaration-specifier abstract-declarator[opt]
///
static struct vector *prototype(node_t *ftype)
{
    struct vector *v = vec_new();
    bool first_void = false;
    
    for (int i = 0;; i++) {
        node_t *basety = NULL;
        int sclass, fspec;
        node_t *ty = NULL;
        struct token *id = NULL;
        node_t *sym;
        struct source src = source;

        basety = specifiers(&sclass, &fspec);
        param_declarator(&ty, &id);
        attach_type(&ty, basety);

        if (i == 0 && isvoid(ty))
            first_void = true;

        SAVE_ERRORS;
        sym = paramdecl(id ? TOK_IDENT_STR(id) : NULL,
                        ty, PACK_PARAM(1, i == 0, first_void, sclass), fspec,
                        id ? id->src : src);
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

/// identifier-list:
///   identifier
///   identifier-list ',' identifier
///
static struct vector *oldstyle(node_t *ftype)
{
    struct vector *v = vec_new();
    
    for (;;) {
        if (token->id == ID) {
            node_t *sym = paramdecl(TOK_IDENT_STR(token), inttype, 0, 0, token->src);
            SYM_DEFINED(sym) = false;
            vec_push(v, sym);
        }
        expect(ID);
        if (token->id != ',')
            break;
        expect(',');
    }

    if (SCOPE > PARAM)
        error("a parameter list without types is only allowed in a function definition");
    return v;
}

static node_t **parameters(node_t * ftype)
{
    node_t **params;

    if (first_decl(token)) {
        // prototype
        int i;
        struct vector *v;
        node_t **proto;
        
        v = prototype(ftype);
        params = vtoa(v, FUNC);
        proto = newarray(sizeof(node_t *), vec_len(v) + 1, PERM);
        for (i = 0; params[i]; i++)
            proto[i] = SYM_TYPE(params[i]);

        proto[i] = NULL;
        TYPE_PROTO(ftype) = proto;
        TYPE_OLDSTYLE(ftype) = 0;
    } else if (token->id == ID) {
        // oldstyle
        struct vector *v;
        
        v = oldstyle(ftype);
        params = vtoa(v, FUNC);
        TYPE_OLDSTYLE(ftype) = 1;
    } else if (token->id == ')') {
        params = vtoa(NULL, FUNC);
        TYPE_OLDSTYLE(ftype) = 1;
    } else {
        params = vtoa(NULL, FUNC);
        TYPE_OLDSTYLE(ftype) = 1;

        if (token->id == ELLIPSIS)
            error("ISO C requires a named parameter before '...'");
        else
            error("expect parameter declarator at '%s'", tok2s(token));
        gettok();
    }

    return params;
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

static node_t *func_or_array(bool abstract, node_t ***params)
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
            node_t **args;
            node_t *ftype = func_type();
            expect('(');
            /**
             * To make it easy to distinguish between 'paramaters in parameter'
             * and 'compound statement of function definition', they both may be
             * at scope LOCAL (aka PARAM+1), so enter scope again to make things
             * easy.
             */
            enter_scope();
            if (SCOPE > PARAM)
                enter_scope();
            args = parameters(ftype);
            if (params && *params == NULL)
                *params = args;
            else
                exit_params(args);
            match(')', follow);
            attach_type(&ty, ftype);
        }
    }

    return ty;
}

/// enum-specifier:
///   'enum' identifier[opt] '{' enumerator-list '}'
///   'enum' identifier[opt] '{' enumerator-list ',' '}'
///   'enum' identifier
///
/// struct-or-union-specifier:
///   struct-or-union identifier[opt] '{' struct-declaration-list '}'
///   struct-or-union identifier
///
/// struct-or-union:
///   'struct'
///   'union'
///
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
                error_at(src,
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

/// enumerator-list:
///   enumerator
///   enumerator-list ',' enumerator
///
/// enumerator:
///   enumeration-constant
///   enumeration-constant '=' constant-expression
///
/// enumeration-constant:
///   identifier
///
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

/// struct-declaration-list:
///   struct-declaration
///   struct-declaration-list struct-declaration
///
/// struct-declaration:
///   specifier-qualifier-list struct-declarator-list ';'
///
/// specifier-qualifier-list:
///   type-specifier specifier-qualifier-list[opt]
///   type-qualifier specifier-qualifier-list[opt]
///
/// struct-declarator-list:
///   struct-declarator
///   struct-declarator-list ',' struct-declarator
///
/// struct-declarator:
///   declarator
///   declarator[opt] ':' constant-expression
///
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
                size_t len = length(TYPE_FIELDS(basety));
                for (int i = 0; i < len; i++) {
                    node_t *field = TYPE_FIELDS(basety)[i];
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
                            error_at(id->src,
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

    TYPE_FIELDS(sty) = vtoa(v, PERM);
    set_typesize(sty);
}

/// pointer:
///   '*' type-qualifier-list[opt]
///   '*' type-qualifier-list[opt] pointer
///
/// type-qualifier-list:
///   type-qualifier
///   type-qualifier-list type-qualifier
///
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

/// abstract-declarator:
///   pointer
///   pointer[opt] direct-abstract-declarator
///
/// direct-abstract-declarator:
///   '(' abstract-declarator ')'
///   direct-abstract-declarator[opt] '[' assignment-expression[opt] ']'
///   direct-abstract-declarator[opt] '[' '*' ']'
///   direct-abstract-declarator[opt] '(' parameter-type-list[opt] ')'
///
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

/// declarator:
///   pointer[opt] direct-declarator
///
/// direct-declarator:
///   identifier
///   '(' declarator ')'
///   direct-declarator '[' type-qualifier-list[opt] assignment-expression[opt] ']'
///   direct-declarator '[' 'static' type-qualifier-list[opt] assignment-expression ']'
///   direct-declarator '[' type-qualifier-list 'static' assignment-expression ']'
///   direct-declarator '[' type-qualifier-list[opt] '*' ']'
///   direct-declarator '(' parameter-type-list ')'
///   direct-declarator '(' identifier-list[opt] ')'
///
static void declarator(node_t ** ty, struct token **id, node_t ***params)
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

static node_t * typedefdecl(const char *id, node_t *ty, int fspec, int kind, struct source src)
{
    int sclass = TYPEDEF;

    assert(id);
    assert(kind != PARAM);

    if (isfunc(ty))
        ensure_func(ty, src);
    else if (isarray(ty))
        ensure_array(ty, src, kind);

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

// id maybe NULL
static node_t *paramdecl(const char *id, node_t * ty, int sclass, int fspec, struct source src)
{
    node_t *sym = NULL;
    bool prototype = PARAM_STYLE(sclass);
    bool first = PARAM_FIRST(sclass);
    bool fvoid = PARAM_FVOID(sclass);
    sclass = PARAM_SCLASS(sclass);

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
            warning_at(src,
                       "declaration of '%s' will not be visible outside of this function",
                       type2s(ty));
    } else if (isvoid(ty)) {
        if (prototype) {
            if (first) {
                if (id)
                    error_at(src, "argument may not have 'void' type");
                else if (isqual(ty))
                    error_at(src, "'void' as parameter must not have type qualifiers");
            }
        } else {
            error_at(src, "argument may not have 'void' type");
        }
    }

    if (prototype && fvoid && !first)
        error_at(src, "'void' must be the first and only parameter if specified");

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
    SYM_DEFINED(sym) = true;
    
    if (token->id == '=') {
        error("C does not support default arguments");
        initializer(NULL);
    }

    return sym;
}

static node_t *localdecl(const char *id, node_t * ty, int sclass, int fspec, struct source src)
{
    node_t *sym = NULL;

    assert(id);
    assert(SCOPE >= LOCAL);

    // typedef
    if (sclass == TYPEDEF)
        return typedefdecl(id, ty, fspec, LOCAL, src);

    if (isfunc(ty)) {
        ensure_func(ty, src);
        ensure_main(ty, id, src);
        if (sclass && sclass != EXTERN) {
            error_at(src,
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

    if (token->id == '=') {
        node_t *init = decl_initializer(sym, sclass, LOCAL);
        SYM_INIT(sym) = init;
    }

    if (!globl) {
        if (SYM_SCLASS(sym) == STATIC)
            vec_push(funcinfo.staticvars, sym);
        else
            vec_push(funcinfo.localvars, sym);
    }

    return sym;
}

static node_t *globaldecl(const char *id, node_t *ty, int sclass, int fspec, struct source src)
{
    node_t *sym = NULL;

    assert(id);
    assert(SCOPE == GLOBAL);

    // typedef
    if (sclass == TYPEDEF)
        return typedefdecl(id, ty, fspec, GLOBAL, src);

    if (sclass == AUTO || sclass == REGISTER) {
        error_at(src, "illegal storage class on file-scoped variable");
        sclass = 0;
    }

    if (isfunc(ty)) {
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
            error_at(src,
                     "static declaration of '%s' follows non-static declaration",
                     id);
        else if (SYM_SCLASS(sym) == STATIC && sclass != STATIC)
            error_at(src,
                     "non-static declaration of '%s' follows static declaration",
                     id);
        if (sclass != EXTERN)
            SYM_SCLASS(sym) = sclass;
    } else {
        conflicting_types_error(src, sym);
    }

    if (token->id == '=') {
        node_t *init = decl_initializer(sym, sclass, GLOBAL);
        SYM_INIT(sym) = init;
        if (init)
            IR->defvar(sym);
    }

    return sym;
}

static void oldparam(node_t *sym, void *context)
{
    node_t **params = context;

    assert(SYM_NAME(sym));
    if (!isvardecl(sym)) {
        warning_at(AST_SRC(sym), "empty declaraion");
        return;
    }
        
    int j;
    for (j = 0; params[j]; j++) {
        node_t *s = params[j];
        if (SYM_NAME(s) && !strcmp(SYM_NAME(s), SYM_NAME(sym)))
            break;
    }

    if (params[j])
        params[j] = sym;
    else
        error_at(AST_SRC(sym), "parameter named '%s' is missing", SYM_NAME(sym));
}

static void make_funcdecl(node_t *sym, node_t *ty, int sclass, struct source src)
{
    SYM_TYPE(sym) = ty;
    AST_SRC(sym) = src;
    SYM_DEFINED(sym) = true;
    SYM_SCLASS(sym) = sclass;
}

// id maybe NULL
static node_t *funcdef(const char *id, node_t *ftype, int sclass, int fspec,
                       node_t *params[], struct source src)
{
    node_t *sym;
    // SCOPE == PARAM (prototype)
    // SCOPE == GLOBAL (oldstyle)

    if (sclass && sclass != EXTERN && sclass != STATIC) {
        error("invalid storage class specifier '%s'", id2s(sclass));
        sclass = 0;
    }
    
    if (id) {
        sym = lookup(id, identifiers);
        if (!sym || SYM_SCOPE(sym) != GLOBAL) {
            sym = install(id, &identifiers, GLOBAL);
            make_funcdecl(sym, ftype, sclass, src);
        } else if (eqtype(ftype, SYM_TYPE(sym)) && !SYM_DEFINED(sym)) {
            if (sclass == STATIC && SYM_SCLASS(sym) != STATIC)
                error_at(src,
                         "static declaaration of '%s' follows non-static declaration",
                         id);
            else
                make_funcdecl(sym, ftype, sclass, src);
        } else {
            redefinition_error(src, sym);
        }

        ensure_func(ftype, src);
        ensure_main(ftype, id, src);
        ensure_inline(ftype, fspec, src);
    } else {
        sym = anonymous(&identifiers, GLOBAL);
        make_funcdecl(sym, ftype, sclass, src);
    }

    // old style function parameters declaration
    if (TYPE_OLDSTYLE(ftype)) {
        enter_scope();
        assert(SCOPE == PARAM);
        /// declaration-list:
        ///   declaration
        ///   declaration-list declaration
        ///
        while (first_decl(token))
            decls(paramdecl);

        foreach(identifiers, PARAM, oldparam, params);

        for (int i = 0; params[i]; i++) {
            node_t *p = params[i];
            if (!SYM_DEFINED(p))
                params[i] = paramdecl(SYM_NAME(p), inttype, 0, 0, AST_SRC(p));
        }

        int i;
        node_t **proto = newarray(sizeof(node_t *), length(params) + 1, PERM);
        for (i = 0; params[i]; i++)
            proto[i] = SYM_TYPE(params[i]);

        proto[i] = NULL;
        TYPE_PROTO(ftype) = proto;
    
        if (token->id != '{')
            error("expect function body after function declarator");
    }

    TYPE_PARAMS(ftype) = params;
    ensure_params(params);

    if (token->id == '{') {
        // function definition
        func_body(sym);
        exit_scope();
        IR->defun(sym);
    }

    return sym;
}

/// type-name:
///   specifier-qualifier-list abstract-declarator[opt]
///
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

/// external-declaration:
///   declaration
///   function-definition
///
/// declaration:
///   declaration-specifier init-declarator-list[opt] ';'
///
/// function-definition:
///   declaration-specifier declarator declaration-list[opt] compound-statement
///
/// init-declarator-list:
///   init-declarator
///   init-declarator-list ',' init-declarator
///
/// init-declarator:
///   declarator
///   declarator '=' initializer
///
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
        node_t **params = NULL;        // for functioness
        struct source src = source;

        // declarator
        if (level == GLOBAL)
            declarator(&ty, &id, &params);
        else
            declarator(&ty, &id, NULL);
        attach_type(&ty, basety);

        if (level == GLOBAL && params) {
            if (isfunc(ty) && (token->id == '{' ||
                               (first_decl(token) && TYPE_OLDSTYLE(ty)))) {
                if (TYPE_OLDSTYLE(ty))
                    exit_scope();
                
                funcdef(id ? TOK_IDENT_STR(id) : NULL,
                        ty, sclass, fspec, params, id ? id->src : src);
                return NULL;
            } else {
                exit_params(params);
            }
        }

        for (;;) {
            if (id) {
                node_t *sym = dcl(TOK_IDENT_STR(id), ty, sclass, fspec, id->src);
                ensure_decl(sym, sclass, level);
                vec_push(v, sym);
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
        IR->deftype(basety);
    } else {
        error("invalid token '%s' in declaration", tok2s(token));
    }
    match(';', follow);

    return v;
}

node_t **declaration(void)
{
    assert(SCOPE >= LOCAL);
    return vtoa(decls(localdecl), FUNC);
}

/// translation-unit:
///   external-declaration
///   translation-unit external-declaration
///
void translation_unit(void)
{
    for (gettok(); token->id != EOI;) {
        if (first_decl(token)) {
            assert(SCOPE == GLOBAL);
            decls(globaldecl);
        } else {
            if (token->id == ';')
                // empty declaration
                gettok();
            else
                skipto(first_decl);
        }
    }
    
    finalize();
}

static void finalize(void)
{
    IR->finalize();
}

static void predefined_ids(void)
{
    {
        /**
         * Predefined identifier: __func__
         * The identifier __func__ is implicitly declared by C99
         * implementations as if the following declaration appeared
         * after the opening brace of each function definition:
         *
         * static const char __func__[] = "function-name";
         *
         */
        const char *name = "__func__";
        node_t *type = array_type(qual(CONST, chartype));
        node_t *sym = make_localvar(name, type, STATIC);
        SYM_PREDEFINE(sym) = true;
        // initializer
        node_t *literal = new_string_literal(funcinfo.name);
        init_string(type, literal);
        SYM_INIT(sym) = literal;
    }
}

static void backfill_labels(void)
{
    for (int i = 0; i < vec_len(funcinfo.gotos); i++) {
        node_t *goto_stmt = vec_at(funcinfo.gotos, i);
        const char *name = STMT_LABEL_NAME(goto_stmt);
        node_t *label_stmt = map_get(funcinfo.labels, name);
        if (label_stmt) {
            STMT_X_LABEL(goto_stmt) = STMT_X_LABEL(label_stmt);
            // update refs
            STMT_LABEL_REFS(label_stmt)++;
        } else {
            error_at(AST_SRC(goto_stmt), "use of undeclared label '%s'", name);
        }
    }
}

// static void warning_unused(void)
// {
//     for (int i = 0; i < vec_len(allvars); i++) {
//         node_t *decl = vec_at(allvars, i);
//         node_t *sym = DECL_SYM(decl);

//         // ONLY warning, not filter out
//         // because it may contains side-effect such as function calls.
//         if (SYM_REFS(sym) == 0) {
//             if (SYM_PREDEFINE(sym))
//                 continue;       // filter-out predefined symbols
//             else
//                 warning_at(AST_SRC(sym), "unused variable '%s'", SYM_NAME(sym));
//         }
//         if (SYM_SCLASS(sym) == STATIC) {
//             SYM_X_LABEL(sym) = gen_static_label();
//             vec_push(staticvars, decl);
//         } else {
//             vec_push(localvars, decl);
//         }
//     }
// }

static void func_body(node_t *sym)
{
    node_t *stmt;
    
    funcinfo.gotos = vec_new();
    funcinfo.labels = map_new();
    funcinfo.type = SYM_TYPE(sym);
    funcinfo.name = SYM_NAME(sym);
    funcinfo.staticvars = vec_new();
    funcinfo.localvars = vec_new();
    funcinfo.calls = vec_new();

    // compound statement
    stmt = compound_stmt(predefined_ids);
    // check goto labels
    backfill_labels();
    // check unused
    // warning_unused();

    // save
    SYM_X_LVARS(sym) = funcinfo.localvars;
    SYM_X_SVARS(sym) = funcinfo.staticvars;
    SYM_X_CALLS(sym) = funcinfo.calls;
    
    memset(&funcinfo, 0, sizeof(struct funcinfo));

    SYM_INIT(sym) = stmt;
}

node_t *make_localvar(const char *name, node_t * ty, int sclass)
{
    return localdecl(name, ty, sclass, 0, source);
}
