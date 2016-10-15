#include <assert.h>
#include "cc.h"

static void param_declarator(struct type ** ty, struct token **id);
static struct type *tag_decl(void);
static void exit_params(struct symbol *params[]);
static void doglobal(struct symbol *sym, void *context);

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
static struct type *specifiers(int *sclass, int *fspec)
{
    int cls, sign, size, type;
    int cons, vol, res, inl;
    struct type *basety, *tydefty;
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
            if (istypedef(TOK_ID_STR(token))) {
                tydefty = lookup_typedef(TOK_ID_STR(token));
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
                CC_UNAVAILABLE
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

static void array_qualifiers(struct type * atype)
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
static struct symbol **prototype(struct type *ftype)
{
    struct list *list = NULL;
    
    for (int i = 0;; i++) {
        struct type *basety = NULL;
        int sclass, fspec;
        struct type *ty = NULL;
        struct token *id = NULL;
        struct symbol *sym;
        struct source src = source;

        basety = specifiers(&sclass, &fspec);
        param_declarator(&ty, &id);
        attach_type(&ty, basety);

        sym = actions.paramdecl(id ? TOK_ID_STR(id) : NULL,
                                ty, sclass, fspec, id ? id->src : src);
        list = list_append(list, sym);
        
        if (token->id != ',')
            break;

        expect(',');
        if (token->id == ELLIPSIS) {
            TYPE_VARG(ftype) = 1;
            gettok();
            break;
        }
    }

    // check
    struct symbol **params = ltoa(&list, FUNC);
    ensure_prototype(ftype, params);

    return params;
}

/// identifier-list:
///   identifier
///   identifier-list ',' identifier
///
static struct symbol **oldstyle(struct type *ftype)
{
    struct list *params = NULL;
    
    for (;;) {
        if (token->id == ID) {
            struct symbol *sym = actions.paramdecl(TOK_ID_STR(token), inttype, 0, 0, token->src);
            sym->defined = false;
            params = list_append(params, sym);
        }
        expect(ID);
        if (token->id != ',')
            break;
        expect(',');
    }

    if (cscope > PARAM)
        error("a parameter list without types is only allowed in a function definition");

    return ltoa(&params, FUNC);
}

static struct symbol **parameters(struct type * ftype)
{
    struct symbol **params;

    if (first_decl(token)) {
        // prototype
        int i;
        struct type **proto;
        
        params = prototype(ftype);
        proto = newarray(sizeof(struct type *), length(params) + 1, PERM);
        for (i = 0; params[i]; i++)
            proto[i] = params[i]->type;

        proto[i] = NULL;
        TYPE_PROTO(ftype) = proto;
        TYPE_OLDSTYLE(ftype) = 0;
    } else if (token->id == ID) {
        // oldstyle
        params = oldstyle(ftype);
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

static void parse_assign(struct type *atype)
{
    struct expr *assign = assign_expr();
    TYPE_A_ASSIGN(atype) =assign;

    if (!assign)
        return;

    if (isint(assign->type)) {
        // try evaluate the length
        struct expr *ret = eval(assign, longtype);
        if (ret) {
            assert(isiliteral(ret));
            TYPE_LEN(atype) = ret->sym->value.i;
            if (ret->sym->value.i < 0)
                error("array has negative size");
        } else {
            error("expect constant expression");
        }
    } else {
        error("size of array has non-integer type '%s'",
              type2s(assign->type));
    }
}

static struct type *arrays(bool abstract)
{
    struct type *atype = array_type(NULL);

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

static struct type *func_or_array(bool abstract, struct symbol ***params)
{
    struct type *ty = NULL;

    for (; token->id == '(' || token->id == '[';) {
        if (token->id == '[') {
            struct type *atype;
            expect('[');
            atype = arrays(abstract);
            match(']', skip_to_squarebracket);
            attach_type(&ty, atype);
        } else {
            struct symbol **args;
            struct type *ftype = func_type(NULL);
            expect('(');
            /**
             * To make it easy to distinguish between 'paramaters in parameter'
             * and 'compound statement of function definition', they both may be
             * at scope LOCAL (aka PARAM+1), so enter scope again to make things
             * easy.
             */
            enter_scope();
            if (cscope > PARAM)
                enter_scope();
            args = parameters(ftype);
            if (params && *params == NULL)
                *params = args;
            else
                exit_params(args);
            match(')', skip_to_bracket);
            attach_type(&ty, ftype);
        }
    }

    return ty;
}

/// pointer:
///   '*' type-qualifier-list[opt]
///   '*' type-qualifier-list[opt] pointer
///
/// type-qualifier-list:
///   type-qualifier
///   type-qualifier-list type-qualifier
///
static struct type *ptr_decl(void)
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
            warning("duplicate type qulifier '%s'", tok2s(token));

        *p = t;

        if (t == CONST || t == VOLATILE || t == RESTRICT)
            ret = qual(t, ret);

        gettok();
    }

    return ret;
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
static void abstract_declarator(struct type ** ty)
{
    assert(ty);

    if (token->id == '*' || token->id == '(' || token->id == '[') {
        if (token->id == '*') {
            struct type *pty = ptr_decl();
            prepend_type(ty, pty);
        }

        if (token->id == '(') {
            if (first_decl(lookahead())) {
                struct type *faty = func_or_array(true, NULL);
                prepend_type(ty, faty);
            } else {
                struct type *type1 = *ty;
                struct type *rtype = NULL;
                expect('(');
                abstract_declarator(&rtype);
                expect(')');
                if (token->id == '[' || token->id == '(') {
                    struct type *faty = func_or_array(true, NULL);
                    attach_type(&faty, type1);
                    attach_type(&rtype, faty);
                } else {
                    attach_type(&rtype, type1);
                }
                *ty = rtype;
            }
        } else if (token->id == '[') {
            struct type *faty = func_or_array(true, NULL);
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
static void declarator(struct type ** ty, struct token **id, struct symbol ***params)
{
    assert(ty && id);
    
    if (token->id == '*') {
        struct type *pty = ptr_decl();
        prepend_type(ty, pty);
    }

    if (token->id == ID) {
        *id = token;
        expect(ID);
        if (token->id == '[' || token->id == '(') {
            struct type *faty = func_or_array(false, params);
            prepend_type(ty, faty);
        }
    } else if (token->id == '(') {
        struct type *type1 = *ty;
        struct type *rtype = NULL;
        expect('(');
        declarator(&rtype, id, params);
        match(')', skip_to_bracket);
        if (token->id == '[' || token->id == '(') {
            struct type *faty = func_or_array(false, params);
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

static struct symbol *tag_symbol(int t, const char *tag, struct source src)
{
    struct type *ty = tag_type(t, tag);
    struct symbol *sym = NULL;
    if (tag) {
        sym = lookup(tag, tags);
        if (sym && is_current_scope(sym)) {
            if (TYPE_OP(sym->type) == t && !sym->defined)
                return sym;

            error_at(src, REDEFINITION_ERROR,
                     sym->name, sym->src.file, sym->src.line, sym->src.column);
        }

        sym = install(tag, &tags, cscope, PERM);
    } else {
        sym = anonymous(&tags, cscope, PERM);
        ty->u.s.tag = sym->name;
    }

    sym->type = ty;
    sym->src = src;
    ty->u.s.tsym = sym;

    return sym;
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
static void ids(struct symbol *sym)
{
    if (token->id == ID) {
        int val = 0;
        do {
            const char *name = TOK_ID_STR(token);
            struct symbol *s = lookup(name, identifiers);
            if (s && is_current_scope(s))
                error(REDEFINITION_ERROR, s->name, s->src.file, s->src.line, s->src.column);

            s = install(name, &identifiers, cscope, cscope < LOCAL ? PERM : FUNC);
            s->type = sym->type;
            s->src = source;
            s->sclass = ENUM;
            expect(ID);
            if (token->id == '=') {
                expect('=');
                val = intexpr();
            }
            s->value.u = val++;
            if (token->id != ',')
                break;
            expect(',');
        } while (token->id == ID);
    } else {
        error("expect identifier");
    }
}

static void bitfield(struct field *field)
{
    field->src = source;
    expect(':');
    field->bitsize = intexpr();
    field->isbit = true;
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
static void fields(struct symbol * sym)
{
    struct type *sty = sym->type;

    if (!first_decl(token)) {
        // supports empty record
        if (token->id != '}')
            error("expect type name or qualifiers");
        return;
    }
    
    struct vector *v = vec_new();
    do {
        struct type *basety = specifiers(NULL, NULL);

        for (;;) {
            struct field *field = alloc_field();
            if (token->id == ':') {
                bitfield(field);
                field->type = basety;
            } else if (token->id == ';' &&
                       isrecord(basety) &&
                       is_anonymous(TYPE_TAG(basety))) {
                //C11: anonymous record
                size_t len = length(TYPE_FIELDS(basety));
                for (int i = 0; i < len; i++) {
                    struct field *field = TYPE_FIELDS(basety)[i];
                    vec_push(v, field);
                    if (i < len - 1)
                        ensure_field(field, vec_len(v), false);
                }
                goto next;
            } else {
                struct type *ty = NULL;
                struct token *id = NULL;
                declarator(&ty, &id, NULL);
                attach_type(&ty, basety);
                if (token->id == ':')
                    bitfield(field);
                field->type = ty;
                if (id) {
                    const char *name = TOK_ID_STR(id);
                    for (int i = 0; i < vec_len(v); i++) {
                        struct field *f = vec_at(v, i);
                        if (f->name && !strcmp(f->name, name)) {
                            error_at(id->src, "redefinition of '%s'", name);
                            break;
                        }
                    }
                    field->name = name;
                    field->src = id->src;
                }
            }

            vec_push(v, field);
            if (token->id != ',')
                break;
            expect(',');
            ensure_field(field, vec_len(v), false);
        }
    next:
        match(';', skip_to_decl);
        ensure_field(vec_tail(v), vec_len(v), isstruct(sty) && !first_decl(token));
    } while (first_decl(token));

    TYPE_FIELDS(sty) = vtoa(v, PERM);
    set_typesize(sty);
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
/// [GNU]:
///   struct-or-union identifier[opt] '{' '}'
///
/// struct-or-union:
///   'struct'
///   'union'
///
static struct type *tag_decl(void)
{
    int t = token->id;
    const char *id = NULL;
    struct symbol *sym = NULL;
    struct source src = source;

    expect(t);
    if (token->id == ID) {
        id = TOK_ID_STR(token);
        expect(ID);
    }
    if (token->id == '{') {
        expect('{');
        sym = tag_symbol(t, id, src);
        if (t == ENUM)
            ids(sym);
        else
            fields(sym);
        match('}', skip_to_brace);
        sym->defined = true;
    } else if (id) {
        sym = lookup(id, tags);
        if (sym) {
            if (is_current_scope(sym) && TYPE_OP(sym->type) != t)
                error_at(src,
                         "use of '%s' with tag type that does not match previous declaration '%s' at %s:%u:%u",
                         id2s(t), type2s(sym->type),
                         sym->src.file,
                         sym->src.line,
                         sym->src.column);
        } else {
            sym = tag_symbol(t, id, src);
        }
    } else {
        error("expected identifier or '{'");
        sym = tag_symbol(t, NULL, src);
    }

    return sym->type;
}

static void param_declarator(struct type ** ty, struct token **id)
{
    if (token->id == '*') {
        struct type *pty = ptr_decl();
        prepend_type(ty, pty);
    }

    if (token->id == '(') {
        if (first_decl(lookahead())) {
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
                if (*id)
                    faty = func_or_array(false, NULL);
                else
                    faty = func_or_array(true, NULL);

                attach_type(&faty, type1);
                attach_type(&rtype, faty);
            } else {
                attach_type(&rtype, type1);
            }
            *ty = rtype;
        }
    } else if (token->id == '[') {
        abstract_declarator(ty);
    } else if (token->id == ID) {
        declarator(ty, id, NULL);
    }
}

static void exit_params(struct symbol *params[])
{
    assert(params);
    if (params[0] && !params[0]->defined)
        error_at(params[0]->src,
                 "a parameter list without types is only allowed in a function definition");
    
    if (cscope > PARAM)
        exit_scope();

    exit_scope();
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
void decls(struct symbol *(*dcl)(const char *, struct type *, int, int, struct source))
{
    struct type *basety;
    int sclass, fspec;
    int level = cscope;

    basety = specifiers(&sclass, &fspec);
    if (token->id == ID || token->id == '*' || token->id == '(') {
        struct token *id = NULL;
        struct type *ty = NULL;
        struct symbol **params = NULL;        // for functioness
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
                
                actions.funcdef(id ? TOK_ID_STR(id) : NULL,
                                ty, sclass, fspec, params, id ? id->src : src);
                return;
            } else {
                exit_params(params);
            }
        }

        for (;;) {
            if (id) {
                if (sclass == TYPEDEF)
                    actions.typedefdecl(TOK_ID_STR(id), ty, fspec, level, id->src);
                else
                    dcl(TOK_ID_STR(id), ty, sclass, fspec, id->src);
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
        actions.deftype(TYPE_TSYM(basety));
    } else {
        error("invalid token '%s' in declaration", tok2s(token));
    }
    match(';', skip_to_decl);
}

/// translation-unit:
///   external-declaration
///   translation-unit external-declaration
///
void translation_unit(void)
{
    for (gettok(); token->id != EOI;) {
        if (first_decl(token)) {
            assert(cscope == GLOBAL);
            decls(actions.globaldecl);
            deallocate(FUNC);
        } else {
            if (token->id == ';') {
                // empty declaration
                gettok();
            } else {
                error("expect declaration");
                skip_to_decl();
            }
        }
    }
    
    foreach(identifiers, GLOBAL, doglobal, NULL);
}

/// type-name:
///   specifier-qualifier-list abstract-declarator[opt]
///
struct type *typename(void)
{
    struct type *basety;
    struct type *ty = NULL;

    basety = specifiers(NULL, NULL);
    if (token->id == '*' || token->id == '(' || token->id == '[')
        abstract_declarator(&ty);

    attach_type(&ty, basety);

    return ty;
}

static void doglobal(struct symbol *sym, void *context)
{
    if (sym->sclass == EXTERN ||
        isfunc(sym->type) ||
        sym->defined)
        return;

    actions.defgvar(sym);
}
