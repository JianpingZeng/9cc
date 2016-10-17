#include <assert.h>
#include "cc.h"

static void param_declarator(struct type ** ty, struct token **id);
static struct type *tag_decl(void);
static void exit_params(struct symbol *params[]);

static void attach_type(struct type ** typelist, struct type * type)
{
    if (*typelist) {
        struct type *tp = *typelist;
        while (tp && tp->type)
            tp = tp->type;
        
        tp->type = type;
    } else {
        *typelist = type;
    }
}

static void prepend_type(struct type ** typelist, struct type * type)
{
    attach_type(&type, *typelist);
    *typelist = type;
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
            {
                struct symbol *sym = lookup_typedef(TOK_ID_STR(token));
                if (sym) {
                    use(sym);
                    tydefty = sym->type;
                    p = &type;
                    gettok();
                } else {
                    p = NULL;
                }
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

    while (1) {
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

        gettok();
        if (token->id == ELLIPSIS) {
            TYPE_VARG(ftype) = 1;
            gettok();
            break;
        }
    }

    // check
    return actions.prototype(ftype, ltoa(&list, FUNC));
}

/// identifier-list:
///   identifier
///   identifier-list ',' identifier
///
static struct symbol **oldstyle(struct type *ftype)
{
    struct list *params = NULL;

    while (1) {
        if (token->id == ID) {
            struct symbol *sym = actions.paramdecl(TOK_ID_STR(token), inttype, 0, 0, token->src);
            sym->defined = false;
            params = list_append(params, sym);
        }
        expect(ID);
        if (token->id != ',')
            break;
        gettok();
    }

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
            CC_UNAVAILABLE
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

static struct type *arrays(bool abstract)
{
    struct type *atype = array_type(NULL);

    //NOTE: '*' is in `first_expr`

    if (abstract) {
        if (token->id == '*' && lookahead()->id == ']') {
            gettok();
            TYPE_A_STAR(atype) = 1;
        } else if (first_expr(token)) {
            struct source src = source;
            actions.array_index(atype, assign_expr(), src);
        }
    } else {
        if (token->id == STATIC) {
            gettok();
            TYPE_A_STATIC(atype) = 1;
            array_qualifiers(atype);
            struct source src = source;
            actions.array_index(atype, assign_expr(), src);
        } else {
            array_qualifiers(atype);
            if (token->id == STATIC) {
                gettok();
                TYPE_A_STATIC(atype) = 1;
                struct source src = source;
                actions.array_index(atype, assign_expr(), src);
            } else if (token->id == '*' && lookahead()->id == ']') {
                gettok();
                TYPE_A_STAR(atype) = 1;
            } else if (first_expr(token)) {
                struct source src = source;
                actions.array_index(atype, assign_expr(), src);
            }
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
            gettok();
            atype = arrays(abstract);
            match(']', skip_to_squarebracket);
            attach_type(&ty, atype);
        } else {
            struct symbol **args;
            struct type *ftype = func_type(NULL);
            gettok();
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
                match(')', skip_to_bracket);
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
        gettok();
        if (token->id == '[' || token->id == '(') {
            struct type *faty = func_or_array(false, params);
            prepend_type(ty, faty);
        }
    } else if (token->id == '(') {
        struct type *type1 = *ty;
        struct type *rtype = NULL;
        gettok();
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

            error_at(src, ERR_REDEFINITION,
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
static void enum_body(struct symbol *sym)
{
    int val = 0;
    struct list *list = NULL;
    
    if (token->id != ID)
        error("expect identifier");

    while (token->id == ID) {
        const char *name = TOK_ID_STR(token);
        struct source src = source;
        gettok();
        if (token->id == '=') {
            gettok();
            val = intexpr();
        }
        struct symbol *p = actions.enum_id(name, val++, sym, src);
        list = list_append(list, p);
        if (token->id != ',')
            break;
        gettok();
    }

    actions.enumdecl(sym, ltoa(&list, PERM));
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
///       specifier-qualifier-list struct-declarator-list ';'
/// [C11] specifier-qualifier-list struct-declarator-list[opt] ';'
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
static void struct_body(struct symbol *sym)
{    
    while (first_decl(token)) {
        struct type *basety = specifiers(NULL, NULL);

        while (1) {
            struct field *field = alloc_field();
            if (token->id == ':') {
                bitfield(field);
                field->type = basety;
            } else if (token->id == ';' &&
                       isrecord(basety) &&
                       TYPE_TSYM(basety)->anonymous) {
                //C11: anonymous struct/union
                field->type = basety;
                field->src = source;
                actions.indirect_field(sym, field);
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
                    field->name = TOK_ID_STR(id);
                    field->src = id->src;
                }
            }

            // link
            actions.direct_field(sym, field);

            if (token->id != ',')
                break;
            gettok();
        }
    next:
        match(';', skip_to_decl);
    }

    actions.recorddecl(sym);
}

/// enum-specifier:
///   'enum' identifier[opt] '{' enumerator-list '}'
///   'enum' identifier[opt] '{' enumerator-list ',' '}'
///   'enum' identifier
///
/// struct-or-union-specifier:
///       struct-or-union identifier[opt] '{' struct-declaration-list '}'
///       struct-or-union identifier
/// [GNU] struct-or-union identifier[opt] '{' '}'
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

    gettok();                   // consume `t`
    if (token->id == ID) {
        id = TOK_ID_STR(token);
        gettok();
    }
    if (token->id == '{') {
        gettok();
        sym = tag_symbol(t, id, src);
        if (t == ENUM)
            enum_body(sym);
        else
            struct_body(sym);
        match('}', skip_to_brace);
    } else if (id) {
        sym = lookup(id, tags);
        if (sym) {
            if (is_current_scope(sym) && TYPE_OP(sym->type) != t)
                error_at(src,
                         "use of '%s' with tag type that does not match previous declaration '%s' at %s:%u:%u",
                         id2s(t), type2s(sym->type), sym->src.file, sym->src.line, sym->src.column);
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
            match(')', skip_to_bracket);
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
static void decls(struct symbol *(*dcl)(const char *, struct type *, int, int, struct source))
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
                if (TYPE_OLDSTYLE(ty)) {
                    exit_scope();
                    // start with a new table
                    enter_scope();
                    assert(cscope == PARAM);
                    /// declaration-list:
                    ///   declaration
                    ///   declaration-list declaration
                    ///
                    while (first_decl(token))
                        decls(actions.paramdecl);
                }

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

            gettok();
            id = NULL;
            ty = NULL;
            // declarator
            declarator(&ty, &id, NULL);
            attach_type(&ty, basety);
        }
    } else if (isenum(basety) || isstruct(basety) || isunion(basety)) {
        // struct/union/enum
        if (isstruct(basety) || isunion(basety)) {
            // anonymous record (can't be referenced)
            if (TYPE_TSYM(basety)->anonymous)
                warning("declaration does not declare anything");
        }
    } else {
        error("invalid token '%s' in declaration", tok2s(token));
    }
    match(';', skip_to_decl);
}

void declaration(void)
{
    assert(cscope >= LOCAL);
    decls(actions.localdecl);
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

    actions.finalize();
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
