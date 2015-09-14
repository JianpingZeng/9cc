#include "cc.h"

static union node * cast_expr();
static union node * cond_expr();
static union node * cond_expr1(union node *o);
static union node * unary_expr();
static union node * uop(int op, struct type *ty, union node *l);
static union node * bop(int op, union node *l, union node *r);
static union node * conv(union node *node);
static struct type * conv2(struct type *l, struct type *r);
static union node * wrap(struct type *ty, union node *node);
static union node * eval(union node *expr);

#define SAVE_ERRORS    unsigned err = errors
#define NO_ERROR       (err == errors)

static unsigned escape(const char **ps)
{
    unsigned c = 0;
    const char *s = *ps;
    assert(*s == '\\');
    s += 1;
    switch (*s++) {
    case 'a': c = 7; break;
    case 'b': c = '\b'; break;
    case 'f': c = '\f'; break;
    case 'n': c = '\n'; break;
    case 'r': c = '\r'; break;
    case 't': c = '\t'; break;
    case 'v': c = '\v'; break;
    case '\'': case '"':
    case '\\': case '\?':
	c = s[-1];
	break;
    case '0': case '1': case '2':
    case '3': case '4': case '5':
    case '6': case '7':
	c = s[-1] - '0';
	if (*s >= '0' && *s <= '7') {
	    c = (c<<3) + (*s++) - '0';
	    if (*s >= '0' && *s <= '7')
		c = (c<<3) + (*s++) - '0';
	}
	break;
    case 'x':
        {
            bool overflow = 0;
            for (;is_digithex(*s);) {
                if (overflow) {
                    s++;
                    continue;
                }
                if (c >> (BITS(wchartype) - 4)) {
                    overflow = 1;
                    error("hex escape sequence out of range");
                } else {
                    if (is_digit(*s))
                        c = (c<<4) + *s - '0';
                    else
                        c = (c<<4) + (*s & 0x5f) - 'A' + 10;
                }
                s++;
            }
        }
	break;
    case 'u': case 'U':
        {
            int x = 0;
            int n = s[-1] == 'u' ? 4 : 8;
            for (;is_digithex(*s); x++, s++) {
                if (x == n)
                    break;
                if (is_digit(*s))
                    c = (c<<4) + *s - '0';
                else
                    c = (c<<4) + (*s & 0x5f) - 'A' + 10;
            }
        }
	break;
    default:
	c = s[-1];
	break;
    }
    
    *ps = s;
    return c;
}

static void char_constant(struct token *t, struct symbol *sym)
{
    const char *s = t->name;
    bool wide = s[0] == 'L';
    unsigned long long c = 0;
    char ws[MB_LEN_MAX];
    int len = 0;
    bool overflow = 0;
    bool char_rec = 0;
    wide ? (s += 2) : (s += 1);
    
    for (;*s != '\'';) {
        if (char_rec)
            overflow = 1;
        if (*s == '\\') {
            c = escape(&s);
            char_rec = 1;
        } else {
            if (wide) {
                if (len >= MB_LEN_MAX)
                    error("multibyte character overflow");
                else
                    ws[len++] = (char) *s++;
            } else {
                c = *s++;
                char_rec = 1;
            }
        }
    }
    
    if (!char_rec && !len)
        error("incomplete character constant: %s", t->name);
    else if (overflow)
        error("extraneous characters in character constant: %s", t->name);
    else if ((!wide && c > unsignedchartype->limits.max.u) ||
             (wide && c > wchartype->limits.max.u))
        error("character constant overflow: %s", t->name);
    else if (len && mbtowc((wchar_t *)&c, ws, len) != len)
        error("illegal multi-character sequence");
    
    sym->value.u = wide ? (wchar_t)c : (unsigned char)c;
    sym->type = wide ? wchartype : unsignedchartype;
}

static void integer_constant(struct token *t, struct symbol *sym)
{
    const char *s = t->name;
    if (s[0] == '\'' || s[1] == 'L')
        return char_constant(t, sym);
    
    int base;
    struct type *ty;
    bool overflow = 0;
    unsigned long long n = 0;
    
    if (s[0] == '0' && (s[1] == 'x' || s[1] == 'X')) {
        base = 16;
        s = s + 2;
        for (;is_digithex(*s);) {
            if (n & ~(~0ULL >> 4)) {
                overflow = 1;
            } else {
                int d;
                if (is_hex(*s))
                    d = (*s & 0x5f) - 'A' + 10;
                else
                    d = *s - '0';
                
                n = (n<<4) + d;
            }
            s++;
        }
    } else if (s[0] == '0') {
        base = 8;
        bool err = 0;
        for (;is_digit(*s);) {
            if (*s == '8' || *s == '9')
                err = 1;
            
            if (n & ~(~0ULL >> 3))
                overflow = 1;
            else
                n = (n<<3) + (*s - '0');
            
            s++;
        }
        
        if (err)
            error("invalid octal constant %s", t->name);
    } else {
        base = 10;
        for (;is_digit(*s);) {
            int d = *s - '0';
            if (n > (unsignedlonglongtype->limits.max.u - d)/10)
                overflow = 1;
            else
                n = n*10 + (*s - '0');
            
            s++;
        }
    }
    
    int ull = (s[0] == 'u' || s[0] == 'U') &&
	((s[1] == 'l' && s[2] == 'l') || (s[1] == 'L' && s[2] == 'L'));
    int llu = ((s[0] == 'l' && s[1] == 'l') || (s[0] == 'L' && s[1] == 'L')) &&
	(s[2] == 'u' || s[2] == 'U');
    int ll = (s[0] == 'l' && s[1] == 'l') || (s[0] == 'L' && s[1] == 'L');
    int lu = (s[0] == 'l' || s[0] == 'L') && (s[1] == 'u' || s[1] == 'U');
    int ul = (s[0] == 'u' || s[0] == 'U') && (s[1] == 'l' || s[1] == 'L');
    int l = s[0] == 'l' || s[0] == 'L';
    int u = s[0] == 'u' || s[0] == 'U';
    
    if (ull || llu) {
        ty = unsignedlonglongtype;
    } else if (ll) {
        if (n > longlongtype->limits.max.i && base != 10)
            ty = unsignedlonglongtype;
        else
            ty = longlongtype;
    } else if (lu || ul) {
        if (n > unsignedlongtype->limits.max.u)
            ty = unsignedlonglongtype;
        else
            ty = unsignedlongtype;
    } else if (l) {
        if (base == 10) {
            if (n > longtype->limits.max.i)
                ty = longlongtype;
            else
                ty = longtype;
        } else {
            if (n > longlongtype->limits.max.i)
                ty = unsignedlonglongtype;
            else if (n > unsignedlongtype->limits.max.u)
                ty = longlongtype;
            else if (n > longtype->limits.max.i)
                ty = unsignedlongtype;
            else
                ty = longtype;
        }
    } else if (u) {
        if (n > unsignedlongtype->limits.max.u)
            ty = unsignedlonglongtype;
        else if (n > unsignedinttype->limits.max.u)
            ty = unsignedlongtype;
        else
            ty = unsignedinttype;
    } else {
        if (base == 10) {
            if (n > longtype->limits.max.i)
                ty = longlongtype;
            else if (n > inttype->limits.max.i)
                ty = longtype;
            else
                ty = inttype;
        } else {
            if (n > longlongtype->limits.max.i)
                ty = unsignedlonglongtype;
            else if (n > unsignedlongtype->limits.max.u)
                ty = longlongtype;
            else if (n > longtype->limits.max.i)
                ty = unsignedlongtype;
            else if (n > unsignedinttype->limits.max.u)
                ty = longtype;
            else if (n > inttype->limits.max.i)
                ty = unsignedinttype;
            else
                ty = inttype;
        }
    }
    
    sym->type = ty;
    
    switch (op(sym->type)) {
    case INT:
	if (overflow || n > longlongtype->limits.max.i)
	    error("integer constant overflow: %s", t->name);
	sym->value.i = n;
	break;
    case UNSIGNED:
	if (overflow)
	    error("integer constant overflow: %s", t->name);
	sym->value.u = n;
	break;
    default:
	assert(0);
    }
}

static void float_constant(struct token *t, struct symbol *sym)
{
    const char *s = t->name;
    char c = s[strlen(s)-1];
    errno = 0;			// must clear first
    if (c == 'f' || c == 'F') {
        sym->type = floattype;
        sym->value.d = strtof(s, NULL);
    } else if (c == 'l' || c == 'L') {
        sym->type = longdoubletype;
        sym->value.ld = strtold(s, NULL);
    } else {
        sym->type = doubletype;
        sym->value.d = strtod(s, NULL);
    }
    
    if (errno == ERANGE)
        error("float constant overflow: %s", s);
}

static void string_constant(struct token *t, struct symbol *sym)
{
    const char *s = t->name;
    bool wide = s[0] == 'L' ? true : false;
    struct type *ty;
    if (wide) {
        size_t len = strlen(s) - 3;
        wchar_t ws[len+1];
        errno = 0;
        size_t wlen = mbstowcs(ws, s+2, len);
        if (errno == EILSEQ)
            error("invalid multibyte sequence: %s", s);
        assert(wlen<=len+1);
        ty = array_type();
        ty->type = wchartype;
        ty->size = wlen;
    } else {
        ty = array_type();
        ty->type = chartype;
        ty->size = strlen(s)-2;
    }
    sym->type = ty;
}

static void ensure_type(union node *node, bool (*is) (struct type *))
{
    const char *name;
    if (is == isint)
        name = "integer";
    else if (is == isscalar)
        name = "scalar";
    else if (is == isarith)
        name = "arithmetic";
    else if (is == isrecord)
	name = "struct or union";
    else
        assert(0);
    
    if (node && !is(AST_TYPE(node)))
        error("%s type expected, not type '%s'", name, type2s(AST_TYPE(node)));
}

static bool is_lvalue(union node *node)
{
    if (AST_ID(node) == STRING_LITERAL)
        return true;
    if (AST_ID(node) == PAREN_EXPR)
	return is_lvalue(EXPR_OPERAND(node, 0));
    if (AST_ID(node) == UNARY_OPERATOR && EXPR_OP(node) == '*')
	return true;
    if (AST_ID(node) == MEMBER_EXPR)
	return EXPR_OP(node) == DEREF ? true : is_lvalue(EXPR_OPERAND(node, 0));
    if (AST_ID(node) == REF_EXPR) {
        if (EXPR_OP(node) == ENUM)
            return false;
        if (isfunc(AST_TYPE(node)))
            return false;
        return true;
    }
    
    return false;
}

static void ensure_lvalue(union node *node)
{
    if (!node)
	return;
    if (!is_lvalue(node))
        error("lvalue expect");
}

static bool is_assignable(union node *node)
{
    if (node == NULL)
	return false;
    if (!is_lvalue(node))
	return false;
    if (AST_ID(node) == STRING_LITERAL)
	return false;
    if (AST_ID(node) == PAREN_EXPR)
	return is_assignable(node);
    if (AST_ID(node) == MEMBER_EXPR) {
	struct type *ty = AST_TYPE(EXPR_OPERAND(node, 0));
	const char *name = AST_NAME(EXPR_OPERAND(node, 1));
	struct field *field = find_field(ty, name);
	if (field)
	    return !isarray(field->type);

	return false;
    }
    if (isarray(AST_TYPE(node)))
	return false;
    
    return true;
}

static void ensure_assignable(union node *or)
{
    if (!or)
	return;
        
    if (!is_assignable(or))
        error("expression not assignable");
}

static bool is_bitfield(union node *node)
{
    if (!node || AST_ID(node) != MEMBER_EXPR)
        return false;

    struct type *ty = AST_TYPE(EXPR_OPERAND(node, 0));
    const char *name = AST_NAME(EXPR_OPERAND(node, 1));
    struct field *field = find_field(ty, name);
    if (field)
	return field->bitsize > 0;

    return false;
}

static void ensure_funcall(struct type *fty, union node **args)
{
    if (!isfunc(fty))
	return;
    // TODO: 
}

static union node * compound_literal(struct type *ty)
{
    union node * ret;
    union node * inits;
    
    inits = initializer_list(ty);
    ret = ast_expr(COMPOUND_LITERAL, 0, inits, NULL);
    AST_TYPE(ret) = ty;
    
    return ret;
}

static struct type * cast_type()
{
    struct type *ty;
    
    expect('(');
    ty = typename();
    expect(')');
    
    return ty;
}

static union node * primary_expr()
{
    int t = token->id;
    struct symbol *sym;
    union node *ret = NULL;
    
    switch (t) {
    case ID:
	sym = lookup(token->name, identifiers);
	if (sym) {
	    sym->refs++;
	    ret = ast_expr(REF_EXPR, 0, NULL, NULL);
	    AST_TYPE(ret) = sym->type;
	    EXPR_SYM(ret) = sym;
	    if (isenum(sym->type) && sym->sclass == ENUM)
		EXPR_OP(ret) = ENUM; // enum ids
	} else {
	    error("use of undeclared symbol '%s'", token->name);
	}
	expect(t);
	break;
    case ICONSTANT:
    case FCONSTANT:
    case SCONSTANT:
        {
	    int id;
            sym = lookup(token->name, constants);
            if (!sym) {
                sym = install(token->name, &constants, CONSTANT);
	        if (t == ICONSTANT)
		    integer_constant(token, sym);
		else if (t == FCONSTANT)
		    float_constant(token, sym);
		else
		    string_constant(token, sym);
            }
            expect(t);
	    if (t == ICONSTANT)
		id = INTEGER_LITERAL;
	    else if (t == FCONSTANT)
		id = FLOAT_LITERAL;
	    else
		id = STRING_LITERAL;
            ret = ast_expr(id, 0, NULL, NULL);
            AST_TYPE(ret) = sym->type;
	    EXPR_SYM(ret) = sym;
        }
	break;
    case '(':
	if (istypename(lookahead())) {
	    struct type *ty = cast_type();
	    ret = compound_literal(ty);
	} else {
	    expect('(');
	    union node *e = expression();
	    if (e) {
		ret = ast_expr(PAREN_EXPR, 0, e, NULL);
		AST_TYPE(ret) = AST_TYPE(e);
	    }
	    expect(')');
	}
	break;
    default:
	error("invalid postfix expression at '%s'", token->name);
	break;
    }
    
    return ret;
}

static union node ** argument_expr_list()
{
    union node **args = NULL;
    
    if (firstexpr(token)) {
        struct vector *v = vec_new();
        for (;;) {
	    union node *e = assign_expr();
	    if (e)
		vec_push(v, e);
	    if (token->id != ',')
		break;
	    expect(',');
        }
        args = (union node **)vtoa(v);
    } else if (token->id != ')') {
        error("expect assignment expression");
    }
    
    return args;
}

static union node * subscript(union node *node)
{
    union node *e;
    union node *ret = NULL;
    
    expect('[');
    e = expression();
    expect(']');
    if (node == NULL || e == NULL)
	return ret;

    SAVE_ERRORS;
    if (!isarray(AST_TYPE(node)) && !isptr(AST_TYPE(node)))
	error("subscripted value is not an array or pointer");
    ensure_type(e, isint);
    if (NO_ERROR) {
	ret = bop('+', conv(node), conv(e));
	ret = uop('*', rtype(AST_TYPE(ret)), ret);
    }
    return ret;
}

static union node * funcall(union node *node)
{
    union node **args;
    union node *ret = NULL;
    
    expect('(');
    args = argument_expr_list();
    expect(')');
    if (node == NULL)
	return ret;

    SAVE_ERRORS;
    ensure_type(node, isfunc);
    ensure_funcall(AST_TYPE(node), args);
    if (NO_ERROR) {
	ret = ast_expr(CALL_EXPR, 0, ret, NULL);
	EXPR_ARGS(ret) = args;
    }
    return ret;
}

static union node * direction(union node *node)
{
    int t = token->id;
    union node *ret = NULL;
    const char *name = NULL;

    expect(t);
    if (token->id == ID)
	name = token->name;
    expect(ID);
    if (node == NULL || name == NULL)
	return ret;
    
    SAVE_ERRORS;
    struct field *field = NULL;
    struct type *ty = AST_TYPE(node);
    if (t == '.') {
	ensure_type(node, isrecord);
    } else {
	if (!isptr(ty) || (kind(rtype(ty)) != STRUCT && kind(rtype(ty)) != UNION))
	    error("pointer to struct/union type expected, not type '%s'", type2s(ty));
	else
	    ty = rtype(ty);
    }
    if (isrecord(ty)) {
	field = find_field(ty, name);
        if (field == NULL)
	    error("'%s' has no field named '%s'", type2s(ty), name);
    }
    if (NO_ERROR) {
	ret = ast_expr(MEMBER_EXPR, t, node, ast_expr(REF_EXPR, 0, NULL, NULL));
	AST_NAME(EXPR_OPERAND(ret, 1)) = field->name;
	AST_TYPE(ret) = field->type;
    }
    return ret;
}

static union node * post_increment(union node *node)
{
    int t = token->id;
    union node *ret = NULL;
    
    expect(t);
    if (node == NULL)
	return ret;

    SAVE_ERRORS;
    ensure_type(node, isscalar);
    ensure_assignable(node);
    if (NO_ERROR)
	ret = uop(t, AST_TYPE(node), node);
    return ret;
}

static union node * postfix_expr1(union node *ret)
{
    for (;token->id == '[' || token->id == '(' || token->id == '.'
	     || token->id == DEREF || token->id == INCR || token->id == DECR;) {
        switch (token->id) {
	case '[':   ret = subscript(ret); break;
	case '(':   ret = funcall(ret);   break;
	case '.':
	case DEREF: ret = direction(ret); break;
	case INCR:
	case DECR:  ret = post_increment(ret); break;
	default:    assert(0);
        }
    }

    return ret;
}

static union node * postfix_expr()
{
    union node * expr = primary_expr();
    
    return postfix_expr1(expr);
}

static union node * sizeof_expr()
{
    int t = token->id;
    expect(t);
    
    struct token *ahead = lookahead();
    union node *n = NULL;
    struct type *ty = NULL;
    
    if (token->id == '(' && istypename(ahead)) {
        ty = cast_type();
        if (token->id == '{') {
            union node * node = compound_literal(ty);
            n = uop(t, ty, postfix_expr1(node));
        }
    } else {
        n = unary_expr();
    }
    
    ty = n ? AST_TYPE(n) : ty;
    if (isfunc(ty) || isvoid(ty))
        error("'sizeof' to a '%s' type is invalid", type2s(ty));
    else if (isarray(ty) && typesize(ty) == 0)
        error("'sizeof' to an incomplete array type is invalid");
    else if (n && is_bitfield(n))
	error("'sizeof' to a bitfield is invalid");
    
    return uop(t, unsignedinttype, NULL);
}

static union node * pre_increment()
{
    int t = token->id;
    union node *ret = NULL;
    
    expect(t);
    union node *operand = unary_expr();
    if (operand == NULL)
	return ret;

    SAVE_ERRORS;
    ensure_type(operand, isscalar);
    ensure_assignable(operand);
    if (NO_ERROR) {
	ret = uop(t, AST_TYPE(operand), operand);
	EXPR_PREFIX(ret) = true;
    }
    
    return ret;
}

static union node * minus_plus()
{
    int t = token->id;
    union node *ret = NULL;
    
    expect(t);
    union node *operand = cast_expr();
    if (operand == NULL)
	return ret;

    SAVE_ERRORS;
    ensure_type(operand, isarith);
    if (NO_ERROR) {
	union node *c = conv(operand);
	ret = uop(t, AST_TYPE(c), c);
    }

    return ret;
}

static union node * bitwise_not()
{
    int t = token->id;
    union node *ret = NULL;
    
    expect(t);
    union node *operand = cast_expr();
    if (operand == NULL)
	return ret;

    SAVE_ERRORS;
    ensure_type(operand, isint);
    if (NO_ERROR) {
	union node *c = conv(operand);
	ret = uop(t, AST_TYPE(c), c);
    }

    return ret;
}

static union node * logical_not()
{
    int t = token->id;
    union node *ret = NULL;
    
    expect(t);
    union node *operand = cast_expr();
    if (operand == NULL)
	return ret;

    SAVE_ERRORS;
    ensure_type(operand, isscalar);
    if (NO_ERROR)
	ret = uop(t, inttype, conv(operand));

    return ret;
}

static union node * address()
{
    int t = token->id;
    union node *ret = NULL;
    
    expect(t);
    union node *operand = cast_expr();
    if (operand == NULL)
	return ret;

    SAVE_ERRORS;
    if (!isfunc(AST_TYPE(operand))) {
	ensure_lvalue(operand);
	if (EXPR_SYM(operand) && EXPR_SYM(operand)->sclass == REGISTER)
	    error("address of register variable requested");
	else if (is_bitfield(operand))
	    error("address of bitfield requested");
    }
    if (NO_ERROR)
	ret = uop(t, ptr_type(AST_TYPE(operand)), operand);

    return ret;
}

static union node * indirection()
{
    int t = token->id;
    union node *ret = NULL;
    
    expect(t);
    union node *operand = conv(cast_expr());
    if (operand == NULL)
	return ret;

    SAVE_ERRORS;
    ensure_type(operand, isptr);
    if (NO_ERROR)
	ret = uop(t, rtype(AST_TYPE(operand)), operand);
    
    return ret;
}

static union node * unary_expr()
{
    switch (token->id) {
    case INCR:
    case DECR: 	 return pre_increment();
    case '+':
    case '-':    return minus_plus();
    case '~':    return bitwise_not();
    case '!':    return logical_not();
    case '&':    return address();
    case '*':    return indirection();
    case SIZEOF: return sizeof_expr();
    default:
	{
	    union node *n = postfix_expr();
	    if (n && (isarray(AST_TYPE(n)) || isfunc(AST_TYPE(n))))
	    	n = conv(n);
	    return n;
	}
    }
}

static union node * cast_expr()
{
    struct token * ahead = lookahead();
    
    if (token->id == '(' && istypename(ahead)) {
        struct type *ty = cast_type();
        if (token->id == '{') {
            union node * node = compound_literal(ty);
            return postfix_expr1(node);
        }

	union node *ret = NULL;
	union node *cast = cast_expr();
	if (cast) {
	    union node *ret = ast_expr(CAST_EXPR, 0, cast, NULL);
	    AST_TYPE(ret) = ty;
	}
        
	return ret;
    }
    return unary_expr();
}

static union node * multiple_expr()
{
    union node * mulp1;
    
    mulp1 = cast_expr();
    while (token->id == '*' || token->id == '/' || token->id == '%') {
        int t = token->id;
        expect(t);
        mulp1 = bop(t, conv(mulp1), conv(cast_expr()));
    }
    
    return mulp1;
}

static union node * additive_expr()
{
    union node * add1;
    
    add1 = multiple_expr();
    while (token->id == '+' || token->id == '-') {
        int t = token->id;
        expect(t);
        add1 = bop(t, conv(add1), conv(multiple_expr()));
    }
    
    return add1;
}

static union node * shift_expr()
{
    union node * shift1;
    
    shift1 = additive_expr();
    while (token->id == LSHIFT || token->id == RSHIFT) {
        int t = token->id;
        expect(t);
        shift1 = bop(t, conv(shift1), conv(additive_expr()));
    }
    
    return shift1;
}

static union node * relation_expr()
{
    union node * rel;
    
    rel = shift_expr();
    while (token->id == '<' || token->id == '>' || token->id == LEQ || token->id == GEQ) {
        int t = token->id;
        expect(t);
        rel = bop(t, conv(rel), conv(shift_expr()));
    }
    
    return rel;
}

static union node * equality_expr()
{
    union node * equl;
    
    equl = relation_expr();
    while (token->id == EQ || token->id == NEQ) {
        int t = token->id;
        expect(t);
        equl = bop(t, conv(equl), conv(relation_expr()));
    }
    
    return equl;
}

static union node * and_expr()
{
    union node * and1;
    
    and1 = equality_expr();
    while (token->id == '&') {
        expect('&');
        and1 = bop('&', conv(and1), conv(equality_expr()));
    }
    
    return and1;
}

static union node * exclusive_or()
{
    union node * eor;
    
    eor = and_expr();
    while (token->id == '^') {
        expect('^');
        eor = bop('^', conv(eor), conv(and_expr()));
    }
    
    return eor;
}

static union node * inclusive_or()
{
    union node * ior;
    
    ior = exclusive_or();
    while (token->id == '|') {
        expect('|');
        ior = bop('|', conv(ior), conv(exclusive_or()));
    }
    
    return ior;
}

static union node * logic_and()
{
    union node * and1;
    
    and1 = inclusive_or();
    while (token->id == AND) {
        expect(AND);
	union node *and2 = inclusive_or();
	if (and1 && and2) {
	    and1 = ast_bop(AND, conv(and1), conv(and2));
	    AST_TYPE(and1) = inttype;
	} else {
	    and1 = NULL;
	}
    }
    
    return and1;
}

static union node * logic_or()
{
    union node * or1;
    
    or1 = logic_and();
    while (token->id == OR) {
        expect(OR);
	union node *or2 = logic_and();
	if (or1 && or2) {
	    or1 = ast_bop(OR, conv(or1), conv(or2));
	    AST_TYPE(or1) = inttype;
	} else {
	    or1 = NULL;
	}
    }
    
    return or1;
}

static union node * cond_expr1(union node *cond)
{
    union node *ret, *then, *els;
    
    ensure_type(cond, isscalar);
    expect('?');
    then = conv(expression());
    expect(':');
    els = conv(cond_expr());
    
    ret = ast_expr(COND_EXPR, 0, NULL, NULL);
    EXPR_COND(ret) = cond;
    EXPR_THEN(ret) = then;
    EXPR_ELSE(ret) = els;
    
    if (isarith(AST_TYPE(then)) && isarith(AST_TYPE(els))) {
        struct type *ty = conv2(AST_TYPE(then), AST_TYPE(els));
        EXPR_THEN(ret) = wrap(ty, then);
        EXPR_ELSE(ret) = wrap(ty, els);
        AST_TYPE(ret) = ty;
    } else if ((isstruct(AST_TYPE(then)) && isstruct(AST_TYPE(els))) ||
               (isunion(AST_TYPE(then)) && isunion(AST_TYPE(els)))) {
        if (!eqtype(AST_TYPE(then), AST_TYPE(els)))
            ;
        AST_TYPE(ret) = unqual(AST_TYPE(then));
    }
    //TODO: other cases
    
    return ret;
}

static union node * cond_expr()
{
    union node * or1 = logic_or();
    if (token->id == '?')
        return cond_expr1(or1);
    return or1;
}

union node * assign_expr()
{
    union node *or1 = logic_or();
    if (token->id == '?')
        return cond_expr1(or1);
    if (is_assign_op(token->id)) {
        int t = token->id;
        expect(token->id);
        ensure_assignable(or1);
        or1 = ast_bop(t, or1, assign_expr());
    }
    return or1;
}

union node * expression()
{
    union node *expr;
    
    expr = assign_expr();
    while (token->id == ',') {
        expect(',');
        expr = ast_bop(',', expr, assign_expr());
    }
    
    return expr;
}

//TODO
int intexpr()
{
    union node *n = cond_expr();
    if (AST_ID(n) == INTEGER_LITERAL) {
	struct symbol *sym = EXPR_SYM(n);
	if (op(sym->type) == UNSIGNED)
	    return (int) sym->value.u;
	else
	    return (int) sym->value.i;
    } else if (AST_ID(n) == FLOAT_LITERAL) {

    }
    
    return 0;
}

union node * constexpr()
{
    union node *expr = cond_expr();
    
    return expr;
}

//TODO
static union node * eval(union node *expr)
{
    assert(isexpr(expr));
    switch (AST_ID(expr)) {
    case BINARY_OPERATOR:
        {

	}
	break;
    case UNARY_OPERATOR:
        {

	}
	break;
    case PAREN_EXPR:
	return eval(EXPR_OPERAND(expr, 0));
    case COND_EXPR:
    case MEMBER_EXPR:
    case REF_EXPR:
    case CAST_EXPR:
    case CALL_EXPR:
    case INITS_EXPR:
	return NULL;
    case INTEGER_LITERAL:
    case FLOAT_LITERAL:
    case COMPOUND_LITERAL:
	return expr;
    case CONV_EXPR:
    default:
	assert(0);
    }
}

// TODO
static union node * uop(int op, struct type *ty, union node *l)
{
    union node *node = ast_uop(op, ty, l);
    return node;
}

static union node * bop(int op, union node *l, union node *r)
{
    union node *node = NULL;
    struct type *ty;
    bool (*is) (struct type *ty);

    if (l == NULL || r == NULL)
	return NULL;
    
    switch (op) {
    case '*': case '/':
	is = isarith;
    case '%':
    case LSHIFT: case RSHIFT:
    case '&': case '^': case '|':
	is = isint;
            
	ensure_type(l, is);
	ensure_type(r, is);
	ty = conv2(AST_TYPE(l), AST_TYPE(r));
	node = ast_bop(op, wrap(ty, l), wrap(ty, r));
	AST_TYPE(node) = ty;
	break;
    case '+':
	if (isptr(AST_TYPE(l))) {
	    ensure_type(r, isint);
	    node = ast_bop(op, l, r);
	    AST_TYPE(node) = AST_TYPE(l);
	} else if (isptr(AST_TYPE(r))) {
	    ensure_type(l, isint);
	    node = ast_bop(op, l, r);
	    AST_TYPE(node) = AST_TYPE(r);
	} else {
	    ensure_type(l, isarith);
	    ensure_type(r, isarith);
	    ty = conv2(AST_TYPE(l), AST_TYPE(r));
	    node = ast_bop(op, wrap(ty, l), wrap(ty, r));
	    AST_TYPE(node) = ty;
	}
	break;
    case '-':
	if (isptr(AST_TYPE(l))) {
	    node = ast_bop(op, l, r);
	    if (isint(AST_TYPE(r))) {
		AST_TYPE(node) = AST_TYPE(l);
	    } else if (isptr(AST_TYPE(r))) {
		AST_TYPE(node) = inttype;
	    } else {
		error("expect integer or pointer type, but got type '%s'", type2s(AST_TYPE(r)));
		AST_TYPE(node) = AST_TYPE(l);
	    }
	} else {
	    ensure_type(l, isarith);
	    ensure_type(r, isarith);
	    ty = conv2(AST_TYPE(l), AST_TYPE(r));
	    node = ast_bop(op, wrap(ty, l), wrap(ty, r));
	    AST_TYPE(node) = ty;
	}
	break;
    case '>': case '<': case LEQ: case GEQ:
    case EQ: case NEQ:
	ensure_type(l, isscalar);
	ensure_type(r, isscalar);
	node = ast_bop(op, l, r);
	AST_TYPE(node) = inttype;
	break;
    default:
	error("unknown op '%s'", tname(op));
	assert(0);
    }
    return node;
}

union node * wrap(struct type *ty, union node *node)
{
    if (eqarith(ty, AST_TYPE(node)))
        return node;
    else
        return ast_conv(ty, node);
}

// Universal Binary Conversion
static struct type * conv2(struct type *l, struct type *r)
{
    assert(isarith(l));
    assert(isarith(r));
    
    assert(size(l) >= size(inttype));
    assert(size(r) >= size(inttype));
    
    struct type *max = rank(l) > rank(r) ? l : r;
    if (isfloat(l) || isfloat(r) || op(l) == op(r))
        return max;
    
    struct type *u = op(l) == UNSIGNED ? l : r;
    struct type *s = op(l) == INT ? l : r;
    
    if (rank(u) >= rank(s))
        return u;
    
    if (size(u) < size(s)) {
        return s;
    } else {
        if (unqual(s) == inttype)
            return unsignedinttype;
        else if (unqual(s) == longtype)
            return unsignedlongtype;
        else
            return unsignedlonglongtype;
    }
    
    return l;
}

// Universal Unary Conversion
static union node * conv(union node *node)
{
    if (node == NULL)
	return NULL;
    switch (kind(AST_TYPE(node))) {
    case _BOOL: case CHAR: case SHORT:
	return ast_conv(inttype, node);
            
    case FUNCTION:
	return ast_conv(ptr_type(AST_TYPE(node)), node);
            
    case ARRAY:
	return ast_conv(ptr_type(rtype(AST_TYPE(node))), node);
            
    default:
	return node;
    }
}
