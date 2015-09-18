#include "cc.h"

static node_t * cast_expr();
static node_t * cond_expr();
static node_t * cond_expr1(node_t *o);
static node_t * unary_expr();
static node_t * uop(int op, node_t *ty, node_t *l);
static node_t * bop(int op, node_t *l, node_t *r);
static node_t * logicop(int op, node_t *l, node_t *r);
static node_t * commaop(int op, node_t *l, node_t *r);
static node_t * assignop(int op, node_t *l, node_t *r);
static node_t * decay(node_t *node);
static node_t * ltor(node_t *node);
static node_t * conv(node_t *node);
static node_t * conva(node_t *node);
static node_t * conv2(node_t *l, node_t *r);
static node_t * wrap(node_t *ty, node_t *node);
static node_t * bitconv(node_t *ty, node_t *node);
static node_t * assignconv(node_t *ty, node_t *node);
static bool is_nullptr(node_t *node);
static bool assign_types_check(node_t *ty1, node_t *r);

#define SAVE_ERRORS    unsigned err = errors
#define NO_ERROR       (err == errors)
#define HAS_ERROR      (err != errors)
#define INCOMPATIBLE_TYPES    "incompatible type conversion from '%s' to '%s'"
#define INCOMPATIBLE_TYPES2   "imcompatible types '%s' and '%s' in conditional expression"

static int splitop(int op)
{
    switch (op) {
    case MULEQ: return '*';
    case DIVEQ: return '/';
    case MODEQ: return '%';
    case ADDEQ: return '+';
    case MINUSEQ: return '-';
    case LSHIFTEQ: return LSHIFT;
    case RSHIFTEQ: return RSHIFT;
    case BANDEQ: return '&';
    case BOREQ: return '|';
    case XOREQ: return '^';
    default: CCAssert(0);
    }
}

static unsigned escape(const char **ps)
{
    unsigned c = 0;
    const char *s = *ps;
    CCAssert(*s == '\\');
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

static void char_constant(struct token *t, node_t *sym)
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
    else if ((!wide && c > TYPE_LIMITS_MAX(unsignedchartype).u) ||
             (wide && c > TYPE_LIMITS_MAX(wchartype).u))
        error("character constant overflow: %s", t->name);
    else if (len && mbtowc((wchar_t *)&c, ws, len) != len)
        error("illegal multi-character sequence");
    
    SYM_VALUE(sym).u = wide ? (wchar_t)c : (unsigned char)c;
    SYM_TYPE(sym) = wide ? wchartype : unsignedchartype;
}

static void integer_constant(struct token *t, node_t *sym)
{
    const char *s = t->name;
    if (s[0] == '\'' || s[1] == 'L')
        return char_constant(t, sym);
    
    int base;
    node_t *ty;
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
            if (n > (TYPE_LIMITS_MAX(unsignedlonglongtype).u - d)/10)
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
        if (n > TYPE_LIMITS_MAX(longlongtype).i && base != 10)
            ty = unsignedlonglongtype;
        else
            ty = longlongtype;
    } else if (lu || ul) {
        if (n > TYPE_LIMITS_MAX(unsignedlongtype).u)
            ty = unsignedlonglongtype;
        else
            ty = unsignedlongtype;
    } else if (l) {
        if (base == 10) {
            if (n > TYPE_LIMITS_MAX(longtype).i)
                ty = longlongtype;
            else
                ty = longtype;
        } else {
            if (n > TYPE_LIMITS_MAX(longlongtype).i)
                ty = unsignedlonglongtype;
            else if (n > TYPE_LIMITS_MAX(unsignedlongtype).u)
                ty = longlongtype;
            else if (n > TYPE_LIMITS_MAX(longtype).i)
                ty = unsignedlongtype;
            else
                ty = longtype;
        }
    } else if (u) {
        if (n > TYPE_LIMITS_MAX(unsignedlongtype).u)
            ty = unsignedlonglongtype;
        else if (n > TYPE_LIMITS_MAX(unsignedinttype).u)
            ty = unsignedlongtype;
        else
            ty = unsignedinttype;
    } else {
        if (base == 10) {
            if (n > TYPE_LIMITS_MAX(longtype).i)
                ty = longlongtype;
            else if (n > TYPE_LIMITS_MAX(inttype).i)
                ty = longtype;
            else
                ty = inttype;
        } else {
            if (n > TYPE_LIMITS_MAX(longlongtype).i)
                ty = unsignedlonglongtype;
            else if (n > TYPE_LIMITS_MAX(unsignedlongtype).u)
                ty = longlongtype;
            else if (n > TYPE_LIMITS_MAX(longtype).i)
                ty = unsignedlongtype;
            else if (n > TYPE_LIMITS_MAX(unsignedinttype).u)
                ty = longtype;
            else if (n > TYPE_LIMITS_MAX(inttype).i)
                ty = unsignedinttype;
            else
                ty = inttype;
        }
    }
    
    SYM_TYPE(sym) = ty;
    
    switch (op(SYM_TYPE(sym))) {
    case INT:
	if (overflow || n > TYPE_LIMITS_MAX(longlongtype).i)
	    error("integer constant overflow: %s", t->name);
	SYM_VALUE(sym).i = n;
	break;
    case UNSIGNED:
	if (overflow)
	    error("integer constant overflow: %s", t->name);
	SYM_VALUE(sym).u = n;
	break;
    default:
	CCAssert(0);
    }
}

static void float_constant(struct token *t, node_t *sym)
{
    const char *s = t->name;
    char c = s[strlen(s)-1];
    errno = 0;			// must clear first
    if (c == 'f' || c == 'F') {
        SYM_TYPE(sym) = floattype;
        SYM_VALUE(sym).d = strtof(s, NULL);
    } else if (c == 'l' || c == 'L') {
        SYM_TYPE(sym) = longdoubletype;
        SYM_VALUE(sym).ld = strtold(s, NULL);
    } else {
        SYM_TYPE(sym) = doubletype;
        SYM_VALUE(sym).d = strtod(s, NULL);
    }
    
    if (errno == ERANGE)
        error("float constant overflow: %s", s);
}

static void string_constant(struct token *t, node_t *sym)
{
    const char *s = t->name;
    bool wide = s[0] == 'L' ? true : false;
    node_t *ty;
    if (wide) {
        size_t len = strlen(s) - 3;
        wchar_t ws[len+1];
        errno = 0;
        size_t wlen = mbstowcs(ws, s+2, len);
        if (errno == EILSEQ)
            error("invalid multibyte sequence: %s", s);
        CCAssert(wlen<=len+1);
        ty = array_type();
        TYPE_TYPE(ty) = wchartype;
        TYPE_SIZE(ty) = wlen;
    } else {
        ty = array_type();
        TYPE_TYPE(ty) = chartype;
        TYPE_SIZE(ty) = strlen(s)-1;
    }
    SYM_TYPE(sym) = ty;
}

static void ensure_type(node_t *node, bool (*is) (node_t *))
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
    else if (is == isfunc)
        name = "function";
    else if (is == isptr)
	name = "pointer";
    else
	CCAssert(0);
    
    if (!is(AST_TYPE(node)))
        error("%s type expected, not type '%s'", name, type2s(AST_TYPE(node)));
}

bool islvalue(node_t *node)
{
    if (AST_ID(node) == STRING_LITERAL)
        return true;
    if (AST_ID(node) == PAREN_EXPR)
	return islvalue(EXPR_OPERAND(node, 0));
    if (AST_ID(node) == UNARY_OPERATOR && EXPR_OP(node) == '*') {
	if (isfunc(AST_TYPE(node)))
	    return false;
	return true;
    }
    if (AST_ID(node) == MEMBER_EXPR)
	return EXPR_OP(node) == DEREF ? true : islvalue(EXPR_OPERAND(node, 0));
    if (AST_ID(node) == REF_EXPR) {
        if (EXPR_OP(node) == ENUM)
            return false;
        if (isfunc(AST_TYPE(node)))
            return false;
        return true;
    }
    
    return false;
}

static void ensure_lvalue(node_t *node)
{
    if (!islvalue(node))
        error("lvalue expect");
}

static const char * is_assignable(node_t *node)
{
    node_t *ty = AST_TYPE(node);
    if (!islvalue(node))
	return "expression is not assignable";
    if (AST_ID(node) == PAREN_EXPR)
	return is_assignable(node);
    if (isarray(ty))
	return format("array type '%s' is not assignable", type2s(ty));
    if (isconst(ty)) {
	if (AST_ID(node) == REF_EXPR) {
	    return format("read-only variable '%s' is not assignable", SYM_NAME(EXPR_SYM(node)));
	} else if (AST_ID(node) == MEMBER_EXPR) {
	    const char *op = EXPR_OP(node) == '.' ? "." : "->";
	    const char *l = SYM_NAME(EXPR_SYM(EXPR_OPERAND(node, 0)));
	    const char *r = AST_NAME(EXPR_OPERAND(node, 1));
	    return format("read-only variable '%s%s%s' is not assignable", l, op, r);
	} else {
	    return "read-only variable is not assignable";
	}
    }
    
    return NULL;
}

static void ensure_assignable(node_t *or)
{
    const char *msg = is_assignable(or);
    if (msg)
        error(msg);
}

static bool is_bitfield(node_t *node)
{
    if (AST_ID(node) != MEMBER_EXPR)
        return false;

    node_t *ty = AST_TYPE(EXPR_OPERAND(node, 0));
    const char *name = AST_NAME(EXPR_OPERAND(node, 1));
    node_t *field = find_field(ty, name);
    return isbitfield(field);
}

static const char * is_castable(node_t *dst, node_t *src)
{
    if (isvoid(dst))
	return NULL;
    if (isarith(dst) && isarith(src))
	return NULL;
    if (isint(dst) && isptr(src))
	return NULL;
    if (isptrto(dst, FUNCTION)) {
	if (isint(src) ||
	    isptrto(src, FUNCTION))
	    return NULL;
    } else if (isptr(dst)) {
	if (isint(src) ||
	    isptrto(src, VOID))
	    return NULL;
	if (isptr(src) && !isfunc(rtype(src)))
	    return NULL;
    }

    return format(INCOMPATIBLE_TYPES, type2s(src), type2s(dst));
}

static void ensure_cast(node_t *dst, node_t *src)
{
    const char *msg = is_castable(dst, src);
    if (msg)
	error(msg);
}

static void argcast1(node_t *fty, node_t **args, struct vector *v)
{
    node_t **params = TYPE_PARAMS(fty);
    int len1 = array_len((void **)params);
    int len2 = array_len((void **)args);
    bool oldstyle = TYPE_OLDSTYLE(fty);
    int cmp1;

    if (oldstyle) {
	cmp1 = MIN(len1, len2);
    } else {
	node_t *last = params[len1 - 1];
	bool vargs = unqual(SYM_TYPE(last)) == vartype;
	cmp1 = vargs ? len1 - 1 : len1;
    }
    
    for (int i = 0; i < cmp1; i++) {
	node_t *dst = SYM_TYPE(params[i]);
	node_t *src = AST_TYPE(args[i]);
	node_t *ret;
	if (assign_types_check(dst, args[i]) &&
	    (ret = assignconv(dst, args[i]))) {
	    vec_push(v, ret);
	} else {
	    if (oldstyle)
		warning(INCOMPATIBLE_TYPES, type2s(src), type2s(dst));
	    else
		error(INCOMPATIBLE_TYPES, type2s(src), type2s(dst));
	}
    }
    for (int i = cmp1; i < len2; i++)
	vec_push(v, conva(args[i]));
}

static struct vector * argscast(node_t *fty, node_t **args)
{
    struct vector *v = vec_new();
    CCAssert(isfunc(fty));

    /* There are 5 cases:
     *
     * 1. function declaration with prototype
     * 2. function definition with prototype
     * 3. function declaration with oldstyle
     * 4. function definition with oldstyle
     * 5. no function declaration/definition found
     */

    node_t **params = TYPE_PARAMS(fty);
    int len1 = array_len((void **)params);
    int len2 = array_len((void **)args);
    
    if (TYPE_OLDSTYLE(fty)) {
	if (len1 > len2)
	    warning("too few arguments to function call");

        argcast1(fty, args, v);
    } else {
	if (len1 == 0)
	    return NULL;	// parsing error
	
	node_t *last = params[len1 - 1];
	bool vargs = unqual(SYM_TYPE(last)) == vartype;
	node_t *first = params[0];
	if (isvoid(SYM_TYPE(first)))
	    len1 = 0;
	if (len1 <= len2) {
	    if (!vargs && len1 < len2) {
		error("too many arguments to function call, expected %d, have %d", len1, len2);
		return NULL;
	    }
	    if (len1 > 0) {
		SAVE_ERRORS;
		argcast1(fty, args, v);
		if (HAS_ERROR)
		    return NULL;
	    }
	} else {
	    if (vargs)
		error("too few arguments to function call, expected at least %d, have %d", len1, len2);
	    else
		error("too few arguments to function call, expected %d, have %d", len1, len2);
	    return NULL;
	}
    }
    return v;
}

static node_t * compound_literal(node_t *ty)
{
    node_t * ret;
    node_t * inits;
    
    inits = initializer_list(ty);
    ret = ast_expr(COMPOUND_LITERAL, 0, inits, NULL);
    AST_TYPE(ret) = ty;
    
    return ret;
}

static node_t * cast_type()
{
    node_t *ty;
    
    expect('(');
    ty = typename();
    expect(')');
    
    return ty;
}

static node_t * primary_expr()
{
    int t = token->id;
    node_t *sym;
    node_t *ret = NULL;
    
    switch (t) {
    case ID:
	sym = lookup(token->name, identifiers);
	if (sym) {
	    SYM_REFS(sym)++;
	    ret = ast_expr(REF_EXPR, 0, NULL, NULL);
	    AST_TYPE(ret) = SYM_TYPE(sym);
	    EXPR_SYM(ret) = sym;
	    if (isenum(SYM_TYPE(sym)) && SYM_SCLASS(sym) == ENUM)
		EXPR_OP(ret) = ENUM; // enum ids
	} else {
	    error("use of undeclared identifier '%s'", token->name);
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
            AST_TYPE(ret) = SYM_TYPE(sym);
	    EXPR_SYM(ret) = sym;
        }
	break;
    case '(':
	if (istypename(lookahead())) {
	    node_t *ty = cast_type();
	    ret = compound_literal(ty);
	} else {
	    expect('(');
	    node_t *e = expression();
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

static node_t ** argument_expr_list()
{
    node_t **args = NULL;
    
    if (firstexpr(token)) {
        struct vector *v = vec_new();
        for (;;) {
	    vec_push_safe(v, assign_expr());
	    if (token->id != ',')
		break;
	    expect(',');
        }
        args = (node_t **)vtoa(v);
    } else if (token->id != ')') {
        error("expect assignment expression");
    }
    
    return args;
}

static node_t * subscript(node_t *node)
{
    node_t *e;
    node_t *ret = NULL;
    
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

static node_t * funcall(node_t *node)
{
    node_t **args;
    node_t *ret = NULL;
    
    expect('(');
    args = argument_expr_list();
    expect(')');
    if (node == NULL)
	return ret;

    if (isptrto(AST_TYPE(node), FUNCTION)) {
	node_t *fty = rtype(AST_TYPE(node));
	struct vector *v;
	if ((v = argscast(fty, args))) {
	    ret = ast_expr(CALL_EXPR, 0, node, NULL);
	    EXPR_ARGS(ret) = (node_t **)vtoa(v);
	    AST_TYPE(ret) = rtype(fty);
	}
    } else {
	ensure_type(node, isfunc);
    }
    
    return ret;
}

static node_t * direction(node_t *node)
{
    int t = token->id;
    node_t *ret = NULL;
    const char *name = NULL;

    expect(t);
    if (token->id == ID)
	name = token->name;
    expect(ID);
    if (node == NULL || name == NULL)
	return ret;
    
    SAVE_ERRORS;
    node_t *field = NULL;
    node_t *ty = AST_TYPE(node);
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
	AST_NAME(EXPR_OPERAND(ret, 1)) = FIELD_NAME(field);
	AST_TYPE(ret) = FIELD_TYPE(field);
    }
    return ret;
}

static node_t * post_increment(node_t *node)
{
    int t = token->id;
    node_t *ret = NULL;
    
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

static node_t * postfix_expr1(node_t *ret)
{
    for (;token->id == '[' || token->id == '(' || token->id == '.'
	     || token->id == DEREF || token->id == INCR || token->id == DECR;) {
        switch (token->id) {
	case '[':   ret = subscript(ret); break;
	case '(':   ret = funcall(conv(ret)); break;
	case '.':
	case DEREF: ret = direction(ret); break;
	case INCR:
	case DECR:  ret = post_increment(ret); break;
	default:    CCAssert(0);
        }
    }

    return ret;
}

static node_t * postfix_expr()
{
    node_t * expr = primary_expr();
    
    return postfix_expr1(expr);
}

static node_t * sizeof_expr()
{
    int t = token->id;
    node_t *ret = NULL;
    
    expect(t);
    
    struct token *ahead = lookahead();
    node_t *n = NULL;
    node_t *ty = NULL;
    
    if (token->id == '(' && istypename(ahead)) {
        ty = cast_type();
        if (token->id == '{') {
            node_t * node = compound_literal(ty);
            n = uop(t, ty, postfix_expr1(node));
        }
    } else {
        n = unary_expr();
    }
    
    ty = n ? AST_TYPE(n) : ty;
    if (ty == NULL)
	return ret;

    SAVE_ERRORS;
    if (isfunc(ty) || isvoid(ty))
        error("'sizeof' to a '%s' type is invalid", type2s(ty));
    else if (isarray(ty) && typesize(ty) == 0)
        error("'sizeof' to an incomplete array type is invalid");
    else if (n && is_bitfield(n))
	error("'sizeof' to a bitfield is invalid");

    if (NO_ERROR) {
	ret = uop(t, unsignedinttype, ty);
	EXPR_SYM(ret) = anonymous(&identifiers, GLOBAL);
	SYM_TYPE(EXPR_SYM(ret)) = ty;
	SYM_VALUE(EXPR_SYM(ret)).u = typesize(ty);
    }

    return ret;
}

static node_t * pre_increment()
{
    int t = token->id;
    node_t *ret = NULL;
    
    expect(t);
    node_t *operand = unary_expr();
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

static node_t * minus_plus()
{
    int t = token->id;
    node_t *ret = NULL;
    
    expect(t);
    node_t *operand = cast_expr();
    if (operand == NULL)
	return ret;

    SAVE_ERRORS;
    ensure_type(operand, isarith);
    if (NO_ERROR) {
	node_t *c = conv(operand);
	ret = uop(t, AST_TYPE(c), c);
    }

    return ret;
}

static node_t * bitwise_not()
{
    int t = token->id;
    node_t *ret = NULL;
    
    expect(t);
    node_t *operand = cast_expr();
    if (operand == NULL)
	return ret;

    SAVE_ERRORS;
    ensure_type(operand, isint);
    if (NO_ERROR) {
	node_t *c = conv(operand);
	ret = uop(t, AST_TYPE(c), c);
    }

    return ret;
}

static node_t * logical_not()
{
    int t = token->id;
    node_t *ret = NULL;
    
    expect(t);
    node_t *operand = cast_expr();
    if (operand == NULL)
	return ret;

    SAVE_ERRORS;
    ensure_type(operand, isscalar);
    if (NO_ERROR)
	ret = uop(t, inttype, conv(operand));

    return ret;
}

static node_t * address()
{
    int t = token->id;
    node_t *ret = NULL;
    
    expect(t);
    node_t *operand = cast_expr();
    if (operand == NULL)
	return ret;

    SAVE_ERRORS;
    if (!isfunc(AST_TYPE(operand))) {
	ensure_lvalue(operand);
	if (EXPR_SYM(operand) && SYM_SCLASS(EXPR_SYM(operand)) == REGISTER)
	    error("address of register variable requested");
	else if (is_bitfield(operand))
	    error("address of bitfield requested");
    }
    if (NO_ERROR)
	ret = uop(t, ptr_type(AST_TYPE(operand)), operand);

    return ret;
}

static node_t * indirection()
{
    int t = token->id;
    node_t *ret = NULL;
    
    expect(t);
    node_t *operand = conv(cast_expr());
    if (operand == NULL)
	return ret;

    SAVE_ERRORS;
    ensure_type(operand, isptr);
    if (NO_ERROR)
	ret = uop(t, rtype(AST_TYPE(operand)), operand);
    
    return ret;
}

static node_t * unary_expr()
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
    default:     return postfix_expr();
    }
}

static node_t * cast_expr()
{
    struct token * ahead = lookahead();
    
    if (token->id == '(' && istypename(ahead)) {
        node_t *ty = cast_type();
        if (token->id == '{') {
            node_t * node = compound_literal(ty);
            return postfix_expr1(node);
        }
	
	node_t *ret = NULL;
	node_t *cast = cast_expr();
	if (cast) {
	    SAVE_ERRORS;
	    ensure_cast(ty, AST_TYPE(cast));
	    if (NO_ERROR) {
		ret = ast_expr(CAST_EXPR, 0, cast, NULL);
		AST_TYPE(ret) = ty;
	    }
	}
        
	return ret;
    }
    return unary_expr();
}

static node_t * multiple_expr()
{
    node_t * mulp1;
    
    mulp1 = cast_expr();
    while (token->id == '*' || token->id == '/' || token->id == '%') {
        int t = token->id;
        expect(t);
        mulp1 = bop(t, conv(mulp1), conv(cast_expr()));
    }
    
    return mulp1;
}

static node_t * additive_expr()
{
    node_t * add1;
    
    add1 = multiple_expr();
    while (token->id == '+' || token->id == '-') {
        int t = token->id;
        expect(t);
        add1 = bop(t, conv(add1), conv(multiple_expr()));
    }
    
    return add1;
}

static node_t * shift_expr()
{
    node_t * shift1;
    
    shift1 = additive_expr();
    while (token->id == LSHIFT || token->id == RSHIFT) {
        int t = token->id;
        expect(t);
        shift1 = bop(t, conv(shift1), conv(additive_expr()));
    }
    
    return shift1;
}

static node_t * relation_expr()
{
    node_t * rel;
    
    rel = shift_expr();
    while (token->id == '<' || token->id == '>' || token->id == LEQ || token->id == GEQ) {
        int t = token->id;
        expect(t);
        rel = bop(t, conv(rel), conv(shift_expr()));
    }
    
    return rel;
}

static node_t * equality_expr()
{
    node_t * equl;
    
    equl = relation_expr();
    while (token->id == EQ || token->id == NEQ) {
        int t = token->id;
        expect(t);
        equl = bop(t, conv(equl), conv(relation_expr()));
    }
    
    return equl;
}

static node_t * and_expr()
{
    node_t * and1;
    
    and1 = equality_expr();
    while (token->id == '&') {
        expect('&');
        and1 = bop('&', conv(and1), conv(equality_expr()));
    }
    
    return and1;
}

static node_t * exclusive_or()
{
    node_t * eor;
    
    eor = and_expr();
    while (token->id == '^') {
        expect('^');
        eor = bop('^', conv(eor), conv(and_expr()));
    }
    
    return eor;
}

static node_t * inclusive_or()
{
    node_t * ior;
    
    ior = exclusive_or();
    while (token->id == '|') {
        expect('|');
        ior = bop('|', conv(ior), conv(exclusive_or()));
    }
    
    return ior;
}

static node_t * logic_and()
{
    node_t * and1;
    
    and1 = inclusive_or();
    while (token->id == AND) {
        expect(AND);
	and1 = logicop(AND, conv(and1), conv(inclusive_or()));
    }
    
    return and1;
}

static node_t * logic_or()
{
    node_t * or1;
    
    or1 = logic_and();
    while (token->id == OR) {
        expect(OR);
	or1 = logicop(OR, conv(or1), conv(logic_and()));
    }
    
    return or1;
}

static node_t * cond_expr1(node_t *cond)
{
    node_t *ret = NULL;
    node_t *then, *els;
    node_t *ty = NULL;

    expect('?');
    then = conv(expression());
    expect(':');
    els = conv(cond_expr());

    if (cond == NULL || then == NULL || els == NULL)
	return ret;

    node_t *ty1 = AST_TYPE(then);
    node_t *ty2 = AST_TYPE(els);

    SAVE_ERRORS;
    ensure_type(cond, isscalar);

    if (isarith(ty1) && isarith(ty2)) {
        ty = conv2(ty1, ty2);
        then = wrap(ty, then);
        els = wrap(ty, els);
    } else if ((isstruct(ty1) && isstruct(ty2)) ||
               (isunion(ty1) && isunion(ty2))) {
        if (!eqtype(ty1, ty2))
	    error(INCOMPATIBLE_TYPES2, type2s(ty1), type2s(ty2));
	ty = ty1;
    } else if (isvoid(ty1) && isvoid(ty2)) {
	ty = voidtype;
    } else if (isptr(ty1) && isptr(ty2)) {
	if (is_nullptr(then) || is_nullptr(els)) {
	    node_t *nty = is_nullptr(then) ? ty1 : ty2;
	    node_t *tty = nty == ty1 ? ty2 : ty1;
	    ty = ptr_type(compose(rtype(tty), rtype(nty)));
	    then = bitconv(ty, then);
	    els = bitconv(ty, els);
	} else if (isptrto(ty1, VOID) || isptrto(ty2, VOID)) {
	    node_t *vty = isptrto(ty1, VOID) ? ty1 : ty2;
	    node_t *tty = vty == ty1 ? ty2 : ty1;
	    if (isptrto(tty, FUNCTION)) {
	        error(INCOMPATIBLE_TYPES2, type2s(ty1), type2s(ty2));
	    } else {
		ty = ptr_type(compose(rtype(vty), rtype(tty)));
		then = bitconv(ty, then);
		els = bitconv(ty, els);
	    }
	} else {
	    node_t *rty1 = rtype(ty1);
	    node_t *rty2 = rtype(ty2);
	    if (eqtype(unqual(rty1), unqual(rty2))) {
		ty = ptr_type(compose(rty1, rty2));
		then = bitconv(ty, then);
		els = bitconv(ty, els);
	    } else {
		error(INCOMPATIBLE_TYPES2, type2s(ty1), type2s(ty2));
	    }
	}
    } else {
	error("type mismatch in conditional expression: '%s' and '%s'", type2s(ty1), type2s(ty2));
    }
    
    if (NO_ERROR) {
	ret = ast_expr(COND_EXPR, 0, NULL, NULL);
	EXPR_COND(ret) = cond;
	EXPR_THEN(ret) = then;
	EXPR_ELSE(ret) = els;
	AST_TYPE(ret) = ty;
    }
    
    return ret;
}

static node_t * cond_expr()
{
    node_t * or1 = logic_or();
    if (token->id == '?')
        return cond_expr1(conv(or1));
    return or1;
}

node_t * assign_expr()
{
    node_t *or1 = logic_or();
    if (token->id == '?')
        return cond_expr1(conv(or1));
    if (is_assign_op(token->id)) {
        int t = token->id;
        expect(t);
	return assignop(t, or1, assign_expr());
    }
    return or1;
}

node_t * expression()
{
    node_t *assign1;
    
    assign1 = assign_expr();
    while (token->id == ',') {
        expect(',');
	assign1 = commaop(',', assign1, assign_expr());
    }
    
    return assign1;
}

static node_t * uop(int op, node_t *ty, node_t *l)
{
    node_t *node = ast_uop(op, ty, l);
    return node;
}

static node_t * bop(int op, node_t *l, node_t *r)
{
    node_t *node = NULL;
    node_t *ty;

    if (l == NULL || r == NULL)
	return NULL;

    SAVE_ERRORS;
    switch (op) {
    case '*': case '/':
	ensure_type(l, isarith);
	ensure_type(r, isarith);
	if (NO_ERROR) {
	    ty = conv2(AST_TYPE(l), AST_TYPE(r));
	    node = ast_bop(op, wrap(ty, l), wrap(ty, r));
	    AST_TYPE(node) = ty;
	}
	break;
    case '%':
    case LSHIFT: case RSHIFT:
    case '&': case '^': case '|':
	ensure_type(l, isint);
	ensure_type(r, isint);
	if (NO_ERROR) {
	    ty = conv2(AST_TYPE(l), AST_TYPE(r));
	    node = ast_bop(op, wrap(ty, l), wrap(ty, r));
	    AST_TYPE(node) = ty;
	}
	break;
    case '+':
	if (isptr(AST_TYPE(l))) {
	    ensure_type(r, isint);
	    if (NO_ERROR) {
		node = ast_bop(op, l, r);
		AST_TYPE(node) = AST_TYPE(l);
	    }
	} else if (isptr(AST_TYPE(r))) {
	    ensure_type(l, isint);
	    if (NO_ERROR) {
		node = ast_bop(op, l, r);
		AST_TYPE(node) = AST_TYPE(r);
	    }
	} else {
	    ensure_type(l, isarith);
	    ensure_type(r, isarith);
	    if (NO_ERROR) {
		ty = conv2(AST_TYPE(l), AST_TYPE(r));
		node = ast_bop(op, wrap(ty, l), wrap(ty, r));
		AST_TYPE(node) = ty;
	    }
	}
	break;
    case '-':
	if (isptr(AST_TYPE(l))) {
	    if (isint(AST_TYPE(r))) {
		node = ast_bop(op, l, r);
		AST_TYPE(node) = AST_TYPE(l);
	    } else if (isptr(AST_TYPE(r))) {
		node = ast_bop(op, l, r);
		AST_TYPE(node) = inttype;
	    } else {
		error("expect integer or pointer type, not type '%s'", type2s(AST_TYPE(r)));
	    }
	} else {
	    ensure_type(l, isarith);
	    ensure_type(r, isarith);
	    if (NO_ERROR) {
		ty = conv2(AST_TYPE(l), AST_TYPE(r));
		node = ast_bop(op, wrap(ty, l), wrap(ty, r));
		AST_TYPE(node) = ty;
	    }
	}
	break;
    case '>': case '<': case LEQ: case GEQ:
    case EQ: case NEQ:
	ensure_type(l, isscalar);
	ensure_type(r, isscalar);
	if (NO_ERROR) {
	    node = ast_bop(op, l, r);
	    AST_TYPE(node) = inttype;
	}
	break;
    default:
	error("unknown op '%s'", tname(op));
	CCAssert(0);
    }
    return node;
}

static node_t * logicop(int op, node_t *l, node_t *r)
{
    node_t *ret = NULL;

    if (l == NULL || r == NULL)
	return NULL;

    ret = ast_bop(op, l, r);
    AST_TYPE(ret) = inttype;
    return ret;
}

static node_t * commaop(int op, node_t *l, node_t *r)
{
    node_t *ret = NULL;

    if (l == NULL || r == NULL)
	return NULL;

    if (isarray(AST_TYPE(l)) || isfunc(AST_TYPE(l)))
    	l = decay(l);
    if (isarray(AST_TYPE(r)) || isfunc(AST_TYPE(r)))
    	r = decay(r);
    if (islvalue(l))
	l = ltor(l);
    if (islvalue(r))
	r = ltor(r);
    
    ret = ast_bop(op, l, r);
    AST_TYPE(ret) = AST_TYPE(r);
    return ret;
}

bool assign_types_check(node_t *ty1, node_t *r)
{
    node_t *ty2 = AST_TYPE(r);
    if ((isarith(ty1) && isarith(ty2)) ||
	(unqual(ty1) == booltype && isptr(ty2))) {
        return true;
    } else if ((isstruct(ty1) && isstruct(ty2)) ||
	       (isunion(ty1) && isunion(ty2))) {
	if (!eqtype(unqual(ty1), unqual(ty2)))
	    return false;
    } else if (isptr(ty1) && isptr(ty2)) {
	if (is_nullptr(r)) {
	    return true;
	} else if (isptrto(ty1, VOID) || isptrto(ty2, VOID)) {
	    node_t *vty = isptrto(ty1, VOID) ? ty1 : ty2;
	    node_t *tty = vty == ty1 ? ty2 : ty1;
	    if (isptrto(tty, FUNCTION)) {
		return false;
	    } else {
		node_t *rty1 = rtype(ty1);
		node_t *rty2 = rtype(ty2);
		int qual1 = isqual(rty1) ? TYPE_KIND(rty1) : 0;
		int qual2 = isqual(rty2) ? TYPE_KIND(rty2) : 0;
		if (!contains(qual1, qual2))
		    return false;
	    }
	} else {
	    node_t *rty1 = rtype(ty1);
	    node_t *rty2 = rtype(ty2);
	    if (eqtype(unqual(rty1), unqual(rty2))) {
		int qual1 = isqual(rty1) ? TYPE_KIND(rty1) : 0;
		int qual2 = isqual(rty2) ? TYPE_KIND(rty2) : 0;
		if (!contains(qual1, qual2))
		    return false;
	    } else {
		return false;
	    }
	}
    } else {
	return false;
    }
    return true;
}

static node_t * assignop(int op, node_t *l, node_t *r)
{
    node_t *ret = NULL;

    if (l == NULL || r == NULL)
	return NULL;

    node_t *retty = unqual(AST_TYPE(l));
    SAVE_ERRORS;
    ensure_assignable(l);
    if (HAS_ERROR)
	return NULL;
    if (op == '=') {
        if (!assign_types_check(AST_TYPE(l), r))
	    error(INCOMPATIBLE_TYPES, type2s(AST_TYPE(l)), type2s(AST_TYPE(r)));
    } else {
	int op2 = splitop(op);
	node_t *l1 = conv(l);
	node_t *r1 = conv(r);
	if (op2 == '+' || op2 == '-') {
	    node_t *ty1 = AST_TYPE(l1);
	    node_t *ty2 = AST_TYPE(r1);
	    if (!((isarith(ty1) && isarith(ty2)) ||
		  (isptr(ty1) && isint(ty2))))
	        error(INCOMPATIBLE_TYPES, type2s(ty1), type2s(ty2));
	}
	r = bop(op2, l1, r1);
    }

    if (NO_ERROR && (r = assignconv(retty, r))) {
	ret = ast_bop('=', l, r);
	AST_TYPE(ret) = retty;
    }

    return ret;
}

static const char * castname(node_t *ty, node_t *l)
{
    if (isfloat(ty) && isfloat(AST_TYPE(l)))
	return FloatCast;
    else if (isfloat(ty) && isint(AST_TYPE(l)))
        return IntegerToFloatCast;
    else if (isint(ty) && isint(AST_TYPE(l)))
	return IntegralCast;
    else if (isint(ty) && isfloat(AST_TYPE(l)))
	return FloatToIntegerCast;
    else
	return BitCast;
}

static node_t * wrap(node_t *ty, node_t *node)
{
    CCAssert(isarith(ty));
    CCAssert(isarith(AST_TYPE(node)));
    
    if (eqarith(ty, AST_TYPE(node)))
        return node;
    else
        return ast_conv(ty, node, castname(ty, node));
}

static node_t * bitconv(node_t *ty, node_t *node)
{
    if (eqtype(ty, AST_TYPE(node)))
	return node;
    else
	return ast_conv(ty, node, castname(ty, node));
}

static node_t * assignconv(node_t *ty, node_t *node)
{
    node_t *ret = NULL;
    node_t *ty2 = AST_TYPE(node);

    if (islvalue(node))
	node = ltor(node);
    
    if (isarith(ty) && isarith(ty2)) {
	ret = wrap(ty, node);
    } else if (ty == booltype && isptr(ty2)) {
	ret = ast_conv(ty, node, PointerToBoolean);
    } else if ((isstruct(ty) && isstruct(ty2)) ||
	       (isunion(ty) && isunion(ty2))) {
	ret = bitconv(ty, node);
    } else if (isptr(ty)) {
	ret = bitconv(ty, node);
    } else {
	error(INCOMPATIBLE_TYPES, type2s(ty2), type2s(ty));
    }
    return ret;
}

// Universal Binary Conversion
static node_t * conv2(node_t *l, node_t *r)
{
    CCAssert(isarith(l));
    CCAssert(isarith(r));
    
    CCAssert(size(l) >= size(inttype));
    CCAssert(size(r) >= size(inttype));
    
    node_t *max = rank(l) > rank(r) ? l : r;
    if (isfloat(l) || isfloat(r) || op(l) == op(r))
        return max;
    
    node_t *u = op(l) == UNSIGNED ? l : r;
    node_t *s = op(l) == INT ? l : r;
    CCAssert(unqual(s) == s);
    
    if (rank(u) >= rank(s))
        return u;
    
    if (size(u) < size(s)) {
        return s;
    } else {
        if (s == inttype)
            return unsignedinttype;
        else if (s == longtype)
            return unsignedlongtype;
        else
            return unsignedlonglongtype;
    }
    
    return l;
}

static node_t * decay(node_t *node)
{
    switch (kind(AST_TYPE(node))) {
    case FUNCTION:
	return ast_conv(ptr_type(AST_TYPE(node)), node, FunctionToPointerDecay);
            
    case ARRAY:
	return ast_conv(ptr_type(rtype(AST_TYPE(node))), node, ArrayToPointerDecay);

    default:
	return node;
    }
}

static node_t * ltor(node_t *node)
{
    return ast_conv(unqual(AST_TYPE(node)), node, LValueToRValue);
}

// Universal Unary Conversion
static node_t * conv(node_t *node)
{
    if (node == NULL)
	return NULL;
    if (islvalue(node))
	node = ltor(node);
    
    switch (kind(AST_TYPE(node))) {
    case _BOOL: case CHAR: case SHORT:
	return ast_conv(inttype, node, IntegralCast);
            
    case FUNCTION:            
    case ARRAY:
	return decay(node);
	
    default:
	return node;
    }
}

// Default function argument conversion
static node_t * conva(node_t *node)
{
    if (node == NULL)
	return NULL;
    if (islvalue(node))
	node = ltor(node);
    
    switch (kind(AST_TYPE(node))) {
    case FLOAT:
	return ast_conv(doubletype, node, FloatCast);
	
    default:
	return conv(node);
    }
}

// TODO: waiting for eval
static bool is_nullptr(node_t *node)
{
    CCAssert(isptr(AST_TYPE(node)));
    node_t *ret = eval(node);
    if (ret == NULL)
	return false;
    if (AST_ID(ret) == INTEGER_LITERAL) {
	if (op(AST_TYPE(ret)) == INT)
	    return SYM_VALUE(EXPR_SYM(ret)).i == 0;
	else
	    return SYM_VALUE(EXPR_SYM(ret)).u == 0;
    }
    return false;
}

// TODO: 
int intexpr()
{
    cond_expr();
    return 0;
}
