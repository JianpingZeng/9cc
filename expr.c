#include "cc.h"

static union node * cast_expr();
static union node * cond_expr();
static union node * cond_expr1(union node *o);
static union node * unary_expr();
static union node * uop(int op, struct type *ty, union node *l);
static union node * bop(int op, union node *l, union node *r);
static union node * logicop(int op, union node *l, union node *r);
static union node * commaop(int op, union node *l, union node *r);
static union node * assignop(int op, union node *l, union node *r);
static union node * conv(union node *node);
static struct type * conv2(struct type *l, struct type *r);
static union node * wrap(struct type *ty, union node *node);
static union node * bitcast(struct type *ty, union node *node);
static union node * eval(union node *expr);

#define SAVE_ERRORS    unsigned err = errors
#define NO_ERROR       (err == errors)
#define HAS_ERROR      (err != errors) 

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
    default: assert(0);
    }
}

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
    
    if (!is(AST_TYPE(node)))
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
    if (!is_lvalue(node))
        error("lvalue expect");
}

static const char * is_assignable(union node *node)
{
    struct type *ty = AST_TYPE(node);
    if (!is_lvalue(node))
	return "expression is not assignable";
    if (AST_ID(node) == PAREN_EXPR)
	return is_assignable(node);
    if (isarray(ty))
	return format("array type '%s' is not assignable", type2s(ty));
    if (isconst(ty)) {
	if (AST_ID(node) == REF_EXPR) {
	    return format("read-only variable '%s' is not assignable", EXPR_SYM(node)->name);
	} else if (AST_ID(node) == MEMBER_EXPR) {
	    const char *op = EXPR_OP(node) == '.' ? "." : "->";
	    const char *l = EXPR_SYM(EXPR_OPERAND(node, 0))->name;
	    const char *r = AST_NAME(EXPR_OPERAND(node, 1));
	    return format("read-only variable '%s%s%s' is not assignable", l, op, r);
	} else {
	    return "read-only variable is not assignable";
	}
    }
    
    return NULL;
}

static void ensure_assignable(union node *or)
{
    const char *msg = is_assignable(or);
    if (msg)
        error(msg);
}

static bool is_bitfield(union node *node)
{
    if (AST_ID(node) != MEMBER_EXPR)
        return false;

    struct type *ty = AST_TYPE(EXPR_OPERAND(node, 0));
    const char *name = AST_NAME(EXPR_OPERAND(node, 1));
    struct field *field = find_field(ty, name);
    return isbitfield(field);
}

static void ensure_funcall(struct type *fty, union node **args)
{
    if (!isfunc(fty))
	return;
    // TODO: 
}

static const char * is_castable(struct type *dst, struct type *src)
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

    return format("cast from '%s' to '%s' is invalid", type2s(src), type2s(dst));
}

static void ensure_cast(struct type *dst, struct type *src)
{
    const char *msg = is_castable(dst, src);
    if (msg)
	error(msg);
}

static bool is_nullptr(union node *node)
{
    assert(isptr(AST_TYPE(node)));
    union node *ret = eval(node);
    if (ret == NULL)
	return false;
    if (AST_ID(ret) == INTEGER_LITERAL) {
	if (op(AST_TYPE(ret)) == INT)
	    return EXPR_SYM(ret)->value.i == 0;
	else
	    return EXPR_SYM(ret)->value.u == 0;
    }
    return false;
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
    union node *ret = NULL;
    
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
	ret = uop(t, unsignedinttype, ast_type(ty));
	EXPR_SYM(ret) = anonymous(&identifiers, GLOBAL);
	EXPR_SYM(ret)->type = ty;
	EXPR_SYM(ret)->value.u = typesize(ty);
    }

    return ret;
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
    default:     return postfix_expr();
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
	and1 = logicop(AND, conv(and1), conv(inclusive_or()));
    }
    
    return and1;
}

static union node * logic_or()
{
    union node * or1;
    
    or1 = logic_and();
    while (token->id == OR) {
        expect(OR);
	or1 = logicop(OR, conv(or1), conv(logic_and()));
    }
    
    return or1;
}

static union node * cond_expr1(union node *cond)
{
    union node *ret = NULL;
    union node *then, *els;
    struct type *ty = NULL;

    expect('?');
    then = conv(expression());
    expect(':');
    els = conv(cond_expr());

    if (cond == NULL || then == NULL || els == NULL)
	return ret;

    struct type *ty1 = AST_TYPE(then);
    struct type *ty2 = AST_TYPE(els);

    SAVE_ERRORS;
    ensure_type(cond, isscalar);

    if (isarith(ty1) && isarith(ty2)) {
        ty = conv2(ty1, ty2);
        then = wrap(ty, then);
        els = wrap(ty, els);
    } else if ((isstruct(ty1) && isstruct(ty2)) ||
               (isunion(ty1) && isunion(ty2))) {
        if (!eqtype(ty1, ty2))
	    incompatible_types_error(ty1, ty2);
	ty = ty1;
    } else if (isvoid(ty1) && isvoid(ty2)) {
	ty = voidtype;
    } else if (isptr(ty1) && isptr(ty2)) {
	if (is_nullptr(then) || is_nullptr(els)) {
	    struct type *nty = is_nullptr(then) ? ty1 : ty2;
	    struct type *tty = nty == ty1 ? ty2 : ty1;
	    ty = ptr_type(compose(rtype(tty), rtype(nty)));
	    then = bitcast(ty, then);
	    els = bitcast(ty, els);
	} else if (isptrto(ty1, VOID) || isptrto(ty2, VOID)) {
	    struct type *vty = isptrto(ty1, VOID) ? ty1 : ty2;
	    struct type *tty = vty == ty1 ? ty2 : ty1;
	    if (isptrto(tty, FUNCTION)) {
	        incompatible_types_error(ty1, ty2);
	    } else {
		ty = ptr_type(compose(rtype(vty), rtype(tty)));
		then = bitcast(ty, then);
		els = bitcast(ty, els);
	    }
	} else {
	    struct type *rty1 = rtype(ty1);
	    struct type *rty2 = rtype(ty2);
	    if (eqtype(unqual(rty1), unqual(rty2))) {
		ty = ptr_type(compose(rty1, rty2));
		then = bitcast(ty, then);
		els = bitcast(ty, els);
	    } else {
		incompatible_types_error(ty1, ty2);
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

static union node * cond_expr()
{
    union node * or1 = logic_or();
    if (token->id == '?')
        return cond_expr1(conv(or1));
    return or1;
}

union node * assign_expr()
{
    union node *or1 = logic_or();
    if (token->id == '?')
        return cond_expr1(conv(or1));
    if (is_assign_op(token->id)) {
        int t = token->id;
        expect(t);
	return assignop(t, or1, assign_expr());
    }
    return or1;
}

union node * expression()
{
    union node *assign1;
    
    assign1 = assign_expr();
    while (token->id == ',') {
        expect(',');
	assign1 = commaop(',', assign1, assign_expr());
    }
    
    return assign1;
}

static union node * const_expr()
{
    return eval(cond_expr());
}

// TODO: 
union node * cast(union node *cnst, struct type *ty)
{
    if (isarith(ty)) {
	
    } else if (isptr(ty)) {

    } else if (isrecord(ty) || isarray(ty)) {

    } else {
	assert(0);
    }
}

//TODO
int intexpr()
{
    int n = 0;
    union node *cnst = const_expr();
    if (cnst && isint(AST_TYPE(cnst)))
        n = EXPR_SYM(cast(cnst, inttype))->value.i;
    else
	error("integer constant expression expected");
    
    return n;
}

// TODO: 
static bool eval_bool(union node *cond)
{
    return false;
}

static union node * eval(union node *expr)
{
    assert(isexpr(expr));
    switch (AST_ID(expr)) {
    case BINARY_OPERATOR:
        {
	    switch (EXPR_OP(expr)) {
	    case '=':
		return NULL;
	    case ',':
	
	    case '+':
	    case '-':
	    case '*':
	    case '/':
	    case '%':

	    case LSHIFT:
	    case RSHIFT:

	    case '>':
	    case '<':
	    case GEQ:
	    case LEQ:
	    case EQ:
	    case NEQ:

	    case '|':
	    case '&':
	    case '^':
	    
	    case AND:
	    case OR:
		if (eval(EXPR_OPERAND(expr, 0)) && eval(EXPR_OPERAND(expr, 1)))    
		    return expr;
		else
		    return NULL;
		break;
	    default:
		assert(0);
	    }
	}
    case UNARY_OPERATOR:
        {
	    switch (EXPR_OP(expr)) {
	    case INCR:
	    case DECR:
	    case '*':
		return NULL;
	    case '&':
	    case '+':
	    case '-':
	    case '~':
	    case '!':
		if (eval(EXPR_OPERAND(expr, 0)))
		    return expr;
		else
		    return NULL;
	    case SIZEOF:
		return expr;
	    default:
		assert(0);
	    }
	}
    case PAREN_EXPR:
    case CONV_EXPR:
    case COMPOUND_LITERAL:
    case CAST_EXPR:
	return eval(EXPR_OPERAND(expr, 0));
    case MEMBER_EXPR:
	if (eval(EXPR_OPERAND(expr, 0)))
	    return expr;
	else
	    return NULL;
    case COND_EXPR:
	{
	    union node *cond = eval(EXPR_COND(expr));
	    if (cond) {
		if (eval_bool(cond))
		    return eval(EXPR_THEN(expr));
		else
		    return eval(EXPR_ELSE(expr));
	    }
	}
	return NULL;
    case REF_EXPR:
        if (EXPR_OP(expr) == ENUM ||
	    EXPR_SYM(expr)->sclass == EXTERN ||
	    EXPR_SYM(expr)->sclass == STATIC ||
	    EXPR_SYM(expr)->scope == GLOBAL)
	    return expr;
	else
	    return NULL;
    case INITS_EXPR:
        for (int i = 0; i < array_len((void **)EXPR_INITS(expr)); i++) {
	    union node *n = EXPR_INITS(expr)[i];
	    if (AST_ID(n) != VINIT_EXPR && eval(n) == NULL)
		return NULL;
	}
	return expr;
    case INTEGER_LITERAL:
    case FLOAT_LITERAL:
    case STRING_LITERAL:
	return expr;
    case CALL_EXPR:
	return NULL;
    default:
	assert(0);
    }
}

static union node * uop(int op, struct type *ty, union node *l)
{
    union node *node = ast_uop(op, ty, l);
    return node;
}

static union node * bop(int op, union node *l, union node *r)
{
    union node *node = NULL;
    struct type *ty;

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
	assert(0);
    }
    return node;
}

static union node * logicop(int op, union node *l, union node *r)
{
    union node *ret = NULL;

    if (l == NULL || r == NULL)
	return NULL;

    ret = ast_bop(op, l, r);
    AST_TYPE(ret) = inttype;
    return ret;
}

static union node * commaop(int op, union node *l, union node *r)
{
    union node *ret = NULL;

    if (l == NULL || r == NULL)
	return NULL;

    ret = ast_bop(op, l, r);
    AST_TYPE(ret) = AST_TYPE(r);
    return ret;
}

static union node * assignop(int op, union node *l, union node *r)
{
    union node *ret = NULL;

    if (l == NULL || r == NULL)
	return NULL;

#define TYPE_ERROR(ty1, op, ty2)  error("'%s'%s'%s' is invalid", type2s(ty1), tname(op), type2s(ty2))

    SAVE_ERRORS;
    ensure_assignable(l);
    if (HAS_ERROR)
	goto out;
    if (op == '=') {
	struct type *ty1 = AST_TYPE(l);
	struct type *ty2 = AST_TYPE(r);
	if (isarith(ty1) && isarith(ty2)) {
	    goto out;
	} else if ((isstruct(ty1) && isstruct(ty2)) ||
	    (isunion(ty1) && isunion(ty2))) {
	    if (!eqtype(unqual(ty1), unqual(ty2)))
		TYPE_ERROR(ty1, op, ty2);
	} else if (isptr(ty1)) {

	} else {
	    TYPE_ERROR(ty1, op, ty2);
	}
    } else {
	int op2 = splitop(op);
	union node *l1 = conv(l);
	union node *r1 = conv(r);
	if (op2 == '+' || op2 == '-') {
	    struct type *ty1 = AST_TYPE(l1);
	    struct type *ty2 = AST_TYPE(r1);
	    if (!((isarith(ty1) && isarith(ty2)) ||
		  (isptr(ty1) && isint(ty2))))
	        TYPE_ERROR(ty1, op, ty2);
	}
	r = bop(op2, l1, r1);
    }
 out:
    if (NO_ERROR) {
	ret = ast_bop('=', l, r);
	AST_TYPE(ret) = unqual(AST_TYPE(l));
    }

#undef TYPE_ERROR
    return ret;
}

static union node * wrap(struct type *ty, union node *node)
{
    if (eqarith(ty, AST_TYPE(node)))
        return node;
    else
        return ast_conv(ty, node, "WrapCast");
}

static union node * bitcast(struct type *ty, union node *node)
{
    if (eqtype(ty, AST_TYPE(node)))
	return node;
    else
	return ast_conv(ty, node, BitCast);
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
    assert(unqual(s) == s);
    
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

// Universal Unary Conversion
static union node * conv(union node *node)
{
    if (node == NULL)
	return NULL;
    if (is_lvalue(node))
	node = ast_conv(unqual(AST_TYPE(node)), node, LValueToRValue);
    switch (kind(AST_TYPE(node))) {
    case _BOOL: case CHAR: case SHORT:
	return ast_conv(inttype, node, IntegralCast);
            
    case FUNCTION:
	return ast_conv(ptr_type(AST_TYPE(node)), node, FunctionToPointerDecay);
            
    case ARRAY:
	return ast_conv(ptr_type(rtype(AST_TYPE(node))), node, ArrayToPointerDecay);
            
    default:
	return node;
    }
}
