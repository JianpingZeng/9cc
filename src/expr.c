#include "cc.h"

static struct node * cast_expr();
static struct type * reduce(struct node *expr);
static void ensure_assignable(struct node *asign);
static bool is_lvalue(struct node *expr);
static struct node * cond_expr();
static struct node * cond_expr1(struct node *o);
static int eval(struct node *expr, int *error);

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
    
    switch (sym->type->op) {
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

static struct node * compound_literal()
{
    struct node * ret;
    struct node * inits;
    
    inits = initializer_list();
    ret = expr_node(COMPOUND_LITERAL, CCONSTANT, inits, NULL);
    
    return ret;
}

static struct node * typename_expr()
{
    struct node *expr;
    struct type *type;
    
    expect('(');
    type = typename();
    expect(')');
    expr = expr_node(CAST_EXPR, 'C', NULL, NULL);
    expr->type = type;
    
    return expr;
}

static struct node ** argument_expr_list()
{
    struct node **args = NULL;
    
    if (firstexpr(token)) {
        struct vector *v = new_vector();
        for (;;) {
            vec_push(v, assign_expr());
            if (token->id == ',')
                expect(',');
            else
                break;
        }
        args = (struct node **)vtoa(v);
    } else if (token->id != ')') {
        error("expect assignment expression");
    }
    
    return args;
}

static struct node * postfix_expr1(struct node *ret)
{
    int t;
    
    for (;token->id == '[' || token->id == '(' || token->id == '.'
         || token->id == DEREF || token->id == INCR || token->id == DECR;) {
        switch (token->id) {
            case '[':
                t = token->id;
                expect('[');
                ret = expr_node(INDEX_EXPR, t, ret, expression());
                expect(']');
                break;
            case '(':
                t = token->id;
                expect('(');
                ret = expr_node(CALL_EXPR, FUNCTION, ret, NULL);
                ret->u.e.args = argument_expr_list();
                expect(')');
                break;
            case '.':
            case DEREF:
            {
                t = token->id;
                expect(t);
                if (token->id == ID) {
                    struct type *lty = reduce(ret);
                    struct type *basety = t == DEREF ? lty->type : lty;
                    if (isstruct(basety) || isunion(basety)) {
                        int i;
                        for (i=0; i < ARRAY_SIZE(basety->u.s.fields); i++) {
                            struct field *f = basety->u.s.fields[i];
                            if (f->name && !strcmp(f->name, token->name))
                                break;
                        }
                        if (i >= ARRAY_SIZE(basety->u.s.fields))
                            error("no member named '%s' in '%s'", token->name, basety->name);
                    } else {
                        error("member reference base type '%s' is not a struct or union", basety->name);
                    }
                }
                ret = expr_node(MEMBER_EXPR, t, ret, expr_node(REF_EXPR, ID, NULL, NULL));
                expect(ID);
            }
                break;
            case INCR:
            case DECR:
                t = token->id;
                expect(token->id);
                ret = expr_node(UNARY_EXPR, t, ret, NULL);
                break;
            default:
                assert(0);
        }
    }
    
    return ret;
}

static struct node * primary_expr()
{
    int t;
    struct symbol *sym;
    struct node *ret;
    
    switch (token->id) {
        case ID:
        {
            t = token->id;
            sym = lookup(token->name, identifiers);
            if (sym)
                sym->refs++;
            else
                error("use of undeclared symbol '%s'", token->name);
            expect(t);
            ret = expr_node(REF_EXPR, ID, NULL, NULL);
            ret->sym = sym;
        }
            break;
        case ICONSTANT:
        case FCONSTANT:
        {
            t = token->id;
            sym = lookup(token->name, constants);
            if (!sym) {
                sym = install(token->name, &constants, CONSTANT);
                t == ICONSTANT ? integer_constant(token, sym) : float_constant(token, sym);
            }
            expect(t);
            ret = expr_node(t == ICONSTANT ? INTEGER_LITERAL : FLOAT_LITERAL, t, NULL, NULL);
            ret->sym = sym;
        }
            break;
        case SCONSTANT:
        {
            t = token->id;
            sym = lookup(token->name, constants);
            if (!sym) {
                sym = install(token->name, &constants, CONSTANT);
                string_constant(token, sym);
            }
            expect(t);
            ret = expr_node(STRING_LITERAL, t, NULL, NULL);
            ret->sym = sym;
        }
            break;
        case '(':
        {
            struct token *ahead = lookahead();
            if (istypename(ahead)) {
                ret = typename_expr();
                LEFT(ret) = compound_literal();
            } else {
                expect('(');
                ret = expr_node(PAREN_EXPR, '(', expression(), NULL);
                expect(')');
            }
        }
            break;
        default:
            ret = NULL;
            error("invalid postfix expression at '%s'", token->name);
            break;
    }
    
    return ret;
}

static struct node * postfix_expr()
{
    struct node * expr = primary_expr();
    
    return postfix_expr1(expr);
}

static struct node * unary_expr()
{
    struct node * ret;
    int t;
    struct token *ahead;
    
    switch (token->id) {
        case INCR:
        case DECR:
        {
            t = token->id;
            expect(t);
            ret = expr_node(UNARY_EXPR, t, unary_expr(), NULL);
            ret->u.e.prefix = true;
        }
            break;
        case '&':
        case '*':
        case '+':
        case '-':
        case '~':
        case '!':
        {
            t = token->id;
            expect(t);
            ret = expr_node(UNARY_EXPR, t, cast_expr(), NULL);
            if (t == '*') {
                struct type *p = reduce(LEFT(ret));
                if (!ispointer(p))
                    error("indirection requires pointer operand ('%s' invalid)", p->name);
            } else if (t == '&') {
                struct type *p = reduce(LEFT(ret));
                if (!is_lvalue(LEFT(ret)))
                    error("cannot take the address of an rvalue of type '%s'", p->name);
            }
        }
            break;
        case SIZEOF:
        {
            t = token->id;
            expect(token->id);
            ahead = lookahead();
            if (token->id == '(' && istypename(ahead)) {
                struct node *texpr = typename_expr();
                if (token->id == '{') {
                    LEFT(texpr) = compound_literal();
                    texpr = postfix_expr1(texpr);
                }
                ret = expr_node(UNARY_EXPR, t, texpr, NULL);
            } else {
                ret = expr_node(UNARY_EXPR, t, unary_expr(), NULL);
            }
        }
            break;
        default:
            ret = postfix_expr();
            break;
    }
    
    return ret;
}

static struct node * cast_expr()
{
    struct node * cast1;
    struct token * ahead = lookahead();
    
    if (token->id == '(' && istypename(ahead)) {
        cast1 = typename_expr();
        if (token->id == '{') {
            LEFT(cast1) = compound_literal();
            cast1 = postfix_expr1(cast1);
        } else {
            LEFT(cast1) = cast_expr();
        }
    } else {
        cast1 = unary_expr();
    }
    
    return cast1;
}

static struct node * multiple_expr()
{
    struct node * mulp1;
    
    mulp1 = cast_expr();
    while (token->id == '*' || token->id == '/' || token->id == '%') {
        int t = token->id;
        expect(token->id);
        mulp1 = expr_node(BINARY_EXPR, t, mulp1, cast_expr());
    }
    
    return mulp1;
}

static struct node * additive_expr()
{
    struct node * add1;
    
    add1 = multiple_expr();
    while (token->id == '+' || token->id == '-') {
        int t = token->id;
        expect(token->id);
        add1 = expr_node(BINARY_EXPR, t, add1, multiple_expr());
    }
    
    return add1;
}

static struct node * shift_expr()
{
    struct node * shift1;
    
    shift1 = additive_expr();
    while (token->id == LSHIFT || token->id == RSHIFT) {
        int t = token->id;
        expect(token->id);
        shift1 = expr_node(BINARY_EXPR, t, shift1, additive_expr());
    }
    
    return shift1;
}

static struct node * relation_expr()
{
    struct node * rel;
    
    rel = shift_expr();
    while (token->id == '<' || token->id == '>' || token->id == LEQ || token->id == GEQ) {
        int t = token->id;
        expect(token->id);
        rel = expr_node(BINARY_EXPR, t, rel, shift_expr());
    }
    
    return rel;
}

static struct node * equality_expr()
{
    struct node * equl;
    
    equl = relation_expr();
    while (token->id == EQ || token->id == NEQ) {
        int t = token->id;
        expect(token->id);
        equl = expr_node(BINARY_EXPR, t, equl, relation_expr());
    }
    
    return equl;
}

static struct node * and_expr()
{
    struct node * and1;
    
    and1 = equality_expr();
    while (token->id == '&') {
        expect('&');
        and1 = expr_node(BINARY_EXPR, '&', and1, equality_expr());
    }
    
    return and1;
}

static struct node * exclusive_or()
{
    struct node * eor;
    
    eor = and_expr();
    while (token->id == '^') {
        expect('^');
        eor = expr_node(BINARY_EXPR, '^', eor, and_expr());
    }
    
    return eor;
}

static struct node * inclusive_or()
{
    struct node * ior;
    
    ior = exclusive_or();
    while (token->id == '|') {
        expect('|');
        ior = expr_node(BINARY_EXPR, '|', ior, exclusive_or());
    }
    
    return ior;
}

static struct node * logic_and()
{
    struct node * and1;
    
    and1 = inclusive_or();
    while (token->id == AND) {
        expect(AND);
        and1 = expr_node(BINARY_EXPR, AND, and1, inclusive_or());
    }
    
    return and1;
}

static struct node * logic_or()
{
    struct node * or1;
    
    or1 = logic_and();
    while (token->id == OR) {
        expect(OR);
        or1 = expr_node(BINARY_EXPR, OR, or1, logic_and());
    }
    
    return or1;
}

static struct node * cond_expr1(struct node *cond)
{
    struct node *ret, *then, *els;
    expect('?');
    
    then = expression();
    expect(':');
    els = cond_expr();
    
    ret = expr_node(COND_EXPR, '?', NULL, NULL);
    ret->u.e.c.cond = cond;
    ret->u.e.c.then = then;
    ret->u.e.c.els = els;
    
    return ret;
}

static struct node * cond_expr()
{
    struct node * or1 = logic_or();
    if (token->id == '?')
        return cond_expr1(or1);
    return or1;
}

struct node * assign_expr()
{
    struct node *or1 = logic_or();
    if (token->id == '?')
        return cond_expr1(or1);
    if (is_assign_op(token->id)) {
        int t = token->id;
        expect(token->id);
        or1 = expr_node(BINARY_EXPR, t, or1, assign_expr());
        ensure_assignable(or1);
    }
    return or1;
}

struct node * expression()
{
    struct node *expr;
    
    expr = assign_expr();
    while (token->id == ',') {
        expect(',');
        expr = expr_node(BINARY_EXPR, ',', expr, assign_expr());
    }
    
    return expr;
}

int intexpr()
{
    struct source src = source;
    struct node *expr = cond_expr();
    int error = 0;
    int val = eval(expr, &error);
    if (error)
        errorf(src, "expect constant expression");
    
    return val;
}

//TODO
static int eval(struct node *expr, int *error)
{
    if (!expr || (error && *error))
        return 0;
    
    assert(isexpr(expr));
    
    bool bop = expr->id == BINARY_EXPR;
    struct node *l = LEFT(expr);
    struct node *r = RIGHT(expr);
#define L eval(l, error)
#define R eval(r, error)
    
    switch (expr->u.e.op) {
            //binary
        case ',': return L , R;
        case '/': return L / R;
        case '%': return L % R;
        case LSHIFT: return L << R;
        case RSHIFT: return L >> R;
        case '>': return L > R;
        case '<': return L < R;
        case GEQ: return L >= R;
        case LEQ: return L <= R;
        case EQ: return L == R;
        case NEQ: return L != R;
        case AND: return L && R;
        case OR: return L || R;
        case '^': return L ^ R;
        case '|': return L | R;
        case '+': return (bop ? (L + R) : (+L));
        case '-': return (bop ? (L - R) : (-L));
        case '*': if (bop) return L * R; else goto err;
        case '&': if (bop) return L & R; else goto err;
            
        case '~': return ~L;
        case '!': return !L;
        case SIZEOF:
            
            // cast TODO
        case 'C':
            return 0;
            
            // index TODO
        case '[':
            return 0;
            
            // member TODO
        case '.': case DEREF:
            return 0;
            
        case '?':
        {
            int cond = eval(expr->u.e.c.cond, error);
            if (cond)
                return eval(expr->u.e.c.then, error);
            else
                return eval(expr->u.e.c.els, error);
        }
            
            // paren
        case '(':
        case CCONSTANT:
            return L;
            
            // inits
        case '{':
        {
            if (expr->u.e.args)
                return eval(expr->u.e.args[0], error);
            else
                return 0;
        }
            
        case ICONSTANT:
        {
            struct symbol *sym = expr->sym;
            union value v = sym->value;
            if (sym->type->op == INT)
                return v.i;
            else if (sym->type->op == UNSIGNED)
                return v.u;
            else
                assert(0);
        }
            
        case FCONSTANT:
        {
            struct symbol *sym = expr->sym;
            union value v = sym->value;
            if (sym->type == floattype || sym->type == doubletype)
                return v.d;
            else if (sym->type == longdoubletype)
                return v.ld;
            else
                assert(0);
        }
            
        case SCONSTANT:
            return (const char *)expr->sym->name - (const char *)0;
            
        case INCR: case DECR:
        case '=':
        case MULEQ:case ADDEQ:case MINUSEQ:case DIVEQ:
        case MODEQ:case XOREQ:case BANDEQ:case BOREQ:
        case LSHIFTEQ:case RSHIFTEQ:
        case ID:
        case FUNCTION:
        err:
            if (error)
                *error = 1;
            return 0;
            
        default:
            assert(0);
    }
}

// TODO
static struct type * reduce(struct node *expr)
{
    return NULL;
}

// TODO
static bool is_lvalue(struct node *expr)
{
    return true;
}

// TODO
static void ensure_assignable(struct node *asign)
{
    struct node *l = LEFT(asign);
    struct node *r = RIGHT(asign);
    
    if (l == NULL || r == NULL)
        error("assign expression invalid");
    
    assert(isexpr(l) && isexpr(r));
    
}
