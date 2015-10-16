#include "cc.h"

static const char *tnames[] = {
#define _a(a, b, c)     b,
#define _x(a, b, c, d)  b,
#define _t(a, b, c)     b,
#define _k(a, b, c)     b,
#include "token.def"
};

struct token *eoi_token = &(struct token){.id = EOI};
struct token *newline_token = &(struct token){.id = '\n', .name = "\n"};
struct token *space_token = &(struct token){.id = ' '};

struct source source;

#define BOL    (current_file()->bol)

int isletter(int c)
{
    return isalpha(c) || c == '_';
}

int isxalpha(int c)
{
    return (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f');
}

static inline int isnewline(int c)
{
    return c == '\n';
}

static inline int isdigitletter(int c)
{
    return isdigit(c) || isletter(c);
}

static inline int iswhitespace(int c)
{
    return c == ' ' || c == '\t' || c == '\f' || c == '\v' || c == '\r';
}

static struct source chsrc()
{
    struct file *fs = current_file();
    struct source src;
    src.file = fs->name;
    src.line = fs->line;
    src.column = fs->column;
    return src;
}

static void markc()
{
    struct file *fs = current_file();
    source.file = fs->name;
    source.line = fs->line;
    source.column = fs->column;
}

static inline void mark(struct token *t)
{
    source = t->src;
}

static bool next(int c)
{
    int ch = readc();
    if (ch == c)
	return true;
    unreadc(ch);
    return false;
}

static int peek(void)
{
    int ch = readc();
    unreadc(ch);
    return ch;
}

struct token * new_token(struct token *tok)
{
    struct token *t = alloc_token();
    memcpy(t, tok, sizeof(struct token));
    if (!tok->name)
	t->name = id2s(tok->id);
    return t;
}

static struct token * make_token(struct token *tok)
{
    struct token *t = new_token(tok);
    t->src = source;
    t->bol = BOL;
    BOL = false;
    return t;
}

static void readch(struct strbuf *s, int (*is) (int))
{
    for (;;) {
        int ch = readc();
	if (!is(ch)) {
	    unreadc(ch);
	    break;
	}
	strbuf_catc(s, ch);
    }
}

static void skipline(bool over)
{
    int ch;
    for (;;) {
	ch = readc();
	if (isnewline(ch) || ch == EOI)
	    break;
    }
    if (isnewline(ch) && !over)
	unreadc(ch);
}

static inline void line_comment(void)
{
    skipline(false);
}

static void block_comment(void)
{
    for (;;) {
        int ch = readc();
	if (ch == '*' && next('/'))
	    break;
	if (ch == EOI) {
	    error("unterminated /* comment");
	    break;
	}
    }
}

static struct token * ppnumber(int c)
{
    struct strbuf *s = strbuf_new();
    strbuf_catc(s, c);
    for (;;) {
        int ch = readc();
	if (!isdigitletter(ch) && ch != '.') {
	    unreadc(ch);
	    break;
	}
	bool is_float = strchr("eEpP", ch) && strchr("+-", peek());
	if (is_float) {
	    strbuf_catc(s, ch);
	    ch = readc();
	    strbuf_catc(s, ch);
	} else {
	    strbuf_catc(s, ch);
	}
    }
    return make_token(&(struct token){.id = ICONSTANT, .name = strs(s->str)});
}

static void escape(struct strbuf *s)
{
    // current char is '\\'
    struct source src = chsrc();
    strbuf_catc(s, '\\');
    int ch = readc();
    strbuf_catc(s, ch);
    switch (ch) {
    case 'a': case 'b': case 'f':
    case 'n': case 'r': case 't':
    case 'v': case '\'': case '"':
    case '\\': case '\?':
	break;
    case '0': case '1': case '2':
    case '3': case '4': case '5':
    case '6': case '7':
	{
	    int c = peek();
	    if (c >= '0' && c <= '7') {
		strbuf_catc(s, readc());
		c = peek();
		if (c >= '0' && c <= '7') {
		    strbuf_catc(s, readc());
		}
	    }
	}
	break;
    case 'x':
	if (!ishexnumber(peek())) {
	    errorf(src, "\\x used with no following hex digits");
	    break;
	}
	readch(s, ishexnumber);
	break;
    case 'u': case 'U':
	{
            // universal character name: expect 4(u)/8(U) hex digits
	    int x;
            int n = ch == 'u' ? 4 : 8;
	    for (x = 0; x < n; x++) {
		ch = readc();
		if (!ishexnumber(ch)) {
		    unreadc(ch);
		    break;
		}
		strbuf_catc(s, ch);
	    }
            if (x < n)
                errorf(src, "incomplete universal character name: %s", s->str);
        }
	break;
    default:
	errorf(src, "unrecognized escape character 0x%x", ch);
	break;
    }
}

static struct token * sequence(bool wide, int sep)
{
    struct strbuf *s = strbuf_new();
    
    if (wide)
	strbuf_catc(s, 'L');
    strbuf_catc(s, sep);

    int ch;
    for (;;) {
	ch = readc();
	if (ch == sep || isnewline(ch) || ch == EOI)
	    break;
	if (ch == '\\')
	    escape(s);
	else
	    strbuf_catc(s, ch);
    }

    bool is_char = sep == '\'' ? true : false;
    const char *name = is_char ? "character" : "string";
    if (ch != sep)
	error("untermiated %s constant: %s", name, s->str);
    strbuf_catc(s, sep);

    if (is_char)
	return make_token(&(struct token){.id = ICONSTANT, .name = strs(s->str)});
    else
	return make_token(&(struct token){.id = SCONSTANT, .name = strbuf_str(s)});
}

static struct token * identifier(int c)
{
    struct strbuf *s = strbuf_new();
    strbuf_catc(s, c);
    readch(s, isdigitletter);
    return make_token(&(struct token){.id = ID, .name = strs(s->str)});
}

static struct token *newline(void)
{
    BOL = true;
    newline_token->src = source;
    return newline_token;
}

static struct token * spaces(int c)
{
    struct strbuf *s = strbuf_new();
    strbuf_catc(s, c);
    readch(s, iswhitespace);
    space_token->name = strbuf_str(s);
    space_token->src = source;
    return space_token;
}

struct token * dolex(void)
{
    register int rpc;
    
    for (; ;) {
	rpc = readc();
	markc();

	switch (rpc) {
	case EOI:
	    return eoi_token;
	    
	case '\n':
	    return newline();
	    
	    // spaces
	case '\t':
	case '\v':
	case '\f':
	case '\r':
	case ' ':
	    return spaces(rpc);
	
	    // punctuators
	case '/':
	    if (next('/')) {
		line_comment();
		continue;
	    } else if (next('*')) {
		block_comment();
		continue;
	    } else if (next('=')) {
		return make_token(&(struct token){.id = DIVEQ});
	    } else {
		return make_token(&(struct token){.id = rpc});
	    }

	case '+':
	    if (next('+'))
		return make_token(&(struct token){.id = INCR});
	    else if (next('='))
		return make_token(&(struct token){.id = ADDEQ});
	    else
		return make_token(&(struct token){.id = rpc});

	case '-':
	    if (next('-'))
		return make_token(&(struct token){.id = DECR});
	    else if (next('='))
		return make_token(&(struct token){.id = MINUSEQ});
	    else if (next('>'))
		return make_token(&(struct token){.id = DEREF});
	    else
		return make_token(&(struct token){.id = rpc});

	case '*':
	    if (next('='))
		return make_token(&(struct token){.id = MULEQ});
	    else
		return make_token(&(struct token){.id = rpc});

	case '=':
	    if (next('='))
		return make_token(&(struct token){.id = EQ});
	    else
		return make_token(&(struct token){.id = rpc});

	case '!':
	    if (next('='))
		return make_token(&(struct token){.id = NEQ});
	    else
		return make_token(&(struct token){.id = rpc});

	case '%':
	    if (next('=')) {
		return make_token(&(struct token){.id = MODEQ});
	    } else if (next('>')) {
		return make_token(&(struct token){.id = '}'});
	    } else if (next(':')) {
		if (next('%')) {
		    if (next(':'))
			return make_token(&(struct token){.id = SHARPSHARP});
		    unreadc('%');
		}
		return make_token(&(struct token){.id = '#'});
	    } else {
		return make_token(&(struct token){.id = rpc});
	    }

	case '^':
	    if (next('='))
		return make_token(&(struct token){.id = XOREQ});
	    else
		return make_token(&(struct token){.id = rpc});

	case '&':
	    if (next('='))
		return make_token(&(struct token){.id = BANDEQ});
	    else if (next('&'))
		return make_token(&(struct token){.id = AND});
	    else
		return make_token(&(struct token){.id = rpc});

	case '|':
	    if (next('='))
		return make_token(&(struct token){.id = BOREQ});
	    else if (next('|'))
		return make_token(&(struct token){.id = OR});
	    else
		return make_token(&(struct token){.id = rpc});

	case '<':
	    if (next('=')) {
		return make_token(&(struct token){.id = LEQ});
	    } else if (next('<')) {
		if (next('='))
		    return make_token(&(struct token){.id = LSHIFTEQ});
		else
		    return make_token(&(struct token){.id = LSHIFT});
	    } else if (next('%')) {
		return make_token(&(struct token){.id = '{'});
	    } else if (next(':')) {
		return make_token(&(struct token){.id = '['});
	    } else {
		return make_token(&(struct token){.id = rpc});
	    }

	case '>':
	    if (next('=')) {
		return make_token(&(struct token){.id = GEQ});
	    } else if (next('>')) {
		if (next('='))
		    return make_token(&(struct token){.id = RSHIFTEQ});
		else
		    return make_token(&(struct token){.id = RSHIFT});
	    } else {
		return make_token(&(struct token){.id = rpc});
	    }
	    
	case '(': case ')': case '{': case '}':
	case '[': case ']': case ',': case ';':
	case '~': case '?':
	    return make_token(&(struct token){.id = rpc});

	case ':':
	    if (next('>'))
		return make_token(&(struct token){.id = ']'});
	    else
		return make_token(&(struct token){.id = rpc});

	case '#':
	    if (next('#'))
		return make_token(&(struct token){.id = SHARPSHARP});
	    else
		return make_token(&(struct token){.id = rpc});

	    // constants
	case '\'':
	    return sequence(false, '\'');

	case '"':
	    return sequence(false, '"');


	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
	    return ppnumber(rpc);

	case '.':
	    if (next('.')) {
		if (next('.'))
		    return make_token(&(struct token){.id = ELLIPSIS});
		unreadc('.');
		return make_token(&(struct token){.id = rpc});
	    } else if (isdigit(peek())) {
		return ppnumber(rpc);
	    } else {
		return make_token(&(struct token){.id = rpc});
	    }
	    
	    // identifiers
	case 'L':
	    if (next('\''))
		return sequence(true, '\'');
	    else if (next('"'))
		return sequence(true, '"');
	    // go through
	case 'a': case 'b': case 'c': case 'd':
	case 'e': case 'f': case 'g': case 'h':
	case 'i': case 'j': case 'k': case 'l':
	case 'm': case 'n': case 'o': case 'p':
	case 'q': case 'r': case 's': case 't':
	case 'u': case 'v': case 'w': case 'x':
	case 'y': case 'z':
	case 'A': case 'B': case 'C': case 'D':
	case 'E': case 'F': case 'G': case 'H':
	case 'I': case 'J': case 'K':
	case 'M': case 'N': case 'O': case 'P':
	case 'Q': case 'R': case 'S': case 'T':
	case 'U': case 'V': case 'W': case 'X':
	case 'Y': case 'Z':
	case '_':
	    return identifier(rpc);

	default:
	    // illegal character
	    if (isgraph(rpc))
		error("illegal character '%c'", rpc);
	    else
		error("illegal character '\\0%o'", rpc);
	}
    }
}

static const char * hq_char_sequence(int sep)
{
    struct strbuf *s = strbuf_new();
    int ch;
    
    for (;;) {
	ch = readc();
	if (ch == sep || isnewline(ch) || ch == EOI)
	    break;
	strbuf_catc(s, ch);
    }

    if (ch != sep)
	error("missing '%c' in header name", sep);
    
    skipline(true);
    return strbuf_str(s);
}

struct token *header_name(void)
{
    int ch;
 beg:
    ch = readc();
    if (iswhitespace(ch))
	goto beg;

    markc();
    if (ch == '<') {
	const char *name = hq_char_sequence('>');
	return new_token(&(struct token){.name = name, .kind = '<'});
    } else if (ch == '"') {
	const char *name = hq_char_sequence('"');
	return new_token(&(struct token){.name = name, .kind = '"'});
    } else {
	// pptokens
	unreadc(ch);
	return NULL;
    }
}

void unget(struct token *t)
{
    vec_push(current_file()->buffer, t);
}

static void skip_sequence(int sep)
{
    int ch;
    for (;;) {
	ch = readc();
	if (ch == sep || isnewline(ch) || ch == EOI)
	    break;
	if (ch == '\\')
	    readc();
    }
    if (ch != sep)
	unreadc(ch);
}

void skip_spaces(void)
{
    // skip spaces, including comments
    int ch;

 beg:
    ch = readc();
    if (iswhitespace(ch)) {
	goto beg;
    } else if (ch == '/') {
	if (next('/')) {
	    line_comment();
	    goto beg;
	} else if (next('*')) {
	    block_comment();
	    goto beg;
	}
    }
    unreadc(ch);
}

void skip_ifstub(void)
{
    /* Skip part of conditional group.
     */
    unsigned lines = 0;
    bool bol = true;
    int nest = 0;
    struct token *t0 = lex();
    lines++;
    CCAssert(IS_NEWLINE(t0) || t0->id == EOI);
    for (;;) {
	// skip spaces
        skip_spaces();
        int ch = readc();
	if (ch == EOI)
	    break;
	if (isnewline(ch)) {
	    bol = true;
	    lines++;
	    continue;
	}
	if (ch == '\'' || ch == '"') {
	    skip_sequence(ch);
	    bol = false;
	    continue;
	}
	if (ch != '#' || !bol) {
	    bol = false;
	    continue;
	}
	struct source src = chsrc();
        struct token *t = lex();
	while (IS_SPACE(t))
	    t = lex();
        if (t->id != ID) {
	    if (IS_NEWLINE(t)) {
		bol = true;
		lines++;
	    } else {
		bol = false;
	    }
	    continue;
	}
	const char *name = t->name;
	if (!strcmp(name, "if") || !strcmp(name, "ifdef") || !strcmp(name, "ifndef")) {
	    nest++;
	    bol = false;
	    continue;
	}
	if (!nest &&
	    (!strcmp(name, "elif") || !strcmp(name, "else") || !strcmp(name, "endif"))) {
	    // found
	    unget(t);
	    struct token *t0 = new_token(&(struct token){.id = '#', .src = src, .bol = true});
	    unget(t0);
	    break;
	}
	if (nest && !strcmp(name, "endif")) {
	    nest--;
	    bol = false;
	}
	skipline(false);
    }

    while (lines-- > 0)
	unget(newline_token);
}

struct token * lex(void)
{
    struct vector *v = current_file()->buffer;
    struct token *t;
    if (vec_len(v))
	t = vec_pop(v);
    else
	t = dolex();
    mark(t);
    return t;
}

const char *id2s(int t)
{
    if (t < 0)
        return "(null)";
    else if (t < 128)
        return tnames[t];
    else if (t < 256)
        return "(null)";
    else if (t < TOKEND)
        return tnames[128+t-ID];
    else
        return "(null)";
}

static void unget_token(struct token *t)
{
    vec_push(current_file()->tokens, t);
}

static struct token * do_one_token(void)
{
    for (;;) {
	struct token *t = get_pptok();
	if (IS_SPACE(t) || IS_NEWLINE(t) || IS_LINENO(t))
	    continue;
	return t;
    }
}

static struct token * one_token(void)
{
    if (vec_len(current_file()->tokens))
	return vec_pop(current_file()->tokens);
    else
	return do_one_token();
}

static struct token * peek_token(void)
{
    struct token *t = one_token();
    unget_token(t);
    return t;
}

const char *unwrap_scon(const char *name)
{
    struct strbuf *s = strbuf_new();
    
    if (name[0] == '"')
	strbuf_catn(s, name+1, strlen(name)-2);
    else
	strbuf_catn(s, name+2, strlen(name)-3);

    return strbuf_str(s);
}

static struct token * combine_scons(struct vector *v, bool wide)
{
    struct token *t = new_token(vec_head(v));
    struct strbuf *s = strbuf_new();
    if (wide)
	strbuf_catc(s, 'L');
    strbuf_catc(s, '"');
    for (int i = 0; i < vec_len(v); i++) {
	struct token *ti = vec_at(v, i);
	const char *name = unwrap_scon(ti->name);
	if (name)
	    strbuf_cats(s, name);
    }
    strbuf_catc(s, '"');
    t->name = strbuf_str(s);
    return t;
}

static struct token * do_cctoken(void)
{
    struct token *t = one_token();
    if (t->id == SCONSTANT) {
	struct vector *v = vec_new1(t);
	struct token *t1 = peek_token();
	bool wide = t->name[0] == 'L';
	while (t1->id == SCONSTANT) {
	    if (t1->name[0] == 'L')
		wide = true;
	    vec_push(v, one_token());
	    t1 = peek_token();
	}
	if (vec_len(v) > 1)
	    return combine_scons(v, wide);
    }
    return t;
}

/* Parser interfaces
 *
 * 1. gettok
 * 2. lookahead
 * 3. expect
 * 4. match
 */

static int kinds[] = {
#define _a(a, b, c)     c,
#define _x(a, b, c, d)  c,
#define _t(a, b, c)     c,
#define _k(a, b, c)     c,
#include "token.def"
};

static const char *kws[] = {
#define _a(a, b, c)
#define _x(a, b, c, d)
#define _t(a, b, c)
#define _k(a, b, c)  b,
#include "token.def"
};

static int kwi[] = {
#define _a(a, b, c)
#define _x(a, b, c, d)
#define _t(a, b, c)
#define _k(a, b, c)  a,
#include "token.def"
};

struct token *token;
#define ahead_token  (current_file()->ahead)

static int tkind(int t)
{
    if (t < 0)
        return 0;
    else if (t < 128)
        return kinds[t];
    else if (t >= ID && t < TOKEND)
        return kinds[128+t-ID];
    else
        return 0;
}

static struct token * cctoken(void)
{
    struct token *t = do_cctoken();
    // keywords
    if (t->id == ID) {
	for (int i = 0; i < ARRAY_SIZE(kws); i++) {
	    if (!strcmp(t->name, kws[i])) {
		t->id = kwi[i];
		break;
	    }
	}
    }
    // TODO: ppnumber

    // set kind finally
    t->kind = tkind(t->id);
    return t;
}

int gettok(void)
{
    if (ahead_token) {
	token = ahead_token;
	ahead_token = NULL;
    } else {
	token = cctoken();
    }
    mark(token);
    return token->id;
}

struct token * lookahead(void)
{
    if (ahead_token == NULL)
	ahead_token = cctoken();
    return ahead_token;
}

void expect(int t)
{
    if (token->id == t) {
        gettok();
    } else {
        if (token->id == EOI)
            error("expect token '%s' at the end", id2s(t));
        else
            error("expect token '%s' before '%s'", id2s(t), token->name);
    }
}

void match(int t, int follow[])
{
    if (token->id == t) {
        gettok();
    } else {
        int n;
        expect(t);
        for (n=0; token->id != EOI; gettok()) {
            int *k;
            for (k=follow; *k && *k != token->kind; k++)
		; // continue
            if (*k == token->kind)
                break;
        }
        
        if (n > 0)
            fprintf(stderr, "%d tokens skipped.\n", n);
    }
}
