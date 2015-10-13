#include "cc.h"

enum {
    BLANK = 01, NEWLINE = 02, LETTER = 04,
    DIGIT = 010, HEX = 020, OTHER = 040,
};

static unsigned char map[256] = {
#define _a(a, b, c, d)     c,
#define _x(a, b, c, d)
#define _t(a, b, c)
#include "token.def"
    OTHER,
};

/* Don't use macros here, because macros make things wrong.
 * For example:
 *
 * #define is_visible(c)     ((c) >= 040 && (c) < 0177)
 *
 * Then:
 *
 * is_visible(*pc++)
 *
 * will be expanded to:
 *
 * ((*pc++) >= 040 && (*pc++) < 0177)
 *
 * which is not we want.
 *
 */

bool is_digit(char c)
{
    return map[(unsigned char)c] & DIGIT;
}

bool is_letter(char c)
{
    return map[(unsigned char)c] & LETTER;
}

bool is_digitletter(char c)
{
    return is_digit(c) || is_letter(c);
}

bool is_blank(char c)
{
    return map[(unsigned char)c] & BLANK;
}

bool is_newline(char c)
{
    return map[(unsigned char)c] & NEWLINE;
}

bool is_hex(char c)
{
    return map[(unsigned char)c] & HEX;
}

bool is_digithex(char c)
{
    return is_digit(c) || is_hex(c);
}

bool is_visible(char c)
{
    return c >= 040 && c < 0177;
}

static const char *tnames[] = {
#define _a(a, b, c, d)  b,
#define _x(a, b, c, d)  b,
#define _t(a, b, c)     b,
#include "token.def"
};

struct token *eoi_token = &(struct token){.id = EOI};
struct token *newline_token = &(struct token){.id = '\n', .name = "\n"};
struct token *space_token = &(struct token){.id = ' '};
static struct vector *buffers;

struct token *token;
struct source source;
static struct vector *tokens;

#define BOL    (current_file()->bol)

static void pin(struct cc_char *ch)
{
    struct file *fs = current_file();
    source.file = fs->file;
    source.line = ch->line;
    source.column = ch->column;
}

static struct source chsrc(struct cc_char *ch)
{
    struct file *fs = current_file();
    struct source src;
    src.file = fs->file;
    src.line = ch->line;
    src.column = ch->column;
    return src;
}

void mark(struct token *t)
{
    source = t->src;
}

static bool next(char c)
{
    struct cc_char *ch = readc();
    if (CH(ch) == c)
	return true;
    unreadc(ch);
    return false;
}

static char peek(void)
{
    struct cc_char *ch = readc();
    unreadc(ch);
    return CH(ch);
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

static void readch(struct strbuf *s, bool (*is) (char))
{
    for (;;) {
	struct cc_char *ch = readc();
	if (!is(CH(ch))) {
	    unreadc(ch);
	    break;
	}
	strbuf_catc(s, CH(ch));
    }
}

static void skipline(bool over)
{
    struct cc_char *ch;
    for (;;) {
	ch = readc();
	if (is_newline(CH(ch)) || CH(ch) == EOI)
	    break;
    }
    if (is_newline(CH(ch)) && !over)
	unreadc(ch);
}

static inline void line_comment(void)
{
    skipline(false);
}

static void block_comment(void)
{
    for (;;) {
	struct cc_char *ch = readc();
	if (CH(ch) == '*' && next('/'))
	    break;
	if (CH(ch) == EOI) {
	    error("unterminated /* comment");
	    break;
	}
    }
}

static struct token * ppnumber(char c)
{
    struct strbuf *s = strbuf_new();
    strbuf_catc(s, c);
    for (;;) {
	struct cc_char *ch = readc();
	if (!is_digitletter(CH(ch)) && CH(ch) != '.') {
	    unreadc(ch);
	    break;
	}
	bool is_float = strchr("eEpP", CH(ch)) && strchr("+-", peek());
	if (is_float) {
	    strbuf_catc(s, CH(ch));
	    ch = readc();
	    strbuf_catc(s, CH(ch));
	} else {
	    strbuf_catc(s, CH(ch));
	}
    }
    return make_token(&(struct token){.id = ICONSTANT, .name = strs(s->str)});
}

static void escape(struct strbuf *s, struct cc_char *ch)
{
    CCAssert(CH(ch) == '\\');
    struct source src = chsrc(ch);
    strbuf_catc(s, CH(ch));
    ch = readc();
    strbuf_catc(s, CH(ch));
    switch (CH(ch)) {
    case 'a': case 'b': case 'f':
    case 'n': case 'r': case 't':
    case 'v': case '\'': case '"':
    case '\\': case '\?':
	break;
    case '0': case '1': case '2':
    case '3': case '4': case '5':
    case '6': case '7':
	{
	    char c = peek();
	    if (c >= '0' && c <= '7') {
		strbuf_catc(s, CH(readc()));
		c = peek();
		if (c >= '0' && c <= '7') {
		    strbuf_catc(s, CH(readc()));
		}
	    }
	}
	break;
    case 'x':
	if (!is_digithex(peek())) {
	    errorf(src, "\\x used with no following hex digits");
	    break;
	}
	readch(s, is_digithex);
	break;
    case 'u': case 'U':
	{
            // universal character name: expect 4(u)/8(U) hex digits
	    int x;
            int n = CH(ch) == 'u' ? 4 : 8;
	    for (x = 0; x < n; x++) {
		ch = readc();
		if (!is_digithex(CH(ch))) {
		    unreadc(ch);
		    break;
		}
		strbuf_catc(s, CH(ch));
	    }
            if (x < n)
                errorf(src, "incomplete universal character name: %s", s->str);
        }
	break;
    default:
	errorf(src, "unrecognized escape character 0x%x", 0xFF & CH(ch));
	break;
    }
}

static struct token * sequence(bool wide, char sep)
{
    struct strbuf *s = strbuf_new();
    
    if (wide)
	strbuf_catc(s, 'L');
    strbuf_catc(s, sep);

    struct cc_char *ch;
    for (;;) {
	ch = readc();
	if (CH(ch) == sep || is_newline(CH(ch)) || CH(ch) == EOI)
	    break;
	if (CH(ch) == '\\')
	    escape(s, ch);
	else
	    strbuf_catc(s, CH(ch));
    }

    bool is_char = sep == '\'' ? true : false;
    const char *name = is_char ? "character" : "string";
    if (CH(ch) != sep)
	error("untermiated %s constant: %s", name, s->str);
    strbuf_catc(s, sep);

    if (is_char)
	return make_token(&(struct token){.id = ICONSTANT, .name = strs(s->str)});
    else
	return make_token(&(struct token){.id = SCONSTANT, .name = strs(s->str)});
}

static struct token * identifier(char c)
{
    struct strbuf *s = strbuf_new();
    strbuf_catc(s, c);
    readch(s, is_digitletter);
    return make_token(&(struct token){.id = ID, .name = strs(s->str)});
}

static struct token *newline(void)
{
    BOL = true;
    newline_token->src = source;
    return newline_token;
}

static struct token * spaces(char c)
{
    struct strbuf *s = strbuf_new();
    strbuf_catc(s, c);
    readch(s, is_blank);
    space_token->name = strs(s->str);
    space_token->src = source;
    return space_token;
}

struct token * dolex(void)
{
    register struct cc_char *rpc;
    
    for (; ;) {
	rpc = readc();
	pin(rpc);

	switch (CH(rpc)) {
	case EOI:
	    return eoi_token;
	    
	case '\n':
	    return newline();
	    
	    // spaces
	case TOK9:
	case TOK11:
	case TOK12:
	case TOK13:
	case TOK32:
	    return spaces(CH(rpc));
	
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
		return make_token(&(struct token){.id = CH(rpc)});
	    }

	case '+':
	    if (next('+'))
		return make_token(&(struct token){.id = INCR});
	    else if (next('='))
		return make_token(&(struct token){.id = ADDEQ});
	    else
		return make_token(&(struct token){.id = CH(rpc)});

	case '-':
	    if (next('-'))
		return make_token(&(struct token){.id = DECR});
	    else if (next('='))
		return make_token(&(struct token){.id = MINUSEQ});
	    else if (next('>'))
		return make_token(&(struct token){.id = DEREF});
	    else
		return make_token(&(struct token){.id = CH(rpc)});

	case '*':
	    if (next('='))
		return make_token(&(struct token){.id = MULEQ});
	    else
		return make_token(&(struct token){.id = CH(rpc)});

	case '=':
	    if (next('='))
		return make_token(&(struct token){.id = EQ});
	    else
		return make_token(&(struct token){.id = CH(rpc)});

	case '!':
	    if (next('='))
		return make_token(&(struct token){.id = NEQ});
	    else
		return make_token(&(struct token){.id = CH(rpc)});

	case '%':
	    if (next('=')) {
		return make_token(&(struct token){.id = MODEQ});
	    } else if (next('>')) {
		return make_token(&(struct token){.id = '}'});
	    } else if (next(':')) {
		if (next('%')) {
		    struct cc_char *ch = readc();
		    if (CH(ch) == ':')
			return make_token(&(struct token){.id = SHARPSHARP});
		    else
			unreadc(ch);
		}
		return make_token(&(struct token){.id = '#'});
	    } else {
		return make_token(&(struct token){.id = CH(rpc)});
	    }

	case '^':
	    if (next('='))
		return make_token(&(struct token){.id = XOREQ});
	    else
		return make_token(&(struct token){.id = CH(rpc)});

	case '&':
	    if (next('='))
		return make_token(&(struct token){.id = BANDEQ});
	    else if (next('&'))
		return make_token(&(struct token){.id = AND});
	    else
		return make_token(&(struct token){.id = CH(rpc)});

	case '|':
	    if (next('='))
		return make_token(&(struct token){.id = BOREQ});
	    else if (next('|'))
		return make_token(&(struct token){.id = OR});
	    else
		return make_token(&(struct token){.id = CH(rpc)});

	case '<':
	    if (next('=')) {
		return make_token(&(struct token){.id = LEQ});
	    } else if (next('<')) {
		struct cc_char *ch = readc();
		if (CH(ch) == '=')
		    return make_token(&(struct token){.id = LSHIFTEQ});
		else
		    unreadc(ch);
		return make_token(&(struct token){.id = LSHIFT});
	    } else if (next('%')) {
		return make_token(&(struct token){.id = '{'});
	    } else if (next(':')) {
		return make_token(&(struct token){.id = '['});
	    } else {
		return make_token(&(struct token){.id = CH(rpc)});
	    }

	case '>':
	    if (next('=')) {
		return make_token(&(struct token){.id = GEQ});
	    } else if (next('>')) {
	        struct cc_char *ch = readc();
		if (CH(ch) == '=')
		    return make_token(&(struct token){.id = RSHIFTEQ});
		else
		    unreadc(ch);
		return make_token(&(struct token){.id = RSHIFT});
	    } else {
		return make_token(&(struct token){.id = CH(rpc)});
	    }
	    
	case '(': case ')': case '{': case '}':
	case '[': case ']': case ',': case ';':
	case '~': case '?':
	    return make_token(&(struct token){.id = CH(rpc)});

	case ':':
	    if (next('>'))
		return make_token(&(struct token){.id = ']'});
	    else
		return make_token(&(struct token){.id = CH(rpc)});

	case '#':
	    if (next('#'))
		return make_token(&(struct token){.id = SHARPSHARP});
	    else
		return make_token(&(struct token){.id = CH(rpc)});

	    // constants
	case '\'':
	    return sequence(false, '\'');

	case '"':
	    return sequence(false, '"');


	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
	    return ppnumber(CH(rpc));

	case '.':
	    if (peek() == '.') {
		struct cc_char *ch1 = readc();
		struct cc_char *ch2 = readc();
		if (CH(ch2) == '.')
		    return make_token(&(struct token){.id = ELLIPSIS});
		unreadc(ch2);
		unreadc(ch1);
		return make_token(&(struct token){.id = CH(rpc)});
	    } else if (is_digit(peek())) {
		return ppnumber(CH(rpc));
	    } else {
		return make_token(&(struct token){.id = CH(rpc)});
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
	    return identifier(CH(rpc));

	default:
	    // invalid character
	    if (!is_blank(CH(rpc))) {
		if (is_visible(CH(rpc)))
		    error("invalid character '%c'", CH(rpc));
		else
		    error("invalid character '\\0%o'", 0xFF & CH(rpc));
	    }
	}
    }
}

static const char * hq_char_sequence(char sep)
{
    struct strbuf *s = strbuf_new();
    struct cc_char *ch;
    
    for (;;) {
	ch = readc();
	if (CH(ch) == sep || is_newline(CH(ch)) || CH(ch) == EOI)
	    break;
	strbuf_catc(s, CH(ch));
    }

    if (CH(ch) != sep)
	error("missing '%c' in header name", sep);
    
    skipline(true);
    return strbuf_str(s);
}

struct token *header_name(void)
{
    struct cc_char *ch;
 beg:
    ch = readc();
    if (is_blank(CH(ch)))
	goto beg;

    pin(ch);
    if (CH(ch) == '<') {
	const char *name = hq_char_sequence('>');
	return new_token(&(struct token){.name = name, .kind = '<'});
    } else if (CH(ch) == '"') {
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
    vec_push(vec_tail(buffers), t);
}

// create a temp file
// so that get_pptok will not
// generate 'unterminated conditional directive'
void buffer_stub(struct vector *v)
{
    file_stub(with_shadow());
    vec_push(buffers, v);
}

void buffer_unstub(void)
{
    vec_pop(buffers);
    file_unstub();
}

// parse the input string to a token
struct token * with_temp_lex(const char *input)
{
    struct source src = source;
    file_stub(with_string(input, NULL));
    struct token *t = dolex();
    next('\n');
    if (peek() != EOI) {
	struct token *t2 = dolex();
	errorf(src, "pasting formed '%s%s', an invalid preprocessing token", t->name, t2->name);
    }
    file_unstub();
    return t;
}

static void skip_sequence(char sep)
{
    struct cc_char *ch;
    for (;;) {
	ch = readc();
	if (CH(ch) == sep || is_newline(CH(ch)) || CH(ch) == EOI)
	    break;
	if (CH(ch) == '\\')
	    readc();
    }
    if (CH(ch) != sep)
	unreadc(ch);
}

void skip_spaces(void)
{
    // skip spaces, including comments
    struct cc_char *ch;

 beg:
    ch = readc();
    if (is_blank(CH(ch))) {
	goto beg;
    } else if (CH(ch) == '/') {
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
    lex();
    lines++;
    for (;;) {
	// skip spaces
        skip_spaces();
	struct cc_char *ch = readc();
	if (CH(ch) == EOI)
	    break;
	if (is_newline(CH(ch))) {
	    bol = true;
	    lines++;
	    continue;
	}
	if (CH(ch) == '\'' || CH(ch) == '"') {
	    skip_sequence(CH(ch));
	    bol = false;
	    continue;
	}
	if (CH(ch) != '#' || !bol) {
	    bol = false;
	    continue;
	}
	struct source src = chsrc(ch);
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
    struct vector *v = vec_tail(buffers);
    struct token *t;
    if (vec_len(v))
	// no matter which is the last
	t = vec_pop(v);
    else if (vec_len(buffers) > 1)
	// if the last vec is empty and buffers len > 1
	t = eoi_token;
    else
	// do lex
	t = dolex();
    mark(t);
    return t;
}

void lex_init(void)
{
    buffers = vec_new();
    vec_push(buffers, vec_new());
    tokens = vec_new();
}

const char *id2s(int t)
{
    if (t < 0)
        return "EOI";
    else if (t < 128)
        return tnames[t];
    else if (t < 256)
        return "(null)";
    else if (t < TOKEND)
        return tnames[128+t-ID];
    else
        return "(null)";
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

static void unget_token(struct token *t)
{
    vec_push(tokens, t);
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
    if (vec_len(tokens))
	return vec_pop(tokens);
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
    t->name = strs(s->str);
    return t;
}

static struct token * do_gettok(void)
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

int gettok(void)
{
    token = do_gettok();
    mark(token);
    return token->id;
}

struct token * lookahead(void)
{
    return peek_token();
}
