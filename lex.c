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

static const char *tnames[] = {
#define _a(a, b, c, d)  b,
#define _x(a, b, c, d)  b,
#define _t(a, b, c)     b,
#include "token.def"
};

struct cc_char {
    char ch;
    unsigned line;
    unsigned column;
};

#define CH(c)    ((c)->ch)

#define LBUFSIZE     64
#define RBUFSIZE     4096
#define MAXTOKEN     LBUFSIZE

struct cc_file {
    char buf[LBUFSIZE+RBUFSIZE+1];
    char *pc;
    char *pe;
    long bread;
    struct vector *chars;	// unread chars
    FILE *fp;
    const char *file;
    unsigned line;
    unsigned column;
};

static struct vector *files;

struct token *token;
static struct token *eoi_token = &(struct token){.id = EOI};
static struct token *newline_token = &(struct token){.id = '\n', .name = "\n"};
static struct token *space_token = &(struct token){.id = ' '};
static struct vector *buffers;

struct source source;

static void genlineno(unsigned line, const char *file);

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

static void fillbuf(struct cc_file *fs)
{
    if (fs->bread == 0) {
        if (fs->pc > fs->pe)
            fs->pc = fs->pe;
        return;
    }
    
    if (fs->pc >= fs->pe) {
        fs->pc = &fs->buf[LBUFSIZE];
    } else {
        long n;
        char *dst, *src;
        
        // copy
        n = fs->pe - fs->pc;
        dst = &fs->buf[LBUFSIZE] - n;
        src = fs->pc;
        while (src < fs->pe)
            *dst++ = *src++;
        
        fs->pc = &fs->buf[LBUFSIZE] - n;
    }

    if (feof(fs->fp))
        fs->bread = 0;
    else
        fs->bread = fread(&fs->buf[LBUFSIZE], 1, RBUFSIZE, fs->fp);
    
    if (fs->bread < 0)
        die("read error: %s", strerror(errno));
    
    fs->pe = &fs->buf[LBUFSIZE] + fs->bread;

    /* Add a newline character to the end if the
     * file doesn't have one, thus the include
     * directive would work well.
     */
    if (fs->pe < &fs->buf[LBUFSIZE+RBUFSIZE]) {
	if (fs->pe > fs->pc && fs->pe[-1] != '\n')
	    *fs->pe++ = '\n';
    }
    *fs->pe = 0;
}

static inline struct cc_file * current_file(void)
{
    return vec_tail(files);
}

static void pin(struct cc_char *ch)
{
    struct cc_file *fs = current_file();
    source.file = fs->file;
    source.line = ch->line;
    source.column = ch->column;
}

static struct source chsrc(struct cc_char *ch)
{
    struct cc_file *fs = current_file();
    struct source src;
    src.file = fs->file;
    src.line = ch->line;
    src.column = ch->column;
    return src;
}

static inline void mark(struct token *t)
{
    source = t->src;
}

static struct cc_file * open_file(const char *file)
{
    FILE *fp = fopen(file, "r");
    if (fp == NULL) {
	perror(file);
	die("Cannot open file %s", file);
    }
    struct cc_file *fs = xmalloc(sizeof(struct cc_file));
    fs->pc = fs->pe = &fs->buf[LBUFSIZE];
    fs->bread = -1;
    fs->chars = vec_new();
    fs->fp = fp;
    fs->file = file;
    fs->line = 1;
    fs->column = 0;
    genlineno(1, file);
    return fs;
}

static void close_file(struct cc_file *file)
{
    fclose(file->fp);
    vec_free(file->chars);
    free(file);
    struct cc_file *fs = current_file();
    genlineno(fs->line, fs->file);
}

static char get(void)
{
    struct cc_file *fs = current_file();
    if (fs->pe - fs->pc < MAXTOKEN)
	fillbuf(fs);
    if (fs->pc == fs->pe)
	return EOI;
    if (*fs->pc == '\n') {
	fs->line++;
	fs->column = 0;
    } else {
	fs->column++;
    }
    return *fs->pc++;
}

static inline struct cc_char * newch(char c, unsigned line, unsigned column)
{
    struct cc_char *ch =  zmalloc(sizeof(struct cc_char));
    ch->ch = c;
    ch->line = line;
    ch->column = column;
    return ch;
}

static void unreads(const char *s)
{

}

static void unreadc(struct cc_char * ch)
{
    vec_push(current_file()->chars, ch);
}

static struct cc_char * readc(void)
{
    struct cc_file *fs;

beg:
    fs = current_file();
    if (vec_len(fs->chars))
	return vec_pop(fs->chars);
    
    for (;;) {
        char c = get();
	unsigned line = fs->line;
	unsigned column = fs->column;
	if (c == EOI) {
	    if (vec_len(files) == 1) {
		goto end;
	    } else {
		close_file(vec_pop(files));
		goto beg;
	    }
	}
	if (c != '\\')
	    goto end;
	char c2 = get();
	if (c2 == '\n')
	    continue;
	// cache
	struct cc_char *ch2 = newch(c2, fs->line, fs->column);
	unreadc(ch2);
    end:
	return newch(c, line, column);
    }
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

void lex_init(const char *file)
{
    files = vec_new();
    buffers = vec_new();
    vec_push(buffers, vec_new());
    vec_push(files, open_file(file));
}

struct token * new_token(struct token *tok)
{
    struct token *t = alloc_token();
    t->id = tok->id;
    t->name = tok->name;
    t->kind = tok->kind;
    if (!tok->name)
	t->name = id2s(tok->id);
    t->src = source;
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

static void do_skipline(bool over)
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

void skipline(void)
{
    do_skipline(false);
}

static inline void line_comment(void)
{
    skipline();
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
    return new_token(&(struct token){.id = ICONSTANT, .name = strs(s->str)});
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
	return new_token(&(struct token){.id = ICONSTANT, .name = strs(s->str)});
    else
	return new_token(&(struct token){.id = SCONSTANT, .name = strs(s->str)});
}

static struct token * identifier(char c)
{
    struct strbuf *s = strbuf_new();
    strbuf_catc(s, c);
    readch(s, is_digitletter);
    return new_token(&(struct token){.id = ID, .name = strs(s->str)});
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
	    return newline_token;
	    
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
		return new_token(&(struct token){.id = DIVEQ});
	    } else {
		return new_token(&(struct token){.id = CH(rpc)});
	    }

	case '+':
	    if (next('+'))
		return new_token(&(struct token){.id = INCR});
	    else if (next('='))
		return new_token(&(struct token){.id = ADDEQ});
	    else
		return new_token(&(struct token){.id = CH(rpc)});

	case '-':
	    if (next('-'))
		return new_token(&(struct token){.id = DECR});
	    else if (next('='))
		return new_token(&(struct token){.id = MINUSEQ});
	    else if (next('>'))
		return new_token(&(struct token){.id = DEREF});
	    else
		return new_token(&(struct token){.id = CH(rpc)});

	case '*':
	    if (next('='))
		return new_token(&(struct token){.id = MULEQ});
	    else
		return new_token(&(struct token){.id = CH(rpc)});

	case '=':
	    if (next('='))
		return new_token(&(struct token){.id = EQ});
	    else
		return new_token(&(struct token){.id = CH(rpc)});

	case '!':
	    if (next('='))
		return new_token(&(struct token){.id = NEQ});
	    else
		return new_token(&(struct token){.id = CH(rpc)});

	case '%':
	    if (next('=')) {
		return new_token(&(struct token){.id = MODEQ});
	    } else if (next('>')) {
		return new_token(&(struct token){.id = '}'});
	    } else if (next(':')) {
		if (next('%')) {
		    struct cc_char *ch = readc();
		    if (CH(ch) == ':')
			return new_token(&(struct token){.id = SHARPSHARP});
		    else
			unreadc(ch);
		}
		return new_token(&(struct token){.id = '#'});
	    } else {
		return new_token(&(struct token){.id = CH(rpc)});
	    }

	case '^':
	    if (next('='))
		return new_token(&(struct token){.id = XOREQ});
	    else
		return new_token(&(struct token){.id = CH(rpc)});

	case '&':
	    if (next('='))
		return new_token(&(struct token){.id = BANDEQ});
	    else if (next('&'))
		return new_token(&(struct token){.id = AND});
	    else
		return new_token(&(struct token){.id = CH(rpc)});

	case '|':
	    if (next('='))
		return new_token(&(struct token){.id = BOREQ});
	    else if (next('|'))
		return new_token(&(struct token){.id = OR});
	    else
		return new_token(&(struct token){.id = CH(rpc)});

	case '<':
	    if (next('=')) {
		return new_token(&(struct token){.id = LEQ});
	    } else if (next('<')) {
		struct cc_char *ch = readc();
		if (CH(ch) == '=')
		    return new_token(&(struct token){.id = LSHIFTEQ});
		else
		    unreadc(ch);
		return new_token(&(struct token){.id = LSHIFT});
	    } else if (next('%')) {
		return new_token(&(struct token){.id = '{'});
	    } else if (next(':')) {
		return new_token(&(struct token){.id = '['});
	    } else {
		return new_token(&(struct token){.id = CH(rpc)});
	    }

	case '>':
	    if (next('=')) {
		return new_token(&(struct token){.id = GEQ});
	    } else if (next('>')) {
	        struct cc_char *ch = readc();
		if (CH(ch) == '=')
		    return new_token(&(struct token){.id = RSHIFTEQ});
		else
		    unreadc(ch);
		return new_token(&(struct token){.id = RSHIFT});
	    } else {
		return new_token(&(struct token){.id = CH(rpc)});
	    }
	    
	case '(': case ')': case '{': case '}':
	case '[': case ']': case ',': case ';':
	case '~': case '?':
	    return new_token(&(struct token){.id = CH(rpc)});

	case ':':
	    if (next('>'))
		return new_token(&(struct token){.id = ']'});
	    else
		return new_token(&(struct token){.id = CH(rpc)});

	case '#':
	    if (next('#'))
		return new_token(&(struct token){.id = SHARPSHARP});
	    else
		return new_token(&(struct token){.id = CH(rpc)});

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
		    return new_token(&(struct token){.id = ELLIPSIS});
		unreadc(ch2);
		unreadc(ch1);
		return new_token(&(struct token){.id = CH(rpc)});
	    } else if (is_digit(peek())) {
		return ppnumber(CH(rpc));
	    } else {
		return new_token(&(struct token){.id = CH(rpc)});
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

static void genlineno(unsigned line, const char *file)
{
    
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
    
    do_skipline(true);
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

void include_file(const char *file)
{
    CCAssert(vec_len(vec_tail(buffers)) == 0);
    vec_push(files, open_file(file));
}

void unget(struct token *t)
{
    struct vector *v = vec_tail(buffers);
    vec_push(v, t);
}

void push_buffer(struct vector *v)
{
    vec_push(buffers, v);
}

void pop_buffer(void)
{
    vec_pop(buffers);
}

struct token * lex(void)
{
    struct vector *v = vec_tail(buffers);
    // no matter which is the last
    if (vec_len(v))
        token = vec_pop(v);
    // if the last vec is empty and buffers len > 1
    else if (vec_len(buffers) > 1)
	token = eoi_token;
    // do lex
    else
	token = dolex();
    mark(token);
    return token;
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

int gettok(void)
{
    return EOI;
}

struct token * lookahead(void)
{
    return NULL;
}
