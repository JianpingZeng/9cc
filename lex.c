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
    int ch;
    unsigned line;
    unsigned column;
};

#define CH(c)    ((c)->ch)

#define LCACHE    8
#define RCACHE    64
#define MAXCACHE  8

static struct cc_char chs[LCACHE+RCACHE+1];
static struct cc_char *pe;
static struct cc_char *pc;
static long bread;
static struct vector *files;

struct token *token;
static struct token *eoi_token = &(struct token){.id = EOI, .kind = TEOI};
static struct token *newline_token = &(struct token){.id = TOK10, .kind = TNEWLINE};
static struct token *space_token = &(struct token){.id = TOK32, .kind = TSPACE};

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

/* Input and buffer
 */
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
    *fs->pe = 0;
}

struct cc_file * open_file(const char *file)
{
    FILE *fp = fopen(file, "r");
    if (fp == NULL) {
	perror(file);
	die("Cannot open file %s", file);
    }
    struct cc_file *fs = xmalloc(sizeof(struct cc_file));
    fs->pc = fs->pe = &fs->buf[LBUFSIZE];
    fs->bread = -1;
    fs->fp = fp;
    fs->file = file;
    fs->line = 1;
    fs->column = 0;
    return fs;
}

void close_file(struct cc_file *file)
{
    fclose(file->fp);
    free(file);
}

static char get(void)
{
    struct cc_file *fs = vec_tail(files);
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

static inline struct cc_char * skeleton_char(struct cc_char *ch)
{
    static struct cc_char ch1;
    ch1.ch = ch->ch;
    ch1.line = ch->line;
    ch1.column = ch->column;
    return &ch1;
}

static struct cc_char * readc(void)
{
    static struct cc_char ch2;
    struct cc_file *fs = vec_tail(files);

    if (ch2.ch) {
        struct cc_char *ch1 = skeleton_char(&ch2);
	ch2.ch = 0;
	return ch1;
    }
    
    for (;;) {
        char c = get();
	unsigned line = fs->line;
	unsigned column = fs->column;
	if (c == EOI || c != '\\')
	    goto end;
	char c2 = get();
	if (c2 == '\n')
	    continue;
	// cache
	ch2.ch = c2;
	ch2.line = fs->line;
	ch2.column = fs->column;
    end:
	return skeleton_char(&(struct cc_char){.ch = c, .line = line, .column = column});
    }
}

static void fillchs(void)
{
    if (bread == 0) {
	if (pc > pe)
	    pc = pe;
	return;
    }
    
    if (pc >= pe) {
	pc = &chs[LCACHE];
    } else {
	long n;
	struct cc_char *dst, *src;

	// copy
	n = pe - pc;
	dst = &chs[LCACHE] - n;
	src = pc;
	while (src < pe)
	    *dst++ = *src++;

	pc = &chs[LCACHE] - n;
    }

    int i;
    struct cc_char *rpc = &chs[LCACHE];
    for (i = 0; i < RCACHE; i++) {
        struct cc_char *ch = readc();
	rpc[i].ch = ch->ch;
	rpc[i].line = ch->line;
	rpc[i].column = ch->column;
	if (ch->ch == EOI) {
	    i++;
	    bread = 0;
	    break;
	}
    }

    pe = &chs[LCACHE] + i;
    pe->ch = 0;
}

void input_init(const char *file)
{
    files = vec_new();
    vec_push(files, open_file(file));
    pc = pe = &chs[LCACHE];
    bread = -1;
}

static struct token * new_token(struct token *tok)
{
    struct token *t = alloc_token();
    t->id = tok->id;
    t->name = tok->name;
    t->kind = tok->kind;
    if (!tok->name)
	t->name = tname(tok->id);
    return t;
}

static inline struct token * skeleton_token(struct token *tok)
{
    tok->name = tname(tok->id);
    return tok;
}

static inline void concat(struct strbuf *s, struct cc_char *p, int n)
{
    while (n--) {
	strbuf_catc(s, CH(p));
	p++;
    }
}

static void readch(struct strbuf *s, bool (*is) (char))
{
    struct cc_char *rpc = pc;
    for (; is(CH(rpc)) || rpc == pe; ) {
        if (rpc == pe) {
	    concat(s, pc, rpc-pc);
	    pc = rpc;
	    fillchs();
	    rpc = pc;
	    if (pc == pe)
		break;
	    else
		continue;
	}
	rpc++;
    }
    concat(s, pc, rpc-pc);
    pc = rpc;
}

static void line_comment(void)
{

}

static void block_comment(void)
{

}

static struct token * number(void)
{

}

static struct token * fnumber(struct strbuf *s, int base)
{

}

static struct token * sequence(bool wide, char sep)
{

}

static struct token * identifier(void)
{
    struct strbuf *s = strbuf_new();
    pc = pc - 1;
    readch(s, is_digitletter);
    return new_token(&(struct token){.id = ID, .name = strs(s->str), .kind = TIDENTIFIER});
}

struct token * lex(void)
{
    struct cc_char *rpc;
    for (; ;) {
	if (pe - pc < MAXCACHE)
	    fillchs();
	
	rpc = pc++;

	switch (CH(rpc)) {
	case EOI:
	    return eoi_token;
	    
	case '\n':
	    return skeleton_token(newline_token);
	    
	    // spaces
	case TOK9:
	case TOK11:
	case TOK12:
	case TOK13:
	case TOK32:
	    space_token->id = CH(rpc);
	    return skeleton_token(space_token);
	
	    // punctuators
	case '/':
	    if (CH(rpc+1) == '/') {
		line_comment();
		continue;
	    } else if (CH(rpc+1) == '*') {
		block_comment();
		continue;
	    } else if (CH(rpc+1) == '=') {
		pc = rpc + 2;
		return new_token(&(struct token){.id = DIVEQ, .kind = TPUNCTUATOR});
	    } else {
		return new_token(&(struct token){.id = CH(rpc), .kind = TPUNCTUATOR});
	    }

	case '+':
	    if (CH(rpc+1) == '+') {
		pc = rpc + 2;
		return new_token(&(struct token){.id = INCR, .kind = TPUNCTUATOR});
	    } else if (CH(rpc+1) == '=') {
		pc = rpc + 2;
		return new_token(&(struct token){.id = ADDEQ, .kind = TPUNCTUATOR});
	    } else {
		return new_token(&(struct token){.id = CH(rpc), .kind = TPUNCTUATOR});
	    }

	case '-':
	    if (CH(rpc+1) == '-') {
		pc = rpc + 2;
		return new_token(&(struct token){.id = DECR, .kind = TPUNCTUATOR});
	    } else if (CH(rpc+1) == '=') {
		pc = rpc + 2;
		return new_token(&(struct token){.id = MINUSEQ, .kind = TPUNCTUATOR});
	    } else if (CH(rpc+1) == '>') {
		pc = rpc + 2;
		return new_token(&(struct token){.id = DEREF, .kind = TPUNCTUATOR});
	    } else {
		return new_token(&(struct token){.id = CH(rpc), .kind = TPUNCTUATOR});
	    }

	case '*':
	    if (CH(rpc+1) == '=') {
		pc = rpc + 2;
		return new_token(&(struct token){.id = MULEQ, .kind = TPUNCTUATOR});
	    } else {
		return new_token(&(struct token){.id = CH(rpc), .kind = TPUNCTUATOR});
	    }

	case '=':
	    if (CH(rpc+1) == '=') {
		pc = rpc + 2;
		return new_token(&(struct token){.id = EQ, .kind = TPUNCTUATOR});
	    } else {
		return new_token(&(struct token){.id = CH(rpc), .kind = TPUNCTUATOR});
	    }

	case '!':
	    if (CH(rpc+1) == '=') {
		pc = rpc + 2;
		return new_token(&(struct token){.id = NEQ, .kind = TPUNCTUATOR});
	    } else {
		return new_token(&(struct token){.id = CH(rpc), .kind = TPUNCTUATOR});
	    }

	case '%':
	    if (CH(rpc+1) == '=') {
		pc = rpc + 2;
		return new_token(&(struct token){.id = MODEQ, .kind = TPUNCTUATOR});
	    } else {
		return new_token(&(struct token){.id = CH(rpc), .kind = TPUNCTUATOR});
	    }

	case '^':
	    if (CH(rpc+1) == '=') {
		pc = rpc + 2;
		return new_token(&(struct token){.id = XOREQ, .kind = TPUNCTUATOR});
	    } else {
		return new_token(&(struct token){.id = CH(rpc), .kind = TPUNCTUATOR});
	    }

	case '&':
	    if (CH(rpc+1) == '=') {
		pc = rpc + 2;
		return new_token(&(struct token){.id = BANDEQ, .kind = TPUNCTUATOR});
	    } else if (CH(rpc+1) == '&') {
		pc = rpc + 2;
		return new_token(&(struct token){.id = AND, .kind = TPUNCTUATOR});
	    } else {
		return new_token(&(struct token){.id = CH(rpc), .kind = TPUNCTUATOR});
	    }

	case '|':
	    if (CH(rpc+1) == '=') {
		pc = rpc + 2;
		return new_token(&(struct token){.id = BOREQ, .kind = TPUNCTUATOR});
	    } else if (CH(rpc+1) == '|') {
		pc = rpc + 2;
		return new_token(&(struct token){.id = OR, .kind = TPUNCTUATOR});
	    } else {
		return new_token(&(struct token){.id = CH(rpc), .kind = TPUNCTUATOR});
	    }

	case '<':
	    if (CH(rpc+1) == '=') {
		pc = rpc + 2;
		return new_token(&(struct token){.id = LEQ, .kind = TPUNCTUATOR});
	    } else if (CH(rpc+1) == '<' && CH(rpc+2) == '=') {
		pc = rpc + 3;
		return new_token(&(struct token){.id = LSHIFTEQ, .kind = TPUNCTUATOR});
	    } else if (CH(rpc+1) == '<') {
		pc = rpc + 2;
		return new_token(&(struct token){.id = LSHIFT, .kind = TPUNCTUATOR});
	    } else {
		return new_token(&(struct token){.id = CH(rpc), .kind = TPUNCTUATOR});
	    }

	case '>':
	    if (CH(rpc+1) == '=') {
		pc = rpc + 2;
		return new_token(&(struct token){.id = GEQ, .kind = TPUNCTUATOR});
	    } else if (CH(rpc+1) == '>' && CH(rpc+2) == '=') {
		pc = rpc + 3;
		return new_token(&(struct token){.id = RSHIFTEQ, .kind = TPUNCTUATOR});
	    } else if (CH(rpc+1) == '>') {
		pc = rpc + 2;
		return new_token(&(struct token){.id = RSHIFT, .kind = TPUNCTUATOR});
	    } else {
		return new_token(&(struct token){.id = CH(rpc), .kind = TPUNCTUATOR});
	    }
	    
	case '(': case ')': case '{': case '}':
	case '[': case ']': case ',': case ';':
	case ':': case '~': case '?':
	    return new_token(&(struct token){.id = CH(rpc), .kind = TPUNCTUATOR});

	case '#':
	    if (CH(rpc+1) == '#') {
		pc = rpc + 2;
		return new_token(&(struct token){.id = SHARPSHARP, .kind = TPUNCTUATOR});
	    } else {
		return new_token(&(struct token){.id = CH(rpc), .kind = TPUNCTUATOR});
	    }

	    // constants
	case '\'':
	    return sequence(false, '\'');

	case '"':
	    return sequence(false, '"');


	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
	    return number();

	case '.':
	    if (CH(rpc+1) == '.' && CH(rpc+2) == '.') {
		pc = rpc + 3;
		return new_token(&(struct token){.id = ELLIPSIS, .kind = TPUNCTUATOR});
	    } else if (is_digit(CH(rpc+1))) {
		pc = rpc;
		return fnumber(NULL, 10);
	    } else {
		return new_token(&(struct token){.id = CH(rpc), .kind = TPUNCTUATOR});
	    }
	    
	    // identifiers
	case 'L':
	    if (CH(rpc+1) == '\'') {
		pc = rpc + 2;
		return sequence(true, '\'');
	    } else if (CH(rpc+1) == '"') {
		pc = rpc + 2;
		return sequence(true, '"');
	    }
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
	    return identifier();

	default:
	    // invalid character
	    if (!is_blank(CH(rpc))) {
		if (is_visible(CH(rpc)))
		    error("invalid character '%c'", CH(rpc));
		else
		    error("invalid character '\\0%o'", CH(rpc));
	    }
	}
    }
}

const char *tname(int t)
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
}

void match(int t, int follow[])
{
}

int gettok(void)
{
    return EOI;
}

struct token * lookahead(void)
{
    return NULL;
}
