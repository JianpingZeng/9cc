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

static struct vector *files;

struct token *token;
static struct token *eoi_token = &(struct token){ .id = EOI, .kind = TEOI };
static struct token *newline_token = &(struct token){ .id = TOK10, .kind = TNEWLINE };
static struct token *space_token = &(struct token){ .id = TOK32, .kind = TSPACE };

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
    fs->file = file;
    fs->fp = fp;
    fs->pc = fs->pe = &fs->buf[LBUFSIZE];
    fs->bread = -1;
    return fs;
}

void close_file(struct cc_file *file)
{
    fclose(file->fp);
    free(file);
}

/* Because of the 'backslash' character,
 * (concats characters across lines)
 * a function is needed to get the processed
 * character.
 */
static char readc(void)
{
    // for (;;) {
    // 	if (pe - pc < MAXTOKEN)
    // 	    fillbuf();
    // 	if (pe == pc)
    // 	    return EOI;
    // 	if (*pc != '\\')
    // 	    return *pc++;
    // 	if (pc[1] == '\n') {
    // 	    pc = pc + 2;
    // 	    continue;
    // 	}
    // 	return *pc++;
    // }
}

static void unreadc(char c)
{
    
}

static char peek(void)
{
    char c = readc();
    unreadc(c);
    return c;
}

static bool next(char expect)
{
    char c = readc();
    if (c == expect)
	return true;
    unreadc(c);
    return false;
}

void input_init(const char *file)
{
    files = vec_new();
}

static struct token * new_token(struct token *tok)
{
    struct token *t = alloc_token();
    t->id = tok->id;
    t->name = tok->name;
    t->kind = tok->kind;
    return t;
}

static void readch(struct strbuf *s, bool (*is) (char))
{
    
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
    
}

struct token * lex(void)
{    
    for (; ;) {
        char rpc = readc();
	switch (rpc) {
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
	    space_token->id = rpc;
	    return space_token;
	
	    // operators
	case '/':
	    if (next('/')) {
		line_comment();
		continue;
	    } else if (next('*')) {
		block_comment();
		continue;
	    } else if (next('=')) {
		return new_token(&(struct token){.id = DIVEQ, .kind = TOPERATOR});
	    } else {
		return new_token(&(struct token){.id = rpc, .kind = TOPERATOR});
	    }

	case '+':
	    if (next('+'))
		return new_token(&(struct token){.id = INCR, .kind = TOPERATOR});
	    else if (next('='))
		return new_token(&(struct token){.id = ADDEQ, .kind = TOPERATOR});
	    else
		return new_token(&(struct token){.id = rpc, .kind = TOPERATOR});

	case '-':
	    if (next('-'))
		return new_token(&(struct token){.id = DECR, .kind = TOPERATOR});
	    else if (next('='))
		return new_token(&(struct token){.id = MINUSEQ, .kind = TOPERATOR});
	    else if (next('>'))
		return new_token(&(struct token){.id = DEREF, .kind = TOPERATOR});
	    else
		return new_token(&(struct token){.id = rpc, .kind = TOPERATOR});

	case '*':
	    if (next('='))
		return new_token(&(struct token){.id = MULEQ, .kind = TOPERATOR});
	    else
		return new_token(&(struct token){.id = rpc, .kind = TOPERATOR});

	case '=':
	    if (next('='))
		return new_token(&(struct token){.id = EQ, .kind = TOPERATOR});
	    else
		return new_token(&(struct token){.id = rpc, .kind = TOPERATOR});

	case '!':
	    if (next('='))
		return new_token(&(struct token){.id = NEQ, .kind = TOPERATOR});
	    else
		return new_token(&(struct token){.id = rpc, .kind = TOPERATOR});

	case '%':
	    if (next('='))
		return new_token(&(struct token){.id = MODEQ, .kind = TOPERATOR});
	    else
		return new_token(&(struct token){.id = rpc, .kind = TOPERATOR});

	case '^':
	    if (next('='))
		return new_token(&(struct token){.id = XOREQ, .kind = TOPERATOR});
	    else
		return new_token(&(struct token){.id = rpc, .kind = TOPERATOR});

	case '&':
	    if (next('='))
		return new_token(&(struct token){.id = BANDEQ, .kind = TOPERATOR});
	    else if (next('&'))
		return new_token(&(struct token){.id = AND, .kind = TOPERATOR});
	    else
		return new_token(&(struct token){.id = rpc, .kind = TOPERATOR});

	case '|':
	    if (next('='))
		return new_token(&(struct token){.id = BOREQ, .kind = TOPERATOR});
	    else if (next('|'))
		return new_token(&(struct token){.id = OR, .kind = TOPERATOR});
	    else
		return new_token(&(struct token){.id = rpc, .kind = TOPERATOR});

	case '<':
	    if (next('=')) {
		return new_token(&(struct token){.id = LEQ, .kind = TOPERATOR});
	    } else if (next('<')) {
		if (next('='))
		    return new_token(&(struct token){.id = RSHIFTEQ, .kind = TOPERATOR});
		else
		    return new_token(&(struct token){.id = RSHIFT, .kind = TOPERATOR});
	    } else {
		return new_token(&(struct token){.id = rpc, .kind = TOPERATOR});
	    }

	case '>':
	    if (next('=')) {
		return new_token(&(struct token){.id = GEQ, .kind = TOPERATOR});
	    } else if (next('>')) {
		if (next('='))
		    return new_token(&(struct token){.id = RSHIFTEQ, .kind = TOPERATOR});
		else
		    return new_token(&(struct token){.id = RSHIFT, .kind = TOPERATOR});
	    } else {
		return new_token(&(struct token){.id = rpc, .kind = TOPERATOR});
	    }

	case '~':
	    return new_token(&(struct token){.id = rpc, .kind = TOPERATOR});
	    
	    // separators
	case '(': case ')': case '{': case '}':
	case '[': case ']': case ',': case ';':
	case ':': case '?':
	case '#':
	    return new_token(&(struct token){.id = rpc, .kind = TSEPARATOR});

	    // constants
	case '\'':
	    return sequence(false, '\'');

	case '"':
	    return sequence(false, '"');


	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
	    return number();

	case '.':
	    if (is_digit(peek())) {
		struct strbuf *s = strbuf_new();
		strbuf_cats(s, ".");
		return fnumber(s, 10);
	    } else if (next('.')) {
		if (next('.'))
		    return new_token(&(struct token){.id = ELLIPSIS, .kind = TSEPARATOR});
		else
		    ;
	    } else {
		return new_token(&(struct token){.id = rpc, .kind = TOPERATOR});
	    }
	    // if (rpc[1] == '.' && rpc[2] == '.') {
	    // 	pc = rpc + 3;
	    // 	return new_token(&(struct token){.id = ELLIPSIS, .kind = TSEPARATOR});
	    // } else if (is_digit(rpc[1])) {
	    // 	pc = rpc;
	    // 	return fnumber(NULL, 10);
	    // } else {
	    // 	return new_token(&(struct token){.id = rpc, .kind = TOPERATOR});
	    // }
	    
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
	    return identifier();

	default:
	    // invalid character
	    if (!is_blank(rpc)) {
		if (is_visible(rpc))
		    error("invalid character '%c'", rpc);
		else
		    error("invalid character '\\0%o'", rpc);
	    }
	}
    }
}

const char *tname(int t)
{
    return NULL;
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
