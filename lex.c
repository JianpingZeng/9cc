#include "cc.h"

#define LBUFSIZE     1024
#define RBUFSIZE     4096
#define MAXTOKEN     LBUFSIZE

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

static char ibuf[LBUFSIZE+RBUFSIZE+1];
static char *pc;
static char *pe;
static long bread;
struct source source;

static void fillbuf()
{
    if (bread == 0) {
        if (pc > pe)
            pc = pe;
        return;
    }
    
    if (pc >= pe) {
        pc = &ibuf[LBUFSIZE];
    } else {
        long n;
        char *dst, *src;
        
        // copy
        n = pe - pc;
        dst = &ibuf[LBUFSIZE] - n;
        src = pc;
        while (src < pe)
            *dst++ = *src++;
        
        pc = &ibuf[LBUFSIZE] - n;
    }
    
    if (feof(stdin))
        bread = 0;
    else
        bread = fread(&ibuf[LBUFSIZE], 1, RBUFSIZE, stdin);
    
    if (bread < 0)
        die("read error: %s", strerror(errno));
    
    pe = &ibuf[LBUFSIZE] + bread;
    *pe = '\n';
}

static void skipblank()
{
    do {
        while (is_blank(*pc))
            pc++;
        if (pe - pc < LBUFSIZE) {
            fillbuf();
            if (pc == pe)
                return;
        }
    } while (is_blank(*pc));
}

static void skipline()
{
    while (*pc++ != '\n') {
        if (pe - pc < LBUFSIZE) {
            fillbuf();
            if (pc == pe)
                break;
        }
    }
}

static void fline()
{
    CCAssert(is_digit(*pc));
    
    unsigned line = 0;
    
    while (is_digit(*pc)) {
        line = line * 10 + *pc - '0';
        pc++;
    }
    source.line = line-1;
    
    skipblank();
    
    if (*pc == '"') {
        struct strbuf *s = strbuf_new();
        pc++;
        for (; *pc != '"' || pc == pe;) {
            if (pe - pc < LBUFSIZE) {
                fillbuf();
                if (pc == pe)
                    break;
            }
            if (*pc == '\\' && pc[1] == '"') {
                strbuf_catn(s, pc+1, 1);
                pc += 2;
            } else {
                strbuf_catn(s, pc++, 1);
            }
        }
        source.file = strs(s->str);
        skipline();
    } else {
        skipline();
    }
}

static void fpragma()
{
    //TODO:
    skipline();
}

static void fsync()
{
    CCAssert(*pc++ == '#');
    
    skipblank();
    
    if (is_digit(*pc)) {
        // # n "filename"
        fline();
    } else if (!strncmp(pc, "line", 4)) {
        // #line n "filename"
        pc += 4;
        skipblank();
        fline();
    } else if (!strncmp(pc, "pragma", 6)) {
        // #pragma
        pc += 6;
        fpragma();
    } else {
        // others
        skipline();
    }
}

static void nextline()
{
    do {
        if (pc >= pe) {
            fillbuf();
            if (pc == pe)
                return;
        } else {
            source.line++;
            while (is_blank(*pc))
                pc++;
            if (*pc == '#') {
                fsync();
                nextline();
            }
        }
    } while (*pc == '\n' && pc == pe);
}

void input_init()
{
    pc = pe = &ibuf[LBUFSIZE];
    bread = -1;
    fillbuf();
    nextline();
}

static const char *tnames[] = {
#define _a(a, b, c, d)  b,
#define _x(a, b, c, d)  b,
#define _t(a, b, c)     b,
#include "token.def"
};

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

static int kinds[] = {
#define _a(a, b, c, d)  d,
#define _x(a, b, c, d)  c,
#define _t(a, b, c)     c,
#include "token.def"
};

static int tokind(int t)
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

static const char *tokname;
static struct token token1, token2;
struct token *token = &token1;

static void nextline();
static void line_comment();
static void block_comment();
static void identifier();
static int number();
static void fnumber(struct strbuf *s, int base);
static void sequence(bool wide, char ch);
static void integer_suffix(struct strbuf *s);
static void escape(struct strbuf *s);
static void readch(struct strbuf *s, bool (*is) (char));

static int do_gettok()
{
    register char *rpc;
    
    for (; ; ) {
        while (is_blank(*pc))
            pc++;
        
        if (pe - pc < MAXTOKEN)
            fillbuf();
        
        rpc = pc++;
        
        switch (*rpc) {
            case '\n':
                nextline();
                if (pc == pe)
                    return EOI;
                else
                    continue;
                
                // separators & operators
            case '/':
                if (rpc[1] == '/') {
                    // line comment
                    line_comment();
                    continue;
                } else if (rpc[1] == '*') {
                    // block comment
                    block_comment();
                    continue;
                } else if (rpc[1] == '=') {
                    pc = rpc + 2;
                    return DIVEQ;
                } else {
                    return '/';
                }
                
            case '+':
                if (rpc[1] == '+') {
                    pc = rpc + 2;
                    return INCR;
                } else if (rpc[1] == '=') {
                    pc = rpc + 2;
                    return ADDEQ;
                } else {
                    return '+';
                }
                
            case '-':
                if (rpc[1] == '-') {
                    pc = rpc + 2;
                    return DECR;
                } else if (rpc[1] == '=') {
                    pc = rpc + 2;
                    return MINUSEQ;
                } else if (rpc[1] == '>') {
                    pc = rpc + 2;
                    return DEREF;
                } else {
                    return '-';
                }
                
            case '*':
                return rpc[1] == '=' ? (pc = rpc+2, MULEQ) : (pc = rpc+1, '*');
                
            case '=':
                return rpc[1] == '=' ? (pc = rpc+2, EQ) : (pc = rpc+1, '=');
                
            case '!':
                return rpc[1] == '=' ? (pc = rpc+2, NEQ) : (pc = rpc+1, '!');
                
            case '%':
                return rpc[1] == '=' ? (pc = rpc+2, MODEQ) : (pc = rpc+1, '%');
                
            case '^':
                return rpc[1] == '=' ? (pc = rpc+2, XOREQ) : (pc = rpc+1, '^');
                
            case '&':
                if (rpc[1] == '=') {
                    pc = rpc + 2;
                    return BANDEQ;
                } else if (rpc[1] == '&') {
                    pc = rpc + 2;
                    return AND;
                } else {
                    return '&';
                }
                
            case '|':
                if (rpc[1] == '=') {
                    pc = rpc + 2;
                    return BOREQ;
                } else if (rpc[1] == '|') {
                    pc = rpc + 2;
                    return OR;
                } else {
                    return '|';
                }
                
            case '<':
                if (rpc[1] == '=') {
                    pc = rpc + 2;
                    return LEQ;
                } else if (rpc[1] == '<' && rpc[2] == '=') {
                    pc = rpc + 3;
                    return LSHIFTEQ;
                } else if (rpc[1] == '<') {
                    pc = rpc + 2;
                    return LSHIFT;
                } else {
                    return '<';
                }
                
            case '>':
                if (rpc[1] == '=') {
                    pc = rpc + 2;
                    return GEQ;
                } else if (rpc[1] == '>' && rpc[2] == '=') {
                    pc = rpc + 3;
                    return RSHIFTEQ;
                } else if (rpc[1] == '>') {
                    pc = rpc + 2;
                    return RSHIFT;
                } else {
                    return '>';
                }
                
            case '(': case ')': case '{': case '}':
            case '[': case ']': case ',': case ';':
            case ':': case '~': case '?':
                return *rpc;
                
            case '\'':
                sequence(false, '\'');
                return ICONSTANT;
                
            case '"':
                sequence(false, '"');
                return SCONSTANT;
                
                // numbers
            case '0': case '1': case '2': case '3': case '4':
            case '5': case '6': case '7': case '8': case '9':
                return number();
                
            case '.':
                if (rpc[1] == '.' && rpc[2] == '.') {
                    pc = rpc + 3;
                    return ELLIPSIS;
                } else if (is_digit(rpc[1])) {
                    pc = rpc;
                    fnumber(NULL, 10);
                    return FCONSTANT;
                } else {
                    return '.';
                }
                
                // keywords
            case 'a':
                if (rpc[1] == 'u' && rpc[2] == 't' && rpc[3] == 'o' &&
                    !is_digitletter(rpc[4])) {
                    pc = rpc + 4;
                    return AUTO;
                }
                goto id;
                
            case 'b':
                if (rpc[1] == 'r' && rpc[2] == 'e' && rpc[3] == 'a' &&
                    rpc[4] == 'k' && !is_digitletter(rpc[5])) {
                    pc = rpc + 5;
                    return BREAK;
                }
                goto id;
                
            case 'c':
                if (rpc[1] == 'a' && rpc[2] == 's' && rpc[3] == 'e' &&
                    !is_digitletter(rpc[4])) {
                    pc = rpc + 4;
                    return CASE;
                } else if (rpc[1] == 'h' && rpc[2] == 'a' && rpc[3] == 'r' &&
                           !is_digitletter(rpc[4])) {
                    pc = rpc + 4;
                    return CHAR;
                } else if (rpc[1] == 'o' && rpc[2] == 'n' && rpc[3] == 's' &&
                           rpc[4] == 't' && !is_digitletter(rpc[5])) {
                    pc = rpc + 5;
                    return CONST;
                } else if (rpc[1] == 'o' && rpc[2] == 'n' && rpc[3] == 't' &&
                           rpc[4] == 'i' && rpc[5] == 'n' && rpc[6] == 'u' &&
                           rpc[7] == 'e' && !is_digitletter(rpc[8])) {
                    pc = rpc + 8;
                    return CONTINUE;
                }
                goto id;
                
            case 'd':
                if (rpc[1] == 'e' && rpc[2] == 'f' && rpc[3] == 'a' &&
                    rpc[4] == 'u' && rpc[5] == 'l' && rpc[6] == 't' &&
                    !is_digitletter(rpc[7])) {
                    pc = rpc + 7;
                    return DEFAULT;
                } else if (rpc[1] == 'o' && !is_digitletter(rpc[2])) {
                    pc = rpc + 2;
                    return DO;
                } else if (rpc[1] == 'o' && rpc[2] == 'u' && rpc[3] == 'b' &&
                           rpc[4] == 'l' && rpc[5] == 'e' && !is_digitletter(rpc[6])) {
                    pc = rpc + 6;
                    return DOUBLE;
                }
                goto id;
                
            case 'e':
                if (rpc[1] == 'x' && rpc[2] == 't' && rpc[3] == 'e' &&
                    rpc[4] == 'r' && rpc[5] == 'n' && !is_digitletter(rpc[6])) {
                    pc = rpc + 6;
                    return EXTERN;
                } else if (rpc[1] == 'l' && rpc[2] == 's' && rpc[3] == 'e' &&
                           !is_digitletter(rpc[4])) {
                    pc = rpc + 4;
                    return ELSE;
                } else if (rpc[1] == 'n' && rpc[2] == 'u' && rpc[3] == 'm' &&
                           !is_digitletter(rpc[4])) {
                    pc = rpc + 4;
                    return ENUM;
                }
                goto id;
                
            case 'f':
                if (rpc[1] == 'l' && rpc[2] == 'o' && rpc[3] == 'a' &&
                    rpc[4] == 't' && !is_digitletter(rpc[5])) {
                    pc = rpc + 5;
                    return FLOAT;
                } else if (rpc[1] == 'o' && rpc[2] == 'r' && !is_digitletter(rpc[3])) {
                    pc = rpc + 3;
                    return FOR;
                }
                goto id;
                
            case 'g':
                if (rpc[1] == 'o' && rpc[2] == 't' && rpc[3] == 'o' &&
                    !is_digitletter(rpc[4])) {
                    pc = rpc + 4;
                    return GOTO;
                }
                goto id;
                
            case 'i':
                if (rpc[1] == 'n' && rpc[2] == 't' && !is_digitletter(rpc[3])) {
                    pc = rpc + 3;
                    return INT;
                } else if (rpc[1] == 'f' && !is_digitletter(rpc[2])) {
                    pc = rpc + 2;
                    return IF;
                } else if (rpc[1] == 'n' && rpc[2] == 'l' && rpc[3] == 'i' &&
                           rpc[4] == 'n' && rpc[5] == 'e' && !is_digitletter(rpc[6])) {
                    pc = rpc + 6;
                    return INLINE;
                }
                goto id;
                
            case 'l':
                if (rpc[1] == 'o' && rpc[2] == 'n' && rpc[3] == 'g' &&
                    !is_digitletter(rpc[4])) {
                    pc = rpc + 4;
                    return LONG;
                }
                goto id;
                
            case 'r':
                if (rpc[1] == 'e' && rpc[2] == 't' && rpc[3] == 'u' &&
                    rpc[4] == 'r' && rpc[5] == 'n' && !is_digitletter(rpc[6])) {
                    pc = rpc + 6;
                    return RETURN;
                } else if (rpc[1] == 'e' && rpc[2] == 's' && rpc[3] == 't' &&
                           rpc[4] == 'r' && rpc[5] == 'i' && rpc[6] == 'c' &&
                           rpc[7] == 't' && !is_digitletter(rpc[8])) {
                    pc = rpc + 8;
                    return RESTRICT;
                } else if (rpc[1] == 'e' && rpc[2] == 'g' && rpc[3] == 'i' &&
                           rpc[4] == 's' && rpc[5] == 't' && rpc[6] == 'e' &&
                           rpc[7] == 'r' && !is_digitletter(rpc[8])) {
                    pc = rpc + 8;
                    return REGISTER;
                }
                goto id;
                
            case 's':
                if (rpc[1] == 't' && rpc[2] == 'a' && rpc[3] == 't' &&
                    rpc[4] == 'i' && rpc[5] == 'c' && !is_digitletter(rpc[6])) {
                    pc = rpc + 6;
                    return STATIC;
                } else if (rpc[1] == 'h' && rpc[2] == 'o' && rpc[3] == 'r' &&
                           rpc[4] == 't' && !is_digitletter(rpc[5])) {
                    pc = rpc + 5;
                    return SHORT;
                } else if (rpc[1] == 'i' && rpc[2] == 'z' && rpc[3] == 'e' &&
                           rpc[4] == 'o' && rpc[5] == 'f' && !is_digitletter(rpc[6])) {
                    pc = rpc + 6;
                    return SIZEOF;
                } else if (rpc[1] == 'w' && rpc[2] == 'i' && rpc[3] == 't' &&
                           rpc[4] == 'c' && rpc[5] == 'h' && !is_digitletter(rpc[6])) {
                    pc = rpc + 6;
                    return SWITCH;
                } else if (rpc[1] == 't' && rpc[2] == 'r' && rpc[3] == 'u' &&
                           rpc[4] == 'c' && rpc[5] == 't' && !is_digitletter(rpc[6])) {
                    pc = rpc + 6;
                    return STRUCT;
                } else if (rpc[1] == 'i' && rpc[2] == 'g' && rpc[3] == 'n' &&
                           rpc[4] == 'e' && rpc[5] == 'd' && !is_digitletter(rpc[6])) {
                    pc = rpc + 6;
                    return SIGNED;
                }
                goto id;
                
            case 't':
                if (rpc[1] == 'y' && rpc[2] == 'p' && rpc[3] == 'e' &&
                    rpc[4] == 'd' && rpc[5] == 'e' && rpc[6] == 'f' &&
                    !is_digitletter(rpc[7])) {
                    pc = rpc + 7;
                    return TYPEDEF;
                }
                goto id;
                
            case 'u':
                if (rpc[1] == 'n' && rpc[2] == 's' && rpc[3] == 'i' &&
                    rpc[4] == 'g' && rpc[5] == 'n' && rpc[6] == 'e' &&
                    rpc[7] == 'd' && !is_digitletter(rpc[8])) {
                    pc = rpc + 8;
                    return UNSIGNED;
                } else if (rpc[1] == 'n' && rpc[2] == 'i' && rpc[3] == 'o' &&
                           rpc[4] == 'n' && !is_digitletter(rpc[5])) {
                    pc = rpc + 5;
                    return UNION;
                }
                goto id;
                
            case 'v':
                if (rpc[1] == 'o' && rpc[2] == 'i' && rpc[3] == 'd' &&
                    !is_digitletter(rpc[4])) {
                    pc = rpc + 4;
                    return VOID;
                } else if (rpc[1] == 'o' && rpc[2] == 'l' && rpc[3] == 'a' &&
                           rpc[4] == 't' && rpc[5] == 'i' && rpc[6] == 'l' &&
                           rpc[7] == 'e' && !is_digitletter(rpc[8])) {
                    pc = rpc + 8;
                    return VOLATILE;
                }
                goto id;
                
            case 'w':
                if (rpc[1] == 'h' && rpc[2] == 'i' && rpc[3] == 'l' &&
                    rpc[4] == 'e' && !is_digitletter(rpc[5])) {
                    pc = rpc + 5;
                    return WHILE;
                }
                goto id;
                
            case '_':
                if (rpc[1] == 'B' && rpc[2] == 'o' && rpc[3] == 'o' &&
                    rpc[4] == 'l' && !is_digitletter(rpc[5])) {
                    pc = rpc + 5;
                    return _BOOL;
                } else if (rpc[1] == 'C' && rpc[2] == 'o' && rpc[3] == 'm' &&
                           rpc[4] == 'p' && rpc[5] == 'l' && rpc[6] == 'e' &&
                           rpc[7] == 'x' && !is_digitletter(rpc[8])) {
                    pc = rpc + 8;
                    return _COMPLEX;
                } else if (rpc[1] == 'I' && rpc[2] == 'm' && rpc[3] == 'a' &&
                           rpc[4] == 'g' && rpc[5] == 'i' && rpc[6] == 'n' &&
                           rpc[7] == 'a' && rpc[8] == 'r' && rpc[9] == 'y' &&
                           !is_digitletter(rpc[10])) {
                    pc = rpc + 10;
                    return _IMAGINARY;
                }
                goto id;
                
            case 'L':
                if (rpc[1] == '\'') {
                    pc = rpc + 2;
                    sequence(true, '\'');
                    return ICONSTANT;
                } else if (rpc[1] == '"') {
                    pc = rpc + 2;
		    sequence(true, '"');
                    return SCONSTANT;
                } else {
                    goto id;
                }
                
                // identifer
            case 'h':case 'j':case 'k':case 'm':case 'n':case 'o':
            case 'p':case 'q':case 'x':case 'y':case 'z':
            case 'A':case 'B':case 'C':case 'D':case 'E':case 'F':
            case 'G':case 'H':case 'I':case 'J':case 'K':case 'M':
            case 'N':case 'O':case 'P':case 'Q':case 'R':case 'S':
            case 'T':case 'U':case 'V':case 'W':case 'X':case 'Y':
            case 'Z':
            id:
                identifier();
                return ID;
                
            default:
                if (!is_blank(*rpc)) {
                    if (is_visible(*rpc))
                        error("invalid character '%c'", *rpc);
                    else
                        error("invalid character '\\0%o'", *rpc);
                }
        }
    }
}

static void line_comment()
{
    skipline();
    nextline();
}

static void block_comment()
{
    pc++;
    struct source src = source;
    for (; pc[0] != '*' || pc[1] != '/'; ) {
        if (pe - pc < MAXTOKEN) {
            fillbuf();
            if (pc == pe)
                break;
        }
        if (is_newline(*pc++))
            nextline();
    }
    if (pc == pe)
        errorf(src, "unterminated /* comment");
    else
        pc += 2;
}

static void readch(struct strbuf *s, bool (*is) (char))
{
    char *rpc = pc;
    for (; is(*rpc) || rpc == pe; ) {
	if (rpc == pe) {
	    strbuf_catn(s, pc, rpc-pc);
	    pc = rpc;
	    fillbuf();
	    rpc = pc;
	    if (pc == pe)
		break;
	    else
		continue;
	}
	rpc++;
    }
    strbuf_catn(s, pc, rpc-pc);
    pc = rpc;
}

static int number()
{
    struct strbuf *s = strbuf_new();
    char *rpc = pc-1;
    strbuf_catn(s, rpc, 1);
    if (rpc[0] == '0' && (rpc[1] == 'x' || rpc[1] == 'X')) {
        // Hex
        strbuf_catn(s, pc++, 1);
        if (!is_digithex(*pc) && *pc != '.') {
            integer_suffix(s);
            error("incomplete hex constant: %s", tokname);
            return ICONSTANT;
        }
	readch(s, is_digithex);
        if (*pc == '.' || *pc == 'p' || *pc == 'P') {
            fnumber(s, 16);
            return FCONSTANT;
        } else {
            integer_suffix(s);
            return ICONSTANT;
        }
    } else {
        // Oct/Dec
	readch(s, is_digit);
        if (*pc == '.' || *pc == 'e' || *pc == 'E') {
            fnumber(s, 10);
            return FCONSTANT;
        } else {
            integer_suffix(s);
            return ICONSTANT;
        }
    }
}

static void fnumber(struct strbuf *s, int base)
{
    // ./e/E/p
    if (!s) s = strbuf_new();
    
    if (base == 10) {
        // . e E
        if (*pc == '.') {
            strbuf_catn(s, pc++, 1);
	    readch(s, is_digit);
        }
        
        if (*pc == 'e' || *pc == 'E') {
            strbuf_catn(s, pc++, 1);
            if (pe - pc < MAXTOKEN)
                fillbuf();
            if (*pc == '+' || *pc == '-')
                strbuf_catn(s, pc++, 1);
            if (is_digit(*pc))
		readch(s, is_digit);
            else
                error("exponent used with no following digits: %s", s->str);
        }
    } else {
        // . p P
        if (*pc == '.') {
            strbuf_catn(s, pc++, 1);
            if (!is_digithex(pc[-2]) && !is_digithex(*pc))
                error("hex floating constants require a significand");

	    readch(s, is_digithex);
        }
        
        if (*pc == 'p' || *pc == 'P') {
            strbuf_catn(s, pc++, 1);
            if (pe - pc < MAXTOKEN)
                fillbuf();
            if (*pc == '+' || *pc == '-')
                strbuf_catn(s, pc++, 1);
            
            if (is_digit(*pc))
                readch(s, is_digit);
            else
                error("exponent has no digits");
        } else {
            error("hex floating constants require an exponent");
        }
    }
    
    if (*pc == 'f' || *pc == 'F' || *pc == 'l' || *pc == 'L')
	strbuf_catn(s, pc++, 1);

    tokname = strs(s->str);
}

static void integer_suffix(struct strbuf *s)
{
    char *rpc = pc;
    int ull = (rpc[0] == 'u' || rpc[0] == 'U') &&
    ((rpc[1] == 'l' && rpc[2] == 'l') || (rpc[1] == 'L' && rpc[2] == 'L'));
    int llu = ((rpc[0] == 'l' && rpc[1] == 'l') || (rpc[0] == 'L' && rpc[1] == 'L')) &&
    (rpc[2] == 'u' || rpc[2] == 'U');
    int ll = (rpc[0] == 'l' && rpc[1] == 'l') || (rpc[0] == 'L' && rpc[1] == 'L');
    int lu = (rpc[0] == 'l' || rpc[0] == 'L') && (rpc[1] == 'u' || rpc[1] == 'U');
    int ul = (rpc[0] == 'u' || rpc[0] == 'U') && (rpc[1] == 'l' || rpc[1] == 'L');
    int l = rpc[0] == 'l' || rpc[0] == 'L';
    int u = rpc[0] == 'u' || rpc[0] == 'U';
    if (ull || llu) {
        // unsigned long long
        pc = rpc + 3;
        strbuf_catn(s, rpc, 3);
    } else if (ll) {
        // long long
        pc = rpc + 2;
        strbuf_catn(s, rpc, 2);
    } else if (lu || ul) {
	// unsigned long
        pc = rpc + 2;	
        strbuf_catn(s, rpc, 2);
    } else if (l) {
        // long
        pc = rpc + 1;
        strbuf_catn(s, rpc, 1);
    } else if (u) {
        // unsigned
        pc = rpc + 1;
        strbuf_catn(s, rpc, 1);
    }
    
    tokname = strs(s->str);
}

static void sequence(bool wide, char ch)
{    
    struct strbuf *s = strbuf_new();
    wide ? strbuf_catn(s, pc-2, 2) : strbuf_catn(s, pc-1, 1);
    for (; *pc != ch;) {
        if (pe - pc < MAXTOKEN) {
            fillbuf();
            if (pc == pe)
                break;
        }

        if (is_newline(*pc))
            break;
        
        if (*pc == '\\')
            escape(s);		// escape
        else
	    strbuf_catn(s, pc++, 1);
    }
    
    const char *name = ch == '\'' ? "character" : "string";
    const char *pad = ch == '\'' ? "'" : "\"";
    if (*pc != ch) {
        error("unterminated %s constant: %s", name, s->str);
	strbuf_cats(s, pad);
    } else {
        strbuf_catn(s, pc++, 1);
    }
    tokname = strs(s->str);
}

static void identifier()
{
    struct strbuf *s = strbuf_new();
    pc = pc - 1;
    readch(s, is_digitletter);
    tokname = strs(s->str);
}

static void escape(struct strbuf *s)
{
    CCAssert(*pc == '\\');
    strbuf_catn(s, pc++, 2);
    switch (*pc++) {
        case 'a': case 'b': case 'f':
        case 'n': case 'r': case 't':
        case 'v': case '\'': case '"':
        case '\\': case '\?':
            break;
        case '0': case '1': case '2':
        case '3': case '4': case '5':
        case '6': case '7':
            if (*pc >= '0' && *pc <= '7') {
                strbuf_catn(s, pc++, 1);
                if (*pc >= '0' && *pc <= '7')
                    strbuf_catn(s, pc++, 1);
            }
            break;
        case 'x':
            if (!is_digithex(*pc)) {
                error("\\x used with no following hex digits");
                break;
            }
	    readch(s, is_digithex);
            break;
        case 'u': case 'U':
        {
            // universal character name: expect 4(u)/8(U) hex digits
            int x = 0;
            int n = pc[-1] == 'u' ? 4 : 8;
            char *ps = pc - 2;
            for (; is_digithex(*pc); x++, pc++) {
                if (x == n)
                    break;
                strbuf_catn(s, pc, 1);
            }
            if (x < n)
                error("incomplete universal character name: %S", ps, pc-ps);
        }
	    break;
        default:
            error("unrecognized escape character 0x%x", pc[-1]);
            break;
    }
}

void expect(int t)
{
    if (token->id == t) {
        gettok();
    } else {
        if (token->id == EOI)
            error("expect token '%s' at the end", tname(t));
        else
            error("expect token '%s' before '%s'", tname(t), token->name);
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

static bool is_looked;
void read_tok(struct token *t)
{
    tokname = NULL;
    t->id = do_gettok();
    if (!tokname)
	tokname = tname(t->id);
    t->name = tokname;
    t->kind = tokind(t->id);
}

int gettok()
{
    if (is_looked) {
        token1 = token2;
        is_looked = 0;	
    } else {
	read_tok(&token1);
    }
    
    return token->id;
}

struct token * lookahead()
{
    if (!is_looked) {
	read_tok(&token2);
        is_looked = 1;	
    }
    
    return &token2;
}
