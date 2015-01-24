#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <errno.h>
#include <string.h>
#include "lex.h"
#include "error.h"
#include "lib.h"

#define LBUFSIZE     512
#define RBUFSIZE     4096
#define MINLEN       LBUFSIZE

enum {
    BLANK = 01, NEWLINE = 02, LETTER = 04,
    DIGIT = 010, HEX = 020, OTHER = 040,
};

static unsigned char map[255] = {
#define _a(x, y, z)     z,
#define _x(a, b, c, d)
#define _t(a, b, c)
#include "token.h"
    OTHER,
};

#define isdigit(c)            (map[c] & DIGIT)
#define isletter(c)           (map[c] & LETTER)
#define isdigitletter(c)      (isdigit(c) || isletter(c))
#define isblank(c)            (map[c] & BLANK)
#define isnewline(c)          (map[c] & NEWLINE)

static unsigned char ibuf[LBUFSIZE+RBUFSIZE+1];
static unsigned char *pc;
static unsigned char *pe;
static unsigned char *pl;
static long bread;
static Source src;

static void fillbuf()
{
    if (bread == 0) {
        return;
    }

    if (pc >= pe) {
	pc = &ibuf[LBUFSIZE];
    }
    else {
	long n;
	unsigned char *dst, *src;

	// copy
	n = pe - pc;
	dst = &ibuf[LBUFSIZE] - n;
	src = pc;
	while (src < pe) {
	    *dst++ = *src++;
	}
    
	pc = &ibuf[LBUFSIZE] - n;
    }
        
    if (feof(stdin)) {
        bread = 0;
    }
    else {
        bread = fread(&ibuf[LBUFSIZE], 1, RBUFSIZE, stdin);
    }
    
    if (bread < 0) {
        fprint(stderr, "read error: %s", strerror(errno));
	exit(EXIT_FAILURE);
    }
    
    pe = &ibuf[LBUFSIZE] + bread;
    *pe = '\n';
}

static void fline()
{
    unsigned line = 0;
    unsigned char *fb;
    const char *p = NULL;
    unsigned char *f = NULL;
    struct {
	unsigned char line_rec : 1;
	unsigned char line_got : 1;
	unsigned char file_rec : 1;
	unsigned char file_got : 1;
    } s;
    memset(&s, 0, sizeof(s));
    assert(isdigit(*pc));
    for (;;) {
	if (pe - pc <= LBUFSIZE) {
	    fillbuf();
	    if (pc == pe) {
		if (p) deallocate(p);
		log("input file seems incorrect when #");
		return;
	    }
	}
	if (!s.line_rec) {
	    while (!isdigit(*pc) && pc < pe) {
		pc++;
	    }
	    if (pc == pe) continue;
	}
	s.line_rec = 1;
	if (!s.line_got) {
	    while (isdigit(*pc)) {
		line = line * 10 + *pc - '0';
		pc++;
	    }
	    if (pc == pe) continue;
	}
	s.line_got = 1;
	if (!s.file_rec) {
	    while (*pc != '"' && pc < pe) {
		pc++;
	    }
	    if (pc == pe) continue;
	    pc++;
	    if (pc == pe) {
		s.file_rec = 1;
		continue;
	    }
	}
	s.file_rec = 1;
	if (!s.file_got) {
	    fb = pc;
	    while (*pc != '"' && pc < pe) {
		pc++;
	    }
	    if (pc == pe) {
		appendstring(&p, fb, pc-fb);
		continue;
	    }
	    appendstring(&p, fb, pc-fb);
	    f = strings(p);
	    deallocate(p);
	    p = NULL;
	}
	s.file_got = 1;
	while (*pc != '\n') {
	    pc++;
	}
	if (pc == pe) continue;
	if (++pc == pe) {
	    fillbuf();
	}
	src.file = f;
	src.line = line;
	log("# %u \"%s\"", src.line, src.file);
	break;
    }
}

static void fsync()
{
    // # n "file"
    assert(*pc++ == '#');

    do {
	if (pe - pc < LBUFSIZE) {
	    fillbuf();
	    if (pc == pe) {
		error("input file seems incorrect while #");
		return;
	    }
	}
	while (isblank(*pc)) {
	    pc++;
	}
    } while (*pc == '\n' && pc == pe);
    
    if (isdigit(*pc)) {
	fline();
    }
    else {
	//TODO:support pragma etc.
	do {
	    if (pe - pc < LBUFSIZE) {
		fillbuf();
		if (pc == pe) {
		    error("input file seems incorrect while #");
		    return;
		}
	    }
	    while (*pc != '\n') {
		pc++;
	    }
	} while (*pc == '\n' && pc == pe);
	
        if (++pc == pe) {
	    fillbuf();
	}
    }
}

static void nextline()
{
    do {
	if (pc >= pe) {
	    fillbuf();
	    if (pc == pe) return;
	}
	else {
	    src.line++;
	    pl = pc;
	    while (isblank(*pc)) {
		pc++;
	    }
	    if (*pc == '#') {
		fsync();
		nextline();
	    }
	}
    } while (*pc == '\n' && pc == pe);
}

void init_input()
{
    pc = pe = &ibuf[LBUFSIZE];
    bread = -1;
    memset(&src, 0, sizeof(Source));
    fillbuf();
    nextline();
}

static const char *tnames[] = {
#define _a(x, y, z)    y,
#define _x(a, b, c, d) b,
#define _t(a, b, c) b,
#include "token.h"
};

static Token token1, token2;
Token *token = &token1;
static int lookaheaded;

static void identifier();
static int number();
static void fnumber(unsigned long);
static void nextline();
static void line_comment();
static void block_comment();

static int do_gettok()
{
    register unsigned char *rpc;
    
    for (; ; ) {
        while (isblank(*pc)) {
            pc++;
        }
        
        if (pe - pc < MINLEN) {
            fillbuf();
        }
        
        rpc = pc++;
        
        switch (*rpc) {
	case '\n': 
	    nextline();
	    if (pc == pe) {
		return EOI;
	    }
	    continue;
                
            // separators & operators
	case '/':
	    if (rpc[1] == '/') {
		// line comment
		line_comment();
		continue;
	    }
	    else if (rpc[1] == '*') {
		// block comment
		block_comment();
		continue;
	    }
	    else if (rpc[1] == '=') {
		pc = rpc + 2;
		return DIVEQ;
	    }
	    else {
		return '/';
	    }
                
	case '+':
	    if (rpc[1] == '+') {
		pc = rpc + 2;
		return INCR;
	    }
	    else if (rpc[1] == '=') {
		pc = rpc + 2;
		return PLUSEQ;
	    }
	    else {
		return '+';
	    }
                
	case '-':
	    if (rpc[1] == '-') {
		pc = rpc + 2;
		return DECR;
	    }
	    else if (rpc[1] == '=') {
		pc = rpc + 2;
		return MINUSEQ;
	    }
	    else if (rpc[1] == '>') {
		pc = rpc + 2;
		return DEREF;
	    }
	    else {
		return '-';
	    }
                
	case '*':
	    return rpc[1] == '=' ? (pc = rpc+2, MULEQ) : (pc = rpc+1, '*');
                
	case '=':
	    return rpc[1] == '=' ? (pc = rpc+2, EQ) : (pc = rpc+1, '=');
                
	case '!':
	    return rpc[1] == '=' ? (pc = rpc+2, NEQ) : (pc = rpc+1, '!');
                
	case '(': case ')': case '{': case '}':
	case '[': case ']': case ',': case ';':
	case ':':
	    return *rpc;
            
            // numbers
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
	    return number();
                
	case '.':
	    if (rpc[1] == '.' && rpc[2] == '.') {
		pc = rpc + 3;
		return ELLIPSIS;
	    }
	    else if (isdigit(rpc[1])) {
		fnumber(0);
		return FCONSTANT;
	    }
	    else {
		return '.';
	    }
            
            // keywords
	case 'a':
	    if (rpc[1] == 'u' && rpc[2] == 't' && rpc[3] == 'o' &&
		!isdigitletter(rpc[4])) {
		pc = rpc + 4;
		return AUTO;
	    }
	    goto id;
                
	case 'b':
	    if (rpc[1] == 'r' && rpc[2] == 'e' && rpc[3] == 'a' &&
		rpc[4] == 'k' && !isdigitletter(rpc[5])) {
		pc = rpc + 5;
		return BREAK;
	    }
	    goto id;
                
	case 'c':
	    if (rpc[1] == 'a' && rpc[2] == 's' && rpc[3] == 'e' &&
		!isdigitletter(rpc[4])) {
		pc = rpc + 4;
		return CASE;
	    }
	    else if (rpc[1] == 'h' && rpc[2] == 'a' && rpc[3] == 'r' &&
		     !isdigitletter(rpc[4])) {
		pc = rpc + 4;
		return CHAR;
	    }
	    else if (rpc[1] == 'o' && rpc[2] == 'n' && rpc[3] == 's' &&
		     rpc[4] == 't' && !isdigitletter(rpc[5])) {
		pc = rpc + 5;
		return CONST;
	    }
	    else if (rpc[1] == 'o' && rpc[2] == 'n' && rpc[3] == 't' &&
		     rpc[4] == 'i' && rpc[5] == 'n' && rpc[6] == 'e' &&
		     !isdigitletter(rpc[7])) {
		pc = rpc + 7;
		return CONTINUE;
	    }
	    goto id;
                
	case 'd':
	    if (rpc[1] == 'e' && rpc[2] == 'f' && rpc[3] == 'a' &&
		rpc[4] == 'u' && rpc[5] == 'l' && rpc[6] == 't' &&
		!isdigitletter(rpc[7])) {
		pc = rpc + 7;
		return DEFAULT;
	    }
	    else if (rpc[1] == 'o' && !isdigitletter(rpc[2])) {
		pc = rpc + 2;
		return DO;
	    }
	    else if (rpc[1] == 'o' && rpc[2] == 'u' && rpc[3] == 'b' &&
		     rpc[4] == 'l' && rpc[5] == 'e' && !isdigitletter(rpc[6])) {
		pc = rpc + 6;
		return DOUBLE;
	    }
	    goto id;
                
	case 'e':
	    if (rpc[1] == 'x' && rpc[2] == 't' && rpc[3] == 'e' &&
		rpc[4] == 'r' && rpc[5] == 'n' && !isdigitletter(rpc[6])) {
		pc = rpc + 6;
		return EXTERN;
	    }
	    else if (rpc[1] == 'l' && rpc[2] == 's' && rpc[3] == 'e' &&
		     !isdigitletter(rpc[4])) {
		pc = rpc + 4;
		return ELSE;
	    }
	    else if (rpc[1] == 'n' && rpc[2] == 'u' && rpc[3] == 'm' &&
		     !isdigitletter(rpc[4])) {
		pc = rpc + 4;
		return ENUM;
	    }
	    goto id;
                
	case 'f':
	    if (rpc[1] == 'l' && rpc[2] == 'o' && rpc[3] == 'a' &&
		rpc[4] == 't' && !isdigitletter(rpc[5])) {
		pc = rpc + 5;
		return FLOAT;
	    }
	    else if (rpc[1] == 'o' && rpc[2] == 'r' && !isdigitletter(rpc[3])) {
		pc = rpc + 3;
		return FOR;
	    }
	    goto id;
                
	case 'g':
	    if (rpc[1] == 'o' && rpc[2] == 't' && rpc[3] == 'o' &&
		!isdigitletter(rpc[4])) {
		pc = rpc + 4;
		return GOTO;
	    }
	    goto id;
                
	case 'i':
	    if (rpc[1] == 'n' && rpc[2] == 't' && !isdigitletter(rpc[3])) {
		pc = rpc + 3;
		return INT;
	    }
	    else if (rpc[1] == 'f' && !isdigitletter(rpc[2])) {
		pc = rpc + 2;
		return IF;
	    }
	    else if (rpc[1] == 'n' && rpc[2] == 'l' && rpc[3] == 'i' &&
		     rpc[4] == 'n' && rpc[5] == 'e' && !isdigitletter(rpc[6])) {
		pc = rpc + 6;
		return INLINE;
	    }
	    goto id;
                
	case 'l':
	    if (rpc[1] == 'o' && rpc[2] == 'n' && rpc[3] == 'g' &&
		!isdigitletter(rpc[4])) {
		pc = rpc + 4;
		return LONG;
	    }
	    goto id;
                
	case 'r':
	    if (rpc[1] == 'e' && rpc[2] == 't' && rpc[3] == 'u' &&
		rpc[4] == 'r' && rpc[5] == 'n' && !isdigitletter(rpc[6])) {
		pc = rpc + 6;
		return RETURN;
	    }
	    else if (rpc[1] == 'e' && rpc[2] == 's' && rpc[3] == 't' &&
		     rpc[4] == 'r' && rpc[5] == 'i' && rpc[6] == 'c' &&
		     rpc[7] == 't' && !isdigitletter(rpc[8])) {
		pc = rpc + 8;
		return RESTRICT;
	    }
	    else if (rpc[1] == 'e' && rpc[2] == 'g' && rpc[3] == 'i' &&
		     rpc[4] == 's' && rpc[5] == 't' && rpc[6] == 'e' &&
		     rpc[7] == 'r' && !isdigitletter(rpc[8])) {
		pc = rpc + 8;
		return REGISTER;
	    }
	    goto id;
                
	case 's':
	    if (rpc[1] == 't' && rpc[2] == 'a' && rpc[3] == 't' &&
		rpc[4] == 'i' && rpc[5] == 'c' && !isdigitletter(rpc[6])) {
		pc = rpc + 6;
		return STATIC;
	    }
	    else if (rpc[1] == 'h' && rpc[2] == 'o' && rpc[3] == 'r' &&
		     rpc[4] == 't' && !isdigitletter(rpc[5])) {
		pc = rpc + 5;
		return SHORT;
	    }
	    else if (rpc[1] == 'i' && rpc[2] == 'z' && rpc[3] == 'e' &&
		     rpc[4] == 'o' && rpc[5] == 'f' && !isdigitletter(rpc[6])) {
		pc = rpc + 6;
		return SIZEOF;
	    }
	    else if (rpc[1] == 'w' && rpc[2] == 'i' && rpc[3] == 't' &&
		     rpc[4] == 'c' && rpc[5] == 'h' && !isdigitletter(rpc[6])) {
		pc = rpc + 6;
		return SWITCH;
	    }
	    else if (rpc[1] == 't' && rpc[2] == 'r' && rpc[3] == 'u' &&
		     rpc[4] == 'c' && rpc[5] == 't' && !isdigitletter(rpc[6])) {
		pc = rpc + 6;
		return STRUCT;
	    }
	    else if (rpc[1] == 'i' && rpc[2] == 'g' && rpc[3] == 'n' &&
		     rpc[4] == 'e' && rpc[5] == 'd' && !isdigitletter(rpc[6])) {
		pc = rpc + 6;
		return SIGNED;
	    }
	    goto id;
                
	case 't':
	    if (rpc[1] == 'y' && rpc[2] == 'p' && rpc[3] == 'e' &&
		rpc[4] == 'd' && rpc[5] == 'e' && rpc[6] == 'f' &&
		!isdigitletter(rpc[7])) {
		pc = rpc + 7;
		return TYPEDEF;
	    }
	    goto id;
                
	case 'u':
	    if (rpc[1] == 'n' && rpc[2] == 's' && rpc[3] == 'i' &&
		rpc[4] == 'g' && rpc[5] == 'n' && rpc[6] == 'e' &&
		rpc[7] == 'd' && !isdigitletter(rpc[8])) {
		pc = rpc + 8;
		return UNSIGNED;
	    }
	    else if (rpc[1] == 'n' && rpc[2] == 'i' && rpc[3] == 'o' &&
		     rpc[4] == 'n' && !isdigitletter(rpc[5])) {
		pc = rpc + 5;
		return UNION;
	    }
	    goto id;
                
	case 'v':
	    if (rpc[1] == 'o' && rpc[2] == 'i' && rpc[3] == 'd' &&
		!isdigitletter(rpc[4])) {
		pc = rpc + 4;
		return VOID;
	    }
	    else if (rpc[1] == 'o' && rpc[2] == 'l' && rpc[3] == 'a' &&
		     rpc[4] == 't' && rpc[5] == 'i' && rpc[6] == 'l' &&
		     rpc[7] == 'e' && !isdigitletter(rpc[8])) {
		pc = rpc + 8;
		return VOLATILE;
	    }
	    goto id;
                
	case 'w':
	    if (rpc[1] == 'h' && rpc[2] == 'i' && rpc[3] == 'l' &&
		rpc[4] == 'e' && !isdigitletter(rpc[5])) {
		pc = rpc + 5;
		return WHILE;
	    }
	    goto id;
                
	case '_':
	    if (rpc[1] == 'B' && rpc[2] == 'o' && rpc[3] == 'o' &&
		rpc[4] == 'l' && !isdigitletter(rpc[5])) {
		pc = rpc + 5;
		return _BOOL;
	    }
	    else if (rpc[1] == 'C' && rpc[2] == 'o' && rpc[3] == 'm' &&
		     rpc[4] == 'p' && rpc[5] == 'l' && rpc[6] == 'e' &&
		     rpc[7] == 'x' && !isdigitletter(rpc[8])) {
		pc = rpc + 8;
		return _COMPLEX;
	    }
	    else if (rpc[1] == 'I' && rpc[2] == 'm' && rpc[3] == 'a' &&
		     rpc[4] == 'g' && rpc[5] == 'i' && rpc[6] == 'n' &&
		     rpc[7] == 'a' && rpc[8] == 'r' && rpc[9] == 'y' &&
		     !isdigitletter(rpc[10])) {
		pc = rpc + 10;
		return _IMAGINARY;
	    }
	    goto id;
                
	case 'L':
                
	    goto id;
                
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
	    if (!isblank(*rpc)) {
		error("invalid character 0x%x", *rpc);
	    }
        }
    }
}

static void line_comment()
{
    unsigned char *rpc = pc + 1;
    for (; ; ) {
        while (!isnewline(*rpc)) {
            rpc++;
        }
        pc = rpc;
        if (pe - pc < MINLEN) {
            fillbuf();
            rpc = pc;
        }
        if (isnewline(*rpc)) {
	    pc++;
            break;
        }
    }
    nextline();
}

static void block_comment()
{
    unsigned char *rpc = pc + 1;
    for (; rpc[0] != '*' || rpc[1] != '/'; ) {
        if (isnewline(*rpc)) {
            pc = rpc;
            if (pe - pc < MINLEN) {
                fillbuf();
            }
            nextline();
            rpc = pc;
            if (pc == pe) {
                break;
            }
        }
        else {
            rpc++;
        }
    }
    if (rpc < pe) {
        pc = rpc + 2;
    }
    else {
        error("unclosed comment");
    }
}

static int number()
{
    unsigned char *rpc = pc;
    unsigned long n = 0;
    if (rpc[0] == '0' && (rpc[1] == 'x' || rpc[1] == 'X')) {
        // hex
        int overflow;
        rpc += 2;
        for (; ; ) {
            int v;
            if (isdigit(*rpc)) {
                v = *rpc - '0';
            }
            else if (*rpc >= 'a' && *rpc <= 'f') {
                v = *rpc - 'a' + 10;
            }
            else if (*rpc >= 'A' && *rpc <= 'F') {
                v = *rpc - 'A' + 10;
            }
            else {
                break;
            }
            n = (n<<4) + v;
        }
        pc = rpc;
        token->v.u = n;
        return ICONSTANT;
    }
    else if (rpc[0] == '0') {
        // Oct
        while (isdigit(*rpc)) {
            rpc++;
        }
        pc = rpc;
        return ICONSTANT;
    }
    else {
        // Dec
        while (isdigit(*rpc)) {
            n = 10*n + (*rpc - '0');
            rpc++;
        }
        pc = rpc;
        if (*rpc == '.') {
            fnumber(n);
            return FCONSTANT;
        }
        else {
	    token->v.u = n;
            return ICONSTANT;
        }
    }
}

static void fnumber(unsigned long base)
{
    // .
    
}

static void identifier()
{
    unsigned char *rpc;
    unsigned char *ps;
    rpc = ps = pc-1;
    const char *idstr = NULL;
    for (; rpc != pe ; ) {
        while (isdigitletter(*rpc)) {
            rpc++;
        }
        appendstring(&idstr, ps, rpc - ps);
        pc = rpc;
        if (pc == pe) {
            fillbuf();
            rpc = pc;
        }
        else {
            break;
        }
    }
    
    token->name = strings(idstr);
    deallocate(idstr);
}

void match(int t)
{
    if (t == token->id) {
        gettok();
    }
    else {
        error("expect token '%s' at '%k'", tname(t), token);
    }
}

const char *tname(int t)
{
    if (t < 0) {
	return "EOI";
    }
    else if (t < 128) {
        return tnames[t];
    }
    else if (t < 256) {
        return NULL;
    }
    else if (t < TOKEND) {
        if (t == ID) {
	    return token->name;
        }
        else {
	    return tnames[128+t-ID];
        }
    }
    else {
        return NULL;
    }
}

int gettok()
{
    if (lookaheaded) {
	token1 = token2;
	lookaheaded = 0;
	return token1.id;
    }
    else {
	token1.id = do_gettok();
	return token1.id;
    }
}

int lookahead()
{
    if (lookaheaded) {
	return token2.id;
    }
    else {
	lookaheaded = 1;
	token2.id = do_gettok();
	return token2.id;
    }
}

const char * token_print_function(void *data)
{
    Token *p = data;
    
}

// test
int fake_gettok()
{
    register unsigned char *rpc;
    
    for (; ; ) {
        while (isblank(*pc)) {
            pc++;
        }
        
        if (pe - pc < MINLEN) {
            fillbuf();
        }
        
        rpc = pc++;
        
        switch (*rpc) {
	case '\n': case '\r':
	    nextline();
	    if (pc == pe) {
		return EOI;
	    }
	    continue;
	    
	default:
	    continue;
	}
    }
}
