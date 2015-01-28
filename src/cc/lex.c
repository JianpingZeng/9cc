#include "cc.h"

#define LBUFSIZE     32
#define RBUFSIZE     32
#define MAXTOKEN     LBUFSIZE

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
#define ishex(c)              (map[c] & HEX)

static unsigned char ibuf[LBUFSIZE+RBUFSIZE+1];
static unsigned char *pc;
static unsigned char *pe;
static long bread;
Source src;

static void fillbuf()
{
    if (bread == 0) {
	if (pc > pe) pc = pe;
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
    unsigned char *f = NULL;
    String *p = new_string();
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
		if (p) free_string(p);
		cclog("input file seems incorrect when #");
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
		string_concatn(p, fb, pc-fb);
		continue;
	    }
	    string_concatn(p, fb, pc-fb);
	    f = strings(p->str);
	    free_string(p);
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
	src.line = line-1;
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

static Token _token;
Token *token = &_token;

static void identifier();
static int number();
static void fnumber(unsigned long long n, int overflow, int base, String *s);
static void nextline();
static void line_comment();
static void block_comment();
static unsigned escape(String *s);
static void float_constant(String *s);
static void char_constant(int wide);
static void string_constant(int wide);
static void integer_constant(unsigned long long value, int overflow, int base, String *s);

static int do_gettok()
{
    register unsigned char *rpc;
    
    for (; ; ) {
        while (isblank(*pc)) {
            pc++;
        }
        
        if (pe - pc < MAXTOKEN) {
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
		return ADDEQ;
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

	case '%':
	    return rpc[1] == '=' ? (pc = rpc+2, MODEQ) : (pc = rpc+1, '%');

	case '^':
	    return rpc[1] == '=' ? (pc = rpc+2, XOREQ) : (pc = rpc+1, '^');

        case '&':
	    if (rpc[1] == '=') {
		pc = rpc + 2;
		return BANDEQ;
	    }
	    else if (rpc[1] == '&') {
		pc = rpc + 2;
		return AND;
	    }
	    else {
		return '&';
	    }

        case '|':
	    if (rpc[1] == '=') {
		pc = rpc + 2;
		return BOREQ;
	    }
	    else if (rpc[1] == '|') {
		pc = rpc + 2;
		return OR;
	    }
	    else {
		return '|';
	    }

	case '<':
	    if (rpc[1] == '=') {
		pc = rpc + 2;
		return LEQ;
	    }
	    else if (rpc[1] == '<' && rpc[2] == '=') {
		pc = rpc + 3;
		return LSHIFTEQ;
	    }
	    else if (rpc[1] == '<') {
		pc = rpc + 2;
		return LSHIFT;
	    }
	    else {
		return '<';
	    }

	case '>':
	    if (rpc[1] == '=') {
		pc = rpc + 2;
		return REQ;
	    }
	    else if (rpc[1] == '>' && rpc[2] == '=') {
		pc = rpc + 3;
		return RSHIFTEQ;
	    }
	    else if (rpc[1] == '>') {
		pc = rpc + 2;
		return RSHIFT;
	    }
	    else {
		return '>';
	    }
	    
	case '(': case ')': case '{': case '}':
	case '[': case ']': case ',': case ';':
	case ':': case '~': case '?':
	    return *rpc;

        case '\'':
	    char_constant(0);
	    return ICONSTANT;

	case '"':
	    string_constant(0);
	    return SCONSTANT;
	    
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
		pc = rpc;
		fnumber(0, 0, 10, NULL);
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
            if (rpc[1] == '\'') {
		pc = rpc + 2;
		char_constant(1);
		return ICONSTANT;
	    }
	    else if (rpc[1] == '"') {
		pc = rpc + 2;
		string_constant(1);
		return SCONSTANT;
	    }
	    else {
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
        if (pe - pc < MAXTOKEN) {
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
            if (pe - pc < MAXTOKEN) {
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
    unsigned char *rpc = pc-1;
    if (rpc[0] == '0' && (rpc[1] == 'x' || rpc[1] == 'X')) {
        // Hex
	unsigned long long n = 0;
        int overflow = 0;
	String *s = new_string();
	string_concatn(s, rpc, 2);
        pc = rpc = rpc + 2;
	if (!isdigit(*rpc) && !ishex(*rpc)) {
	    token->name = strings(s->str);
	    free_string(s);
	    error("incomplete hex constant: %k", token);
	    return ICONSTANT;
	}
        for (; isdigit(*rpc) || ishex(*rpc) || rpc == pe; ) {
	    if (rpc == pe) {
		string_concatn(s, pc, rpc-pc);
		pc = rpc;
		fillbuf();
		rpc = pc;
		if (pc == pe) break;
		continue;
	    }
	    if (n & ~(~0ULL >> 4)) {
		overflow = 1;
	    }
	    else {
		int d;
		if (ishex(*rpc)) {
		    d = (*rpc & 0x5f) - 'A' + 10;
		}
		else {
		    d = *rpc - '0';
		}
		n = (n<<4) + d;
	    }
	    rpc++;
        }
	string_concatn(s, pc, rpc-pc);
        pc = rpc;
	if (*rpc == '.' || *rpc == 'p') {
	    fnumber(n, overflow, 16, s);
	    free_string(s);
	    return FCONSTANT;
	}
	else {
	    integer_constant(n, overflow, 16, s);
	    free_string(s);
	    return ICONSTANT;
	}
    }
    else if (rpc[0] == '0') {
        // Oct
	unsigned long long n = 0;
	int err = 0;
	int overflow = 0;
	String *s = new_string();
	pc = rpc;
	for (;isdigit(*rpc) || rpc == pe;) {
	    if (rpc == pe) {
		string_concatn(s, pc, rpc-pc);
		pc = rpc;
		fillbuf();
		rpc = pc;
		if (pc == pe) break;
		continue;
	    }
	    if (*rpc == '8' || *rpc == '9') {
		err = 1;
	    }
	    if (n & ~(~0ULL >> 3)) {
		overflow = 1;
	    }
	    else {
		n = (n<<3) + (*rpc - '0');
	    }
	    rpc++;
	}
	string_concatn(s, pc, rpc-pc);
	pc = rpc;
	if (*rpc == '.' || *rpc == 'e' || *rpc == 'E') {
	    fnumber(n, overflow, 10, s);
	    free_string(s);
	    return FCONSTANT;
	}
	else {
	    integer_constant(n, overflow, 8, s);
	    if (err) {
		error("invalid octal constant %k", token);
	    }
	    free_string(s);
	    return ICONSTANT;
	}
    }
    else {
        // Dec
	unsigned long long n = 0;
	int overflow = 0;
	String *s = new_string();
	pc = rpc;
	for (;isdigit(*rpc) || rpc == pe;) {
	    if (rpc == pe) {
		string_concatn(s, pc, rpc-pc);
		pc = rpc;
		fillbuf();
		rpc = pc;
		if (pc == pe) break;
		continue;
	    }
	    int d = *rpc - '0';
	    if (n > (longlongtype->limits.max.i - d)/10) {
		overflow = 1;
	    }
	    else {
		n = n*10 + (*rpc - '0');
	    }
	    rpc++;
	}
	string_concatn(s, pc, rpc-pc);
        pc = rpc;
        if (*rpc == '.' || *rpc == 'e' || *rpc == 'E') {
            fnumber(n, overflow, 10, s);
	    free_string(s);
            return FCONSTANT;
        }
        else {
	    integer_constant(n, overflow, 10, s);
	    free_string(s);
            return ICONSTANT;
        }
    }
}

static void fnumber(unsigned long long n, int overflow, int base, String *s)
{
    // ./e/E/p
    if (!s) s = new_string();
    
    if (base == 10) {
	assert(*pc=='.'||*pc=='e'||*pc=='E');
	if (*pc == '.') {
	    string_concatn(s, pc, 1);
	    pc++;
	    for (;isdigit(*pc) || pc == pe;) {
		if (pc == pe) {
		    fillbuf();
		    if (pc == pe) break;
		    continue;
		}
		string_concatn(s, pc, 1);
		pc++;
	    }
	}

	if (*pc == 'e' || *pc == 'E') {
	    string_concatn(s, pc, 1);
	    pc++;
	    if (pe - pc < MAXTOKEN) {
		fillbuf();
	    }
	    if (*pc == '+' || *pc == '-') {
		string_concatn(s, pc, 1);
		pc++;
	    }
	    if (isdigit(*pc)) {
		for (;isdigit(*pc) || pc == pe;) {
		    if (pc == pe) {
			fillbuf();
			if (pc == pe) break;
			continue;
		    }
		    string_concatn(s, pc, 1);
		    pc++;
		}
	    }
	    else {
		error("invalid float constant");
	    }
	}


	
    }
    else {
	assert(*pc=='.'||*pc=='p');
	
    }

    float_constant(s);
}

static void float_constant(String *s)
{
    if (*pc == 'f' || *pc == 'F' || *pc == 'l' || *pc == 'L') {
	string_concatn(s, pc++, 1);
    }
    token->name = strings(s->str);
}

static void integer_constant(unsigned long long n, int overflow, int base, String *s)
{
    unsigned char *rpc = pc;
    int ull = (rpc[0] == 'u' || rpc[0] == 'U') &&
	((rpc[1] == 'l' && rpc[2] == 'l') ||
	 (rpc[1] == 'L' && rpc[2] == 'L'));
    int llu = ((rpc[0] == 'l' && rpc[1] == 'l') || (rpc[0] == 'L' && rpc[1] == 'L')) &&
	(rpc[2] == 'u' || rpc[2] == 'U');
    if (ull || llu) {
	// unsigned long long
	token->v.type = unsignedlonglongtype;
	pc = rpc + 3;
	string_concatn(s, rpc, 3);
    }
    else if ((rpc[0] == 'l' && rpc[1] == 'l') || (rpc[0] == 'L' && rpc[1] == 'L')) {
	// long long
	if (n > longlongtype->limits.max.i && base != 10) {
	    token->v.type = unsignedlonglongtype;
	}
	else {
	    token->v.type = longlongtype;
	}
	pc = rpc + 2;
	string_concatn(s, rpc, 2);
    }
    else if (((rpc[0] == 'l' || rpc[0] == 'L') && (rpc[1] == 'u' || rpc[1] == 'U')) ||
	     ((rpc[0] == 'u' || rpc[0] == 'U') && (rpc[1] == 'l' || rpc[1] == 'L'))) {
	// unsigned long
	if (n > unsignedlongtype->limits.max.u) {
	    token->v.type = unsignedlonglongtype;
	}
	else {
	    token->v.type = unsignedlongtype;
	}
	pc = rpc + 2;	
	string_concatn(s, rpc, 2);
    }
    else if (rpc[0] == 'l' || rpc[0] == 'L') {
	// long
	if (base == 10) {
	    if (n > longtype->limits.max.i) {
		token->v.type = longlongtype;
	    }
	    else {
		token->v.type = longtype;
	    }
	}
	else {
	    if (n > longlongtype->limits.max.i) {
		token->v.type = unsignedlonglongtype;
	    }
	    else if (n > unsignedlongtype->limits.max.u) {
		token->v.type = longlongtype;
	    }
	    else if (n > longtype->limits.max.i) {
		token->v.type = unsignedlongtype;
	    }
	    else {
		token->v.type = longtype;
	    }
	}
	pc = rpc + 1;
	string_concatn(s, rpc, 1);
    }
    else if (rpc[0] == 'u' || rpc[0] == 'U') {
	// unsigned
	if (n > unsignedlongtype->limits.max.u) {
	    token->v.type = unsignedlonglongtype;
	}
	else if (n > unsignedinttype->limits.max.u) {
	    token->v.type = unsignedlongtype;
	}
	else {
	    token->v.type = unsignedinttype;
	}
	pc = rpc + 1;
	string_concatn(s, rpc, 1);
    }
    else {
	if (base == 10) {
	    if (n > longtype->limits.max.i) {
		token->v.type = longlongtype;
	    }
	    else if (n > inttype->limits.max.i) {
		token->v.type = longtype;
	    }
	    else {
		token->v.type = inttype;
	    }
	}
	else {
	    if (n > longlongtype->limits.max.i) {
		token->v.type = unsignedlonglongtype;
	    }
	    else if (n > unsignedlongtype->limits.max.u) {
		token->v.type = longlongtype;
	    }
	    else if (n > longtype->limits.max.i) {
		token->v.type = unsignedlongtype;
	    }
	    else if (n > unsignedinttype->limits.max.u) {
		token->v.type = longtype;
	    }
	    else if (n > inttype->limits.max.i) {
		token->v.type = unsignedinttype;
	    }
	    else {
		token->v.type = inttype;
	    }
	}
    }

    token->name = strings(s->str);

    switch (token->v.type->op) {
    case INT:
	if (overflow || n > longlongtype->limits.max.i) {
	    warning("integer constant overflow: %k", token);
	    token->v.u.i = longlongtype->limits.max.i;
	}
	else {
	    token->v.u.i = n;
	}
	break;
    case UNSIGNED:
	if (overflow) {
	    token->v.u.u = unsignedlonglongtype->limits.max.u;
	    warning("integer constant overflow: %k", token);
	}
	else {
	    token->v.u.u = n;
	}
	break;
    default:
	assert(0);
    }
}

static void char_constant(int wide)
{    
    unsigned long long c = 0;
    int overflow = 0;
    int char_rec = 0;
    char ws[MB_LEN_MAX];
    int len = 0;
    
    String *s = new_string();
    wide ? string_concatn(s, pc-2, 2) : string_concatn(s, pc-1, 1);
    
    for (; *pc != '\'';) {
	if (pc >= pe) {
	    fillbuf();
	    if (pc == pe) break;
	}
	if (char_rec) {
	    overflow = 1;
	}
	if (isnewline(*pc)) {
	    break;
	}

	if (*pc == '\\') {
	    // escape
	    c = escape(s);
	    char_rec = 1;
	}
	else {
	    if (wide) {
		string_concatn(s, pc, 1);
		if (len >= MB_LEN_MAX) {
		    error("multibyte character overflow");
		}
		else {
		    ws[len++] = (char) *pc;
		}
		pc++;
	    }
	    else {
		string_concatn(s, pc, 1);
		c = *pc++;
		char_rec = 1;
	    }
	}
    }

    
    if (*pc != '\'') {
	token->name = strings(s->str);
	error("unclosed character constant: %k", token);
    }
    else {
	string_concatn(s, pc, 1);
	token->name = strings(s->str);

	if (!char_rec && !len) {
	    error("incomplete character constant: %k", token);
	}
	else if (overflow) {
	    error("extraneous characters in character constant: %k", token);
	}
	else if ((!wide && c > unsignedchartype->limits.max.u) || (wide && c > wchartype->limits.max.u)){
	    error("character constant overflow: %k", token);
	}
	else if (len) {
	    if (mbtowc(&c, ws, len) != len) {
		error("invalid multi-character sequence");
	    }
	}
	pc++;
    }
    
    token->v.u.u = wide ? (wchar_t) c : (unsigned char) c;
    token->v.type = wide ? wchartype : unsignedchartype;
    free_string(s);
}

static void string_constant(int wide)
{

    
}

static void identifier()
{
    unsigned char *rpc;
    String *s = new_string();
    rpc = pc = pc-1;
    for (;isdigitletter(*rpc) || rpc == pe;) {
	if (rpc == pe) {
	    string_concatn(s, pc, rpc-pc);
	    pc = rpc;
	    fillbuf();
	    rpc = pc;
	    if (pc == pe) break;
	    continue;
	}
	rpc++;
    }
    string_concatn(s, pc, rpc-pc);
    pc = rpc;
    token->name = strings(s->str);
    free_string(s);
}

static unsigned escape(String *s)
{
    assert(*pc == '\\');
    pc++;
    string_concatn(s, pc-1, 2);
    switch (*pc++) {
    case 'a': return 7;
    case 'b': return '\b';
    case 'f': return '\f';
    case 'n': return '\n';
    case 'r': return '\r';
    case 't': return '\t';
    case 'v': return '\v';
    case '\'': case '"':
    case '\\': case '\?':
	return pc[-1];
    case '0': case '1': case '2':
    case '3': case '4': case '5':
    case '6': case '7':
	{
	    unsigned c = pc[-1] - '0';
	    if (*pc >= '0' && *pc <= '7') {
		string_concatn(s, pc, 1);
		c = (c<<3) + (*pc++) - '0';
		if (*pc >= '0' && *pc <= '7') {
		    string_concatn(s, pc, 1);
		    c = (c<<3) + (*pc++) - '0';
		}
	    }
	    return c;
	}
    case 'x':
	{
	    unsigned c = 0;
	    int overflow = 0;
	    if (!isdigit(*pc) && !ishex(*pc)) {
		error("\\x used with no following hex digits");
		return 0;
	    }
	    for (; isdigit(*pc) || ishex(*pc) || pc == pe; ) {
		if (pc == pe) {
		    fillbuf();
		    if (pc == pe) break;
		    continue;
		}
		string_concatn(s, pc, 1);
		if (overflow) {
		    pc++;
		    continue;
		}
		if (c >> (bits(wchartype) - 4)) {
		    overflow = 1;
		    error("hex escape sequence out of range");
		}
		else {
		    if (isdigit(*pc)) {
			c = (c<<4) + *pc - '0';
		    }
		    else {
			c = (c<<4) + (*pc & 0x5f) - 'A' + 10;
		    }
		}
		pc++;
	    }
	    return c;
	}
    case 'u': case 'U':
	{
	    // universal character name: expect 4(u)/8(U) hex digits
	    unsigned c = 0;
	    int x = 0;
	    int n = pc[-1] == 'u' ? 4 : 8;
	    unsigned char *ps = pc - 2;
	    for (; isdigit(*pc) || ishex(*pc); x++, pc++) {
		if (x == n) break;

		if (isdigit(*pc)) {
		    c = (c<<4) + *pc - '0';
		}
		else {
		    c = (c<<4) + (*pc & 0x5f) - 'A' + 10;
		}
		string_concatn(s, pc, 1);
	    }
	    if (x < n) {
		error("incomplete universal character name: %S", ps, pc-ps);
	    }
	    return c;
	}
    default:
	error("unrecognized escape character 0x%x", pc[-1]);
	return pc[-1];
    }
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
        return "(null)";
    }
    else if (t < TOKEND) {
        return tnames[128+t-ID];
    }
    else {
        return "(null)";
    }
}

int gettok()
{
    int tok;
    token->name = NULL;
    tok = do_gettok();
    token->id = tok;
    if (!token->name) {
	token->name = tname(token->id);
    }
    return tok;
}

int lookahead()
{
   
}

const char * token_print_function(void *data)
{
    Token *p = data;
    return p->name;
}
