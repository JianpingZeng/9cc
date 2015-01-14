#include <stdio.h>
#include <stdlib.h>
#include "lex.h"
#include "lib.h"

static unsigned char ibuf[LBUFSIZE+1+RBUFSIZE+1];
static unsigned char *pc;
static unsigned char *pe;
static long bread;
unsigned int lineno;

static void fsync()
{
    // # n "file"
    
}

static void fillbuf()
{
    long n;
    unsigned char *dst, *src;
    
    if (bread == 0) {
        return;
    }
    
    // copy
    n = pe - pc;
    dst = &ibuf[LBUFSIZE+1] - n;
    src = pc;
    while (src < pe) {
        *dst++ = *src++;
    }
    
    pc = &ibuf[LBUFSIZE+1] - n;
    
    if (feof(stdin)) {
        bread = 0;
    }
    else {
        bread = fread(&ibuf[LBUFSIZE+1], 1, RBUFSIZE, stdin);
    }
    
    if (bread < 0) {
        FATAL("read error");
    }
    
    pe = &ibuf[LBUFSIZE+1] + bread;
    *pe = '\n';
}

void init_input()
{
    pc = pe = &ibuf[LBUFSIZE + 1];
    
    bread = -1;
    
    fillbuf();
    
    lineno = 1;
}

static const char *tnames[] = {
#define _a(x, y, z)         y,
#define _x(a, b, c, d)      b,
#define _t(a, b, c, d)    	b,
#include "token.h"
};

int tok;
struct lex toklex;

// context
struct lex_context {
	int tok;
	struct lex toklex;
} con1, con2;
struct lex_context *lexcon;
static int lookaheaded;

enum {
    BLANK = 01, NEWLINE = 02, LETTER = 04,
    DIGIT = 010, HEX = 020, OTHER = 040,
};

static unsigned char map[255] = {
#define _a(x, y, z)     z,
#define _x(a, b, c, d)
#define _t(a, b, c, d)
#include "token.h"
    OTHER,
};

#define isdigit(c)            (map[c] & DIGIT)
#define isletter(c)           (map[c] & LETTER)
#define isdigitletter(c)      (isdigit(c) || isletter(c))
#define isblank(c)            (map[c] & BLANK)
#define isnewline(c)          (map[c] & NEWLINE)

static void identifier();
static int number();
static void fnumber(unsigned long);
static void nextline();
static void line_comment();
static void block_comment();

static int do_gettok()
{
    register unsigned char *pcur;
    
    for (; ; ) {
        while (isblank(*pc)) {
            pc++;
        }
        
        if (pe - pc < MINLEN) {
            fillbuf();
        }
        
        pcur = pc;
        
        switch (*pcur) {
            case '\n': case '\r':
                nextline();
                if (pcur == pe) {
                    return EOI;
                }
                continue;
                
            // separators & operators
            case '/':
                if (pcur[1] == '/') {
                    // line comment
                    line_comment();
                    continue;
                }
                else if (pcur[1] == '*') {
                    // block comment
                    block_comment();
                    continue;
                }
                else if (pcur[1] == '=') {
                    pc = pcur + 2;
                    return DIVEQ;
                }
                else {
                    return '/';
                }
                
            case '+':
                if (pcur[1] == '+') {
                    pc = pcur + 2;
                    return INCR;
                }
                else if (pcur[1] == '=') {
                    pc = pcur + 2;
                    return PLUSEQ;
                }
                else {
                    pc = pcur + 1;
                    return '+';
                }
                
            case '-':
                if (pcur[1] == '-') {
                    pc = pcur + 2;
                    return DECR;
                }
                else if (pcur[1] == '=') {
                    pc = pcur + 2;
                    return MINUSEQ;
                }
                else if (pcur[1] == '>') {
                    pc = pcur + 2;
                    return DEREF;
                }
                else {
                    pc = pcur + 1;
                    return '-';
                }
                
            case '*':
                return pcur[1] == '=' ? (pc = pcur+2, MULEQ) : (pc = pcur+1, '*');
                
            case '=':
                return pcur[1] == '=' ? (pc = pcur+2, EQ) : (pc = pcur+1, '=');
                
            case '!':
                return pcur[1] == '=' ? (pc = pcur+2, NEQ) : (pc = pcur+1, '!');
                
            case '(': case ')': case '{': case '}':
            case '[': case ']': case ',': case ';':
            case ':':
                pc++;
                return *pcur;
            
            // numbers
            case '0': case '1': case '2': case '3': case '4':
            case '5': case '6': case '7': case '8': case '9':
                return number();
                
            case '.':
                if (pcur[1] == '.' && pcur[2] == '.') {
                    pc = pcur + 3;
                    return ELLIPSIS;
                }
                else if (isdigit(pcur[1])) {
                    fnumber(0);
                    return FCONSTANT;
                }
                else {
                    return '.';
                }
            
            // keywords
            case 'a':
                if (pcur[1] == 'u' && pcur[2] == 't' && pcur[3] == 'o' &&
                    !isdigitletter(pcur[4])) {
                    pc = pcur + 4;
                    return AUTO;
                }
                goto id;
                
            case 'b':
                if (pcur[1] == 'r' && pcur[2] == 'e' && pcur[3] == 'a' &&
                    pcur[4] == 'k' && !isdigitletter(pcur[5])) {
                    pc = pcur + 5;
                    return BREAK;
                }
                goto id;
                
            case 'c':
                if (pcur[1] == 'a' && pcur[2] == 's' && pcur[3] == 'e' &&
                    !isdigitletter(pcur[4])) {
                    pc = pcur + 4;
                    return CASE;
                }
                else if (pcur[1] == 'h' && pcur[2] == 'a' && pcur[3] == 'r' &&
                         !isdigitletter(pcur[4])) {
                    pc = pcur + 4;
                    return CHAR;
                }
                else if (pcur[1] == 'o' && pcur[2] == 'n' && pcur[3] == 's' &&
                         pcur[4] == 't' && !isdigitletter(pcur[5])) {
                    pc = pcur + 5;
                    return CONST;
                }
                else if (pcur[1] == 'o' && pcur[2] == 'n' && pcur[3] == 't' &&
                         pcur[4] == 'i' && pcur[5] == 'n' && pcur[6] == 'e' &&
                         !isdigitletter(pcur[7])) {
                    pc = pcur + 7;
                    return CONTINUE;
                }
                goto id;
                
            case 'd':
                if (pcur[1] == 'e' && pcur[2] == 'f' && pcur[3] == 'a' &&
                    pcur[4] == 'u' && pcur[5] == 'l' && pcur[6] == 't' &&
                    !isdigitletter(pcur[7])) {
                    pc = pcur + 7;
                    return DEFAULT;
                }
                else if (pcur[1] == 'o' && !isdigitletter(pcur[2])) {
                    pc = pcur + 2;
                    return DO;
                }
                else if (pcur[1] == 'o' && pcur[2] == 'u' && pcur[3] == 'b' &&
                         pcur[4] == 'l' && pcur[5] == 'e' && !isdigitletter(pcur[6])) {
                    pc = pcur + 6;
                    return DOUBLE;
                }
                goto id;
                
            case 'e':
                if (pcur[1] == 'x' && pcur[2] == 't' && pcur[3] == 'e' &&
                    pcur[4] == 'r' && pcur[5] == 'n' && !isdigitletter(pcur[6])) {
                    pc = pcur + 6;
                    return EXTERN;
                }
                else if (pcur[1] == 'l' && pcur[2] == 's' && pcur[3] == 'e' &&
                         !isdigitletter(pcur[4])) {
                    pc = pcur + 4;
                    return ELSE;
                }
                else if (pcur[1] == 'n' && pcur[2] == 'u' && pcur[3] == 'm' &&
                         !isdigitletter(pcur[4])) {
                    pc = pcur + 4;
                    return ENUM;
                }
                goto id;
                
            case 'f':
                if (pcur[1] == 'l' && pcur[2] == 'o' && pcur[3] == 'a' &&
                    pcur[4] == 't' && !isdigitletter(pcur[5])) {
                    pc = pcur + 5;
                    return FLOAT;
                }
                else if (pcur[1] == 'o' && pcur[2] == 'r' && !isdigitletter(pcur[3])) {
                    pc = pcur + 3;
                    return FOR;
                }
                goto id;
                
            case 'g':
                if (pcur[1] == 'o' && pcur[2] == 't' && pcur[3] == 'o' &&
                    !isdigitletter(pcur[4])) {
                    pc = pcur + 4;
                    return GOTO;
                }
                goto id;
                
            case 'i':
                if (pcur[1] == 'n' && pcur[2] == 't' && !isdigitletter(pcur[3])) {
                    pc = pcur + 3;
                    return INT;
                }
                else if (pcur[1] == 'f' && !isdigitletter(pcur[2])) {
                    pc = pcur + 2;
                    return IF;
                }
                else if (pcur[1] == 'n' && pcur[2] == 'l' && pcur[3] == 'i' &&
                         pcur[4] == 'n' && pcur[5] == 'e' && !isdigitletter(pcur[6])) {
                    pc = pcur + 6;
                    return INLINE;
                }
                goto id;
                
            case 'l':
                if (pcur[1] == 'o' && pcur[2] == 'n' && pcur[3] == 'g' &&
                    !isdigitletter(pcur[4])) {
                    pc = pcur + 4;
                    return LONG;
                }
                goto id;
                
            case 'r':
                if (pcur[1] == 'e' && pcur[2] == 't' && pcur[3] == 'u' &&
                    pcur[4] == 'r' && pcur[5] == 'n' && !isdigitletter(pcur[6])) {
                    pc = pcur + 6;
                    return RETURN;
                }
                else if (pcur[1] == 'e' && pcur[2] == 's' && pcur[3] == 't' &&
                         pcur[4] == 'r' && pcur[5] == 'i' && pcur[6] == 'c' &&
                         pcur[7] == 't' && !isdigitletter(pcur[8])) {
                    pc = pcur + 8;
                    return RESTRICT;
                }
                else if (pcur[1] == 'e' && pcur[2] == 'g' && pcur[3] == 'i' &&
                         pcur[4] == 's' && pcur[5] == 't' && pcur[6] == 'e' &&
                         pcur[7] == 'r' && !isdigitletter(pcur[8])) {
                    pc = pcur + 8;
                    return REGISTER;
                }
                goto id;
                
            case 's':
                if (pcur[1] == 't' && pcur[2] == 'a' && pcur[3] == 't' &&
                    pcur[4] == 'i' && pcur[5] == 'c' && !isdigitletter(pcur[6])) {
                    pc = pcur + 6;
                    return STATIC;
                }
                else if (pcur[1] == 'h' && pcur[2] == 'o' && pcur[3] == 'r' &&
                         pcur[4] == 't' && !isdigitletter(pcur[5])) {
                    pc = pcur + 5;
                    return SHORT;
                }
                else if (pcur[1] == 'i' && pcur[2] == 'z' && pcur[3] == 'e' &&
                         pcur[4] == 'o' && pcur[5] == 'f' && !isdigitletter(pcur[6])) {
                    pc = pcur + 6;
                    return SIZEOF;
                }
                else if (pcur[1] == 'w' && pcur[2] == 'i' && pcur[3] == 't' &&
                         pcur[4] == 'c' && pcur[5] == 'h' && !isdigitletter(pcur[6])) {
                    pc = pcur + 6;
                    return SWITCH;
                }
                else if (pcur[1] == 't' && pcur[2] == 'r' && pcur[3] == 'u' &&
                         pcur[4] == 'c' && pcur[5] == 't' && !isdigitletter(pcur[6])) {
                    pc = pcur + 6;
                    return STRUCT;
                }
                else if (pcur[1] == 'i' && pcur[2] == 'g' && pcur[3] == 'n' &&
                         pcur[4] == 'e' && pcur[5] == 'd' && !isdigitletter(pcur[6])) {
                    pc = pcur + 6;
                    return SIGNED;
                }
                goto id;
                
            case 't':
                if (pcur[1] == 'y' && pcur[2] == 'p' && pcur[3] == 'e' &&
                    pcur[4] == 'd' && pcur[5] == 'e' && pcur[6] == 'f' &&
                    !isdigitletter(pcur[7])) {
                    pc = pcur + 7;
                    return TYPEDEF;
                }
                goto id;
                
            case 'u':
                if (pcur[1] == 'n' && pcur[2] == 's' && pcur[3] == 'i' &&
                    pcur[4] == 'g' && pcur[5] == 'n' && pcur[6] == 'e' &&
                    pcur[7] == 'd' && !isdigitletter(pcur[8])) {
                    pc = pcur + 8;
                    return UNSIGNED;
                }
                else if (pcur[1] == 'n' && pcur[2] == 'i' && pcur[3] == 'o' &&
                         pcur[4] == 'n' && !isdigitletter(pcur[5])) {
                    pc = pcur + 5;
                    return UNION;
                }
                goto id;
                
            case 'v':
                if (pcur[1] == 'o' && pcur[2] == 'i' && pcur[3] == 'd' &&
                    !isdigitletter(pcur[4])) {
                    pc = pcur + 4;
                    return VOID;
                }
                else if (pcur[1] == 'o' && pcur[2] == 'l' && pcur[3] == 'a' &&
                         pcur[4] == 't' && pcur[5] == 'i' && pcur[6] == 'l' &&
                         pcur[7] == 'e' && !isdigitletter(pcur[8])) {
                    pc = pcur + 8;
                    return VOLATILE;
                }
                goto id;
                
            case 'w':
                if (pcur[1] == 'h' && pcur[2] == 'i' && pcur[3] == 'l' &&
                    pcur[4] == 'e' && !isdigitletter(pcur[5])) {
                    pc = pcur + 5;
                    return WHILE;
                }
                goto id;
                
            case '_':
                if (pcur[1] == 'B' && pcur[2] == 'o' && pcur[3] == 'o' &&
                    pcur[4] == 'l' && !isdigitletter(pcur[5])) {
                    pc = pcur + 5;
                    return _BOOL;
                }
                else if (pcur[1] == 'C' && pcur[2] == 'o' && pcur[3] == 'm' &&
                         pcur[4] == 'p' && pcur[5] == 'l' && pcur[6] == 'e' &&
                         pcur[7] == 'x' && !isdigitletter(pcur[8])) {
                    pc = pcur + 8;
                    return _COMPLEX;
                }
                else if (pcur[1] == 'I' && pcur[2] == 'm' && pcur[3] == 'a' &&
                         pcur[4] == 'g' && pcur[5] == 'i' && pcur[6] == 'n' &&
                         pcur[7] == 'a' && pcur[8] == 'r' && pcur[9] == 'y' &&
                         !isdigitletter(pcur[10])) {
                    pc = pcur + 10;
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
                if (!isblank(*pcur)) {
                    pc++;
                    ERROR("invalid character 0x%x\n", *pcur);
                }
                break;
        }
    }
}

static void line_comment()
{
    unsigned char *pcur = pc + 2;
    for (; ; ) {
        while (!isnewline(*pcur)) {
            pcur++;
        }
        pc = pcur;
        if (pe - pc < MINLEN) {
            fillbuf();
            pcur = pc;
        }
        if (isnewline(*pcur)) {
            break;
        }
    }
    nextline();
}

static void block_comment()
{
    int line = lineno;
    unsigned char *pcur = pc + 2;
    for (; pcur[0] != '*' || pcur[1] != '/'; ) {
        if (isnewline(*pcur)) {
            pc = pcur;
            if (pe - pc < MINLEN) {
                fillbuf();
            }
            nextline();
            pcur = pc;
            if (pc == pe) {
                break;
            }
        }
        else {
            pcur++;
        }
    }
    if (pcur < pe) {
        pc = pcur + 2;
    }
    else {
        WARNINGL(line, "unclosed comment");
    }
}

static void nextline()
{
    // win: \r\n
    // mac: \r
    // *nix: \n
    unsigned char *pcur = pc;
    if (pcur[0] == '\n') {
        if (pcur != pe) {
            lineno++;
            pc = pcur + 1;
        }
    }
    else if (pcur[0] == '\r') {
        if (pcur[1] == '\n' && (&pcur[1] != pe)) {
            pc = pcur + 2;
        }
        else {
            pc = pcur + 1;
        }
        
        lineno++;
    }
}

static int number()
{
    unsigned char *pcur = pc;
    unsigned long n = 0;
    if (pcur[0] == '0' && (pcur[1] == 'x' || pcur[1] == 'X')) {
        // hex
        int overflow;
        pcur += 2;
        for (; ; ) {
            int v;
            if (isdigit(*pcur)) {
                v = *pcur - '0';
            }
            else if (*pcur >= 'a' && *pcur <= 'f') {
                v = *pcur - 'a' + 10;
            }
            else if (*pcur >= 'A' && *pcur <= 'F') {
                v = *pcur - 'A' + 10;
            }
            else {
                break;
            }
            n = (n<<4) + v;
        }
        pc = pcur;
        lexcon->toklex.u.u = n;
        return ICONSTANT;
    }
    else if (pcur[0] == '0') {
        // Oct
        while (isdigit(*pcur)) {
            pcur++;
        }
        pc = pcur;
        return ICONSTANT;
    }
    else {
        // Dec
        while (isdigit(*pcur)) {
            n = 10*n + (*pcur - '0');
            pcur++;
        }
        pc = pcur;
        if (*pcur == '.') {
            fnumber(n);
            return FCONSTANT;
        }
        else {
            lexcon->toklex.u.u = n;
            return ICONSTANT;
        }
    }
}

static void fnumber(unsigned long base)
{
    // .
    
}

// length of identifier is no limit
static void identifier()
{
    unsigned char *pcur = pc;
    const char *idstr = NULL;
    for (; pcur != pe ; ) {
        while (isdigitletter(*pcur)) {
            pcur++;
        }
        appendstring(&idstr, pc, (int) (pcur - pc));
        pc = pcur;
        if (pc == pe) {
            fillbuf();
            pcur = pc;
        }
        else {
            break;
        }
    }
    
    lexcon->toklex.name = string(idstr);
    deallocate(idstr);
}

void match(int t)
{
    if (t == tok) {
        tok = gettok();
    }
    else {
        ERROR("expect token '%s' at '%s'", tname(t), tname(tok));
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
			return lexcon->toklex.name;
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
		con1 = con2;
		lookaheaded = 0;
		return con1.tok;
	}
	else {
		lexcon = &con1;
		con1.tok = do_gettok();
		toklex = con1.toklex;
		return con1.tok;
	}
}

int lookahead()
{
	if (lookaheaded) {
		return con2.tok;
	}
	else {
		lookaheaded = 1;
		lexcon = &con2;
		con2.tok = do_gettok();
		return con2.tok;
	}
}
