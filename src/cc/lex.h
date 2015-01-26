#ifndef cc_lex_h
#define cc_lex_h

#define EOI  -1

// token
enum {
#define _a(x, y, z)     x,
#define _x(a, b, c, d)  a=d,
#define _t(a, b, c)     a,
#include "token.h"
    TOKEND
};

typedef struct {
    const char *file;
    unsigned line;
    unsigned col;
} Source;

typedef struct {
    int id;
    const char *name;
    struct {
	union {
	    long long i;
	    unsigned long long u;
	}u;
	Type *type;
    }v;
} Token;

extern Source src;
extern Token *token;

// lex
extern const char * token_print_function(void *data);

extern void init_input();
extern int  gettok();
extern int  lookahead();
extern void match(int t);
extern const char *tname(int t);

#endif
