#ifndef _lex_h
#define _lex_h

union value {
    long long i;
    unsigned long long u;
    double d;
    long double ld;
    void *p;
    void (*g) ();
};

#define EOI  -1

enum {
#define _a(a, b, c, d)  a,
#define _x(a, b, c, d)  a=d,
#define _t(a, b, c)     a,
#include "token.def"
    TOKEND
};

struct source {
    const char *file;
    unsigned line;
};

struct token {
    int id;
    const char *name;
    struct {
        union value u;
        struct type *type;
    }v;
};

extern struct source source;
extern struct token  *token;

extern void lexer_init();
extern int  gettok();
extern struct token *  lookahead();
extern void match(int t);
extern void skipto(int t);
extern const char *tname(int t);

#endif
