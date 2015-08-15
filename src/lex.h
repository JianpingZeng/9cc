#ifndef _lex_h
#define _lex_h

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
    unsigned column;
};

struct token {
    int id;
    const char *name;
    int kind;
};

extern struct source source;
extern struct token  *token;

extern bool is_digit(char c);
extern bool is_hex(char c);
extern bool is_digithex(char c);

extern void lexer_init();
extern int  gettok();
extern struct token *  lookahead();
extern void expect(int t);
extern void match(int t, int follow[]);
extern const char *tname(int t);

#endif
