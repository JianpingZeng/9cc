#ifndef cc_lex_h
#define cc_lex_h

#define LBUFSIZE     512
#define RBUFSIZE     1024
#define MINLEN       32
#define EOI         -1

// token
enum {
#define _a(x, y, z)     x,
#define _x(a, b, c, d)  a=c,
#define _t(a, b, c, d)  a,
#include "token.h"
    TOKEND
};

typedef struct lex *Lex;

union value {
    long i;
    unsigned long u;
    float f;
    long double d;
    void *p;
};

struct lex {
    const char *name;
    union value u;
};

extern int tok;
extern struct lex toklex;
extern unsigned int lineno;

// lex
extern void init_input();
extern int  gettok();
extern int  lookahead();
extern const char *tname(int tok);
extern void match(int t);

#define ERROR(fmt, ...)     errorl(__FILE__, __LINE__, lineno, fmt, ##__VA_ARGS__)
#define WARNING(fmt, ...)   warningl(__FILE__, __LINE__, lineno, fmt, ##__VA_ARGS__)
#define FATAL(fmt, ...)     fatal(fmt, ##__VA_ARGS__)

#define ERRORL(line, fmt, ...)    errorl(__FILE__, __LINE__, line, fmt, ##__VA_ARGS__)
#define WARNINGL(line, fmt, ...)   warningl(__FILE__, __LINE__, line, fmt, ##__VA_ARGS__)

#endif
