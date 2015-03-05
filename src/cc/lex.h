#ifndef cc_lex_h
#define cc_lex_h

#define EOI  -1

// token
enum {
#define _a(a, b, c, d)  a,
#define _x(a, b, c, d)  a=d,
#define _t(a, b, c)     a,
#include "token.h"
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

// lex
extern const char * token_print_function(void *data);

extern void init_lexer();
extern int  gettok();
extern struct token *  lookahead();
extern void match(int t);
extern void skipto(int t);
extern void stopat(int t);
extern const char *tname(int t);

#endif
