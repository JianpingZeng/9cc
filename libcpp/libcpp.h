#ifndef LIBCPP_H
#define LIBCPP_H

///
/// type declarations
///

struct source {
    unsigned int line;
    unsigned int column;
    const char *file;
};

union value {
    long i;
    unsigned long u;
    long double d;
    void *p;
    void (*g) ();
};

enum {
#define _a(a, b, c, d)  a,
#define _x(a, b, c, d)  a=d,
#define _t(a, b, c)     a,
#define _k(a, b, c)     a,
#include "token.def"
    EOI
};

// token
#define TOK_ID_STR(t)    ((t)->u.ident->str)
#define TOK_LIT_STR(t)   ((t)->u.lit.str)

struct token {
    unsigned short id;
    unsigned short kind;
    bool bol;                // beginning of line
    bool space;              // leading space
    bool param;              // macro param
    unsigned int pos;        // param posistion
    struct source src;
    struct hideset *hideset;
    union {
        // identifier
        struct ident *ident;
        // string/number literal
        struct {
            const char *str;    // literal lexeme
            bool wide;          // wide string
            char base;          // 0:dec, 8:Oct, 16:hex
            int suffix;
            union value v;
        } lit;
    } u;
};

// The file read by preprocessor.
struct file {
    const char *file;           // file name
    struct buffer *buffer;      // current buffer (top of buffer stack)
    struct vector *tokens;      // parser ungets
    struct idtab *idtab;        // identifier hash map
    struct vector *std_include_paths;
    struct vector *usr_include_paths;
    struct tokenrun *tokenrun;
    struct token *cur_token;
    const char *date;            // current date string (quoted)
    const char *time;            // current time string (quoted)
    unsigned int errors, warnings;
};

// An identifier
struct ident {
    unsigned int hash;
    unsigned int len;
    const char *str;
    int type:8;
    union {
        struct macro *macro;
    } u;
};


///
/// external functions
///

// cpp.c
extern void cpp_init(int argc, char *argv[]);
extern struct token *get_pptok(struct file *pfile);

// input.c
extern void cpp_dump(struct file *pfile);

// lex.c
extern const char *id2s(int t);
extern const char *tok2s(struct token *t);

extern int gettok(void);
extern struct token *lookahead(void);
extern void expect(int t);
extern void match(int t, void (*otherwise) (void));
extern const char *ids(const char *name);
#define token_is(t)  (token->id == (t))
#define token_is_not(t)  (token->id != (t))
#define next_token_is(t)  (lookahead()->id == (t))
#define is_char_cnst(t)  ((t)->id == ICONSTANT && \
                          (t)->u.lit.str && \
                          (t)->u.lit.str[0] == '\'' && \
                          (t)->u.lit.str[0] == 'L')
#define is_assign_tok(t)    ((t)->id == '=' || (t)->kind == ADDEQ)

///
/// external variables
///

extern struct file *cpp_file;
extern struct source source;
extern struct token *token;

#endif /* LIBCPP_H */
