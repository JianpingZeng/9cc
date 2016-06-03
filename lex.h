#ifndef _LEX_H
#define _LEX_H

// source
struct source {
    unsigned line;
    unsigned column;
    const char *file;
};

// input.c
#define MAX_UNREADC  8

struct cc_char {
    bool dirty:1;
    int ch:16;
    unsigned line;
    unsigned column;
};

struct file {
    int kind:3;
    bool bol:1;                // beginning of line
    bool stub:1;
    int histp:8;
    int charp:8;
    char *buf;
    char *pc;
    char *pe;
    long bread;
    FILE *fp;                // FILE handle
    size_t pos;                // input string position
    const char *file;        // file name or input string
    const char *name;        // buffer name
    unsigned line;
    unsigned column;
    struct vector *ifstubs;
    struct cc_char hists[MAX_UNREADC + 1];        // readc history
    struct cc_char chars[MAX_UNREADC];        // readc ungets
    struct vector *buffer;        // lex ungets
    struct vector *tokens;        // parser ungets
};

struct ifstub {
    int id:10;
    bool b:1;
    struct source src;
};

extern void input_init(const char *file);
extern int readc(void);
extern void unreadc(int c);

extern struct file *with_string(const char *input, const char *name);
extern struct file *with_file(const char *file, const char *name);
extern struct file *with_buffer(struct vector *v);

extern void file_sentinel(struct file *f);
extern void file_unsentinel(void);
extern void file_stub(struct file *f);
extern void file_unstub(void);

extern bool is_original_file(const char *file);
extern struct file *current_file(void);

extern void if_sentinel(struct ifstub *i);
extern void if_unsentinel(void);
extern struct ifstub *new_ifstub(struct ifstub *i);
extern struct ifstub *current_ifstub(void);

enum {
#define _a(a, b, c)     a,
#define _x(a, b, c, d)  a=d,
#define _t(a, b, c)     a,
#define _k(a, b, c)     a,
#include "token.def"
    TOKEND
};

#define ID_BITS    10

// token
struct token {
    int id:ID_BITS;
    int kind:ID_BITS;
    bool bol:1;                // beginning of line
    bool space:1;                // leading space
    const char *name;
    struct source src;
    struct hideset *hideset;
};

// cpp.c
// macro kind
enum {
    MACRO_OBJ,
    MACRO_FUNC,
    MACRO_SPECIAL
};

struct macro {
    int kind:3;
    bool vararg:1;
    bool builtin:1;
    struct vector *body;
    struct vector *params;
    void (*handler) (struct token *);        // special macro handler
    struct source src;
};

extern void cpp_init(struct vector *options);
extern struct token *get_pptok(void);

// lex.c
extern struct source source;
extern struct token *token;
extern struct token *ahead_token;
extern struct token *newline_token;
extern struct token *space_token;

extern int isletter(int c);
extern int isxalpha(int c);

#define IS_SPACE(t)    (((struct token *)(t))->id == ' ')
#define IS_NEWLINE(t)  (((struct token *)(t))->id == '\n')
#define IS_LINENO(t)   (((struct token *)(t))->id == LINENO)

extern struct token *lex(void);
extern void unget(struct token *t);
extern struct token *header_name(void);
extern struct token *new_token(struct token *tok);
extern void skip_ifstub(void);

extern int gettok(void);
extern struct token *lookahead(void);
extern void expect(int t);
extern void match(int t, int follow[]);
extern int skipto(int (*test[]) (struct token *));
extern const char *id2s(int t);
extern const char *unwrap_scon(const char *name);

extern void print_buffer_stat(void);

#define FARRAY(...)  ((int (*[]) (struct token *)){__VA_ARGS__, NULL})

#endif
