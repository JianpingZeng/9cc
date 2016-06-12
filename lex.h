#ifndef _LEX_H
#define _LEX_H

// source
struct source {
    unsigned int line;
    unsigned int column;
    const char *file;
};

struct line_note {
    const unsigned char *pos;
    int type;
};

// input.c
struct file {
    unsigned char kind;                  // kind (regular/string)
    bool bol;                            // beginning of line
    bool stub;
    bool need_line;
    const char *file;                    // file name
    const char *name;                    // buffer name
    const unsigned char *buf;            // entire buffer
    const unsigned char *cur;            // current position
    const unsigned char *limit;          // end position
    const unsigned char *line_base;      // start of current physical line
    const unsigned char *next_line;      // start of to-be-cleaned logical line
    struct line_note *notes;             // array of notes
    unsigned int cur_note;               // current note
    unsigned int notes_used;             // number of notes
    unsigned int notes_alloc;            // number of notes allocated
    struct vector *ifstubs;
    struct vector *buffer;               // lex ungets
    struct vector *tokens;               // parser ungets
    unsigned line, column;
};

struct ifstub {
    int id:10;
    bool b;
    struct source src;
};

extern void input_init(const char *file);

extern struct file *with_string(const char *input, const char *name);
extern struct file *with_file(const char *file, const char *name);
extern struct file *with_buffer(struct vector *v);

extern void file_sentinel(struct file *f);
extern void file_unsentinel(void);
extern void file_stub(struct file *f);
extern void file_unstub(void);

extern bool is_original_file(const char *file);
extern struct file *current_file;

extern void if_sentinel(struct ifstub *i);
extern void if_unsentinel(void);
extern struct ifstub *new_ifstub(struct ifstub *i);
extern struct ifstub *current_ifstub(void);

enum {
#define _a(a, b, c, d)  a,
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

extern int gettok(void);
extern struct token *lookahead(void);
extern void expect(int t);
extern void match(int t, int follow[]);
extern int skipto(int (*test[]) (struct token *));
extern const char *unwrap_scon(const char *name);

// lex.c
#define MARK(t)  source = t->src
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

extern struct token *lex(struct file *fs);
extern void unget(struct file *fs, struct token *t);
extern struct token *header_name(struct file *fs);
extern struct token *new_token(struct token *tok);
extern void skip_ifstub(struct file *fs);
extern const char *id2s(int t);

#define FARRAY(...)  ((int (*[]) (struct token *)){__VA_ARGS__, NULL})

#endif
