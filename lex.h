#ifndef _LEX_H
#define _LEX_H

#include "hideset.h"

// source
struct source {
    unsigned int line;
    unsigned int column;
    const char *file;
};

enum {
    BLANK = 01, NEWLINE = 02, LETTER = 04,
    DIGIT = 010, HEX = 020, OTHER = 040,
};

enum {
#define _a(a, b, c, d)  a,
#define _x(a, b, c, d)  a=d,
#define _t(a, b, c)     a,
#define _k(a, b, c)     a,
#include "token.def"
    TOKEND
};

#include "imap.h"

#define ID_BITS    10

// token
struct token {
    int id:ID_BITS;
    int kind:ID_BITS;
    bool bol;                // beginning of line
    bool space;                // leading space
    const char *lexeme;
    struct source src;
    struct hideset *hideset;
};

struct line_note {
    const unsigned char *pos;
    int type;
};

// input.c

// buffer kind
enum { BK_REGULAR = 1, BK_STRING, BK_TOKEN };

// A buffer represents a file's content.
struct buffer {
    int kind:8;                          // kind (regular/string)
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
    struct vector *ungets;               // lex ungets
    unsigned line, column;
    struct buffer *prev;                 // previous buffer
};

// The file read by preprocessor.
struct file {
    const char *file;           // file name
    struct buffer *current;     // current buffer (top of buffer stack)
    struct vector *tokens;      // parser ungets
    struct imap *imap; // identifier hash map
    struct map *macros;
    struct vector *std_include_paths;
    struct vector *usr_include_paths;
    // current timestamp
    struct tm now;
};

extern struct file *cpp_file;

struct ifstub {
    int id:ID_BITS;
    bool b;
    struct source src;
};

extern void input_init(const char *file);

extern struct buffer *with_string(const char *input, const char *name);
extern struct buffer *with_file(const char *file, const char *name);
extern struct buffer *with_tokens(struct vector *v, struct buffer *cur);

enum buffer_sentinel_option { BS_NONE = 0, BS_STUB };

extern void buffer_sentinel(struct file *pfile, struct buffer *pb,
                          enum buffer_sentinel_option opt);
extern void buffer_unsentinel(struct file *pfile);

extern void if_sentinel(struct file *pfile, struct ifstub *i);
extern void if_unsentinel(struct file *pfile);
extern struct ifstub *current_ifstub(struct file *pfile);

extern bool is_original_file(struct file *pfile, const char *file);

// cpp.c
// macro kind
enum {
    MACRO_OBJ,
    MACRO_FUNC,
    MACRO_SPECIAL
};

struct macro {
    int kind:4;
    bool vararg;
    bool builtin;
    struct vector *body;
    struct vector *params;
    // special macro handler
    void (*handler) (struct file *, struct token *);
    struct source src;
};

// cpp_ident type
enum { CT_MACRO = 1 };

struct cpp_ident {
    struct ident id;
    int type:8;
    union {
        struct macro *macro;
    } value;
};

extern void cpp_init(struct file *pfile, struct vector *options);
extern struct token *get_pptok(struct file *pfile);

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
extern struct token *space_token;

extern int isletter(int c);
extern int isxalpha(int c);

#define IS_SPACE(t)    (((struct token *)(t))->id == ' ')
#define IS_NEWLINE(t)  (((struct token *)(t))->id == '\n')
#define IS_LINENO(t)   (((struct token *)(t))->id == LINENO)

extern struct token *lex(struct file *pfile);
extern void unget(struct file *pfile, struct token *t);
extern struct token *header_name(struct file *pfile);
extern struct token *new_token(struct token *tok);
extern void skip_ifstub(struct file *pfile);
extern const char *id2s(int t);

#define FARRAY(...)  ((int (*[]) (struct token *)){__VA_ARGS__, NULL})

#endif
