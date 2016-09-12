#ifndef _LEX_H
#define _LEX_H

// for bool/size_t
#include <stdbool.h>
#include <stddef.h>

// hideset.c
struct hideset {
    const unsigned char *name;
    struct hideset *next;
};

extern struct hideset *hideset_add(struct hideset *s, const unsigned char *name);
extern bool hideset_has(struct hideset *s, const unsigned char *name);
extern struct hideset *hideset_union(struct hideset *a, struct hideset *b);
extern struct hideset *hideset_intersection(struct hideset *a,
                                            struct hideset *b);
// imap.c
/*
  An identifier hash map.

  ACKNOWLEDGE:

  imap is modified from cpp_hash_table in GCC.
 */

#define IMAP_HASHSTEP(h, c)  ((h) * 67 + ((c) - 133))
#define IMAP_HASHFINISH(h, len)  ((h) + (len))

enum imap_lookup_option { IMAP_SEARCH = 0, IMAP_CREATE };

// An identifier
struct ident {
    unsigned int hash;
    unsigned int len;
    const unsigned char *str;
};

// An identifier hash table for lexer.
struct imap {
    struct ident **table;
    unsigned int nslots;        // number of slots
    unsigned int nelements;     // number of elements
    // Callback, allocate an entry
    struct ident * (*alloc_entry) (struct imap *);
    // Statistics
    unsigned int searches;
    unsigned int collisions;
};

extern struct imap *imap_new(unsigned int cap);
extern void imap_free(struct imap *imap);
extern struct ident *imap_lookup(struct imap *imap,
                                 const unsigned char *str,
                                 size_t len,
                                 enum imap_lookup_option opt);
extern struct ident *imap_lookup_with_hash(struct imap *imap,
                                           const unsigned char *str,
                                           size_t len,
                                           unsigned int hash,
                                           enum imap_lookup_option opt);
typedef int (*imap_cb) (struct imap *imap, struct ident *id, const void *context);
extern void imap_foreach(struct imap *imap, imap_cb cb, const void *context);
extern void imap_dump(struct imap *imap);

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


// value
union value {
    long long i;
    unsigned long long u;
    float f;
    double d;
    long double ld;
    void *p;
    void (*g) ();
};

// token
#define TOK_ID_STR(t)    ((const char *)(t)->u.ident->str)
#define TOK_LIT_STR(t)  ((t)->u.str)

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
        // string or number
        const char *str;
    } u;
};

// tokens
struct tokenrun {
    struct token *base;
    struct token *limit;
    struct tokenrun *prev;
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
    bool return_eoi;                     // return eoi when reach the end
    bool need_line;
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
    struct ifstack *ifstack;             // top of 'if' stack
    struct vector *ungets;               // lex ungets
    unsigned line, column;
    struct buffer *prev;                 // previous buffer
};

// The file read by preprocessor.
struct file {
    const char *file;           // file name
    struct buffer *buffer;      // current buffer (top of buffer stack)
    struct vector *tokens;      // parser ungets
    struct imap *imap;          // identifier hash map
    struct vector *std_include_paths;
    struct vector *usr_include_paths;
    struct tokenrun *tokenrun;
    struct token *cur_token;
    const char *date;            // current date string (quoted)
    const char *time;            // current time string (quoted)
};

extern struct file *cpp_file;

struct ifstack {
    unsigned short id;
    bool b;
    struct source src;
    struct ifstack *prev;
};

extern struct file *new_cpp_file(const char *file);

extern struct buffer *with_string(const char *input, const char *name);
extern struct buffer *with_file(const char *file);
extern struct buffer *with_tokens(struct vector *v, struct buffer *cur);

enum buffer_sentinel_option { BS_CONTINUOUS = 0, BS_RETURN_EOI };

extern void buffer_sentinel(struct file *pfile, struct buffer *pb,
                          enum buffer_sentinel_option opt);
extern void buffer_unsentinel(struct file *pfile);

extern void if_sentinel(struct file *pfile, struct ifstack *i);
extern void if_unsentinel(struct file *pfile);

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
    bool varg;
    bool builtin;
    unsigned int nparams;
    struct token **params;
    unsigned int nbody;
    struct token **body;
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

extern void cpp_init(int argc, char *argv[]);
extern struct token *get_pptok(struct file *pfile);
extern void unget(struct file *pfile, struct token *t);

// lex.c
extern struct source source;
extern struct token *token;
extern struct token *ahead_token;
extern struct token *space_token;

extern int isletter(int c);
extern int isxalpha(int c);

#define IS_SPACE(t)    (((struct token *)(t))->id == ' ')
#define IS_NEWLINE(t)  (((struct token *)(t))->id == '\n')
#define IS_LINENO(t)   (((struct token *)(t))->id == LINENO)

struct tokenrun *next_tokenrun(struct tokenrun *prev, unsigned int count);

extern struct token *lex(struct file *pfile);
extern struct token *header_name(struct file *pfile);
extern struct token *new_token(struct token *tok);
extern void skip_ifstack(struct file *pfile);
extern const char *id2s(int t);
extern const char *tok2s(struct token *t);

extern int gettok(void);
extern struct token *lookahead(void);
extern void expect(int t);
extern void match(int t, int follow[]);
extern int skipto(int (*test) (struct token *));
extern const char *unwrap_scon(const char *name);

#endif
