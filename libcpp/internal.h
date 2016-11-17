#ifndef CPP_INTERNAL_H
#define CPP_INTERNAL_H

// for bool/size_t
#include <stdbool.h>
#include <stddef.h>
#include "libcpp.h"

///
/// type declarations
///

enum {
    BLANK = 01, NEWLINE = 02, LETTER = 04,
    DIGIT = 010, HEX = 020, OTHER = 040,
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
    unsigned int line, column;
    struct buffer *prev;                 // previous buffer
};

struct ifstack {
    unsigned short id;
    bool b;
    struct source src;
    struct ifstack *prev;
};

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

// ident type
enum { CT_MACRO = 1 };

enum buffer_sentinel_option { BS_CONTINUOUS = 0, BS_RETURN_EOI };


///
/// external functions
///

// input.c
extern struct file *input_init(const char *file);

extern struct buffer *with_string(const char *input, const char *name);
extern struct buffer *with_file(const char *file, const char *name);
extern struct buffer *with_tokens(struct vector *v, struct buffer *cur);

extern void buffer_sentinel(struct file *pfile, struct buffer *pb,
                          enum buffer_sentinel_option opt);
extern void buffer_unsentinel(struct file *pfile);

extern void if_sentinel(struct file *pfile, struct ifstack *i);
extern void if_unsentinel(struct file *pfile);

extern bool is_original_file(struct file *pfile, const char *file);

// cpp.c
extern void unget(struct file *pfile, struct token *t);

// lex.c
#define IS_SPACE(t)    (((struct token *)(t))->id == ' ')
#define IS_NEWLINE(t)  (((struct token *)(t))->id == '\n')
#define IS_LINENO(t)   (((struct token *)(t))->id == LINENO)

struct tokenrun *next_tokenrun(struct tokenrun *prev, unsigned int count);

extern struct token *lex(struct file *pfile);
extern struct token *header_name(struct file *pfile);
extern struct token *alloc_token(void);
extern void skip_ifstack(struct file *pfile);

// strtab.c
extern char *strs(const char *);
extern char *strn(const char *, size_t);
extern char *strd(long);
extern char *stru(unsigned long);

// hideset.c
struct hideset {
    const char *name;
    struct hideset *next;
};

extern struct hideset *hideset_add(struct hideset *, const char *);
extern bool hideset_has(struct hideset *, const char *);
extern struct hideset *hideset_union(struct hideset *,
                                     struct hideset *);
extern struct hideset *hideset_intersection(struct hideset *,
                                            struct hideset *);

// idtab
enum idtab_lookup_option { ID_SEARCH = 0, ID_CREATE };

struct idtab_entry {
    struct ident *ident;
    struct idtab_entry *link;
};

struct idtab {
    struct idtab_entry **table;
    unsigned int nslots;        // number of slots
    unsigned int nelements;     // number of elements
    unsigned int searches;
    unsigned int collisions;
    struct ident * (*alloc_ident) (struct idtab *);
};

extern struct idtab *idtab_new(unsigned int);
extern void idtab_free(struct idtab *);
extern struct ident *idtab_lookup(struct idtab *,
                                  const char *, size_t,
                                  enum idtab_lookup_option);
extern struct ident *idtab_lookup_with_hash(struct idtab *,
                                            const char *, size_t,
                                            unsigned int,
                                            enum idtab_lookup_option);
typedef int (*idtab_cb) (struct idtab *, struct ident *, const void *);
extern void idtab_foreach(struct idtab *, idtab_cb, const void *);
extern void idtab_dump(struct idtab *t);

// error.c
enum { WRN = 1, ERR, FTL };

struct source;

extern void cpp_warning_at(struct source, const char *, ...);
extern void cpp_error_at(struct source, const char *, ...);
extern void cpp_fatal_at(struct source, const char *, ...);

#define SAVE_ERRORS    unsigned int __err = cpp_file->errors
#define NO_ERROR       (__err == cpp_file->errors)
#define HAS_ERROR      (__err != cpp_file->errors)

#define cpp_warning(...)  cpp_warning_at(source, __VA_ARGS__)
#define cpp_error(...)    cpp_error_at(source, __VA_ARGS__)
#define cpp_fatal(...)    cpp_fatal_at(source, __VA_ARGS__)

// expr.c
extern bool eval_cpp_const_expr(void);

// sys.c
extern struct vector *sys_include_dirs(void);

// dump
extern void strtab_dump(void);

///
/// external variables
///

extern struct token *ahead_token;
extern struct token *space_token;

#endif
