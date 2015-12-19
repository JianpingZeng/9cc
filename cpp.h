#ifndef _CPP_H
#define _CPP_H

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
extern struct file *current_file(void);

extern void if_sentinel(struct ifstub *i);
extern void if_unsentinel(void);
extern struct ifstub *new_ifstub(struct ifstub *i);
extern struct ifstub *current_ifstub(void);

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
extern struct vector *all_pptoks(void);

#endif
