#ifndef cc_sym_h
#define cc_sym_h

// scope level
enum {
    GLOBAL,
    PARAM,
    LOCAL,
};

struct symbol {
    int scope;
    struct token *token;
    struct type  *type;
    struct symbol *up;
};

struct table {
    int     scope;
    struct table   *prev;
    struct sentry {
        struct symbol  *sym;
        struct sentry *next;
    } *buckets[256];
    struct symbol *all;
};

// sym
extern int scopelevel();
extern void enterscope();
extern void exitscope();
extern struct table * table(struct table *tp, int scope);
extern struct symbol * lookupsym(const char *name, struct table *tp);
extern struct symbol * installsym(const char *name, struct table **tpp, int scope);

extern struct table * identifiers;

#endif
