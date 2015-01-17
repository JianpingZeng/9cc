#ifndef cc_sym_h
#define cc_sym_h

typedef struct symbol   Symbol;
typedef struct table    Table;

// scope level
enum {
    GLOBAL,
    PARAM,
    LOCAL,
};

struct symbol {
    Token token;
    Type *type;
    int scope;
    Symbol *up;
};

struct table {
    int     scope;
    Table   *prev;
    struct sentry {
        Symbol  *sym;
        struct sentry *next;
    } *buckets[256];
    Symbol *all;
};

// sym
extern void enterscope();
extern void exitscope();
extern Table * table(Table *tp, int scope);
extern Symbol * lookupsym(const char *name, Table *tp);
extern Symbol * installsym(const char *name, Table **tpp, int scope);

extern Table * identifiers;

#endif
