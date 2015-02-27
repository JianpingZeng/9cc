#ifndef cc_sym_h
#define cc_sym_h

// scope level
enum {
    CONSTANT,
    GLOBAL,
    PARAM,
    LOCAL,
};

struct symbol {
    int scope;
    const char *name;
    union value value;
    struct type  *type;
    struct source src;
    unsigned refs;
    struct symbol *up;
};

// sym
extern void init_symbol();
extern int scopelevel();
extern void enter_scope();
extern void exit_scope();
extern struct table * new_table(struct table *up, int scope);

// create an anonymous symbol
extern struct symbol * anonymous_symbol(struct table **tpp, int scope);

// look up a symbol only in current table
extern struct symbol * locate_symbol(const char *name , struct table *table);

// look up a symbol from this table to previous one, and so on
extern struct symbol * lookup_symbol(const char *name, struct table *table);
extern struct symbol * install_symbol(const char *name, struct table **tpp, int scope);
extern struct symbol * find_symbol(const char *name, struct table **tpp, int scope);

extern struct table * identifiers;
extern struct table * constants;
extern struct table * records;

#define SCOPE  scopelevel()

#endif
