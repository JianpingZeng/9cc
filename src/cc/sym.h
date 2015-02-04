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
    const char *name;
    union value value;
    struct type  *type;
    unsigned refs;
    struct symbol *up;
};

// sym
extern int scopelevel();
extern void enter_scope();
extern void exit_scope();
extern struct table * new_table(struct table *up, int scope);
extern struct symbol * lookup_symbol(const char *name, struct table *table);
extern struct symbol * install_symbol(const char *name, struct table **tpp, int scope);

extern struct table * identifiers;
extern struct table * constants;

#endif
