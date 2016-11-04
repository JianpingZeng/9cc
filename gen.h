#ifndef GEN_H
#define GEN_H

// forward declaration
struct expr;

struct xface {
    void (*label) (struct expr *);
    struct expr ** (*kids) (struct expr *, int ruleno, struct expr *kids[]);
    int (*rule) (void *state, int nt_kind);
    const char **rule_names;
    const char **nt_names;
    const char **templates;
    short **nts;
    int max_kids;
    int nts_count;
};

#endif
