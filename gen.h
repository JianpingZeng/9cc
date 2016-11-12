#ifndef GEN_H
#define GEN_H

// forward declaration
struct tree;

struct xface {
    void (*label) (struct tree *);
    struct tree ** (*kids) (struct tree *, int, struct tree *[]);
    int (*rule) (void *, int);
    const char **rule_names;
    const char **nt_names;
    const char **templates;
    short **nts;
    int max_kids;
    int nts_count;
};

#endif
