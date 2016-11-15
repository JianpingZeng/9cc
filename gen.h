#ifndef GEN_H
#define GEN_H

// forward declaration
struct tree;

struct xinterface {
    void (*label) (struct tree *);
    struct tree ** (*nt_kids) (struct tree *, int, struct tree *[]);
    int (*rule) (void *, int);
    const char **rule_names;
    const char **nt_names;
    const char **templates;
    short **nts;
    int max_kids;
    int nts_count;
};

struct xsymbol {
    const char *name;
    int label;                  // for goto labels
    int framesize;
    struct {
        const char *alias[4];
        int mask;
        int index;
        char kind;
        bool preserved;
    } reg;
};

enum { IREG, FREG };
enum { B, W, L, Q };
enum { Zero = 0, Byte = 1, Word = 2, Long = 4, Quad = 8 };

extern struct symbol *mkreg(const char *, int, int);
extern struct symbol *mksreg(const char *);
extern void gen(struct symbol *s);
extern void emit(struct symbol *s);

#define reg_alias(s, i, name)                           \
    do { (s)->x.reg.alias[i] = name; } while (0)

#define reg_preserved(s)                                \
    do { (s)->x.reg.preserved = true; } while (0)

#endif
