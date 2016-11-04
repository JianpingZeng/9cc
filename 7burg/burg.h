#ifndef BURG_H
#define BURG_H

enum { kTERM, kNONTERM };
struct tok {
    char *name;
    int kind;
};
struct term {
    struct tok t;
    int op;
    int nkids;
    struct rule *tlink;         // rules starts with term
    struct term *all;
};
struct nonterm {
    struct tok t;
    int num;
    unsigned int nrules;
    struct rule *rules;         // rules with the same lhs
    struct rule *nlink;         // rules with the same rhs
    struct nonterm *all;
};
// tree patterns
struct pattern {
    struct tok *t;              // a term or nonterm
    struct pattern *left, *right;
    int nterms;                 // number of terms
};
struct rule {
    struct nonterm *nterm;      // lhs
    struct pattern *pattern;
    char *template;
    char *code;                 // cost code
    int cost;                   // -1 if cost is not integer literal
    int num;                    // external num (in all rules)
    int innum;                  // internal num (in the same nonterm)
    struct rule *tlink;         // next rule with the same root (term)
    struct rule *nlink;         // next rule with the same rhs (nonterm)
    struct rule *link;          // next rule with the same lhs (nonterm)
    struct rule *all;           // next rule in 'num' order
};

extern int yyparse(void);
extern void yyerror(const char *msg, ...);
extern struct nonterm *nonterm(char *name);
extern struct term *term(char *name, int val);
extern struct pattern *pattern(char *name, struct pattern *l, struct pattern *r);
extern void rule(char *name, struct pattern *pattern, char *template, char *cost);

#endif
