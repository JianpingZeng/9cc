#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <ctype.h>
#include "7burg.h"

/*
 The following symbols must be defined in the prologue section
 of the specification file by client:

 TREE_TYPE: the tree type, for example:
	struct tree {
            int op;
	    struct tree *kids[2];
            void *state;
            // other fields ...
	};
	typedef struct tree TREE_TYPE;

 LEFT_KID(p): left kid of tree 'p'

 RIGHT_KID(p): right kid of tree 'p'

 TREE_OP(p): op of tree 'p'

 TREE_STATE(p): state handle of tree 'p'

 STATE_TYPE: state handle type, default is:
	#define STATE_TYPE long

 ?_new(size): allocate and zero 'size' bytes, default is:
	#define ?_new(size) memset(malloc(size), 0, (size))

 NOTE: '?' standards for the prefix, default is "burg".
*/

#define FNV32_BASIS ((unsigned int) 0x811c9dc5)
#define FNV32_PRIME ((unsigned int) 0x01000193)
#define ARRAY_SIZE(array)  (sizeof (array)/sizeof (array)[0])
#define NEWS(st)  malloc(sizeof (st))
#define NEWS0(st)  memset(NEWS(st), 0, sizeof (st))
#define NEWARRAY(size, count)  calloc(count, size)
#define kTREE_TYPE    "TREE_TYPE"
#define kTREE_OP      "TREE_OP"
#define kLEFT_KID     "LEFT_KID"
#define kRIGHT_KID    "RIGHT_KID"
#define kSTATE_TYPE   "STATE_TYPE"
#define kTREE_STATE   "TREE_STATE"
#define MAX_COST      SHRT_MAX
#define kPREFIX       prefix

// terminal table
struct entry {
    struct tok *t;
    struct entry *link;
};

static char *prefix = "burg";
static int trace;
static struct entry *tokens[509]; // prime
static struct nonterm *start;
static unsigned int rules_cnt;     // count of rules
static struct rule *rules;         // all rules
static unsigned int nts_cnt;       // count of nonterms
static struct nonterm *nts;        // all nonterms
static unsigned int ts_cnt;        // count of terms
static struct term *ts;            // all terms

static void fprint(FILE *fp, const char *fmt, ...);

static void vfprint(FILE *fp, const char *fmt, va_list ap)
{
    for (; *fmt; fmt++) {
        if (*fmt == '%') {
            switch (*++fmt) {
            case 'd':
                fprintf(fp, "%d", va_arg(ap, int));
                break;
            case 'u':
                fprintf(fp, "%u", va_arg(ap, unsigned int));
                break;
            case 'x':
                fprintf(fp, "%x", va_arg(ap, int));
                break;
            case 'X':
                fprintf(fp, "%X", va_arg(ap, int));
                break;
            case 's':
                fputs(va_arg(ap, char *), fp);
                break;
            case 'p':
                fprintf(fp, "%p", va_arg(ap, void *));
                break;
                /// customize
                // tree pattern
            case 'P':
                {
                    struct pattern *p = va_arg(ap, struct pattern *);
                    fprint(fp, "%K", p->t);
                    if (p->left && p->right)
                        fprint(fp, "(%P, %P)", p->left, p->right);
                    else if (p->left)
                        fprint(fp, "(%P)", p->left);
                }
                break;
                // rule
            case 'R':
                {
                    struct rule *r = va_arg(ap, struct rule *);
                    fprint(fp, "%K: %P", r->nterm, r->pattern);
                }
                break;
                // tok
            case 'K':
                {
                    struct tok *t = va_arg(ap, struct tok *);
                    fputs(t->name, fp);
                }
                break;
                // prefix
            case '?':
                fputs(kPREFIX, fp);
                break;
                // tab indent
            case '1': case '2': case '3': case '4': case '5':
            case '6': case '7': case '8': case '9':
                {
                    int n = *fmt - '0';
                    while (n-- > 0)
                        putc('\t', fp);
                }
                break;
            default:
                putc(*fmt, fp);
                break;
            }
        } else {
            putc(*fmt, fp);
        }
    }
}

static void fprint(FILE *fp, const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    vfprint(fp, fmt, ap);
    va_end(ap);
}

static void print(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    vfprint(stdout, fmt, ap);
    va_end(ap);
}

static void fatal(const char *fmt, ...)
{
    va_list ap;
    
    va_start(ap, fmt);
    fprintf(stderr, "error: ");
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    va_end(ap);
    exit(EXIT_FAILURE);
}

char *xstrdup(const char *s)
{
    return strcpy(malloc(strlen(s) + 1), s);
}

char *xstrndup(const char *s, size_t n)
{
    char *d = malloc(n + 1);
    strncpy(d, s, n);
    d[n] = '\0';
    return d;
}

// 'i' can be represented in 'n' bits.
static int bits(int i)
{
    int n = 1;
    while ((i >>= 1))
        n++;
    return n;
}

static char *format(char *fmt, ...)
{
    va_list ap;
    char buf[512];

    va_start(ap, fmt);
    vsnprintf(buf, ARRAY_SIZE(buf), fmt, ap);
    va_end(ap);
    return xstrdup(buf);
}

// FNV-1a
static unsigned int strhash(const char *s)
{
    unsigned int hash = FNV32_BASIS;
    for (; *s; s++) {
        hash ^= *s;
        hash *= FNV32_PRIME;
    }
    return hash;
}

static struct tok *install(char *name, int kind)
{
    unsigned int hash;
    struct entry *entry;
    struct tok *tok;

    hash = strhash(name) % ARRAY_SIZE(tokens);

    if (kind == kTERM)
        tok = NEWS0(struct term);
    else if (kind == kNONTERM)
        tok = NEWS0(struct nonterm);
    else
        assert(0 && "illegal kind");
    tok->kind = kind;
    tok->name = name;
    
    entry = NEWS(struct entry);
    entry->t = tok;
    entry->link = tokens[hash];
    tokens[hash] = entry;

    return tok;
}

static struct tok *lookup(char *name)
{
    unsigned int hash;
    struct entry *entry;

    hash = strhash(name) % ARRAY_SIZE(tokens);
    for (entry = tokens[hash]; entry; entry = entry->link)
        if (!strcmp(name, entry->t->name))
            return entry->t;

    return NULL;
}

struct nonterm *nonterm(char *name)
{
    static struct nonterm **pnts = &nts;
    struct nonterm *nt;

    nt = (struct nonterm *)lookup(name);
    if (nt) {
        if (nt->t.kind != kNONTERM)
            yyerror("corrupted nonterm: '%s'", name);
        return nt;
    }

    nt = (struct nonterm *)install(name, kNONTERM);
    nt->num = ++nts_cnt;

    // start symbol
    if (nt->num == 1)
        start = nt;

    // link to all nonterms
    *pnts = nt;
    pnts = &nt->all;

    return nt;
}

struct term *term(char *name, int val)
{
    struct term *term;
    struct term **p;

    term = (struct term *)lookup(name);
    if (term)
        yyerror("redefinition of terminal '%s'", name);

    term = (struct term *)install(name, kTERM);
    term->op = val;
    term->nkids = -1;
    ts_cnt++;

    // link
    for (p = &ts; *p && (*p)->op < term->op; p = &(*p)->all)
        /* continue next */ ;

    term->all = *p;
    *p = term;
    
    return term;
}

struct pattern *pattern(char *name, struct pattern *l, struct pattern *r)
{
    struct pattern *p = NEWS0(struct pattern);
    struct tok *t = lookup(name);
    int nkids;

    if (l && r)
        nkids = 2;
    else if (l)
        nkids = 1;
    else
        nkids = 0;

    if (t == NULL && nkids == 0)
        t = (struct tok *)nonterm(name);
    else if (t == NULL && nkids > 0)
        yyerror("undefined terminal '%s'", name);
    else if (t && t->kind == kNONTERM && nkids > 0)
        yyerror("non-terminal has kids");

    if (t->kind == kTERM && ((struct term *)t)->nkids == -1)
        ((struct term *)t)->nkids = nkids;
    else if (t->kind == kTERM && ((struct term *)t)->nkids != nkids)
        yyerror("inconsistent kids in termial '%s' (%d != %d)",
                name, ((struct term *)t)->nkids, nkids);
    
    p->t = t;
    p->left = l;
    p->right = r;
    // count terms in the pattern
    p->nterms = t->kind == kTERM;
    if (p->left)
        p->nterms += p->left->nterms;
    if (p->right)
        p->nterms += p->right->nterms;
    return p;
}

// cost may be code or digits.
void rule(char *name, struct pattern *pattern, char *template, char *cost)
{
    static struct rule **prules = &rules;
    struct nonterm *nt;
    struct rule *rule;
    struct rule **p;
    char *endptr;

    nt = nonterm(name);
    
    rule = NEWS0(struct rule);
    rule->nterm = nt;
    rule->pattern = pattern;
    rule->template = template;
    rule->code = cost;
    rule->cost = strtol(cost, &endptr, 10);
    if (*endptr) {
        // invalid
        rule->cost = -1;
        rule->code = format("(%s)", cost);
    }
    rule->num = ++rules_cnt;
    rule->innum = ++nt->nrules;

    // add rule to nlink
    for (p = &nt->rules; *p && (*p)->num < rule->num; p = &(*p)->link)
        /* continue next */ ;

    rule->link = *p;
    *p = rule;

    // add rule to tlink if possible
    if (pattern->t->kind == kTERM) {
        struct term *term = (struct term *)pattern->t;
        rule->tlink = term->tlink;
        term->tlink = rule;
    } else if (pattern->left == NULL && pattern->right == NULL) {
        struct nonterm *nterm = (struct nonterm *)pattern->t;
        rule->nlink = nterm->nlink;
        nterm->nlink = rule;
    }

    // link to all rules
    *prules = rule;
    prules = &rule->all;
}

// See also: compute_kids
static char *compute_nts(struct pattern *p, char *bp, int *j)
{
    if (p->t->kind == kTERM) {
        if (p->left)
            bp = compute_nts(p->left, bp, j);
        if (p->right)
            bp = compute_nts(p->right, bp, j);
    } else {
        sprintf(bp, "%s_%s_NT, ", kPREFIX, p->t->name);
        bp += strlen(bp);
        (*j)++;
    }
    return bp;
}

// See also: emit_func_nts_kids
static void emit_var_nts(void)
{
    int i, j, max = 0;
    struct rule *r;
    int *nts = NEWARRAY(sizeof(int), rules_cnt);
    char **str = NEWARRAY(sizeof(char *), rules_cnt);

    for (i = 0, r = rules; r; r = r->all, i++) {
        char buf[1024];
        j = 0;
        *compute_nts(r->pattern, buf, &j) = 0;
        max = j > max ? j : max;
        // lookup
        for (j = 0; str[j] && strcmp(str[j], buf); j++)
            /* continue next */ ;
        if (str[j] == NULL) {
            // if _NOT_ found
            // static short ?_nts_j[] = { buf, 0 };
            print("static short %?_nts_%d[] = { %s0 };\n", j, buf);
            str[j] = xstrdup(buf);
        }
        nts[i] = j;
    }
    // static short *?_nts[] = {
    print("\nstatic short *%?_nts[] = {\n");
    print("%10,\n");
    for (i = 0, r = rules; r; r = r->all, i++)
        print("%1%?_nts_%d, /* %d. %R */\n", nts[i], r->num, r);
    // }
    print("};\n");
    print("#define %?_max_nts %d\n\n", max);
}

static void emit_var_nt_names(void)
{
    print("// indexed by %?_xxx_NT\n");
    print("static const char *%?_nt_names[] = {\n");
    print("%10,\n");
    for (struct nonterm *nt = nts; nt; nt = nt->all)
        print("%1\"%K\",\n", nt);
    print("%10,\n");
    print("};\n\n");
}

static void emit_var_rule_names(void)
{
    print("// rule strings\n");
    print("static const char *%?_rule_names[] = {\n%10,\n");
    for (struct rule *r = rules; r; r = r->all)
        print("%1/* %d */ \"%R\",\n", r->num, r);
    print("};\n\n");
}

static void emit_var_nt_rules(void)
{
    print("// nonterm rule numbers (indexed by inner ruleno)\n");
    for (struct nonterm *nt = nts; nt; nt = nt->all) {
        print("static short %?_%K_rules[] = {\n", nt);
        print("%10,\n");
        for (struct rule *rule = nt->rules; rule; rule = rule->link)
            print("%1%d, /* %R */\n", rule->num, rule);
        print("};\n\n");
    }
}

static void emit_var_templates(void)
{
    print("static const char *%?_rule_templates[] = {\n");
    print("%1\"\",\n");
    for (struct rule *rule = rules; rule; rule = rule->all) {
        if (rule->template)
            print("%1/* %d */ \"%s\",\n", rule->num, rule->template);
        else
            print("%1/* %d */ \"\",\n", rule->num);
    }
    print("};\n\n");
}

// See also: compute_nts
static char *compute_kids(struct pattern *p, char *sub, char *bp, int *idx)
{
    if (p->t->kind == kTERM) {
        if (p->left)
            bp = compute_kids(p->left, format("%s(%s)", kLEFT_KID, sub), bp, idx);
        if (p->right)
            bp = compute_kids(p->right, format("%s(%s)", kRIGHT_KID, sub), bp, idx);
    } else {
        sprintf(bp, "\t\tkids[%d] = %s;\n", (*idx)++, sub);
        bp += strlen(bp);
    }
    return bp;
}

// the kids matched with ?_nts_j[]
// See also: emit_var_nts
static void emit_func_nts_kids(void)
{
    int i;
    struct rule *r;
    int *nts = NEWARRAY(sizeof(int), rules_cnt);
    char **str = NEWARRAY(sizeof(char *), rules_cnt);
    
    // static TREE_TYPE **?_nts_kids(TREE_TYPE *p, int ruleno, TREE_TYPE *kids[])
    print("static %s **%?_nts_kids(%s *p, int ruleno, %s *kids[])\n",
          kTREE_TYPE, kTREE_TYPE, kTREE_TYPE);
    print("{\n");
    // assert(p && "null tree");
    print("%1assert(p && \"%s\");\n", "null tree");
    // assert(kids && "null kids for writing");
    print("%1assert(kids && \"%s\");\n", "null kids for writing");
    // switch (ruleno) {
    print("%1switch (ruleno) {\n");
    // cases
    for (i = 0, r = rules; r; r = r->all, i++) {
        char buf[1024];
        int j = 0;
        *compute_kids(r->pattern, "p", buf, &j) = 0;
        // lookup
        for (j = 0; str[j] && strcmp(str[j], buf); j++)
            /* continue next */ ;
        if (str[j] == NULL)
            str[j] = xstrdup(buf);
        nts[i] = j;
    }
    for (int j = 0; str[j] && j < rules_cnt; j++) {
        for (i = 0, r = rules; r; r = r->all, i++)
            if (nts[i] == j)
                print("%1case %d: /* %R */\n", r->num, r);
        print("%s%2break;\n", str[j]);
    }
    // default
    print("%1default:\n");
    print("%2assert(0 && \"%s\");\n", "illegal rule number");
    // end switch
    print("%1}\n");
    print("%1return kids;\n");
    print("}\n\n");
}

static void emit_func_rule(void)
{
    // static int ?_rule(void *state, int nt_kind)
    print("static int %?_rule(void *state, int nt_kind)\n");
    print("{\n");
    // if (!state) return 0;
    print("%1if (!state)\n");
    print("%2return 0;\n");
    // switch (nt_kind)
    print("%1switch (nt_kind) {\n");
    // cases
    for (struct nonterm *nt = nts; nt; nt = nt->all) {
        // case ?_xx_NT: /* xx */
        print("%1case %?_%K_NT: /* %d */\n", nt, nt->num);
        // return ?_xx_rules[((struct ?_state *)state)->rule.xx];
        print("%2return %?_%K_rules[((struct %?_state *)state)->rule.%K];\n",
              nt, nt);
    }
    // default:
    //   assert(0 && "nt index overflow");
    print("%1default:\n");
    print("%2assert(0 && \"%s\");\n", "nt index overflow");
    // end switch
    print("%1}\n");
    print("}\n\n");
}

static void emit_closure_part(char *tabs, struct rule *r, char *c, int cost)
{
    // call trace function
    // void ?_trace(TREE_TYPE *t, int ruleno, int cost, int bestcost);
    if (trace)
        print("%s%?_trace(t, %d, %s + %d, p->costs[%?_%K_NT]);\n",
              tabs, r->num, c, cost, r->nterm);

    // if (c + r->cost < p->costs[?_xx_NT]) {
    print("%sif (%s + %d < p->costs[%?_%K_NT]) {\n",
          tabs, c, cost, r->nterm);
    // p->costs[?_xx_NT] = c + r->cost;
    print("%s%1p->costs[%?_%K_NT] = %s + %d;\n",
          tabs, r->nterm, c, cost);
    // p->rule.xx = r->innum;
    print("%s%1p->rule.%K = %d;\n",
          tabs, r->nterm, r->innum);
    // ?_closure_xx(t, c + r->cost);
    if (r->nterm->nlink)
        print("%s%1%?_closure_%K(t, %s + %d);\n",
              tabs, r->nterm, c, cost);
    // }
    print("%s}\n", tabs);
}

static void emit_func_closure(struct nonterm *nt)
{
    assert(nt->nlink);
    
    // static void ?_closure_xx(TREE_TYPE *t, int c)
    print("static void %?_closure_%K(%s *t, int c)\n",
          nt, kTREE_TYPE);
    // {
    print("{\n");
    // struct ?_state *p = (struct ?_state *)TREE_STATE(t);
    print("%1struct %?_state *p = (struct %?_state *)%s(t);\n",
          kTREE_STATE);
    for (struct rule *r = nt->nlink; r; r = r->nlink) {
        print("%1/* %d. %R */\n", r->num, r);
        if (r->cost == -1) {
            print("%1c += %s;\n", r->code);
            emit_closure_part("\t", r, "c", 0);
        } else {
            emit_closure_part("\t", r, "c", r->cost);
        }
    }
    // }
    print("}\n\n");
}

// emit the matched condition
static void emit_cond(struct pattern *p, char *var, char *suffix)
{
    if (p->t->kind == kTERM) {
        struct term *t = (struct term *)p->t;
        print("%3%s(%s) == %d%s/* %K */",
              kTREE_OP, var, t->op, p->nterms > 1 ? " && " : suffix, t);

        if (p->left)
            emit_cond(p->left, format("%s(%s)", kLEFT_KID, var),
                      p->right && p->right->nterms ? " && " : suffix);
        if (p->right)
            emit_cond(p->right, format("%s(%s)", kRIGHT_KID, var), suffix);
    }
}

static void emit_cost(struct pattern *p, char *var)
{
    if (p->t->kind == kTERM) {
        if (p->left)
            emit_cost(p->left, format("%s(%s)", kLEFT_KID, var));
        if (p->right)
            emit_cost(p->right, format("%s(%s)", kRIGHT_KID, var));
    } else {
        print("((struct %?_state *)(%s(%s)))->costs[%?_%K_NT] + ",
              kTREE_STATE, var, p->t);
    }
}

static void emit_case(struct term *t)
{
    // case op: /* opname */
    print("%1case %d: /* %K */\n", t->op, t);
    switch (t->nkids) {
    case 0:
    case -1:
        // do nothing
        // terminals never appeared in a pattern
        // would have the default nkids -1.
        break;
    case 1:
        print("%2assert(l);\n");
        print("%2%?_label(l);\n");
        break;
    case 2:
        print("%2assert(l && r);\n");
        print("%2%?_label(l);\n");
        print("%2%?_label(r);\n");
        break;
    default:
        assert(0 && "illegal nkids");
    }
    // walk terminal links
    for (struct rule *r = t->tlink; r; r = r->tlink) {
        char *tabs = "\t\t";
        print("%2/* %d. %R */\n", r->num, r);
        switch (t->nkids) {
        case 0:
        case -1:
            if (r->cost == -1) {
                print("%2c = %s;\n", r->code);
                emit_closure_part(tabs, r, "c", 0);
            } else {
                emit_closure_part(tabs, r, r->code, 0);
            }
            break;
        case 1:
            if (r->pattern->nterms > 1) {
                // sub-tree pattern is a terminal
                print("%2if (\n");
                emit_cond(r->pattern->left, "l", " ");
                print("\n%2) {\n");
                print("%3c = ");
                tabs = "\t\t\t";
            } else {
                print("%2c = ");
            }
            emit_cost(r->pattern->left, "l");
            print("%s;\n", r->code);
            emit_closure_part(tabs, r, "c", 0);
            if (tabs[2])        // end if
                print("%2}\n");
            break;
        case 2:
            if (r->pattern->nterms > 1) {
                // sub-tree patterns have terminal
                print("%2if (\n");
                emit_cond(r->pattern->left, "l",
                          r->pattern->right->nterms ? " && " : " ");
                emit_cond(r->pattern->right, "r", " ");
                print("\n%2) {\n");
                print("%3c = ");
                tabs = "\t\t\t";
            } else {
                print("%2c = ");
            }
            emit_cost(r->pattern->left, "l");
            emit_cost(r->pattern->right, "r");
            print("%s;\n", r->code);
            emit_closure_part(tabs, r, "c", 0);
            if (tabs[2])        // end if
                print("%2}\n");
            break;
        default:
            assert(0 && "illegal nkids");
        }
    }
    print("%2break;\n");
}

static void emit_func_label(void)
{
    // static void ?_label(TREE_TYPE *t)
    print("static void %?_label(%s *t)\n", kTREE_TYPE);
    print("{\n");

    // int c;
    print("%1int c;\n");
    // TREE_TYPE *l, *r;
    print("%1%s *l, *r;\n", kTREE_TYPE);
    // struct ?_state *p;
    print("%1struct %?_state *p;\n\n");
    // assert(t && "null tree")
    print("%1assert(t && \"%s\");\n\n", "null tree");
    // l = LEFT_KID(t);
    print("%1l = %s(t);\n", kLEFT_KID);
    // r = RIGHT_KID(t);
    print("%1r = %s(t);\n", kRIGHT_KID);
    // TREE_STATE(t) = p = ?_new(sizeof(struct ?_state));
    print("%1%s(t) = p = %?_new(sizeof(struct %?_state));\n\n",
          kTREE_STATE);

   // initialize the cost to max
    for (int i = 1; i <= nts_cnt; i++)
        print("%1p->costs[%d] =\n", i);
    print("%20x%x;\n\n", MAX_COST);

    // switch (TREE_OP(t))
    print("%1switch (%s(t)) {\n", kTREE_OP);
    // cases
    for (struct term *t = ts; t; t = t->all)
        emit_case(t);
    // default:
    //   assert(0 && "unknown op in label");
    print("%1default:\n");
    print("%2assert(0 && \"%s\");\n", "unknown op in label");
    // end switch
    print("%1}\n");
    print("}\n\n");
}

static void emit_functions(void)
{
    print("/// functions\n");
    emit_func_rule();
    for (struct nonterm *nt = nts; nt; nt = nt->all)
        if (nt->nlink)          // has closure
            emit_func_closure(nt);
    emit_func_label();
    emit_func_nts_kids();
}

static void emit_forwards(void)
{
    print("/// forwards\n");
    for (struct nonterm *nt = nts; nt; nt = nt->all)
        if (nt->nlink)          // has closure
            // static void ?_closure_xx(TREE_TYPE *t, int c)
            print("static void %?_closure_%K(%s *t, int c);\n",
                  nt, kTREE_TYPE);;
    print("\n");
}

static void emit_variables(void)
{
    print("/// variables\n");
    emit_var_nts();
    emit_var_nt_names();
    emit_var_rule_names();
    emit_var_templates();
    emit_var_nt_rules();
}

static void emit_types(void)
{
    print("/// types\n");
    // burg state
    print("struct %?_state {\n");
    print("%1short costs[%d];\n", nts_cnt+1);
    print("%1// indexed by inner rule number\n");
    print("%1struct {\n");
    for (struct nonterm *nt = nts; nt; nt = nt->all)
        print("%2unsigned int %K: %d;\n", nt, bits(nt->nrules));
    print("%1} rule;\n");
    print("};\n\n");
}

static void emit_macros(void)
{
    print("/// macros\n");
    // STATE_TYPE
    print("#ifndef STATE_TYPE\n");
    print("#define STATE_TYPE long\n");
    print("#endif\n\n");
    // ?_new
    print("#ifndef %?_new\n");
    print("#define %?_new(size) memset(malloc(size), 0, (size))\n");
    print("#endif\n\n");
    // xx_NT
    for (struct nonterm *nt = nts; nt; nt = nt->all)
        print("#define %?_%K_NT %d\n", nt, nt->num);
    print("#define %?_max_nt %d\n", nts_cnt);
    print("\n");
}

static void emit_includes(void)
{
    print("#include <assert.h>\n");
    print("#include <stdlib.h>\n");
    print("#include <string.h>\n");
    print("\n");
}

static void emit_prologue(void)
{
    print("\n/*--------------------------------------.\n"
          "| [BEGIN] code generated automatically. |\n"
          "`---------------------------------------*/\n\n");
}

static void emit_epilogue(void)
{
    print("/*------------------------------------.\n"
          "| [END] code generated automatically. |\n"
          "`-------------------------------------*/\n\n");
}

static void usage(void)
{
    fprintf(stderr,
            "OVERVIEW: %s - A Bottom-Up Tree Rewriting Generator v%s\n\n"
            "USAGE: %s [options] <file>\n\n"
            "OPTIONS:\n"
            "  -o <file>                 Write output to <file>\n"
            "  -prefix <prefix>\n"
            "  -p <prefix>               Using <prefix> as prefix for generated names\n"
            "  -T, -trace                Generate trace funtion calls\n"
            "  -h, --help                Display available options\n"
            "  -v, --version             Display version and options\n",
            "burg", "1.0", "burg");
}

int main(int argc, char *argv[])
{
    char *ifile = NULL;
    char *ofile = NULL;
    int ret;

    for (int i = 1; i < argc; i++) {
        char *arg = argv[i];
        if (!strcmp(arg, "-h") || !strcmp(arg, "--help") ||
            !strcmp(arg, "-v") || !strcmp(arg, "--version")) {
            usage();
            exit(EXIT_FAILURE);
        } else if (!strcmp(arg, "-o")) {
            if (++i >= argc)
                fatal("missing output file while -o specified");
            ofile = argv[i];
        } else if (!strcmp(arg, "-prefix")) {
            if (++i >= argc)
                fatal("missing prefix while -prefix specified");
            if (!isalpha(argv[i][0]) && argv[i][0] != '_')
                fatal("prefix must start with alpha or '_'");
            prefix = argv[i];
        } else if (!strcmp(arg, "-T") || !strcmp(arg, "-trace")) {
            trace = 1;
        } else if (!strcmp(arg, "-")) {
            ifile = NULL;
        } else if (arg[0] == '-') {
            fatal("unknown option '%s' specified", arg);
        } else {
            ifile = arg;
        }
    }

    if (ifile && !freopen(ifile, "r", stdin)) {
        perror("Can't read input file");
        exit(EXIT_FAILURE);
    }
    if (ofile && !freopen(ofile, "w", stdout)) {
        perror("Can't open file for output");
        exit(EXIT_FAILURE);
    }
    
    if ((ret = yyparse()))
        fatal("parser failed with code: %d", ret);

    // check start symbol
    if (!start || !start->rules)
        fatal("missing 'start' rule");

    emit_prologue();
    emit_includes();
    emit_macros();
    emit_types();
    emit_variables();
    emit_forwards();
    emit_functions();
    emit_epilogue();

    // emit text left
    if (!feof(stdin)) {
        int c;
        while ((c = getc(stdin)) != EOF)
            putc(c, stdout);
    }
    
    return 0;
}
