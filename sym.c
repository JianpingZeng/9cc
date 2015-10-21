#include "cc.h"

static void warning_unused(void);

struct table *identifiers;
struct table *constants;
struct table *tags;

static int level = GLOBAL;

static struct table * new_table(struct table *up, int scope)
{
    struct table *t = zmalloc(sizeof(struct table));
    t->up = up;
    t->scope = scope;
    t->dict = dict_new();
    return t;
}

static void free_table(struct table *t)
{
    dict_free(t->dict);
    free(t);
}

void symbol_init(void)
{
    identifiers = new_table(NULL, GLOBAL);
    constants = new_table(NULL, CONSTANT);
    tags = new_table(NULL, GLOBAL);
}

int scopelevel(void)
{
    return level;
}

void enter_scope(void)
{
    level++;
}

void exit_scope(void)
{
    if (tags->scope == level) {
	struct table *up = tags->up;
	free_table(tags);
        tags = up;
    }
    if (identifiers->scope == level) {
	if (errors == 0 && level >= LOCAL)
	    warning_unused();
	struct table *up = identifiers->up;
	free_table(identifiers);
        identifiers = up;
    }
    cc_assert(level >= GLOBAL);
    level--;
}

node_t * anonymous(struct table **tpp, int scope)
{
    static long i;
    return install(strs(format("@%ld", i++)), tpp, scope);
}

node_t * lookup(const char *name, struct table *table)
{
    cc_assert(name);
    node_t *s = NULL;
    
    for (struct table *t = table; t; t = t->up) {
        if ((s = dict_get(t->dict, name)))
            return s;
    }

    return s;
}

node_t * install(const char *name, struct table **tpp, int scope)
{
    node_t *sym;
    struct table *tp = *tpp;
    
    if (scope > tp->scope) {
        tp = *tpp = new_table(tp, scope);
    } else {
        while (scope != tp->scope)
            tp = tp->up;
    }
    
    cc_assert(tp);
        
    sym = alloc_symbol();
    SYM_SCOPE(sym) = scope;
    SYM_NAME(sym) = name;
    dict_put(tp->dict, name, sym);

    return sym;
}

static int compare(const void *val1, const void *val2)
{
    node_t *sym1 = (node_t *)val1;
    node_t *sym2 = (node_t *)val2;
    struct source src1 = AST_SRC(sym1);
    struct source src2 = AST_SRC(sym2);
    if (src1.line != src2.line)
	return src1.line < src2.line;
    return src1.column < src2.column;
}

static void warning_unused(void)
{
    node_t **syms = (node_t **)dict_allvalues(identifiers->dict);
    qsort(syms, LIST_LEN(syms), sizeof(node_t *), compare);
    for (int i = 0; i < LIST_LEN(syms); i++) {
	node_t *sym = syms[i];
	if (!isanonymous(SYM_NAME(sym)) && !SYM_PREDEFINE(sym) && SYM_REFS(sym) == 0)
	    warningf(AST_SRC(sym), "unused variable '%s'", SYM_NAME(sym));
    }
}
