#include "cc.h"

struct table *identifiers;
struct table *constants;
struct table *tags;

static int level = GLOBAL;

static struct table *new_table(struct table *up, int scope)
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
        struct table *up = identifiers->up;
        free_table(identifiers);
        identifiers = up;
    }
    cc_assert(level >= GLOBAL);
    level--;
}

bool is_current_scope(node_t *sym)
{
    return SYM_SCOPE(sym) == SCOPE || (SYM_SCOPE(sym) == PARAM && SCOPE == LOCAL);
}

bool is_anonymous(const char *name)
{
    return name == NULL || !isletter(name[0]);
}

node_t *anonymous(struct table **tpp, int scope)
{
    static long i;
    return install(strs(format("@%ld", i++)), tpp, scope);
}

node_t *lookup(const char *name, struct table * table)
{
    cc_assert(name);
    node_t *s = NULL;

    for (struct table * t = table; t; t = t->up) {
        if ((s = dict_get(t->dict, name)))
            return s;
    }

    return s;
}

node_t *install(const char *name, struct table ** tpp, int scope)
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
    SYM_X(sym).label = name;
    dict_put(tp->dict, name, sym);

    return sym;
}
