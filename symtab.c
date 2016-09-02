#include "cc.h"

struct table *identifiers;
struct table *constants;
struct table *tags;

static int level = GLOBAL;

struct table *new_table(struct table *up, int scope)
{
    struct table *t = zmalloc(sizeof(struct table));
    t->up = up;
    t->scope = scope;
    t->map = map_new();
    return t;
}

static void free_table(struct table *t)
{
    map_free(t->map);
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
    assert(level >= GLOBAL);
    level--;
}

void foreach(struct table *tp, int level, void (*apply) (node_t *))
{
    assert(tp);
    while (tp && tp->scope > level)
        tp = tp->up;
    if (tp && tp->scope == level) {
        for (node_t *p = tp->all; p && SYM_SCOPE(p) == level; p = SYM_LINK(p))
            apply(p);
    }
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
    return install(format("@%ld", i++), tpp, scope);
}

node_t *gen_tmp_sym(void)
{
    const char *name = gen_tmpname();
    node_t *sym = alloc_symbol();
    SYM_NAME(sym) = SYM_X_LABEL(sym) = name;
    SYM_REFS(sym)++;
    return sym;
}

node_t *lookup(const char *name, struct table * table)
{
    assert(name);
    node_t *s = NULL;

    for (struct table * t = table; t; t = t->up) {
        if ((s = map_get(t->map, name)))
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

    assert(tp);

    sym = alloc_symbol();
    SYM_SCOPE(sym) = scope;
    SYM_NAME(sym) = name;
    SYM_X_LABEL(sym) = name;
    map_put(tp->map, name, sym);
    // all/link
    SYM_LINK(sym) = tp->all;
    tp->all = sym;

    return sym;
}
