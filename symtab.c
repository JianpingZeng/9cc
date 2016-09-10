#include "cc.h"

struct table *identifiers;
struct table *constants;
struct table *tags;

int _scope = GLOBAL;

struct symbol *alloc_symbol(int area)
{
    return NEWS0(struct symbol, area);
}

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

void enter_scope(void)
{
    _scope++;
}

void exit_scope(void)
{
    if (tags->scope == _scope) {
        struct table *up = tags->up;
        free_table(tags);
        tags = up;
    }
    if (identifiers->scope == _scope) {
        struct table *up = identifiers->up;
        free_table(identifiers);
        identifiers = up;
    }
    assert(_scope >= GLOBAL);
    _scope--;
}

void foreach(struct table *tp, int level, void (*apply) (struct symbol *, void *), void *context)
{
    assert(tp);
    while (tp && tp->scope > level)
        tp = tp->up;
    if (tp && tp->scope == level) {
        for (struct symbol *p = tp->all; p && SYM_SCOPE(p) == level; p = SYM_LINK(p))
            apply(p, context);
    }
}

bool is_current_scope(struct symbol *sym)
{
    return SYM_SCOPE(sym) == SCOPE || (SYM_SCOPE(sym) == PARAM && SCOPE == LOCAL);
}

bool is_anonymous(const char *name)
{
    return name == NULL || !isletter(name[0]);
}

struct symbol *anonymous(struct table **tpp, int scope, int area)
{
    static long i;
    return install(format("@%ld", i++), tpp, scope, area);
}

struct symbol *gen_tmp_sym(int area)
{
    const char *name = gen_tmpname();
    struct symbol *sym = alloc_symbol(area);
    SYM_NAME(sym) = SYM_X_LABEL(sym) = name;
    SYM_REFS(sym)++;
    return sym;
}

struct symbol *lookup(const char *name, struct table * table)
{
    assert(name);
    struct symbol *s = NULL;

    for (struct table * t = table; t; t = t->up) {
        if ((s = map_get(t->map, name)))
            return s;
    }

    return s;
}

struct symbol *install(const char *name, struct table ** tpp, int scope, int area)
{
    struct symbol *sym;
    struct table *tp = *tpp;

    if (scope > tp->scope) {
        tp = *tpp = new_table(tp, scope);
    } else {
        while (scope != tp->scope)
            tp = tp->up;
    }

    assert(tp);

    sym = alloc_symbol(area);
    SYM_SCOPE(sym) = scope;
    SYM_NAME(sym) = name;
    SYM_X_LABEL(sym) = name;
    map_put(tp->map, name, sym);
    // all/link
    SYM_LINK(sym) = tp->all;
    tp->all = sym;

    return sym;
}
