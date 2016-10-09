#include <stdlib.h>
#include <assert.h>
#include "cc.h"

struct table *identifiers;
struct table *constants;
struct table *tags;
struct table *globals;
struct table *externals;

int cscope = GLOBAL;

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

void free_table(struct table *t)
{
    map_free(t->map);
    free(t);
}

void symbol_init(void)
{
    globals = identifiers = new_table(NULL, GLOBAL);
    constants = new_table(NULL, CONSTANT);
    tags = new_table(NULL, GLOBAL);
    externals = new_table(NULL, GLOBAL);
}

void enter_scope(void)
{
    cscope++;
}

void exit_scope(void)
{
    if (tags->scope == cscope) {
        struct table *up = tags->up;
        free_table(tags);
        tags = up;
    }
    if (identifiers->scope == cscope) {
        struct table *up = identifiers->up;
        free_table(identifiers);
        identifiers = up;
    }
    assert(cscope >= GLOBAL);
    cscope--;
}

void foreach(struct table *tp, int level, void (*apply) (struct symbol *, void *), void *context)
{
    assert(tp);
    while (tp && tp->scope > level)
        tp = tp->up;
    if (tp && tp->scope == level) {
        for (struct symbol *p = tp->all; p && p->scope == level; p = p->link)
            apply(p, context);
    }
}

bool is_current_scope(struct symbol *sym)
{
    return sym->scope == cscope || (sym->scope == PARAM && cscope == LOCAL);
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
    sym->scope = scope;
    sym->name = name;
    sym->x.name = name;
    map_put(tp->map, name, sym);
    // all/link
    sym->link = tp->all;
    tp->all = sym;

    return sym;
}
