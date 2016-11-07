#include <stdlib.h>
#include <assert.h>
#include "cc.h"

#define NBUCKETS  ARRAY_SIZE(((struct table *)0)->buckets)

struct table *identifiers;
struct table *constants;
struct table *tags;
struct table *globals;
struct table *externals;

int cscope = GLOBAL;

struct table *new_table(struct table *up, int scope)
{
    struct table *t = zmalloc(sizeof(struct table));
    t->up = up;
    t->scope = scope;
    return t;
}

void free_table(struct table *t)
{
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

void foreach(struct table *tp, int level,
             void (*apply) (struct symbol *, void *),
             void *context)
{
    struct symbol *p;

    assert(tp);

    while (tp && tp->scope > level)
        tp = tp->up;

    if (tp && tp->scope == level)
        for (p = tp->all; p && p->scope == level; p = p->link)
            apply(p, context);
}

bool is_current_scope(struct symbol *sym)
{
    return sym->scope == cscope || (sym->scope == PARAM && cscope == LOCAL);
}

struct symbol *anonymous(struct table **tpp, int scope, int area)
{
    static long i;
    struct symbol *sym;

    sym = install(format("@%ld", i++), tpp, scope, area);
    sym->anonymous = true;
    return sym;
}

struct symbol *lookup(const char *name, struct table * table)
{
    unsigned int hash;

    assert(name);

    hash = strhash(name) & (NBUCKETS - 1);
    for (struct table *t = table; t; t = t->up)
        for (struct entry *p = t->buckets[hash]; p; p = p->link)
            if (name == p->sym.name)
                return &p->sym;

    return NULL;
}

struct symbol *install(const char *name,
                       struct table ** tpp, int scope, int area)
{
    struct table *tp = *tpp;
    unsigned int hash;
    struct entry *p;

    if (scope > tp->scope) {
        tp = *tpp = new_table(tp, scope);
    } else {
        while (scope != tp->scope)
            tp = tp->up;
    }

    assert(tp);

    // entry
    hash = strhash(name) & (NBUCKETS - 1);
    p = NEWS0(struct entry, area);
    p->sym.name = name;
    p->sym.scope = scope;
    p->link = tp->buckets[hash];
    tp->buckets[hash] = p;
    // all/link
    p->sym.link = tp->all;
    tp->all = &p->sym;

    return &p->sym;
}
