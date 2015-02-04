#include "cc.h"

#define BUCKET_SIZE 256

struct table {
    int scope;
    struct table *up;
    struct sentry {
        struct symbol *symbol;
        struct sentry *next;
    } *buckets[BUCKET_SIZE];
    struct symbol *all;
};

struct table * identifiers;
static int scope;

int scopelevel()
{
    return scope;
}

void enter_scope()
{
    scope++;
}

void exit_scope()
{
    if (identifiers->scope == scope) {
        struct table *tp = identifiers;
        identifiers = identifiers->up;
        drop_table(tp);
    }
    assert(scopelevel >= GLOBAL);
    scope--;
}

struct table * new_table(struct table *up, int scope)
{
    struct table *t = alloc_table(sizeof(struct table));
    t->up = up;
    t->scope = scope;
    if (up) {
        t->all = up->all;
    }
    return t;
}

static unsigned hash(const char *src)
{
    register unsigned h;
    register unsigned char *p;
    
    for(h = 0, p = (unsigned char *)src; *p ; p++)
        h = 31 * h + *p;
    
    return h;
}

struct symbol * lookup_symbol(const char *name, struct table *table)
{
    assert(table);
    
    for (struct table *t = table; t; t = t->up) {
        unsigned h = hash(name) % BUCKET_SIZE;
        for (struct sentry *entry = t->buckets[h]; entry; entry = entry->next) {
            if (entry->symbol->name == name) {
                return entry->symbol;
            }
        }
    }
    
    return NULL;
}

struct symbol * install_symbol(const char *name, struct table **tpp, int scope)
{
    unsigned h = hash(name) % BUCKET_SIZE;
    struct sentry *entry;
    struct symbol *symbol;
    struct table *tp = *tpp;
    
    assert(scope >= tp->scope);
    if (scope > tp->scope) {
        tp = *tpp = new_table(tp, scope);
    }

    entry = alloc_table_entry(tp, sizeof(struct sentry));
    symbol = alloc_symbol_node();
    symbol->scope = scope;
    symbol->name = strings(name);
    tp->all = symbol;
    symbol->up = tp->all;
    entry->symbol = symbol;
    
    entry->next = tp->buckets[h];
    tp->buckets[h] = entry;
    
    return entry->symbol;
}

