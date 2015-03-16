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
struct table * constants;
struct table * tags;
static int _scope = GLOBAL;

static void free_symbol_table(struct table *table)
{
    while (table) {
        struct table *tp = table;
        table = table->up;
        drop_table(tp);
    }
}

void symbol_init()
{
    identifiers = new_table(NULL, GLOBAL);
    constants = new_table(NULL, CONSTANT);
    tags = new_table(NULL, GLOBAL);
}

void symbol_exit()
{
    free_symbol_table(identifiers);
    free_symbol_table(tags);
    free_symbol_table(constants);
}

int scopelevel()
{
    return _scope;
}

void enter_scope()
{
    _scope++;
}

void exit_scope()
{
    if (tags->scope == _scope) {
        struct table *tp = tags;
        tags = tags->up;
        drop_table(tp);
    }
    if (identifiers->scope == _scope) {
        struct table *tp = identifiers;
        identifiers = identifiers->up;
        drop_table(tp);
    }
    assert(_scope >= GLOBAL);
    _scope--;
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
    
    for(h = 0, p = (unsigned char *)src; p && *p ; p++)
        h = 31 * h + *p;
    
    return h;
}

struct symbol * anonymous_symbol(struct table **tpp, int scope)
{
    return install_symbol(NULL, tpp, scope);
}

struct symbol * find_symbol(const char *name, struct table *table, int scope)
{
    assert(name);
    
    for (struct table *t = table; t && t->scope >= scope; t = t->up) {
        unsigned h = hash(name) % BUCKET_SIZE;
        for (struct sentry *entry = t->buckets[h]; entry; entry = entry->next) {
            if (entry->symbol->name == name) {
                return entry->symbol;
            }
        }
    }
    
    return NULL;
}

struct symbol * locate_symbol(const char *name , struct table *table, int scope)
{
    assert(name);
    
    if (scope > table->scope)
        return NULL;
    
    while (scope != table->scope)
        table = table->up;
    
    for (struct table *t = table; t && t->scope == scope; t = t->up) {
        unsigned h = hash(name) % BUCKET_SIZE;
        for (struct sentry *entry = t->buckets[h]; entry; entry = entry->next) {
            if (entry->symbol->name == name) {
                return entry->symbol;
            }
        }
    }
    
    return NULL;
}

struct symbol * lookup_symbol(const char *name, struct table *table)
{
    return find_symbol(name, table, CONSTANT);
}

struct symbol * install_symbol(const char *name, struct table **tpp, int scope)
{
    unsigned h = hash(name) % BUCKET_SIZE;
    struct sentry *entry;
    struct symbol *symbol;
    struct table *tp = *tpp;
    
    if (scope > tp->scope) {
        tp = *tpp = new_table(tp, scope);
    } else {
        while (scope != tp->scope)
            tp = tp->up;
    }
    
    entry = alloc_table_entry(tp, sizeof(struct sentry));
    symbol = alloc_symbol_node();
    symbol->scope = scope;
    symbol->name = strings(name);
    symbol->up = tp->all;
    tp->all = symbol;
    entry->symbol = symbol;
    
    entry->next = tp->buckets[h];
    tp->buckets[h] = entry;
    
    return entry->symbol;
}
