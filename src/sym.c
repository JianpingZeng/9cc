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

static struct table _identifiers = { .scope = GLOBAL };
static struct table _constants = { .scope = CONSTANT };
static struct table _tags = { .scope = GLOBAL };

struct table * identifiers = &_identifiers;
struct table * constants = &_constants;
struct table * tags = &_tags;

static int level = GLOBAL;

static struct symbol * new_symbol()
{
    return NEWS(symbol);
}

static struct table * new_table(struct table *up, int scope)
{
    struct table *t = NEWS(table);
    t->up = up;
    t->scope = scope;
    if (up) {
        t->all = up->all;
    }
    return t;
}

int scopelevel()
{
    return level;
}

void enter_scope()
{
    level++;
}

void exit_scope()
{
    if (tags->scope == level) {
        tags = tags->up;
    }
    if (identifiers->scope == level) {
        identifiers = identifiers->up;
    }
    assert(level >= GLOBAL);
    level--;
}

static unsigned hash(const char *src)
{
    register unsigned h;
    register unsigned char *p;
    
    for(h = 0, p = (unsigned char *)src; p && *p ; p++)
        h = 31 * h + *p;
    
    return h;
}

struct symbol * anonymous(struct table **tpp, int scope)
{
    static long i;
    return install(stringd(i++), tpp, scope);
}

struct symbol * lookup(const char *name, struct table *table)
{
    assert(name);
    
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

struct symbol * install(const char *name, struct table **tpp, int scope)
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
    
    entry = NEWS(sentry);
    symbol = new_symbol();
    symbol->scope = scope;
    symbol->name = strings(name);
    symbol->up = tp->all;
    tp->all = symbol;
    entry->symbol = symbol;
    
    entry->next = tp->buckets[h];
    tp->buckets[h] = entry;
    
    return entry->symbol;
}
