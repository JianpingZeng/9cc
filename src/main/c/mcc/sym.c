#include "c.h"

struct table {
    int     scope;
    Table   prev;
    struct  sentry {
        Symbol  sym;
        struct sentry *next;
    } *buckets[256];
    Symbol all;
};

Table   identifiers;

static Table newtable()
{
    Table tp;
    NEW(tp);
    
    return tp;
}

static void rmtable(Table tp)
{
    if (tp) {
        for (int i=0; i < sizeof(tp->buckets)/sizeof(tp->buckets[0]); i++) {
            for (struct sentry *bucket = tp->buckets[i]; bucket; ) {
                struct sentry *next = bucket->next;
                deallocate(bucket);
                bucket = next;
            }
        }
        deallocate(tp);
    }
}

void enterscope()
{
    scopelevel++;
    printf("enter to scope %d...\n", scopelevel);
}

void exitscope()
{
    if (identifiers->scope == scopelevel) {
        Table tp = identifiers;
        identifiers = identifiers->prev;
        rmtable(tp);
    }
    assert(scopelevel >= GLOBAL);
    scopelevel--;
    printf("exit to scope %d...\n", scopelevel);
}

Table table(Table tp, int scope)
{
    Table t = newtable();
    t->prev = tp;
    t->scope = scope;
    if (tp) {
        t->all = tp->all;
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

Symbol lookupsym(const char *name, Table tp)
{
    assert(tp);
    
    for (Table t = tp; t; t = t->prev) {
        unsigned h = hash(name) % 256;
        for (struct sentry *entry = tp->buckets[h]; entry; entry = entry->next) {
            if (entry->sym->lex.name == name) {
                return entry->sym;
            }
        }
    }
    
    return NULL;
}

Symbol installsym(const char *name, Table *tpp, int scope)
{
    unsigned h = hash(name) % 256;
    struct sentry *s;
    Symbol sym;
    Table tp = *tpp;
    
    assert(scope >= tp->scope);
    if (scope > tp->scope) {
        tp = *tpp = table(tp, scope);
    }
    
    NEW(s);
    NEW(sym);
    s->sym = sym;
    s->sym->scope = scope;
    s->sym->lex.name = string(name);
    s->next = tp->buckets[h];
    tp->buckets[h] = s;
    tp->all = s->sym;
    s->sym->up = tp->all;
    
    return s->sym;
}

