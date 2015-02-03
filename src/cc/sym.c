#include "cc.h"

struct table * identifiers;
static int _scopelevel;

static struct table * newtable()
{
    struct table *tp = alloc_node(struct table);    
    return tp;
}

//TODO: can't rm
static void rmtable(struct table *tp)
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

int scopelevel()
{
    return _scopelevel;
}

void enterscope()
{
    _scopelevel++;
}

void exitscope()
{
    if (identifiers->scope == _scopelevel) {
        struct table *tp = identifiers;
        identifiers = identifiers->prev;
        rmtable(tp);
    }
    assert(scopelevel >= GLOBAL);
    _scopelevel--;
}

struct table * table(struct table *tp, int scope)
{
    struct table *t = newtable();
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

struct symbol * lookupsym(const char *name, struct table *tp)
{
    assert(tp);
    
    for (struct table *t = tp; t; t = t->prev) {
        unsigned h = hash(name) % 256;
        for (struct sentry *entry = tp->buckets[h]; entry; entry = entry->next) {
            if (entry->sym->token->name == name) {
                return entry->sym;
            }
        }
    }
    
    return NULL;
}

//TODO: token alloc
struct symbol * installsym(const char *name, struct table **tpp, int scope)
{
    unsigned h = hash(name) % 256;
    struct sentry *s = alloc_node(struct sentry);
    struct symbol *sym = alloc_node(struct symbol);
    struct table *tp = *tpp;
    
    assert(scope >= tp->scope);
    if (scope > tp->scope) {
        tp = *tpp = table(tp, scope);
    }
    
    s->sym = sym;
    s->sym->scope = scope;
    s->sym->token->name = strings(name);
    s->next = tp->buckets[h];
    tp->buckets[h] = s;
    tp->all = s->sym;
    s->sym->up = tp->all;
    
    return s->sym;
}

