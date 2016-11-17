#include "compat.h"
#include <stdlib.h>
#include "lex.h"
#include "internal.h"
#include "libutils.h"

static void idtab_expand(struct idtab *);

struct idtab *idtab_new(unsigned int cap)
{
    struct idtab *t = zmalloc(sizeof(struct idtab));
    unsigned int nslots = 1 << cap;
    t->nslots = nslots;
    t->table = xcalloc(nslots, sizeof(struct idtab_entry *));
    return t;
}

void idtab_free(struct idtab *t)
{
    free(t->table);
    free(t);
}

struct ident *idtab_lookup(struct idtab *t,
                           const char *str, size_t len,
                           enum idtab_lookup_option opt)
{
    return idtab_lookup_with_hash(t, str, len, strnhash(str, len), opt);
}

struct ident *idtab_lookup_with_hash(struct idtab *t,
                                     const char *str, size_t len,
                                     unsigned int hash,
                                     enum idtab_lookup_option opt)
{
    unsigned int index;
    struct idtab_entry *p;
    struct ident *result;

    t->searches++;
    index = hash & (t->nslots - 1);

    for (p = t->table[index]; p; p = p->link)
        if (p->ident->len == len && !strncmp(str, p->ident->str, len))
            return p->ident;

    if (t->table[index])
        t->collisions++;
    if (opt == ID_SEARCH)
        return NULL;

    result = t->alloc_ident(t);
    result->len = len;
    result->hash = hash;
    result->str = strndup(str, len);
    
    p = xmalloc(sizeof(struct idtab_entry));
    p->ident = result;
    p->link = t->table[index];
    t->table[index] = p;

    if (++t->nelements * 4 >= t->nslots * 3)
        idtab_expand(t);

    return result;
}

static void idtab_expand(struct idtab *t)
{
    unsigned int oldsize = t->nslots;
    struct idtab_entry **oldtable = t->table;
    unsigned int sizemask;

    t->nslots = oldsize * 2;
    t->table = xcalloc(t->nslots, sizeof(struct idtab_entry *));
    sizemask = t->nslots - 1;

    for (unsigned int i = 0; i < oldsize; i++) {
        struct idtab_entry *entry = oldtable[i];
        while (entry) {
            struct idtab_entry *next = entry->link;
            unsigned int index = entry->ident->hash & sizemask;
            entry->link = t->table[index];
            t->table[index] = entry;
            entry = next;
        }
    }
    free(oldtable);
}

void idtab_foreach(struct idtab *t, idtab_cb cb, const void *v)
{
    struct idtab_entry *p;

    for (unsigned int i = 0; i < t->nslots; i++)
        for (p = t->table[i]; p; p = p->link)
            if (cb(t, p->ident, v))
                goto stop;
 stop:;
}

void idtab_dump(struct idtab *t)
{
    dlog("idtab: %u elements, %u slots, %u searches, %u collisions.",
         t->nelements, t->nslots, t->searches, t->collisions);
}
