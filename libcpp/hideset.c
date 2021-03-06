#include "internal.h"
#include "libutils.h"

struct hideset *hideset_add(struct hideset *s, const char *name)
{
    struct hideset *r = NEWS0(struct hideset, PERM);
    r->name = name;
    r->next = s;
    return r;
}

bool hideset_has(struct hideset * s, const char *name)
{
    for (; s; s = s->next) {
        if (s->name == name)
            return true;
    }
    return false;
}

struct hideset *hideset_union(struct hideset *a, struct hideset *b)
{
    struct hideset *r = a;
    for (; b; b = b->next) {
        if (!hideset_has(a, b->name))
            r = hideset_add(r, b->name);
    }
    return r;
}

struct hideset *hideset_intersection(struct hideset *a, struct hideset *b)
{
    struct hideset *r = NULL;
    for (; a; a = a->next) {
        if (hideset_has(b, a->name))
            r = hideset_add(r, a->name);
    }
    return r;
}
