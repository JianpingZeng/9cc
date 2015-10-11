#include <stdbool.h>
#include <stdlib.h>
#include "utils.h"

struct set * set_add(struct set *s, const char *name)
{
    struct set *r = zmalloc(sizeof(struct set));
    r->name = name;
    r->next = s;
    return r;
}

bool set_has(struct set *s, const char *name)
{
    for (; s; s = s->next) {
	if (s->name == name)
	    return true;
    }
    return false;
}

struct set * set_union(struct set *a, struct set *b)
{
    struct set *r = a;
    for (; b; b = b->next) {
	if (!set_has(a, b->name))
	    r = set_add(r, b->name);
    }
    return r;
}

struct set * set_intersection(struct set *a, struct set *b)
{
    struct set *r = NULL;
    for (; a; a = a->next) {
	if (set_has(b, a->name))
	    r = set_add(r, a->name);
    }
    return r;
}
