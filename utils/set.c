#include <stdbool.h>
#include <stdlib.h>
#include "utils.h"

struct set * set_new(void)
{
    return zmalloc(sizeof (struct set));
}

void set_free(struct set *s)
{
    while (s) {
	struct set *next = s->next;
	free(s);
	s = next;
    }
}

struct set * set_add(struct set *s, const char *name)
{
    struct set *r = set_new();
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
    struct set *r = set_new();
    for (; a; a = a->next) {
	if (set_has(b, a->name))
	    r = set_add(r, a->name);
    }
    return r;
}
