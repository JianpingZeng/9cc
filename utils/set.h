#ifndef _SET_H
#define _SET_H

struct set {
    const char *name;
    struct set *next;
};

extern struct set * set_add(struct set *s, const char *name);

extern bool set_has(struct set *s, const char *name);

extern struct set * set_union(struct set *a, struct set *b);

extern struct set * set_intersection(struct set *a, struct set *b);

#endif
