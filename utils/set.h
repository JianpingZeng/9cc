#ifndef _SET_H
#define _SET_H

/// pointers set

struct set {
    struct map *map;
};

extern struct set *set_new(void);
extern void set_free(struct set *set);
extern struct set *set_copy(struct set *set);
extern void set_add(struct set *set, void *element);
extern void set_remove(struct set *set, void *element);
extern bool set_has(struct set *set, void *element);
extern struct set *set_union(struct set *set1, struct set *set2);
extern struct set *set_intersection(struct set *set1, struct set *set2);
extern struct vector *set_objects(struct set *set);
extern size_t set_size(struct set *set);

#endif
