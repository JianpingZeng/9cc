#include "utils.h"

static char value;

struct set *set_new(void)
{
    struct set *set = zmalloc(sizeof(struct set));
    set->map = map_newf(NULL);
    return set;
}

struct set *set_new1(void *element)
{
    struct set *set = set_new();
    set_add(set, element);
    return set;
}

void set_free(struct set *set)
{
    map_free(set->map);
    free(set);
}

struct set *set_copy(struct set *set)
{
    struct set *s = set_new();
    struct vector *objects = set_objects(set);
    for (size_t i = 0; i < vec_len(objects); i++)
        set_add(s, vec_at(objects, i));
    return s;
}

void set_add(struct set *set, void *element)
{
    assert(element);
    map_put(set->map, element, &value);
}

void set_remove(struct set *set, void *element)
{
    assert(element);
    map_put(set->map, element, NULL);
}

struct set *set_substract(struct set *set1, struct set *set2)
{
    struct set *set = set_new();
    struct vector *objects1 = set_objects(set1);
    for (size_t i = 0; i < vec_len(objects1); i++) {
        void *obj1 = vec_at(objects1, i);
        if (!set_has(set2, obj1))
            set_add(set, obj1);
    }
    return set;
}

bool set_has(struct set *set, void *element)
{
    assert(element);
    return set ? map_get(set->map, element) != NULL : false;
}

struct set *set_union(struct set *set1, struct set *set2)
{
    struct set *set = set_new();
    struct vector *objects1 = set_objects(set1);
    struct vector *objects2 = set_objects(set2);
    for (size_t i = 0; i < vec_len(objects1); i++)
        set_add(set, vec_at(objects1, i));
    for (size_t i = 0; i < vec_len(objects2); i++)
        set_add(set, vec_at(objects2, i));
    return set;
}

struct set *set_intersection(struct set *set1, struct set *set2)
{
    struct set *set = set_new();
    struct vector *objects1 = set_objects(set1);
    for (size_t i = 0; i < vec_len(objects1); i++) {
        void *obj1 = vec_at(objects1, i);
        if (set_has(set2, obj1))
            set_add(set, obj1);
    }
    return set;
}

struct vector *set_objects(struct set *set)
{
    return set ? map_keys(set->map) : NULL;
}

size_t set_size(struct set *set)
{
    return set ? set->map->size : 0;
}

bool set_equal(struct set *set1, struct set *set2)
{
    if (set_size(set1) != set_size(set2))
        return false;
    struct vector *objects1 = set_objects(set1);
    for (size_t i = 0; i < vec_len(objects1); i++) {
        void *obj1 = vec_at(objects1, i);
        if (!set_has(set2, obj1))
            return false;
    }
    return true;
}

bool set_empty(struct set *set)
{
    return set_size(set) == 0;
}

void set_clear(struct set *set)
{
    map_free(set->map);
    set->map = map_newf(NULL);
}
