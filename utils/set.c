#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>
#include "utils.h"

static char value;

struct set *set_new(void)
{
    struct set *set = zmalloc(sizeof(struct set));
    set->dict = dict_new();
    return set;
}

void set_free(struct set *set)
{
    dict_free(set->dict);
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
    dict_put(set->dict, element, &value);
}

void set_remove(struct set *set, void *element)
{
    dict_put(set->dict, element, NULL);
}

bool set_has(struct set *set, void *element)
{
    assert(element);
    return dict_get(set->dict, element) != NULL;
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
    if (!set1 || !set2)
        return NULL;
    if (!set_len(set1) || !set_len(set2))
        return NULL;
    struct set *set = set_new();
    struct vector *objects1 = set_objects(set1);
    struct vector *objects2 = set_objects(set2);
    for (size_t i = 0; i < vec_len(objects1); i++) {
        void *obj1 = vec_at(objects1, i);
        for (size_t j = 0; j < vec_len(objects2); j++) {
            void *obj2 = vec_at(objects2, j);
            if (obj1 == obj2)
                set_add(set, obj1);
        }
    }
    return set;
}

struct vector *set_objects(struct set *set)
{
    return set ? set->dict->keys : NULL;
}

size_t set_len(struct set *set)
{
    return set ? vec_len(set->dict->keys) : 0;
}
