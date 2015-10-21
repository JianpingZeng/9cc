#include <stdlib.h>
#include <stdbool.h>
#include "utils.h"

struct dict * dict_new(void)
{
    struct dict *dict = xmalloc(sizeof(struct dict));
    dict->map = map_new();
    dict->keys = vec_new();
    return dict;
}

void dict_free(struct dict *dict)
{
    map_free(dict->map);
    vec_free(dict->keys);
    free(dict);
}

void * dict_get(struct dict *dict, const char *key)
{
    return map_get(dict->map, key);
}

void dict_put(struct dict *dict, const char *key, void *value)
{
    map_put(dict->map, key, value);
    vec_push(dict->keys, (void *)key);
}

struct vector * dict_allkeys(struct dict *dict)
{
    return dict->keys;
}

void ** dict_allvalues(struct dict *dict)
{
    struct vector *v = vec_new();
    for (int i = 0; i < vec_len(dict->keys); i++) {
	const char *key = vec_at(dict->keys, i);
	void *value = dict_get(dict, key);
	vec_push_safe(v, value);
    }
    return vtoa(v);
}
