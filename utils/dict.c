#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "utils.h"

struct dict *dict_new(void)
{
	struct dict *dict = zmalloc(sizeof(struct dict));
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

void dict_put(struct dict *dict, const void *key, void *value)
{
	map_put(dict->map, key, value);
	vec_push(dict->keys, (void *)key);
}

void *dict_get(struct dict *dict, const void *key)
{
	return map_get(dict->map, key);
}

struct vector *dict_values(struct dict *dict)
{
	struct vector *v = vec_new();
	for (int i = 0; i < vec_len(dict->keys); i++) {
		const void *key = vec_at(dict->keys, i);
		void *value = map_get(dict->map, key);
		vec_push_safe(v, value);
	}
	return v;
}
