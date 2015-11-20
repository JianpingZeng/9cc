#ifndef _DICT_H
#define _DICT_H

struct dict {
	struct map *map;
	struct vector *keys;
};

extern struct dict *dict_new(void);

extern void dict_free(struct dict *dict);

extern void dict_put(struct dict *dict, const void *key, void *value);

extern void *dict_get(struct dict *dict, const void *key);

extern struct vector *dict_values(struct dict *dict);

#endif
