#ifndef _DICT_H
#define _DICT_H

struct dict {
    struct map *map;
    struct vector *keys;
};

extern struct dict * dict_new(void);

extern void dict_free(struct dict *dict);

extern void * dict_get(struct dict *dict, const char *key);

extern void dict_put(struct dict *dict, const char *key, void *value);

extern struct vector * dict_allkeys(struct dict *dict);

extern void ** dict_allvalues(struct dict *dict);

#endif
