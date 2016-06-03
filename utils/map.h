#ifndef _MAP_H
#define _MAP_H

struct map_entry {
    const void *key;
    void *value;
    struct map_entry *next;
    struct map_entry *all;
};

struct map {
    unsigned size, tablesize;
    unsigned grow_at, shrink_at;
    struct map_entry **table;
    int (*cmpfn) (const void *key1, const void *key2);
    struct map_entry *all;
};

extern struct map *map_new(void);

extern struct map *map_newf(int (*cmp) (const void *, const void *));

extern void *map_get(struct map *map, const void *key);

extern void map_put(struct map *map, const void *key, void *value);

extern struct vector *map_keys(struct map *map);

extern struct vector *map_objs(struct map *map);

#endif
