#include "utils.h"

// a key-value implementation

#define MAP_INIT_SIZE       64
#define MAP_GROW_FACTOR     80
#define MAP_RESIZE_BITS     2

static void do_alloc_map(struct map *map, unsigned size)
{
    map->table = zmalloc(size * sizeof(struct map_entry *));
    map->tablesize = size;
    map->grow_at = (unsigned)(size * MAP_GROW_FACTOR / 100);
    map->shrink_at = map->grow_at / ((1 << MAP_RESIZE_BITS) + 1);
}

static unsigned bucket(struct map *map, const void *key)
{
    if (map->cmpfn)
        return strhash(key) & (map->tablesize - 1);
    else
        return (unsigned)key & (map->tablesize - 1);
}

static void rehash(struct map *map, unsigned newsize)
{
    unsigned oldsize = map->tablesize;
    struct map_entry **oldtable = map->table;

    do_alloc_map(map, newsize);
    for (int i = 0; i < oldsize; i++) {
        struct map_entry *entry = oldtable[i];
        while (entry) {
            struct map_entry *next = entry->next;
            unsigned b = bucket(map, entry->key);
            entry->next = map->table[b];
            map->table[b] = entry;
            entry = next;
        }
    }
}

static int cmp(const void *key1, const void *key2)
{
    return strcmp(key1, key2);
}

static int eqentry(struct map *map, struct map_entry *entry, const void *key)
{
    if (map->cmpfn)
        return entry->key == key || !map->cmpfn(entry->key, key);
    else
        return entry->key == key;
}

static struct map_entry **find_entry(struct map *map, const void *key)
{
    struct map_entry **entry = &map->table[bucket(map, key)];
    while (*entry && !eqentry(map, *entry, key))
        entry = &(*entry)->next;

    return entry;
}

static struct map_entry **find_all(struct map *map, struct map_entry *entry)
{
    for (struct map_entry **p = &map->all; *p; p = &(*p)->all) {
        if (*p == entry)
            return p;
    }
    return NULL;
}

static void map_remove(struct map *map, const void *key)
{
    struct map_entry **entry = find_entry(map, key);
    if (!*entry)
        return;

    struct map_entry *old = *entry;
    *entry = old->next;
    // all link
    struct map_entry **all = find_all(map, old);
    *all = old->all;

    map->size--;
    if (map->size < map->shrink_at)
        rehash(map, map->tablesize >> MAP_RESIZE_BITS);
}

static void map_add(struct map *map, const void *key, void *value)
{
    unsigned b = bucket(map, key);
    struct map_entry *entry = zmalloc(sizeof(struct map_entry));
    entry->key = key;
    entry->value = value;
    entry->next = map->table[b];
    map->table[b] = entry;
    map->size++;
    // all link
    entry->all = map->all;
    map->all = entry;
    if (map->size > map->grow_at)
        rehash(map, map->tablesize << MAP_RESIZE_BITS);
}

struct map *map_new(void)
{
    struct map *map = zmalloc(sizeof(struct map));
    map->size = 0;
    map->cmpfn = cmp;
    do_alloc_map(map, MAP_INIT_SIZE);
    return map;
}

void map_free(struct map *map)
{
    for (unsigned i = 0; i < map->tablesize; i++) {
        struct map_entry *entry = map->table[i];
        while (entry) {
            struct map_entry *next = entry->next;
            free(entry);
            entry = next;
        }
    }
    free(map->table);
    free(map);
}

struct map *map_newf(int (*cmp) (const void *, const void *))
{
    struct map *map = map_new();
    map->cmpfn = cmp;
    return map;
}

void *map_get(struct map *map, const void *key)
{
    struct map_entry *entry = *find_entry(map, key);
    return entry ? entry->value : NULL;
}

void map_put(struct map *map, const void *key, void *value)
{
    if (value) {
        struct map_entry **entry = find_entry(map, key);
        if (!*entry) {
            map_add(map, key, value);
        } else {
            // just replace the value
            struct map_entry *old = *entry;
            old->value = value;
        }
    } else {
        map_remove(map, key);
    }
}

struct vector *map_keys(struct map *map)
{
    if (!map || map->size == 0)
        return NULL;
    struct vector *v = vec_newn(map->size);
    for (struct map_entry *entry = map->all; entry; entry = entry->all)
        vec_push(v, (void *)entry->key);
    return v;
}

struct vector *map_objs(struct map *map)
{
    if (!map || map->size == 0)
        return NULL;
    struct vector *v = vec_newn(map->size);
    for (struct map_entry *entry = map->all; entry; entry = entry->all)
        vec_push(v, (void *)entry->value);
    return v;
}
