#include "utils.h"

// a key-value implementation

#define MAP_INIT_SIZE       64
#define MAP_GROW_FACTOR     80
#define MAP_RESIZE_BITS     2

static void alloc_map(struct map *map, unsigned size)
{
    map->table = zmalloc(size * sizeof(struct map_entry *));
    map->tablesize = size;
    map->grow_at = (unsigned)(size * MAP_GROW_FACTOR / 100);
    map->shrink_at = map->grow_at / ((1 << MAP_RESIZE_BITS) + 1);
}

static unsigned bucket(struct map *map, const void *key)
{
    return strhash(key) & (map->tablesize - 1);
}

static void rehash(struct map *map, unsigned newsize)
{
    unsigned oldsize = map->tablesize;
    struct map_entry **oldtable = map->table;

    alloc_map(map, newsize);
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
    free(oldtable);
}

static int cmp(const void *key1, const void *key2)
{
    return strcmp(key1, key2);
}

int nocmp(const void *key1, const void *key2)
{
    return 1;
}

static int eqentry(struct map *map, struct map_entry *entry, const void *key)
{
    return entry->key == key || !map->cmpfn(entry->key, key);
}

static struct map_entry **find_entry(struct map *map, const void *key)
{
    struct map_entry **entry = &map->table[bucket(map, key)];
    while (*entry && !eqentry(map, *entry, key))
        entry = &(*entry)->next;
    return entry;
}

static void map_remove(struct map *map, const void *key)
{
    struct map_entry **entry = find_entry(map, key);
    if (!*entry)
        return;

    struct map_entry *old = *entry;
    *entry = old->next;
    free(old);

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
    if (map->size > map->grow_at)
        rehash(map, map->tablesize << MAP_RESIZE_BITS);
}

struct map *map_new(void)
{
    struct map *map = zmalloc(sizeof(struct map));
    map->size = 0;
    map->cmpfn = cmp;
    alloc_map(map, MAP_INIT_SIZE);
    return map;
}

struct map *map_newf(int (*cmp) (const void *, const void *))
{
    struct map *map = map_new();
    if (cmp)
        map->cmpfn = cmp;
    return map;
}

void map_free(struct map *map)
{
    if (!map)
        return;
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

void *map_get(struct map *map, const void *key)
{
    struct map_entry *entry = *find_entry(map, key);
    return entry ? entry->value : NULL;
}

void map_put(struct map *map, const void *key, void *value)
{
    map_remove(map, key);
    if (value)
        map_add(map, key, value);
}

struct vector *map_keys(struct map *map)
{
    if (!map || map->size == 0)
        return NULL;
    struct vector *v = vec_new();
    for (unsigned i = 0; i < map->tablesize; i++) {
        struct map_entry *entry = map->table[i];
        while (entry) {
            vec_push(v, (void *)entry->key);
            entry = entry->next;
        }
    }
    return v;
}
