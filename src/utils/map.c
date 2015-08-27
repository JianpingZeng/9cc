#include <stdlib.h>
#include <string.h>
#include "utils.h"

// a key-value implementation

#define FNV32_BASIS ((unsigned) 0x811c9dc5)
#define FNV32_PRIME ((unsigned) 0x01000193)

#define MAP_INIT_SIZE       64
#define MAP_GROW_FACTOR     80
#define MAP_RESIZE_BITS     2

// FNV-1a
unsigned strhash(const char *s)
{
    unsigned hash = FNV32_BASIS;
    for (; *s; s++) {
        hash ^= *s;
        hash *= FNV32_PRIME;
    }
    return hash;
}

static void alloc_map(struct map *map, unsigned size)
{
    map->table = zmalloc(size * sizeof(struct map_entry *));
    map->tablesize = size;
    map->grow_at = (unsigned) (size * MAP_GROW_FACTOR / 100);
    map->shrink_at = map->grow_at / ((1<<MAP_RESIZE_BITS) + 1);
}

static unsigned bucket(struct map *map, const char *key)
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

int always_notequal(const char *key1, const char *key2)
{
    return 1;
}

int always_equal(const char *key1, const char *key2)
{
    return 0;
}

static int default_cmpfn(const char *key1, const char *key2)
{
    return strcmp(key1, key2);
}

static int eqentry(struct map *map, struct map_entry *entry, const char *key)
{
    return entry->key == key || !map->cmpfn(entry->key, key);
}

static struct map_entry ** find_entry(struct map *map, const char *key)
{
    struct map_entry **entry = &map->table[bucket(map, key)];
    while (*entry && !eqentry(map, *entry, key))
        entry = &(*entry)->next;
    return entry;
}

static void map_remove(struct map *map, const char *key)
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

static void map_add(struct map *map, const char *key, void *value)
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

struct map * new_map(int (*cmpfn) (const char *, const char *))
{
    struct map *map = zmalloc(sizeof(struct map));
    map->size = 0;
    map->cmpfn = cmpfn ? cmpfn : default_cmpfn;
    alloc_map(map, MAP_INIT_SIZE);
    return map;
}

void free_map(struct map *map)
{
    if (!map)
        return;
    if (map->table) {
        for (int i = 0; i < map->tablesize; i++) {
            struct map_entry *entry = map->table[i];
            while (entry) {
                struct map_entry *next = entry->next;
                free(entry);
                entry = next;
            }
        }
        free(map->table);
    }
    free(map);
}

void *map_get(struct map *map, const char *key)
{
    struct map_entry *entry;
    if (!map->table)
        return NULL;
    entry = *find_entry(map, key);
    return entry ? entry->value : NULL;
}

void map_put(struct map *map, const char *key, void *value)
{
    if (!map->table)
        alloc_map(map, MAP_INIT_SIZE);
    map_remove(map, key);
    if (value)
        map_add(map, key, value);
}

