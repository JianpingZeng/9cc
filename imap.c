#include "utils/utils.h"
#include "imap.h"

static void imap_expand(struct imap *imap);

static unsigned int calc_hash(const unsigned char *s, size_t len)
{
    size_t n = len;
    unsigned int h = 0;

    while (n--)
        h = IMAP_HASHSTEP(h, *s++);

    return IMAP_HASHFINISH(h, len);
}

struct imap *imap_new(unsigned int cap)
{
    struct imap *imap = zmalloc(sizeof(struct imap));
    unsigned int nslots = 1 << cap;
    imap->nslots = nslots;
    imap->table = zmalloc(nslots * sizeof(struct ident *));
    return imap;
}

void imap_free(struct imap *imap)
{
    free(imap->table);
    free(imap);
}

struct ident *imap_lookup(struct imap *imap,
                          const unsigned char *str,
                          size_t len,
                          enum imap_lookup_option opt)
{
    return imap_lookup_with_hash(imap, str, len, calc_hash(str, len), opt);
}

struct ident *imap_lookup_with_hash(struct imap *imap,
                                    const unsigned char *str,
                                    size_t len,
                                    unsigned int hash,
                                    enum imap_lookup_option opt)
{
    size_t sizemask;
    unsigned int index;
    unsigned int hash2;
    struct ident *result;

    sizemask = imap->nslots - 1;
    index = hash & sizemask;
    imap->searches++;

    result = imap->table[index];

    if (result != NULL) {
        if (result->hash == hash &&
            result->len == (unsigned int)len &&
            !memcmp(result->str, str, len))
            return result;

        // hash2 must be odd.
        hash2 = ((hash * 17) & sizemask) | 1;

        for (;;) {
            imap->collisions++;
            index = (index + hash2) & sizemask;
            result = imap->table[index];
            if (result == NULL)
                break;

            if (result->hash == hash &&
                result->len == (unsigned int)len &&
                !memcmp(result->str, str, len))
                return result;
        }
    }

    if (opt == IMAP_SEARCH)
        return NULL;

    result = imap->alloc_entry(imap);
    imap->table[index] = result;

    result->len = len;
    result->hash = hash;
    
    char *d = xmalloc(len + 1);
    memcpy(d, str, len);
    d[len] = '\0';
    result->str = (const unsigned char *)d;

    if (++imap->nelements * 4 >= imap->nslots * 3)
        imap_expand(imap);
    
    return result;
}

static void imap_expand(struct imap *imap)
{
    struct ident **ntable, **p, **limit;
    unsigned int size, sizemask;

    size = imap->nslots * 2;
    ntable = zmalloc(size * sizeof(struct ident *));
    sizemask = size - 1;

    p = imap->table;
    limit = p + imap->nslots;

    do {
        if (*p) {
            unsigned int index, hash, hash2;

            hash = (*p)->hash;
            index = hash & sizemask;

            if (ntable[index]) {
                hash2 = ((hash * 17) & sizemask) | 1;
                do {
                    index = (index + hash2) & sizemask;
                } while (ntable[index]);
            }
            ntable[index] = *p;
        }
    } while (++p < limit);

    free(imap->table);
    imap->table = ntable;
    imap->nslots = size;
    imap->expansions++;
}

void imap_foreach(struct imap *imap, imap_cb cb, const void *context)
{
    struct ident **p, **limit;

    p = imap->table;
    limit = p + imap->nslots;

    do {
        if (*p) {
            if (cb(imap, *p, context) == 0)
                break;
        }
    } while (++p < limit);
}

void imap_dump(struct imap *imap)
{
    println("imap: %p", imap);
    println("nslots: %u", imap->nslots);
    println("nelements: %u", imap->nelements);
    println("searches: %u", imap->searches);
    println("collisions: %u", imap->collisions);
    println("expandsions: %u", imap->expansions);
}
