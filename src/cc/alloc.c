#include "lib.h"

#define RESERVED_SIZE 8192

#define ALIGN_SIZE    (sizeof (unsigned long long))
#define ROUNDUP(size) ((size+(ALIGN_SIZE - 1))&(~(ALIGN_SIZE - 1)))

struct alloc_bucket {
    void *p;                   /* free position */
    void *limit;               /* end position */
    struct alloc_bucket *next; /* next bucket */
};

struct alloc_info {
    struct alloc_bucket *head;
    struct alloc_bucket *free;
};

static struct alloc_info alloc_info[3];

static void * xmalloc(size_t size)
{
    void *p = malloc(size);
    if (!p) {
	die("Can't malloc");
    }
    memset(p, 0, size);
    return p;
}

static void xfree(void *p)
{
    free(p);
}

static void * new_page(size_t size)
{
    struct alloc_bucket *pb = xmalloc(sizeof(struct alloc_bucket) + size + RESERVED_SIZE);
    pb->p = pb + 1;
    pb->limit = (char *)pb->p + size + RESERVED_SIZE;
    return pb;
}

static void * alloc_for_bucket(size_t size, int index)
{
    void *ret;
    struct alloc_info info = alloc_info[index];
    size = ROUNDUP(size);

    if (!info.head) {
	struct alloc_bucket *pb = new_page(size);
	info.head = info.free = pb;
    }

    if ((char *)info.free->p + size > info.free->limit) {
	struct alloc_bucket *pb = new_page(size);
	info.free->next = pb;
	info.free = pb;
    }

    ret = info.free->p;
    info.free->p = (char *)info.free->p + size;
    return ret;
}

void * allocate(size_t size, int flags)
{
    switch (flags) {
    case ALLOC_TEMP:
	return xmalloc(size);
    case ALLOC_NODE:
    case ALLOC_STRING:
	return alloc_for_bucket(size, flags);
    default:
	die("unknown alloc type");
    }
}

void deallocate(void *p)
{
    xfree(p);
}

/* string bucket
 *
 */

struct string_bucket {
    char *str;
    int len;
    struct string_bucket *next;
};

static struct string_bucket * buckets[1024];

struct string_bucket * new_string_bucket()
{
    return allocate(sizeof(struct string_bucket), ALLOC_NODE);
}

const char *stringn(const char *src, int len)
{
    struct string_bucket *ps;
    register unsigned int hash;
    register unsigned char *p;
    const char *end = src + len;
    
    if (src == NULL || len < 0) {
        return NULL;
    }
    
    for(hash = 0, p = (unsigned char *)src; *p ; p++)
        hash = 31 * hash + *p;
    
    hash %= ARRAY_SIZE(buckets) - 1;
    for (ps = buckets[hash]; ps; ps = ps->next) {
        if (ps->len == len) {
            const char *s1 = src;
            char *s2 = ps->str;
            do {
                if (s1 == end) {
                    return ps->str;
                }
            } while (*s1++ == *s2++);
        }
    }
    
    // alloc
    {
        char *dst = (char *) allocate(len+1, ALLOC_STRING);
        ps = new_string_bucket();
        ps->len = len;
        for (ps->str = dst; src < end; ) {
            *dst++ = *src++;
        }
        *dst++ = 0;
        ps->next = buckets[hash];
        buckets[hash] = ps;

        return ps->str;
    }
}

const char *strings(const char *str)
{
    const char *s = str;
    while (*s) {
        s++;
    }
    return stringn(str, s - str);
}

const char *stringd(long n)
{
    char str[25], *s = str + sizeof (str);
    unsigned long m;
    
    if (n == LONG_MIN)
	m = (unsigned long)LONG_MAX + 1;
    else if (n < 0)
	m = -n;
    else
	m = n;
    do
	*--s = m%10 + '0';
    while ((m /= 10) != 0);
    if (n < 0)
	*--s = '-';
    return stringn(s, str + sizeof (str) - s);
}
