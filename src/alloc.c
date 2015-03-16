#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include <limits.h>
#include "lib.h"

void * cc_malloc(size_t size)
{
    void *p = malloc(size);
    if (!p)
        die("Can't malloc");
    memset(p, 0, size);
    return p;
}

void cc_free(void *p)
{
    free(p);
}

void die(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    va_end(ap);
    exit(EXIT_FAILURE);
}

#define RESERVED_SIZE       8192
#define BUCKET_INFO(table)  ((struct bucket_info *)table - 1)

void * alloc_bucket(size_t size)
{
    struct bucket_info *pb;
    
    pb = cc_malloc(sizeof(struct bucket_info) + size);
    pb->p = pb + 1;
    pb->limit = (char *)pb->p + size;
    return pb;
}

void free_bucket(struct bucket_info *s)
{
    while (s) {
        struct bucket_info *c = s;
        s = c->next;
        cc_free(c);
    }
}

void *table_alloc_bucket(size_t size)
{
    return alloc_bucket(size + RESERVED_SIZE);
}

void *node_alloc_bucket(size_t size)
{
    return alloc_bucket(size * RESERVED_SIZE);
}

void * alloc_for_size(struct bucket_info *s, size_t size, void *(alloc_bucket_func)(size_t size))
{
    void *ret;
    
    while (s->next)
        s = s->next;
    
    if ((char *)s->p + size > (char *)s->limit) {
        struct bucket_info *pb = alloc_bucket_func(size);
        s->next = pb;
        s = pb;
    }
    
    ret = s->p;
    s->p = (char *)s->p + size;
    return ret;
}

void * alloc_table(size_t size)
{
    struct bucket_info *s = table_alloc_bucket(size);
    return alloc_for_size(s, size, table_alloc_bucket);
}

void drop_table(void *table)
{
    struct bucket_info *s = BUCKET_INFO(table);
    free_bucket(s);
}

void * alloc_table_entry(void *table, size_t size)
{
    struct bucket_info *s = BUCKET_INFO(table);
    return alloc_for_size(s, size, table_alloc_bucket);
}

int array_len(void **array)
{
    int i;
    if (array == NULL)
        return 0;
    for (i=0; array[i]; i++)
        ;
    return i;
}

struct string_table {
    struct string_bucket {
        char *str;
        int len;
        struct string_bucket *next;
    } *buckets[1024];
};

static struct string_table *string_table;
const char *stringn(const char *src, int len)
{
    struct string_bucket *ps;
    register unsigned int hash;
    register unsigned char *p;
    const char *end = src + len;
    
    if (src == NULL || len <= 0)
        return NULL;
    
    if (!string_table)
        string_table = alloc_table(sizeof(struct string_table));
    
    for(hash = 0, p = (unsigned char *)src; *p ; p++)
        hash = 31 * hash + *p;
    
    hash %= ARRAY_SIZE(string_table->buckets) - 1;
    for (ps = string_table->buckets[hash]; ps; ps = ps->next) {
        if (ps->len == len) {
            const char *s1 = src;
            char *s2 = ps->str;
            do {
                if (s1 == end)
                    return ps->str;
            } while (*s1++ == *s2++);
        }
    }
    
    // alloc
    {
        char *dst = alloc_table_entry(string_table, len+1);
        ps = alloc_table_entry(string_table, sizeof(struct string_bucket));
        ps->len = len;
        for (ps->str = dst; src < end; )
            *dst++ = *src++;
        *dst++ = 0;
        ps->next = string_table->buckets[hash];
        string_table->buckets[hash] = ps;
        
        return ps->str;
    }
}

const char *strings(const char *str)
{
    const char *s = str;
    if (!str)
        return NULL;
    while (*s)
        s++;
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
    
    do {
        *--s = m%10 + '0';
    } while ((m /= 10) != 0);
    
    if (n < 0)
        *--s = '-';
    
    return stringn(s, str + sizeof (str) - s);
}

/**
 * vtoa/stoa are different from vec_to_array/str_to_array,
 * the memory is managed automatically by unit_info here.
 */
static struct bucket_info *unit_info;
static void * alloc_unit(size_t size)
{
    if (!unit_info)
        unit_info = table_alloc_bucket(size);
    return alloc_for_size(unit_info, size, table_alloc_bucket);
}

char * stoa(struct string *s)
{
    assert(s);
    char *str = alloc_unit(s->size+1);
    memcpy(str, s->str, s->size);
    free_string(s);
    return str;
}

void ** vtoa(struct vector *v)
{
    void **array = NULL;
    int vlen = vec_len(v);
    if (vlen > 0) {
        array = alloc_unit((vlen+1) * v->elemsize);
        memcpy(array, v->mem, vlen * v->elemsize);
    }
    free_vector(v);
    return array;
}

void free_unit()
{
    drop_table(string_table);
    free_bucket(unit_info);
}

// for debug
void print_bucket(struct bucket_info *s, const char *name)
{
    int nr = 0;
    fprintf(stderr, "%s: ", name);
    while (s) {
        nr++;
        s = s->next;
    }
    fprintf(stderr, "%d buckets allocated.\n", nr);
}

void print_table(void *table, const char *name)
{
    struct bucket_info *s = BUCKET_INFO(table);
    print_bucket(s, name);
}
