#include <string.h>
#include <assert.h>
#include <limits.h>
#include "utils.h"

static void str_grow(struct string *s, int len)
{
    char *mem = NEW0(len + s->reserve);
    memcpy(mem, s->str, s->size);
    s->str = mem;
    s->capelems = len + s->reserve;
}

struct string * new_string()
{
    struct string *s = NEWS(string);
    s->reserve = 128;
    s->size = 0;
    s->capelems = s->reserve;
    s->str = NEW0(s->capelems);
    return s;
}

unsigned str_len(struct string *s)
{
    assert(s);
    return s->size;
}

void str_cats(struct string *s, const char *src)
{
    str_catn(s, src, strlen(src));
}

void str_catn(struct string *s, const char *src, int len)
{
    assert(s);
    if (s->size + len >= s->capelems) {
        str_grow(s, s->size+len);
    }
    
    char *dst = s->str + s->size;
    int size = len;
    while (size-- > 0) {
        *dst++ = *src++;
    }
    s->size += len;
}

void str_catd(struct string *s, long n)
{
    assert(s);
    char str[32], *ps = str + sizeof (str);
    unsigned long m;
    
    if (n == LONG_MIN)
        m = (unsigned long)LONG_MAX + 1;
    else if (n < 0)
        m = -n;
    else
        m = n;
    
    do {
        *--ps = m%10 + '0';
    } while ((m /= 10) != 0);
    
    if (n < 0) {
        *--ps = '-';
    }
    return str_catn(s, ps, str + sizeof (str) - ps);
}

char * stoa(struct string *s)
{
    assert(s);
    char *str = NEW0(s->size+1);
    memcpy(str, s->str, s->size);
    return str;
}

struct string_table {
    struct string_bucket {
        char *str;
        int len;
        struct string_bucket *next;
    } *buckets[1024];
};

static struct string_table *string_table;
char *stringn(const char *src, int len)
{
    struct string_bucket *ps;
    register unsigned int hash;
    register unsigned char *p;
    const char *end = src + len;
    
    if (src == NULL || len <= 0)
        return NULL;
    
    if (!string_table)
        string_table = NEWS(string_table);
    
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
        char *dst = NEW0(len+1);
        ps = NEWS(string_bucket);
        ps->len = len;
        for (ps->str = dst; src < end; )
            *dst++ = *src++;
        *dst++ = 0;
        ps->next = string_table->buckets[hash];
        string_table->buckets[hash] = ps;
        
        return ps->str;
    }
}

char *strings(const char *str)
{
    const char *s = str;
    if (!str)
        return NULL;
    while (*s)
        s++;
    return stringn(str, s - str);
}

char *stringd(long n)
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
