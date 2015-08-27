#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include <limits.h>
#include <string.h>
#include "utils.h"

char * stoa(struct string *s)
{
    if (s == NULL || s->len == 0)
        return NULL;
    
    return strings(s->str);
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
    const char *end = src + len;
    
    if (src == NULL || len <= 0)
        return NULL;
    
    if (!string_table)
        string_table = NEWS(string_table);
    
    hash = strhash(src) & (ARRAY_SIZE(string_table->buckets) - 1);
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

static char *vformat(const char *fmt, va_list ap)
{
    size_t size = 128;
    void *buffer = NULL;
    va_list aq;
    for (; ; ) {
        size_t avail = size;
        buffer = NEW0(avail);
        va_copy(aq, ap);
        int total = vsnprintf(buffer, avail, fmt, aq);
        va_end(aq);
        if (avail <= total) {
            size = total + 8;
            continue;
        }
        break;
    }
    
    return buffer;
}

char *format(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    char *r = vformat(fmt, ap);
    va_end(ap);
    return strings(r);
}

// Add elements to vector from a null-terminated array
void vec_add_from_array(struct vector *v, void **array)
{
    if (array == NULL) return;
    for (int i=0; array[i]; i++)
        vec_push(v, array[i]);
}

void ** vtoa(struct vector *v)
{
    void **array = NULL;
    int vlen = vec_len(v);
    if (vlen > 0) {
        array = NEW0((vlen+1) * sizeof(void *));
        memcpy(array, v->mem, vlen * sizeof(void *));
    }
    return array;
}

int array_len(void **array)
{
    int i;
    if (array == NULL) return 0;
    for (i=0; array[i]; i++)
        ;
    return i;
}
