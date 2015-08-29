#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <limits.h>
#include "utils.h"

#define FNV32_BASIS ((unsigned) 0x811c9dc5)
#define FNV32_PRIME ((unsigned) 0x01000193)

struct str_table {
    struct str_bucket {
        char *str;
        size_t len;
        struct str_bucket *next;
    } *buckets[1024];
};

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

char *strn(const char *src, size_t len)
{
    static struct str_table *table;
    struct str_bucket *ps;
    register unsigned int hash;
    const char *end = src + len;
    
    if (src == NULL || len <= 0)
        return NULL;
    
    if (!table)
        table = zmalloc(sizeof (struct str_table));
    
    hash = strhash(src) & (ARRAY_SIZE(table->buckets) - 1);
    for (ps = table->buckets[hash]; ps; ps = ps->next) {
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
        char *dst = zmalloc(len + 1);
        ps = zmalloc(sizeof (struct str_bucket));
        ps->len = len;
        for (ps->str = dst; src < end; )
            *dst++ = *src++;
        *dst++ = '\0';
        ps->next = table->buckets[hash];
        table->buckets[hash] = ps;
        
        return ps->str;
    }
}

char *strs(const char *str)
{
    const char *s = str;
    if (!str)
        return NULL;
    while (*s)
        s++;
    return strn(str, s - str);
}

char *strd(long n)
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
    
    return strn(s, str + sizeof (str) - s);
}

static char *vformat(const char *fmt, va_list ap)
{
    size_t size = 128;
    void *buffer = NULL;
    va_list aq;
    for (; ; ) {
        size_t avail = size;
        buffer = zmalloc(avail);
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
    return strs(r);
}
