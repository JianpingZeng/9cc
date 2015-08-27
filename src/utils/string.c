#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <stdarg.h>
#include <ctype.h>
#include "utils.h"

static void str_grow(struct string *s, int len)
{
    char *mem = NEW0(len + s->reserve);
    memcpy(mem, s->str, s->len);
    s->str = mem;
    s->len = len + s->reserve;
}

struct string * new_string()
{
    struct string *s = NEWS(string);
    s->reserve = 128;
    s->len = 0;
    s->nalloc = s->reserve;
    s->str = NEW0(s->nalloc);
    return s;
}

size_t str_len(struct string *s)
{
    return s->len;
}

void str_cats(struct string *s, const char *src)
{
    str_catn(s, src, strlen(src));
}

void str_catn(struct string *s, const char *src, int len)
{
    assert(s);
    if (s->len + len >= s->nalloc) {
        str_grow(s, s->len+len);
    }
    
    char *dst = s->str + s->len;
    int size = len;
    while (size-- > 0) {
        *dst++ = *src++;
    }
    s->len += len;
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

void str_strip(struct string *s)
{
    assert(s);
    if (s->len == 0) return;
    char *head, *tail;
    for (head=s->str, tail=s->str+s->len-1; ;) {
        int b1 = isblank(*head);
        int b2 = isblank(*tail);
        if (b1)
            head++;
        if (b2)
            tail--;
        if (head > tail || (!b1 && !b2))
            break;
    }
    if (head > tail) {
        s->len = 0;
    } else {
        s->len = tail - head + 1;
        char *dst = s->str;
        size_t len = s->len;
        while (len-- > 0) {
            *dst++ = *head++;
        }
        *dst = 0;
    }
}

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
