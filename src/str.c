#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <ctype.h>
#include <stdarg.h>
#include "alloc.h"
#include "str.h"
#include "map.h"

#define STRING_INIT_SIZE    32

static void str_grow(struct str *s, size_t len)
{
    char *oldstr = s->str;
    s->alloc = len << 1;
    s->str = xmalloc(s->alloc);
    memcpy(s->str, oldstr, s->len);
    s->str[s->len] = '\0';
    if (s->len > 0)
        free(oldstr);
}

struct str * new_str()
{
    struct str *s = xmalloc(sizeof(struct str));
    s->len = 0;
    str_grow(s, STRING_INIT_SIZE);
    return s;
}

void free_str(struct str *s)
{
    if (!s)
        return;
    free(s->str);
    free(s);
}

size_t str_len(struct str *s)
{
    return s->len;
}

void str_add(struct str *s, struct str *s2)
{
    str_catn(s, s2->str, str_len(s2));
}

void str_cats(struct str *s, const char *src)
{
    str_catn(s, src, strlen(src));
}

void str_catn(struct str *s, const char *src, size_t len)
{
    if (s->len + len >= s->alloc)
        str_grow(s, s->len + len);
    
    char *dst = s->str + s->len;
    s->len += len;
    while (len-- > 0)
        *dst++ = *src++;
    *dst = '\0';
}

void str_catd(struct str *s, long n)
{
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
    
    if (n < 0)
        *--ps = '-';
    
    return str_catn(s, ps, str + sizeof (str) - ps);
}

void str_lstrip(struct str *s)
{
    char *p = s->str;
    while (s->len > 0 && isblank(*p)) {
        p++;
        s->len--;
    }
    memmove(s->str, p, s->len);
    s->str[s->len] = '\0';
}

void str_rstrip(struct str *s)
{
    while (s->len > 0 && isblank(s->str[s->len-1]))
        s->len--;
    s->str[s->len] = '\0';
}

void str_strip(struct str *s)
{
    str_lstrip(s);
    str_rstrip(s);
}

// key-value storage

char *strn(const char *str, size_t len)
{
    static struct map *map;
    char *dst;
    
    if (!str || len == 0)
        return NULL;
    
    if (!map)
        map = new_map(NULL);
    
    if ((dst = map_get(map, str)))
        return dst;
    
    dst = NEW0(len + 1);
    strncpy(dst, str, len);
    map_put(map, dst, dst);
    
    return dst;
}

char *strd(long d)
{
    char *ret;
    struct str *s = new_str();
    str_catd(s, d);
    ret = strn(s->str, str_len(s));
    free_str(s);
    return ret;
}

char *strs(const char *str)
{
    if (!str)
        return NULL;
    return strn(str, strlen(str));
}

char * stoa(struct str *s)
{
    if (s == NULL || s->len == 0)
        return NULL;
    
    return strs(s->str);
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
    return strs(r);
}
