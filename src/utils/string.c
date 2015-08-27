#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <ctype.h>
#include "utils.h"

#define STRING_INIT_SIZE    32

static void str_grow(struct string *s, size_t len)
{
    char *oldstr = s->str;
    s->alloc = len << 1;
    s->str = xmalloc(s->alloc);
    memcpy(s->str, oldstr, s->len);
    s->str[s->len] = '\0';
    if (s->len > 0)
        free(oldstr);
}

struct string * new_string()
{
    struct string *s = xmalloc(sizeof(struct string));
    s->len = 0;
    str_grow(s, STRING_INIT_SIZE);
    return s;
}

void free_string(struct string *s)
{
    if (!s)
        return;
    free(s->str);
    free(s);
}

size_t str_len(struct string *s)
{
    return s->len;
}

void str_add(struct string *s, struct string *s2)
{
    str_catn(s, s2->str, str_len(s2));
}

void str_cats(struct string *s, const char *src)
{
    str_catn(s, src, strlen(src));
}

void str_catn(struct string *s, const char *src, size_t len)
{
    if (s->len + len >= s->alloc)
        str_grow(s, s->len + len);
    
    char *dst = s->str + s->len;
    s->len += len;
    while (len-- > 0)
        *dst++ = *src++;
    *dst = '\0';
}

void str_catd(struct string *s, long n)
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

void str_lstrip(struct string *s)
{
    char *p = s->str;
    while (s->len > 0 && isblank(*p)) {
        p++;
        s->len--;
    }
    memmove(s->str, p, s->len);
    s->str[s->len] = '\0';
}

void str_rstrip(struct string *s)
{
    while (s->len > 0 && isblank(s->str[s->len-1]))
        s->len--;
    s->str[s->len] = '\0';
}

void str_strip(struct string *s)
{
    str_lstrip(s);
    str_rstrip(s);
}
