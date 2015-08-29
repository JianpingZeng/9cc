#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <ctype.h>
#include <stdarg.h>
#include "strbuf.h"
#include "utils.h"

#define STRING_INIT_SIZE    32

static void strbuf_grow(struct strbuf *s, size_t len)
{
    char *oldstr = s->str;
    s->alloc = len << 1;
    s->str = xmalloc(s->alloc);
    memcpy(s->str, oldstr, s->len);
    s->str[s->len] = '\0';
    if (s->len > 0)
        free(oldstr);
}

struct strbuf * strbuf_new()
{
    struct strbuf *s = xmalloc(sizeof(struct strbuf));
    s->len = 0;
    strbuf_grow(s, STRING_INIT_SIZE);
    return s;
}

void strbuf_free(struct strbuf *s)
{
    if (!s)
        return;
    free(s->str);
    free(s);
}

size_t strbuf_len(struct strbuf *s)
{
    return s->len;
}

void strbuf_add(struct strbuf *s, struct strbuf *s2)
{
    strbuf_catn(s, s2->str, strbuf_len(s2));
}

void strbuf_cats(struct strbuf *s, const char *src)
{
    strbuf_catn(s, src, strlen(src));
}

void strbuf_catn(struct strbuf *s, const char *src, size_t len)
{
    if (s->len + len >= s->alloc)
        strbuf_grow(s, s->len + len);
    
    char *dst = s->str + s->len;
    s->len += len;
    while (len-- > 0)
        *dst++ = *src++;
    *dst = '\0';
}

void strbuf_catd(struct strbuf *s, long n)
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
    
    return strbuf_catn(s, ps, str + sizeof (str) - ps);
}

void strbuf_lstrip(struct strbuf *s)
{
    char *p = s->str;
    while (s->len > 0 && isblank(*p)) {
        p++;
        s->len--;
    }
    memmove(s->str, p, s->len);
    s->str[s->len] = '\0';
}

void strbuf_rstrip(struct strbuf *s)
{
    while (s->len > 0 && isblank(s->str[s->len-1]))
        s->len--;
    s->str[s->len] = '\0';
}

void strbuf_strip(struct strbuf *s)
{
    strbuf_lstrip(s);
    strbuf_rstrip(s);
}
