#include <string.h>
#include <assert.h>
#include <limits.h>
#include "lib.h"

static void str_grow(struct string *s, int len)
{
    char *mem = cc_malloc(len + s->reserve);
    memcpy(mem, s->str, s->size);
    cc_free(s->str);
    s->str = mem;
    s->capelems = len + s->reserve;
}

struct string * new_string()
{
    struct string *s = cc_malloc(sizeof(struct string));
    s->reserve = 128;
    s->size = 0;
    s->capelems = s->reserve;
    s->str = cc_malloc(s->capelems);
    return s;
}

void free_string(struct string *s)
{
    assert(s);
    cc_free(s->str);
    cc_free(s);
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

/**
 * str_to_array is different from stoa,
 * the caller needs to free the memory returned.
 */
char * str_to_array(struct string *s)
{
    assert(s);
    char *str = cc_malloc(s->size+1);
    memcpy(str, s->str, s->size);
    free_string(s);
    return str;
}
