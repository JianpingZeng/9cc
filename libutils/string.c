#include <limits.h>
#include <stdio.h>
#include <ctype.h>
#include "utils.h"

// FNV-1a
unsigned int strhash(const char *s)
{
    unsigned int hash = FNV32_BASIS;
    for (; *s; s++) {
        hash ^= *s;
        hash *= FNV32_PRIME;
    }
    return hash;
}

unsigned int strnhash(const char *s, size_t len)
{
    unsigned int hash = FNV32_BASIS;
    for (size_t i = 0; i < len; i++, s++) {
        hash ^= *s;
        hash *= FNV32_PRIME;
    }
    return hash;
}

char *format(const char *fmt, ...)
{
    char buf[BUFSIZ];
    va_list ap;
    va_start(ap, fmt);
    if (vsnprintf(buf, ARRAY_SIZE(buf), fmt, ap) >= ARRAY_SIZE(buf))
        die("buffer is too short to hold the result string");
    va_end(ap);
    return xstrdup(buf);
}

char *xstrdup(const char *str)
{
    char *ret = xmalloc(strlen(str) + 1);
    return strcpy(ret, str);
}

char *xstrndup(const char *str, size_t n)
{
    char *ret = xmalloc(n + 1);
    strncpy(ret, str, n);
    ret[n] = '\0';
    return ret;
}

bool has_prefix(const char *s, const char *prefix)
{
    if (!s || !prefix)
        return false;
    return !strncmp(s, prefix, strlen(prefix));
}

char *strip(const char *str)
{
    const char *p1 = str, *p2 = str + strlen(str);
    while (isblank(*p1))
        p1++;
    while (p2 > p1 && isblank(p2[-1]))
        p2--;
    return xstrndup(p1, p2 - p1);
}
