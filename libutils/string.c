#include "compat.h"
#include <limits.h>
#include <stdio.h>
#include <ctype.h>
#include "libutils.h"

#define FNV32_BASIS ((unsigned int) 0x811c9dc5)
#define FNV32_PRIME ((unsigned int) 0x01000193)

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
    return strdup(buf);
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
    return strndup(p1, p2 - p1);
}
