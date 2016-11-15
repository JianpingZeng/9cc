#include <limits.h>
#include <stdio.h>
#include "utils.h"

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

unsigned int strhashn(const char *s, size_t len)
{
    unsigned int hash = FNV32_BASIS;
    for (size_t i = 0; i < len; i++, s++) {
        hash ^= *s;
        hash *= FNV32_PRIME;
    }
    return hash;
}

char *vformat(const char *fmt, va_list ap)
{
    size_t size = 128;
    void *buffer = NULL;
    va_list aq;
    for (;;) {
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
    return r;
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
