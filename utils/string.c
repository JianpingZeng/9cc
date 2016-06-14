#include "utils.h"

#define FNV32_BASIS ((unsigned) 0x811c9dc5)
#define FNV32_PRIME ((unsigned) 0x01000193)

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

static unsigned strhashn(const char *s, size_t len)
{
    unsigned hash = HASHSTEP(0, *s);
    for (size_t i = 0; i < len; i++)
        hash = HASHSTEP(hash, *++s);
    hash = HASHFINISH(hash, len);
    return hash;
}

char *strnh(const char *src, size_t len, unsigned int hash)
{
    static struct str_table *table;
    struct str_bucket *ps;

    if (src == NULL || len <= 0)
        return NULL;

    if (!table)
        table = zmalloc(sizeof(struct str_table));

    hash = hash & (ARRAY_SIZE(table->buckets) - 1);
    for (ps = table->buckets[hash]; ps; ps = ps->next) {
        if (ps->len == len &&
            !memcmp(src, ps->str, len))
            return ps->str;
    }

    // alloc
    char *dst = xmalloc(len + 1);
    ps = xmalloc(sizeof(struct str_bucket));
    ps->str = dst;
    ps->len = len;
    memcpy(dst, src, len);
    dst[len] = '\0';
    ps->next = table->buckets[hash];
    table->buckets[hash] = ps;

    return ps->str;
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
        table = zmalloc(sizeof(struct str_table));

    hash = strhashn(src, len) & (ARRAY_SIZE(table->buckets) - 1);
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
        ps = zmalloc(sizeof(struct str_bucket));
        ps->len = len;
        for (ps->str = dst; src < end;)
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

char *strd(long long n)
{
    char str[32], *s = str + sizeof(str);
    unsigned long long m;

    if (n == LLONG_MIN)
        m = (unsigned long long)LLONG_MAX + 1;
    else if (n < 0)
        m = -n;
    else
        m = n;

    do {
        *--s = m % 10 + '0';
    } while ((m /= 10) != 0);

    if (n < 0)
        *--s = '-';

    return strn(s, str + sizeof(str) - s);
}

char *stru(unsigned long long n)
{
    char str[32], *s = str + sizeof(str);
    unsigned long long m = n;

    do {
        *--s = m % 10 + '0';
    } while ((m /= 10) != 0);

    return strn(s, str + sizeof(str) - s);
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
    strcpy(ret, str);
    return ret;
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
