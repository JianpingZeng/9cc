#include <stdlib.h>
#include "lex.h"
#include "libutils/utils.h"

struct strtab {
    struct strbucket {
        char *str;
        size_t len;
        struct strbucket *next;
    } *buckets[1024];
};

char *strn(const char *src, size_t len)
{
    static struct strtab *table;
    struct strbucket *ps;
    unsigned int hash;

    if (src == NULL || len <= 0)
        return NULL;

    if (!table)
        table = zmalloc(sizeof(struct strtab));

    hash = strhashn(src, len);
    hash = hash & (ARRAY_SIZE(table->buckets) - 1);
    for (ps = table->buckets[hash]; ps; ps = ps->next) {
        if (ps->len == len &&
            !memcmp(src, ps->str, len))
            return ps->str;
    }

    // alloc
    char *dst = xmalloc(len + 1);
    ps = xmalloc(sizeof(struct strbucket));
    ps->str = dst;
    ps->len = len;
    memcpy(dst, src, len);
    dst[len] = '\0';
    ps->next = table->buckets[hash];
    table->buckets[hash] = ps;

    return ps->str;
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
    char str[32], *s = str + sizeof(str);
    unsigned long m;

    if (n == LONG_MIN)
        m = (unsigned long)LONG_MAX + 1;
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

char *stru(unsigned long n)
{
    char str[32], *s = str + sizeof(str);
    unsigned long m = n;

    do {
        *--s = m % 10 + '0';
    } while ((m /= 10) != 0);

    return strn(s, str + sizeof(str) - s);
}
