#include <stdlib.h>
#include "lex.h"
#include "libutils/utils.h"

struct strtab {
    char *str;
    size_t len;
    struct strtab *link;
};

static struct strtab *strtab[1024];

char *strn(const char *src, size_t len)
{
    struct strtab *p;
    unsigned int hash;
    char *dst;

    hash = strhashn(src, len) & (ARRAY_SIZE(strtab) - 1);
    for (p = strtab[hash]; p; p = p->link) {
        if (p->len == len && !memcmp(src, p->str, len))
            return p->str;
    }

    // alloc
    dst = xmalloc(len + 1);
    p = xmalloc(sizeof(struct strtab));
    p->str = dst;
    p->len = len;
    memcpy(dst, src, len);
    dst[len] = '\0';
    p->link = strtab[hash];
    strtab[hash] = p;
    return p->str;
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
