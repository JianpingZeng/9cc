#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <assert.h>
#include "lib.h"

static struct string {
    char *str;
    int len;
    struct string *next;
} *buckets[1024];

void * allocate(unsigned long size, int flags)
{
    void *p = malloc(size);
    assert(p);
    memset(p, 0, size);
    return p;
}

void deallocate(void *p)
{
    if (p) free(p);
}

const char *stringn(const char *src, int len)
{
    struct string *ps;
    register unsigned int hash;
    register unsigned char *p;
    const char *end = src + len;
    
    if (src == NULL) {
        return NULL;
    }
    
    for(hash = 0, p = (unsigned char *)src; *p ; p++)
        hash = 31 * hash + *p;
    
    hash %= sizeof(buckets)/sizeof(buckets[0]) - 1;
    for (ps = buckets[hash]; ps; ps = ps->next) {
        if (ps->len == len) {
            const char *s1 = src;
            char *s2 = ps->str;
            do {
                if (s1 == end) {
                    return ps->str;
                }
            } while (*s1++ == *s2++);
        }
    }
    
    // alloc
    {
        char *dst = (char *) allocate(len+1, 0);
        ps = new(struct string);
        ps->len = len;
        for (ps->str = dst; src < end; ) {
            *dst++ = *src++;
        }
        *dst++ = 0;
        ps->next = buckets[hash];
        buckets[hash] = ps;
        
        return ps->str;
    }
}

const char *strings(const char *str)
{
    const char *s = str;
    while (*s) {
        s++;
    }
    return stringn(str, s - str);
}

const char *stringd(long n)
{
    char str[25], *s = str + sizeof (str);
	unsigned long m;
    
	if (n == LONG_MIN)
		m = (unsigned long)LONG_MAX + 1;
	else if (n < 0)
		m = -n;
	else
		m = n;
	do
		*--s = m%10 + '0';
	while ((m /= 10) != 0);
	if (n < 0)
		*--s = '-';
	return stringn(s, str + sizeof (str) - s);
}

// create temp string
void appendstring(const char **string, const char *src, int len)
{
    assert(string);
    if (len <= 0) {
        return;
    }
    if (*string) {
        long oldlen = strlen(*string);
        long total = oldlen + len + 1;
        char *dst = realloc((void *) (*string), total);
        dst[total-1] = 0;
        memcpy(dst+oldlen, src, len);
        *string = dst;
    }
    else {
        char *dst = allocate(len+1, 0);
        memcpy(dst, src, len);
        dst[len] = 0;
        *string = dst;
    }
}
