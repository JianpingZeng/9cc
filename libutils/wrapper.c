#include <stdio.h>
#include <stdlib.h>
#include "libutils.h"

void die(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    fprintf(stderr, "die: ");
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    va_end(ap);
    exit(EXIT_FAILURE);
}

void dlog(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    va_end(ap);
}

void dwarn(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    fprintf(stderr, "warning: ");
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    va_end(ap);
}

void derror(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    fprintf(stderr, "error: ");
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    va_end(ap);
}

void *xmalloc(size_t size)
{
    void *p = malloc(size);
    if (!p)
        die("memory exhausted");
    return p;
}

void *xcalloc(size_t count, size_t size)
{
    void *p = calloc(count, size);
    if (!p)
        die("memory exhausted");
    return p;
}

void *xrealloc(void *ptr, size_t size)
{
    void *p = realloc(ptr, size);
    if (!p)
        die("memory exhausted");
    return p;
}

int log2i(size_t i)
{
    if (i == 0) {
        return -1;
    } else {
        int r = 0;
        while ((i & 0x01) == 0) {
            r++;
            i >>= 1;
        }
        return i >> 1 ? -1 : r;
    }
}

size_t length(void *array)
{
    size_t i = 0;
    void **a = (void **)array;
    if (array == NULL)
        return 0;
    while (a[i])
        i++;
    return i;
}
