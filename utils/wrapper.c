#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <stdbool.h>
#include "utils.h"

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

void println(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
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

void *zmalloc(size_t size)
{
    return memset(xmalloc(size), 0, size);
}
