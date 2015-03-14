#include "cc.h"

void * cc_malloc(size_t size)
{
    void *p = malloc(size);
    if (!p)
	die("Can't malloc");
    memset(p, 0, size);
    return p;
}

void cc_free(void *p)
{
    free(p);
}

void die(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    va_end(ap);
    exit(EXIT_FAILURE);
}
