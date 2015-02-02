#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "lib.h"

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
