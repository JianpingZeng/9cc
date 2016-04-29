// Copyright 2015 Guiyang Huang. Released under the MIT license.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <time.h>
#include "test.h"

extern void testmain(void);

void ffail(char *file, int line, char *msg, ...)
{
    printf("\e[0;31mFAIL:\e[0m");
    va_list ap;
    va_start(ap, msg);
    printf("%s:%d: ", file, line);
    vprintf(msg, ap);
    printf("\n");
    va_end(ap);
    exit(1);
}

void fexpectb(char *file, int line, bool result)
{
    if (result)
        return;
    ffail(file, line, "expression got false");
    exit(1);
}

void fexpecti(char *file, int line, long a, long b)
{
    if (a == b)
        return;
    ffail(file, line, "%ld expected, but got %ld\n", b, a);
    exit(1);
}

void fexpectu(char *file, int line, unsigned long a, unsigned long b)
{
    if (a == b)
        return;
    ffail(file, line, "%lu expected, but got %lu\n", b, a);
    exit(1);
}

void fexpects(char *file, int line, const char *a, const char *b)
{
    if (!strcmp(a, b))
        return;
    ffail(file, line, "\"%s\" expected, but got \"%s\"\n", b, a);
    exit(1);
}

void fexpectf(char *file, int line, float a, float b)
{
    if (a == b)
        return;
    ffail(file, line, "%f expected, but got %f\n", b, a);
    exit(1);
}

void fexpectd(char *file, int line, double a, double b)
{
    if (a == b)
        return;
    ffail(file, line, "%lf expected, but got %lf\n", b, a);
    exit(1);
}

void fexpectp(char *file, int line, void *a, void *b)
{
    if (a == b)
        return;
    ffail(file, line, "%p expected, but got %p\n", b, a);
    exit(1);
}

int main()
{
    testmain();
    printf("\e[0;32mPASS\e[0m\n");
    return 0;
}
