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

void fexpectb(char *file, int line, char *msg, bool result)
{
    if (result)
        return;
    ffail(file, line, "expression got false: %s", msg);
    exit(1);
}

void fexpecti(char *file, int line, char *msg, long a, long b)
{
    if (a == b)
        return;
    ffail(file, line, "%ld expected, but got %ld: %s", b, a, msg);
    exit(1);
}

void fexpectu(char *file, int line, char *msg, unsigned long a, unsigned long b)
{
    if (a == b)
        return;
    ffail(file, line, "%lu expected, but got %lu: %s", b, a, msg);
    exit(1);
}

void fexpects(char *file, int line, char *msg, const char *a, const char *b)
{
    if (!strcmp(a, b))
        return;
    ffail(file, line, "\"%s\" expected, but got \"%s\": %s", b, a, msg);
    exit(1);
}

void fexpectf(char *file, int line, char *msg, float a, float b)
{
    if (a == b)
        return;
    ffail(file, line, "%f expected, but got %f: %s", b, a, msg);
    exit(1);
}

void fexpectd(char *file, int line, char *msg, double a, double b)
{
    if (a == b)
        return;
    ffail(file, line, "%lf expected, but got %lf: %s", b, a, msg);
    exit(1);
}

void fexpectp(char *file, int line, char *msg, void *a, void *b)
{
    if (a == b)
        return;
    ffail(file, line, "%p expected, but got %p: %s", b, a, msg);
    exit(1);
}

int main()
{
    testmain();
    printf("\e[0;32mPASS\e[0m\n");
    return 0;
}
