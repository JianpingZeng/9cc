// Copyright 2012 Rui Ueyama. Released under the MIT license.
// Copyright 2015 Guiyang Huang. Released under the MIT license.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "test.h"

extern void testmain(void);

static void printfail()
{
    printf("\e[1;31mFAILED\e[0m\n");
}

void print(char *s)
{
    printf("Testing %s ...", s);
    fflush(stdout);
}

void ffail(char *file, int line, char *msg)
{
    printfail();
    printf("%s:%d: %s\n", file, line, msg);
    exit(1);
}

void fexpectb(char *file, int line, bool result)
{
    if (result)
        return;
    printfail();
    printf("%s:%d: expression got false", file, line);
    exit(1);
}

void fexpecti(char *file, int line, int a, int b)
{
    if (a == b)
        return;
    printfail();
    printf("%s:%d: %d expected, but got %d\n", file, line, a, b);
    exit(1);
}

void fexpectl(char *file, int line, long a, long b)
{
    if (a == b)
        return;
    printfail();
    printf("%s:%d: %ld expected, but got %ld\n", file, line, a, b);
    exit(1);
}

void fexpects(char *file, int line, const char *a, const char *b)
{
    if (!strcmp(a, b))
        return;
    printfail();
    printf("%s:%d: \"%s\" expected, but got \"%s\"\n", file, line, a, b);
    exit(1);
}

void fexpectf(char *file, int line, float a, float b)
{
    if (a == b)
        return;
    printfail();
    printf("%s:%d: %f expected, but got %f\n", file, line, a, b);
    exit(1);
}

void fexpectd(char *file, int line, double a, double b)
{
    if (a == b)
        return;
    printfail();
    printf("%s:%d: %lf expected, but got %lf\n", file, line, a, b);
    exit(1);
}

void fexpectp(char *file, int line, void *a, void *b)
{
    if (a == b)
        return;
    printfail();
    printf("%s:%d: %p expected, but got %p\n", file, line, a, b);
    exit(1);
}

int main()
{
    testmain();
    printf("\e[32mOK\e[0m\n");
    return 0;
}
