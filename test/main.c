// Copyright 2012 Rui Ueyama. Released under the MIT license.
// Copyright 2015 Guiyang Huang. Released under the MIT license.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "test.h"
#include "sys.h"

extern void testmain(void);

static void printfail()
{
    printf("\e[1;31mFAIL\e[0m\n");
}

void printstart(const char *name)
{
    printf("%-30s", name);
    fflush(stdout);
}

void ffail(char *file, int line, char *msg, ...)
{
    va_list ap;
    printfail();
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
    printfail();
    printf("%s:%d: expression got false", file, line);
    exit(1);
}

void fexpecti(char *file, int line, int a, int b)
{
    if (a == b)
        return;
    printfail();
    printf("%s:%d: %d expected, but got %d\n", file, line, b, a);
    exit(1);
}

void fexpectl(char *file, int line, long a, long b)
{
    if (a == b)
        return;
    printfail();
    printf("%s:%d: %ld expected, but got %ld\n", file, line, b, a);
    exit(1);
}

void fexpects(char *file, int line, const char *a, const char *b)
{
    if (!strcmp(a, b))
        return;
    printfail();
    printf("%s:%d: \"%s\" expected, but got \"%s\"\n", file, line, b, a);
    exit(1);
}

void fexpectf(char *file, int line, float a, float b)
{
    if (a == b)
        return;
    printfail();
    printf("%s:%d: %f expected, but got %f\n", file, line, b, a);
    exit(1);
}

void fexpectd(char *file, int line, double a, double b)
{
    if (a == b)
        return;
    printfail();
    printf("%s:%d: %lf expected, but got %lf\n", file, line, b, a);
    exit(1);
}

void fexpectp(char *file, int line, void *a, void *b)
{
    if (a == b)
        return;
    printfail();
    printf("%s:%d: %p expected, but got %p\n", file, line, b, a);
    exit(1);
}

int main()
{
    setup_sys();
    printf("%s", "Testing ");
    testmain();
    printf("\e[32mPASS\e[0m\n");
    return 0;
}
