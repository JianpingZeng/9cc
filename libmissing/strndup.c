#include <stdlib.h>
#include <string.h>

char *strndup(const char *s)
{
    char *d = malloc(n + 1);
    strncpy(d, s, n);
    d[n] = '\0';
    return d;
}
