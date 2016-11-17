#include <stdlib.h>
#include <string.h>

char *strdup(const char *s)
{
    char *d = malloc(strlen(s) + 1);
    return strcpy(d, s);
}
