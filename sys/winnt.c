// system utilities for windows nt.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <time.h>
#include <errno.h>

char *ld[] = {"cl", NULL};
char *as[] = {"cl", NULL};

void setup_sys()
{
    
}

const char * mktmpdir(void)
{
    return NULL;
}

int file_exists(const char *path)
{
    return 0;
}

int file_size(const char *path)
{
    return 0;
}

int isdir(const char *path)
{
    return 0;
}

int rmdir(const char *dir)
{
    return 0;
}

const char *abspath(const char *path)
{
    return NULL;
}

const char *replace_suffix(const char *path, const char *suffix)
{
    return NULL;
}

const char *join(const char *dir, const char *name)
{
    return NULL;
}

char *dirname(const char *path)
{
    return NULL;
}

char *basename(const char *path)
{
    return NULL;
}

// process
int callsys(const char *file, char **argv)
{
    return 0;
}

int runproc(int (*proc) (void *), void *context)
{
    return 0;
}

// time
void set_localtime(const time_t * timep, struct tm *result)
{
    
}
