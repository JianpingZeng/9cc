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
    
}

int file_exists(const char *path)
{
    
}

int file_size(const char *path)
{
    
}

int isdir(const char *path)
{
    
}

int rmdir(const char *dir)
{
    
}

const char *abspath(const char *path)
{
    
}

const char *replace_suffix(const char *path, const char *suffix)
{
    
}

const char *join(const char *dir, const char *name)
{
    
}

char *dirname(const char *path)
{
    
}

char *basename(const char *path)
{
    
}

// process
int callsys(const char *file, char **argv)
{
    
}

int runproc(int (*proc) (void *), void *context)
{
    
}

// time
void set_localtime(const time_t * timep, struct tm *result)
{
    
}
