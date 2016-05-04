#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <time.h>
#include <errno.h>
// windows headers
#include <windows.h>
#include <rpc.h>
#include <shlwapi.h>

char *ld[] = {"link", NULL};
char *as[] = {"masm", NULL};

void setup_sys()
{
    
}

const char *mktmpdir()
{
    static char s[MAX_PATH];
    if (!GetTempPath(MAX_PATH, s))
        return NULL;
    
    UUID uuid;
    UuidCreate(&uuid);
    char *str;
    UuidToStringA(&uuid, (RPC_CSTR *)&str);
    strcat(s, str);
    RpcStringFreeA((RPC_CSTR *)&str);
    return s;
}

int callsys(const char *file, char **argv)
{
    
}

int runproc(int (*proc) (void *), void *context)
{
    
}

int file_exists(const char *path)
{
    return PathFileExists(path);
}

int file_size(const char *path)
{
    HANDLE hfile = OpenFile(path, NULL, OF_READ);
    LARGE_INTEGER size;
    if (GetFileSizeEx(hfile, &size)) {
        CloseHandle(hfile);
        return size.QuadPart;
    } else {
        CloseHandle(hfile);
        return 0;
    }
}

int isdir(const char *path)
{
    
}

int rmdir(const char *dir)
{
    int success = RemoveDirectory(dir);
    return success ? 0 : -1;
}

const char *abspath(const char *path)
{
    char *ret = malloc(MAX_PATH);
    GetFullPathName(path, MAX_PATH, ret, NULL);
    return ret;
}

const char *replace_suffix(const char *path, const char *suffix)
{
    int len = strlen(path) + strlen(suffix) + 1;
    char *ret = malloc(len);
    char *dot = strrchr(path, '.');
    if (dot) {
        int i = dot - path;
        strncpy(ret, path, i);
        ret[i] = '.';
        strncpy(ret+i+1, suffix, strlen(suffix));
    } else {
        strcpy(ret, path);
        ret[strlen(path)] = '.';
        strcpy(ret+strlen(path)+1, suffix);
    }
    ret[len] = '\0';
    return ret;
}

const char *join(const char *dir, const char *name)
{
    char *ret = malloc(MAX_PATH);
    PathCombine(ret, dir, name);
    return ret;
}

char *dirname(const char *path)
{
    char *name = strrchr(path, '\\');
    if (name) {
        int len = name - path;
        char *path = malloc(len+1);
        strncpy(path, path, len);
        path[len] = '\0';
        return path;
    } else {
        return strdup(path);
    }
}

char *basename(const char *path)
{
    char *name = strrchr(path, '\\');
    if (name) {
        return strdup(name+1);
    } else {
        return strdup(path);
    }
}

const char *file_suffix(const char *path)
{
    const char *dot = strrchr(path, '.');
    if (!dot || *dot == '\0')
        return NULL;
    return dot + 1;
}

void set_localtime(const time_t * timep, struct tm *result)
{
    
}
