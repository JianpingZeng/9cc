// system utilities for windows nt.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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

const char * mktmpdir(void)
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

int file_exists(const char *path)
{
    int ret = PathFileExists(path);
    printf("%s: %s: %d\n", __func__, path, ret);
    return ret;
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
    printf("%s\n", __func__);
    return 0;
}

int rmdir(const char *dir)
{
    int success = RemoveDirectory(dir);
    printf("rmdir: %d\n", success);
    return success ? 0 : -1;
}

const char *abspath(const char *path)
{
    char *ret = malloc(MAX_PATH);
    GetFullPathName(path, MAX_PATH, ret, NULL);
    printf("%s: path: %s, ret: %s\n", __func__, path, ret);
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
    printf("%s: %s\n", __func__, ret);
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

// process
int callsys(const char *file, char **argv)
{
    printf("%s\n", __func__);
    return 0;
}

int runproc(int (*proc) (void *), void *context)
{
    printf("%s\n", __func__);
    int ret = EXIT_SUCCESS;
    ret = proc(context);
    return ret;
}

// time
void set_localtime(const time_t * timep, struct tm *result)
{
    printf("%s\n", __func__);
}
