#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <errno.h>
// windows headers
#include <windows.h>
#include <rpc.h>
#include <shlwapi.h>
#include <process.h>
#include <io.h>
#include <share.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "../config.h"

char *ld[] = {"link", NULL};
char *as[] = {"masm", NULL};
char *cc[] = { "cc1.exe", "$1", "$2", "-o", "$0", NULL };

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
    int ret = EXIT_SUCCESS;
    int status = _spawnvp(_P_WAIT, file, (const char *const *)argv);
    if (status == -1) {
        perror("Can't _spawnvp");
        ret = EXIT_FAILURE;
    }
    return ret;
}

int file_exists(const char *path)
{
    return PathFileExists(path);
}

long file_size(const char *path)
{
    int fd;
    long size;
    _sopen_s(&fd, path, _O_RDONLY, _SH_DENYWR, _S_IREAD);
    if (fd == -1)
        return 0;
    size = _filelengthi64(fd);
    if (size == -1) {
        _close(fd);
        return 0;
    }
    _close(fd);
    return size;
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

static struct vector *split_paths(const char *path)
{
    struct vector *v = vec_new();
    const char *s = path;
    const char *b = s;
    while (*s) {
        if (*s == ';') {
            int size = s - b + 1;
            char *d = malloc(size);
            strncpy(d, b, size - 1);
            d[size - 1] = '\0';
            vec_push(v, d);
            b = ++s;
        } else {
            s++;
        }
    }
    return v;
}

struct vector *sys_include_dirs(void)
{
    struct vector *v = vec_new();
    vec_push(v, BUILD_DIR "\\include");
    vec_add(v, split_paths(NT_INCLUDE_DIR));
    return v;
}
