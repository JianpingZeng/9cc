#include <unistd.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/wait.h>
// trace
#include <execinfo.h>
#include <signal.h>

static char template[] = "/tmp/mcc.temp.XXXXXXXXXX";

static void handler(int sig)
{
    void *array[20];
    size_t size;

    size = backtrace(array, sizeof array / sizeof array[0]);

    fprintf(stderr, "Stack trace:\n");
    backtrace_symbols_fd(array, size, STDERR_FILENO);
    exit(EXIT_FAILURE);
}

void setup_sys()
{
    signal(SIGSEGV, handler);
    signal(SIGABRT, handler);
}

char *mktmpdir()
{
    return mkdtemp(template);
}

int file_exists(const char *path)
{
    struct stat st;
    return stat(path, &st) == 0;
}

int isdir(const char *path)
{
    if (path == NULL)
        return 0;
    struct stat st;
    if (stat(path, &st) != 0)
        return 0;
    return S_ISDIR(st.st_mode);
}

int callsys(const char *path, char **argv)
{
    pid_t pid;
    int ret = EXIT_SUCCESS;
    pid = fork();
    if (pid == 0) {
        // child process
        execv(path, argv);
        fprintf(stderr, "%s: %s\n", strerror(errno), path);
        exit(EXIT_FAILURE);
    } else if (pid > 0) {
        int status;
        int n;
        while ((n = waitpid(pid, &status, 0)) != pid ||
               (n == -1 && errno == EINTR))
            ; // may be EINTR by a signal, so loop it.
        if (n != pid || !WIFEXITED(status) || WEXITSTATUS(status) != 0) {
            ret = EXIT_FAILURE;
        }
    } else {
        perror("Can't fork");
        ret = EXIT_FAILURE;
    }
    
    return ret;
}

int runproc(int (*proc) (void *), void *context)
{
    pid_t pid;
    int ret = EXIT_SUCCESS;
    pid = fork();
    if (pid == 0) {
        // child process
        exit(proc(context));
    } else if (pid > 0) {
        int status;
        int n;
        while ((n = waitpid(pid, &status, 0)) != pid ||
               (n == -1 && errno == EINTR))
            ; // may be EINTR by a signal, so loop it.
        if (n != pid || !WIFEXITED(status) || WEXITSTATUS(status) != 0) {
            ret = EXIT_FAILURE;
        }
    } else {
        perror("Can't fork");
        ret = EXIT_FAILURE;
    }
    
    return ret;
}

/* TODO:
 *  Functions below are quick and dirty, not robust at all.
 *  
 */

char *replace_suffix(const char *path, const char *suffix)
{
    assert(path && suffix);
    int dot_index = -1;
    int len = strlen(path);
    char *p;
    
    for (int i=len-1; i >= 0; i--) {
        if (path[i] == '/')
            break;
        if (path[i] == '.') {
            dot_index = i;
            break;
        }
    }
    
    if (dot_index != -1)
        len = dot_index;
    
    p = malloc(len+strlen(suffix)+2);
    memcpy(p, path, len);
    p[len] = '.';
    memcpy(p+len+1, suffix, strlen(suffix));
    p[len+1+strlen(suffix)] = 0;
    return p;
}

int rmdir(const char *dir)
{
    char command[64];
    snprintf(command, sizeof(command), "rm -rf %s", dir);
    return system(command);
}

char *abspath(char *path)
{
    if (!path || !strlen(path))
        return NULL;
    if (path[0] == '~') {
        const char *home = getenv("HOME");
        int hlen = strlen(home);
        int plen = strlen(path);
        char *ret = malloc(hlen+plen);
        strncpy(ret, home, hlen);
        strncpy(ret+hlen, path+1, plen-1);
        ret[hlen+plen-1] = 0;
        return ret;
    }
    else {
        int len = strlen(path);
        char *ret = malloc(len+1);
        strcpy(ret, path);
        ret[len] = 0;
        return ret;
    }
}

const char *join(const char *dir, const char *name)
{
    size_t len1 = strlen(dir);
    size_t len2 = strlen(name);
    int i = 0;
    char *p;
    
    if (dir[len1 - 1] == '/')
        len1--;
    if (name[i] == '/') {
        i++;
        len2--;
    }
    
    p = malloc(len1 + len2 + 2);
    strncpy(p, dir, len1);
    p[len1] = '/';
    strncpy(p + len1 + 1, name + i, len2);
    p[len1 + 1 + len2] = '\0';
    
    return p;
}
