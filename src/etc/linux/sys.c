#include <unistd.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/wait.h>

const char *cpp[] = {"/usr/bin/gcc",
		     "-U__GNUC__",
		     "-Iinclude",
		     "-E", 0};

static char template[] = "/tmp/mcc.temp.XXXXXXXXXX";

char *mk_temp_dir()
{
    return mkdtemp(template);
}

int file_exists(const char *path)
{
    struct stat st;
    return stat(path, &st) == 0;
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
    }
    else if (pid > 0) {
	// wait for
	int status;
	int retpid = waitpid(pid, &status, 0);
        if (retpid != pid || !WIFEXITED(status) || WEXITSTATUS(status) != 0) {
	    ret = EXIT_FAILURE;
	}
    }
    else {
	perror("Can't fork");
	ret = EXIT_FAILURE;
    }

    return ret;
}

char *replace_suffix(const char *path, const char *suffix)
{
    assert(path && suffix);
    int dot_index = -1;
    int len = strlen(path);
    char *p;

    for (int i=len-1; i >= 0; i--) {
	if (path[i] == '/') {
	    break;
	}
	if (path[i] == '.') {
	    dot_index = i;
	    break;
	}
    }

    if (dot_index != -1) {
	len = dot_index;
    }

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

char *expanduser(char *path)
{
    if (!path || !strlen(path)) return NULL;
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
