/*
 * Configurations for Linux
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/type.h>
#include <sys/wait.h>

const char *cpp[] = {"/usr/bin/gcc", "-E", 0};

int callsys(const char *path, char **argv)
{
    pid_t pid;
    int ret = EXIT_SUCCESS;
    pid = fork();
    if (pid == 0) {
	// explicit type cast to make compiler happy
	execv(path, (char * const *)argv);
	perror("execv");
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

int file_exists(const char *path)
{
    struct stat st;
    return stat(path, &st) == 0;
}
