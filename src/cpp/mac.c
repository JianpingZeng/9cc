#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

/*
 * Configurations for Mac OS X
 */

const char *cpp[] = {"/usr/bin/c99", "-E", 0};

int execv_cpp(const char *path, const char *argv[])
{
    pid_t pid;
    int ret = EXIT_SUCCESS;
    pid = fork();
    if (pid == 0) {
	// child process
	execv(path, argv);
	ret = EXIT_FAILURE;
    }
    else if (pid > 0) {
	// wait for
	int status;
	ret = waitpid(pid, &status, 0);
        if (ret != pid || !WIFEXITED(status) || WEXITSTATUS(status) != 0) {
	    ret = EXIT_FAILURE;
	}
    }
    else {
	perror("Can't fork");
	ret = EXIT_FAILURE;
    }

    return ret;
}
