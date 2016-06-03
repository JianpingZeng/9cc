#define _BSD_SOURCE
#include <unistd.h>
// trace
#include <execinfo.h>
#include <signal.h>
#include "../utils/utils.h"

/**
 * $0: output file
 * $1: input files
 * $2: additional options
 */

char *ld[] = {
    "ld",
    "-o", "$0",
    "$1", "$2",
    "-lc", "-lm",
    "-macosx_version_min", OSX_SDK_VERSION,
    "-arch", "x86_64",
    NULL
};

char *as[] = { "as", "-o", "$0", "$1", "$2", NULL };
char *cc[] = { "cc1", "$1", "$2", "-o", "$0", NULL };

struct vector *sys_include_dirs(void)
{
    struct vector *v = vec_new();
    vec_push(v, BUILD_DIR "/include");
    vec_push(v, XCODE_DIR "/usr/include");
    return v;
}

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
