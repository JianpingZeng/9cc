#define _BSD_SOURCE
#include "../config.h"
#include "../utils/utils.h"

/**
 * $0: output file
 * $1: input files
 * $2: additional options
 */

#ifdef CONFIG_UBUNTU

char *ld[] = {
    "ld",
    "-A", "x86_64",
    "-o", "$0",
    "-dynamic-linker", "/lib64/ld-linux-x86-64.so.2",
    "/usr/lib/x86_64-linux-gnu/crt1.o",
    "/usr/lib/x86_64-linux-gnu/crti.o",
    "$1", "$2",
    "-lc", "-lm",
    "/usr/lib/x86_64-linux-gnu/crtn.o",
    NULL
};

#else

char *ld[] = {
    "ld",
    "-A", "x86_64",
    "-o", "$0",
    "-dynamic-linker", "/lib64/ld-linux-x86-64.so.2",
    "/usr/lib64/crt1.o",
    "/usr/lib64/crti.o",
    "$1", "$2",
    "-lc", "-lm",
    "/usr/lib64/crtn.o",
    NULL
};

#endif

char *as[] = { "as", "-o", "$0", "$1", "$2", NULL };
char *cc[] = { "./cc1", "$1", "$2", "-o", "$0", NULL };

struct vector *sys_include_dirs(void)
{
    struct vector *v = vec_new();
    vec_push(v, BUILD_DIR "/include");
    vec_push(v, "/usr/include");
    vec_push(v, "/usr/include/linux");
    vec_push(v, "/usr/include/x86_64-linux-gnu");
    return v;
}

void setup_sys()
{
}
