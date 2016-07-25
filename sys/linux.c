#define _BSD_SOURCE
#include "../config.h"
#include "../utils/utils.h"

/**
 * $0: output file
 * $1: input files
 * $2: additional options
 */

#ifdef CONFIG_LIB64

#define CRT_DIR  "/usr/lib64/"

#else

#define CRT_DIR  "/usr/lib/x86_64-linux-gnu/"

#endif

char *ld[] = {
    "ld",
    "-A", "x86_64",
    "-o", "$0",
    "-dynamic-linker", "/lib64/ld-linux-x86-64.so.2",
    CRT_DIR "crt1.o",
    CRT_DIR "crti.o",
    "$1", "$2",
    "-lc", "-lm",
    CRT_DIR "crtn.o",
    NULL
};
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
