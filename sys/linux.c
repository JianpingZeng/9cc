#define _BSD_SOURCE
#define UNW_LOCAL_ONLY
#include <libunwind.h>
#include "../config.h"
#include "../utils/utils.h"

/**
 * $0: output file
 * $1: input files
 * $2: additional options
 */

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

char *as[] = { "as", "-o", "$0", "$1", "$2", NULL };
char *cc[] = { "cc1", "$1", "$2", "-o", "$0", NULL };

struct vector *sys_include_dirs(void)
{
    struct vector *v = vec_new();
    vec_push(v, BUILD_DIR "/include");
    vec_push(v, "/usr/include");
    vec_push(v, "/usr/include/linux");
    vec_push(v, "/usr/include/x86_64-linux-gnu");
    return v;
}

static void handler(int sig)
{
    unw_cursor_t cursor;
    unw_context_t uc;
    unw_word_t offp;
    char buf[128];
    int i = 0;

    fprintf(stderr, "Stack trace:\n");
    unw_getcontext(&uc);
    unw_init_local(&cursor, &uc);
    
    while (unw_step(&cursor) > 0) {
        int ret = unw_get_proc_name(&cursor, buf, sizeof buf/sizeof buf[0], &offp);
        if (ret == 0) {
            printf("%d\t%s + %ld\n", i, buf, offp);
        } else {
            printf("%d\t???\n", i);
        }
        i++;
    }
    
    exit(EXIT_FAILURE);
}

void setup_sys()
{
    signal(SIGSEGV, handler);
    signal(SIGABRT, handler);
}
