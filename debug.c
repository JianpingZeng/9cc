#include "cc.h"

char debug[128];

void debug_init(int argc, char *argv[])
{
    for (int i = 1; i < argc; i++) {
        const char *arg = argv[i];
        if (!strcmp(arg, "-debugv"))
            // developer verbose mode
            debug['v'] += 1;
        else if (!strcmp(arg, "-debugS"))
            // Statistics
            debug['S'] += 1;
    }
}

void debug_exit(void)
{
    if (debug['S'])
        cpp_dump(cpp_file);
}
