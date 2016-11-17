#include "cc.h"

char debug[128];

void debug_init(int argc, char *argv[])
{
    for (int i = 1; i < argc; i++) {
        const char *arg = argv[i];
        if (!strcmp(arg, "-debugd"))
            // developer verbose mode
            debug['d'] = true;
    }
}

void debug_exit(void)
{
    if (debug['d'])
        cpp_dump(cpp_file);
}
