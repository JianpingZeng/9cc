#include "test.h"

static void invoke_script()
{
    system("/usr/bin/python test/scripts/cpp.py");
}

void testmain()
{
    START("cpp ...");
    printf("\n");
    invoke_script();
    exit(0);
}
