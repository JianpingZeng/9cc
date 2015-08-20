// Copyright 2015 Guiyang Huang. Released under the MIT license.

#include "test.h"
#include <stdlib.h>
#include <string.h>
#include "utils.h"

static void run_script(const char *name)
{
    int ret;
    char *command;
    struct string *s = new_string();
    
    str_cats(s, "/usr/bin/python ");
    str_cats(s, "../test/ast/");
    str_cats(s, name);
    
    command = stoa(s);
    ret = system(command);
    if (ret != EXIT_SUCCESS)
        fail("script failed");
}

void testmain()
{
    print("AST");
    run_script("ast.py");
}
