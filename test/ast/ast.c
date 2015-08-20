// Copyright 2015 Guiyang Huang. Released under the MIT license.

#include "test.h"
#include <stdlib.h>
#include <string.h>
#include "utils.h"

static void run_script(const char *name)
{
    char *command;
    struct string *s = new_string();
    
    str_cats(s, "/usr/bin/python ");
    str_cats(s, "../test/ast/");
    str_cats(s, name);
    
    command = stoa(s);
    system(command);
}

void testmain()
{
    print("AST suite");
    run_script("ast.py");
    exit(0);    // don't print redundant 'OK'
}
