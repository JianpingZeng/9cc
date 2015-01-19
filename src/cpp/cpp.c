#include <stdio.h>
#include <stdlib.h>
#include "cpp.h"

static const char ** concat(int argc, char *argv[], const char **cpp)
{
    return cpp+1;
}

int main(int argc, char *argv[])
{
    const char **list = concat(argc, argv, cpp);
    int ret = execv_cpp(cpp[0], list);
    
    return ret;
}
