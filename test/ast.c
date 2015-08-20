// Copyright 2015 Guiyang Huang. Released under the MIT license.

#include "ast.h"
#include "sys.h"
#include <unistd.h>

extern int cc_main(int argc, char *argv[]);
extern struct node *root;

static char *dir;
static FILE *fp;

static char * write_to_file(const char *code)
{
    char *file;
    struct string *s = new_string();
    int len = strlen(code);
    
    if (!(dir = mk_temp_dir()))
        fail("Can't make temp directory");
    
    str_cats(s, dir);
    str_cats(s, "/1.c");
    
    file = stoa(s);
    if (!(fp = fopen(file, "w")))
        fail("Can't open file for writting");
    
    if (fwrite(code, 1, len, fp) != len)
        fail("Not completly written");
    
    return file;
}

static void run_cc(int argc, char *argv[])
{
    FILE *fp1, *fp2;
    int r, ret;
    
    r = dup(fileno(stderr));
    
    if (!(fp1 = freopen("/dev/null", "w", stderr)))
        fail("Can't redirect stderr to /dev/null");

    ret = cc_main(4, argv);
    
    fclose(fp1);
    
    if (!(fp2 = fdopen(r, "w")))
        fail("Can't restore stderr");
    
    stderr = fp2;
    
    if (ret != EXIT_SUCCESS || !root)
        fail("cc fail");
}

struct node * compile(const char *code)
{
    char *file = write_to_file(code);
    char *argv[] = { "cc", file, "-o", "/dev/null", NULL };
    run_cc(4, argv);
    fclose(fp);
    rmdir(dir);
    return root;
}