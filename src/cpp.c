#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include <string.h>
#include "utils.h"
#include "sys.h"

static const char *ofile, *ifile;
static struct vector *options;
static int errors;

static void cpp_init(void)
{
    options = new_vector();
}

static void cpp_exit(void)
{
    
}

static void preprocess(void)
{
    static const char *cpp[] = {"/usr/bin/c99", "-E", "$in", "-o", "$out", 0};
    struct vector *v = new_vector();
    
    cpp[2] = ifile;
    if (ofile)
	cpp[4] = ofile;
    else
	cpp[3] = 0;

    vec_add_from_array(v,(void *) cpp);
    vec_add_from_vector(v, options);
    errors = callsys(cpp[0], (char **)vtoa(v));
}

static void add_search_path(const char *path)
{
    const char *abspath = expanduser(path);
    struct string *s = new_string();
    str_cats(s, "-I");
    if (abspath) {
	str_cats(s, abspath);
	vec_push(options, stoa(s));
    }
}

static void parse_opts(int argc, char **argv)
{    
    for (int i=1; i < argc; i++) {
        char *arg = argv[i];
        if (!strcmp(arg, "-o")) {
            if (++i >= argc)
                die("missing file name after '-o'");
            ofile = argv[i];
        } else if (!strncmp(arg, "-D", 2)) {
            vec_push(options, arg);
        } else if (!strncmp(arg, "-U", 2)) {
            vec_push(options, arg);
        } else if (!strncmp(arg, "-I", 2)) {
            if (strlen(arg) > 2)
                add_search_path(arg+2);
            else if (++i < argc)
                add_search_path(argv[i]);
            else
                die("missing path after '-I'");
        } else if (arg[0] != '-') {
            ifile = arg;
        }
    }
}

int cpp_main(int argc, char **argv)
{
    cpp_init();
    parse_opts(argc, argv);
    preprocess();
    cpp_exit();
    return errors > 0 ? EXIT_FAILURE : EXIT_SUCCESS;
}
