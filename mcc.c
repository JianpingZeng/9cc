/*
 * mcc
 * driver of the c compiler
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include "sys.h"
#include "config.h"
#include "utils.h"

extern int cc_main(int argc, char *argv[]);

static char *ofile;
static unsigned fails;
static unsigned unit;
static const char *version = "0.0";
static const char *tmpdir;
static const char *progname;

static void usage(void)
{
#define print_opt(opt, msg)     fprintf(stderr, "  %-20s%s\n", opt, msg)
    fprintf(stderr,
            "OVERVIEW: mcc - A Standard C Compiler v%s\n\n"
            "USAGE: mcc [options] <files>\n\n"
            "OPTIONS:\n", version);
    print_opt("-c",              "Only run preprocess, compile and assemble steps");
    print_opt("-E",              "Only run the preprocessor");
    print_opt("-h, --help",      "Display available options");
    print_opt("-I <dir>",        "Add directory to include search path");
    print_opt("-o <file>",       "Write output to <file>");
    print_opt("-S",              "Only run preprocess and compilation steps");
    print_opt("-v, --version",   "Display version and options");
#undef print_opt
}

static const char *tempname(const char *hint)
{
    char p[64];
    hint = hint ? hint : "tmp";
    snprintf(p, sizeof(p), "mcc.%u.%s", unit, hint);
    return join(tmpdir, p);
}

static int unitprocess(void *context)
{
    struct vector *data = (struct vector *)context;
    const char *inputfile = (const char *)vec_at(data, 0);
    struct vector *options = (struct vector *)vec_at(data, 1);
    
    if (!file_exists(inputfile)) {
        fprintf(stderr, "input file '%s' not exists.\n", inputfile);
        return EXIT_FAILURE;
    }

    struct vector *v = vec_new();
    vec_push(v, (void *)progname);
    vec_add(v, options);
    vec_push(v, (void *)inputfile);

    return cc_main(vec_len(v), (char **)vtoa(v));
}

static void translate(void *elem, void *context)
{
    int ret;
    struct vector *v = vec_new();
    vec_push(v, elem);
    vec_push(v, context);
    unit++;
    ret = runproc(unitprocess, (void *)v);
    if (ret == EXIT_FAILURE)
        fails++;
    vec_free(v);
}

static void setup_env(void)
{
    setup_sys();
}

int main(int argc, char **argv)
{
    int ret = EXIT_SUCCESS;
    struct vector *inputs = vec_new();
    struct vector *options = vec_new();

    progname = argv[0];
    setup_env();
    
    for (int i=1; i < argc; i++) {
        char *arg = argv[i];
        if (!strcmp(arg, "-h") || !strcmp(arg, "--help") ||
            !strcmp(arg, "-v") || !strcmp(arg, "--version")) {
            usage();
            exit(EXIT_FAILURE);
        } else if (!strcmp(arg, "-o")) {
            if (++i >= argc)
                die("missing file name after '-o'");
            ofile = argv[i];
        } else if (arg[0] == '-') {
            vec_push(options, arg);
        } else {
            vec_push(inputs, arg);
        }
    }
    
    if (argc == 1) {
        usage();
        ret = EXIT_FAILURE;
        goto end;
    } else if (vec_len(inputs) == 0) {
        fprintf(stderr, "no input file.\n");
        ret = EXIT_FAILURE;
        goto end;
    }

    if (!(tmpdir = mktmpdir()))
	die("Can't make temporary directory.");
    
    for (int i = 0; i < vec_len(inputs); i++)
        translate(vec_at(inputs, i), options);
    
    if (fails)
        fprintf(stderr, "%d fails.\n", fails);
    
end:
    if (tmpdir)
	rmdir(tmpdir);
    return ret;
}
