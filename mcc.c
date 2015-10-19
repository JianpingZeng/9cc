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

static unsigned fails;
static const char *version = "0.0";
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

static const char * tempname(const char *dir, const char *hint)
{
    static long index;
    const char *base = basename(strdup(hint));
    const char *name = base;
    const char *path;

 beg:
    path = join(dir, name);
    if (file_exists(path)) {
	name = format("%s-%d", base, index++);
	goto beg;
    }
    return path;
}

static bool options_has(struct vector *v, const char *name)
{
    for (int i = 0; i < vec_len(v); i++) {
	const char *option = vec_at(v, i);
	if (!strcmp(option, name))
	    return true;
    }
    return false;
}

static int program(void *context)
{
    struct vector *data = (struct vector *)context;
    const char *ifile = (const char *)vec_at(data, 0);
    struct vector *options = (struct vector *)vec_at(data, 1);
    const char *ofile = NULL;
    if (vec_len(data) > 2)
	ofile = (const char *)vec_at(data, 2);
    
    if (!file_exists(ifile)) {
        fprintf(stderr, "input file '%s' not exists.\n", ifile);
        return EXIT_FAILURE;
    }

    struct vector *v = vec_new();
    vec_push(v, (void *)progname);
    vec_add(v, options);
    vec_push(v, (void *)ifile);
    if (ofile) {
	vec_push(v, (char *)"-o");
	vec_push(v, (void *)ofile);
    }

    return cc_main(vec_len(v), (char **)vtoa(v));
}

static void translate(const char *ifile, struct vector *options, const char *ofile)
{
    struct vector *v = vec_new();
    vec_push(v, (void *)ifile);
    vec_push(v, options);
    if (ofile)
	vec_push(v, (void *)ofile);

    if (runproc(program, (void *)v) == EXIT_FAILURE)
        fails++;
    
    vec_free(v);
}

int main(int argc, char **argv)
{
    int ret = EXIT_SUCCESS;
    struct vector *inputs = vec_new();
    struct vector *options = vec_new();
    const char *tmpdir;
    const char *output_file = NULL;

    progname = argv[0];
    setup_sys();
    
    for (int i=1; i < argc; i++) {
        char *arg = argv[i];
        if (!strcmp(arg, "-h") || !strcmp(arg, "--help") ||
            !strcmp(arg, "-v") || !strcmp(arg, "--version")) {
            usage();
	    return EXIT_SUCCESS;
        } else if (!strcmp(arg, "-o")) {
            if (++i >= argc)
                die("missing file name after '-o'");
            output_file = argv[i];
        } else if (arg[0] == '-') {
            vec_push(options, arg);
        } else {
            vec_push(inputs, arg);
        }
    }
    
    if (argc == 1) {
        usage();
        return EXIT_SUCCESS;
    } else if (vec_len(inputs) == 0) {
        fprintf(stderr, "no input file.\n");
        return EXIT_FAILURE;
    } else if (output_file && vec_len(inputs) > 1 &&
	       (options_has(options, "-E") ||
		options_has(options, "-S") ||
		options_has(options, "-c"))) {
	fprintf(stderr, "mcc: cannot specify -o when generating multiple output files\n");
	return EXIT_FAILURE;
    }

    if (!(tmpdir = mktmpdir()))
	die("Can't make temporary directory.");
    
    for (int i = 0; i < vec_len(inputs); i++) {
	const char *ifile = vec_at(inputs, i);
	const char *ofile = NULL;
	if (options_has(options, "-E")) {
	    if (output_file)
		ofile = output_file;
	} else if (options_has(options, "-S")) {
	    if (output_file)
		ofile = output_file;
	    else
		ofile = replace_suffix(ifile, "s");
	} else if (options_has(options, "-c")) {
	    if (output_file)
		ofile = output_file;
	    else
		ofile = replace_suffix(ifile, "o");
	} else {
	    ofile = tempname(tmpdir, ifile);
	}
        translate(ifile, options, ofile);
    }
    
    if (fails) {
        fprintf(stderr, "%d fails.\n", fails);
	ret = EXIT_FAILURE;
    }

    if (tmpdir)
	rmdir(tmpdir);
    return ret;
}
