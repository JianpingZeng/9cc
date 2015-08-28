/*
 * mcc
 * frontend of the c compiler
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <locale.h>
#include <stdarg.h>
#include "sys.h"
#include "str.h"
#include "vector.h"
#include "config.h"
#include "utils.h"

extern int cpp_main(int argc, char *argv[]);
extern int cc_main(int argc, char *argv[]);
extern void die(const char *fmt, ...);

// configs
static struct {
    unsigned E : 1;
    unsigned c : 1;
    unsigned S : 1;
} option;

static char *output_file;

// options
static unsigned fails;
static unsigned unit;

static const char *version = "0.0";

static void usage()
{
#define print_opt(opt, msg)     fprintf(stderr, "  %-20s%s\n", opt, msg)
    fprintf(stderr,
            "OVERVIEW: mcc - A Standard C Compiler v%s\n\n"
            "USAGE: mcc [options] <inputs>\n\n"
            "OPTIONS:\n", version);
    print_opt("-c",              "Only run preprocess, compile, and assemble steps");
    print_opt("-E",              "Only run the preprocessor");
    print_opt("-h, --help",      "Display available options");
    print_opt("-I <dir>",        "Add directory to include search path");
    print_opt("-o <file>",       "Write output to <file>");
    print_opt("-S",              "Only run preprocess and compilation steps");
    print_opt("-v, --version",   "Display version and options");
#undef print_opt
}

static char *tempname(const char *hint)
{
    static char *tmpdir;
    if (!tmpdir) {
        tmpdir = mktmpdir();
        if (!tmpdir)
            die("Can't make temporary directory.");
    }
    
    if (!hint) {
        rmdir(tmpdir);
        return NULL;
    } else {
        size_t len = strlen(tmpdir) + strlen(hint) + 128;
        void *p = zmalloc(len);
        hint = hint ? hint : "tmp";
        snprintf(p, len, "%s/mcc.%u.%s", tmpdir, unit, hint);
        return p;
    }
}

static int unitprocess(void *context)
{
    struct vector *data = (struct vector *)context;
    char *inputfile = (char *)vec_at(data, 0);
    struct vector *options = (struct vector *)vec_at(data, 1);
    
    int ret = EXIT_SUCCESS;
    struct vector *v;
    int argc;
    char **argv;
    static const char *cpp[] = {"cpp", "$in", "-o", "$out", 0};
    static const char *cc[] = {"cc", "$in", "-o", "$out", 0};
    char *ifile = NULL;
    char *sfile = NULL;
    
    if (!file_exists(inputfile)) {
        fprintf(stderr, "input file '%s' not exists.\n", inputfile);
        ret = EXIT_FAILURE;
        goto end;
    }
    
    // preprocess
    if (option.E) {
        if (output_file)
            cpp[3] = output_file;
        else
            cpp[2] = 0;
    } else {
        ifile = tempname("cpp.i");
        cpp[3] = ifile;
    }
    cpp[1] = inputfile;
    v = new_vector();
    vec_add_from_array(v, (void **)cpp);
    vec_add(v, options);
    argc = vec_len(v);
    argv = (char **) vtoa(v);
    if (cpp_main(argc, argv) == EXIT_FAILURE) {
        ret = EXIT_FAILURE;
        goto end;
    }
    if (option.E)
        goto end;
    
    // compile
    if (option.S) {
        if (output_file) {
            cc[3] = output_file;
        }
        else {
            char *new_file = replace_suffix(inputfile, "s");
            sfile = zmalloc(strlen(new_file)+1);
            strcpy(sfile, new_file);
            free(new_file);
            cc[3] = sfile;
        }
    }
    else {
        sfile = tempname("cc.s");
        cc[3] = sfile;
    }
    cc[1] = ifile;
    v = new_vector();
    vec_add_from_array(v, (void **)cc);
    vec_add(v, options);
    argc = vec_len(v);
    argv = (char **) vtoa(v);
    if (cc_main(argc, argv) == EXIT_FAILURE) {
        ret = EXIT_FAILURE;
        goto end;
    }
    
end:
    return ret;
}

static void translate(void *elem, void *context)
{
    int ret;
    struct vector *v = new_vector();
    vec_push(v, elem);
    vec_push(v, context);
    unit++;
    ret = runproc(unitprocess, (void *)v);
    if (ret == EXIT_FAILURE)
        fails++;
}

static void setup_env()
{
    setlocale(LC_ALL, "");
    ENV.is_color_term = is_color_term();
}

int main(int argc, char **argv)
{
    int ret = EXIT_SUCCESS;
    struct vector *inputlist = new_vector();
    struct vector *optionlist = new_vector();
    
    for (int i=1; i < argc; i++) {
        char *arg = argv[i];
        if (!strcmp(arg, "-h") || !strcmp(arg, "--help") ||
            !strcmp(arg, "-v") || !strcmp(arg, "--version")) {
            usage();
            exit(EXIT_FAILURE);
        } else if (!strcmp(arg, "-o")) {
            if (++i >= argc)
                die("missing file name after '-o'");
            output_file = argv[i];
        } else if (!strcmp(arg, "-E")) {
            option.E = 1;
        } else if (!strcmp(arg, "-c")) {
            option.c = 1;
        } else if (!strcmp(arg, "-S")) {
            option.S = 1;
        } else if (arg[0] == '-') {
            vec_push(optionlist, strs(arg));
        } else {
            vec_push(inputlist, strs(arg));
        }
    }
    
    if (vec_len(inputlist) == 0) {
        fprintf(stderr, "no input file.\n");
        ret = EXIT_FAILURE;
        goto end;
    } else if (argc == 1) {
        usage();
        ret = EXIT_FAILURE;
        goto end;
    }
    
    setup_env();
    for (int i = 0; i < vec_len(inputlist); i++) {
        void *p = vec_at(inputlist, i);
        translate(p, optionlist);
    }
    
    if (fails) {
        fprintf(stderr, "%d fails.\n", fails);
    }
    
end:
    tempname(NULL);
    return ret;
}
