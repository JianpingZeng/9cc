/*
 * 7cc
 * driver of the c compiler
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <time.h>
#include <ctype.h>
#include "config.h"
#include "utils/sys.h"
#include "utils/utils.h"

static const char *progname;
static struct vector *inputs;
static const char *output;
static struct {
    int c:1;
    int E:1;
    int S:1;
    int ast_dump:1;
    int ir_dump:1;
    /* Linker options
       -l
       -L
     */
    struct vector *ld_options;

    /* C Preprocessor options
       -I
       -D
       -U
     */
    /* Compiler options
       -ast-dump
       -ir-dump
       -fleading_underscore
       -Wall
       -Werror
     */
    struct vector *cc_options;
} opts;

static void usage(void)
{
    fprintf(stderr,
            "OVERVIEW: 7cc - A Standard C Compiler v%s\n\n"
            "USAGE: 7cc [options] <files>\n\n"
            "OPTIONS:\n", VERSION);
    fprintf(stderr,
            "  -ast-dump       Only print abstract syntax tree\n"
            "  -ir-dump        Only print intermediate representation\n"
            "  -c              Only run preprocess, compile and assemble steps\n"
            "  -Dname          \n"
            "  -Dname=value    Define a macro\n"
            "  -Uname          Undefine a macro\n"
            "  -E              Only run the preprocessor\n"
            "  -h, --help      Display available options\n"
            "  -Idir           Add dir to include search path\n"
            "  -lx             Search for library x\n"
            "  -Ldir           Add dir to library search path\n"
            "  -o <file>       Write output to <file>\n"
            "  -S              Only run preprocess and compilation steps\n"
            "  -Wall           Enable all warnings\n"
            "  -Werror         Treat warnings as errors\n"
            "  -v, --version   Display version and options\n");
}

static void init_env(void)
{
    opts.ld_options = vec_new();
    opts.cc_options = vec_new();
#ifdef CONFIG_DARWIN
    vec_push(opts.cc_options, "-fleading_underscore");
#endif
    inputs = vec_new();
}

static void parse_opts(int argc, char *argv[])
{
    for (int i = 1; i < argc; i++) {
        char *arg = argv[i];
        if (arg[0] == '-') {
            if (!strcmp(arg, "-o")) {
                if (++i >= argc)
                    die("missing file name after '-o'");
                else if (output)
                    fprintf(stderr,
                            "warning: output file overwritten\n");
                output = argv[i];
            } else if (!strcmp(arg, "-h") ||
                       !strcmp(arg, "--help") ||
                       !strcmp(arg, "-v") ||
                       !strcmp(arg, "--version")) {
                usage();
                exit(EXIT_FAILURE);
            } else if (!strcmp(arg, "-c")) {
                opts.c = true;
            } else if (!strcmp(arg, "-E")) {
                opts.E = true;
                vec_push(opts.cc_options, arg);
            } else if (!strcmp(arg, "-S")) {
                opts.S = true;
            } else if (!strcmp(arg, "-ast-dump")) {
                opts.ast_dump = true;
                vec_push(opts.cc_options, arg);
            } else if (!strncmp(arg, "-ir-dump", 8)) {
                opts.ir_dump = true;
                vec_push(opts.cc_options, arg);
            } else if (!strcmp(arg, "-Wall") ||
                       !strcmp(arg, "-Werror") ||
                       !strncmp(arg, "-I", 2) ||
                       !strncmp(arg, "-D", 2) ||
                       !strncmp(arg, "-U", 2)) {
                vec_push(opts.cc_options, arg);
            } else if (!strncmp(arg, "-l", 2) ||
                       !strncmp(arg, "-L", 2)) {
                vec_push(opts.ld_options, arg);
            } else if (!strcmp(arg, "-g") ||
                       !strncmp(arg, "-std=", 5) ||
                       !strncmp(arg, "-O", 2)) {
                // just ignore without any hint
            } else {
                fprintf(stderr,
                        "warning: ignored unknown option: %s\n",
                        arg);
            }
        } else {
            vec_push(inputs, arg);
        }
    }
}

static const char *tempname(const char *dir, const char *hint)
{
    static long index;
    const char *base = basename(xstrdup(hint));
    const char *name = base;
    const char *path;

 beg:
    path = join(dir, name);
    if (file_exists(path)) {
        name = format("%d.%s", index++, base);
        goto beg;
    }
    return path;
}

static char **compose(char *argv[], struct vector *ifiles, const char *ofile,
                      struct vector *options)
{
    size_t argc = LIST_LEN(argv);
    size_t ac = argc + vec_len(ifiles) + vec_len(options);
    char **av = xmalloc(ac * sizeof(char *));
    int j = 0;
    for (int i = 0; i < argc; i++) {
        char *arg = argv[i];
        if (arg[0] == '$' && isdigit(arg[1])) {
            int k = arg[1] - '0';
            assert(k >= 0 && k <= 2);
            if (k == 0) {
                av[j++] = (char *)(ofile ? ofile : "a.out");
            } else if (k == 1) {
                for (int i = 0; i < vec_len(ifiles); i++)
                    av[j++] = vec_at(ifiles, i);
            } else {
                for (int i = 0; i < vec_len(options); i++)
                    av[j++] = vec_at(options, i);
            }
        } else {
            av[j++] = arg;
        }
    }
    assert(j < ac);
    av[j] = NULL;
    return av;
}

static int link(struct vector *ifiles, const char *ofile,
                struct vector *options)
{
    return callsys(ld[0], compose(ld, ifiles, ofile, options));
}

static int assemble(const char *ifile, const char *ofile)
{
    struct vector *ifiles = vec_new1((char *)ifile);
    return callsys(as[0], compose(as, ifiles, ofile, NULL));
}

static int translate(const char *ifile, const char *ofile,
                     struct vector *options)
{
    struct vector *ifiles = vec_new1((char *)ifile);
    cc[3] = ofile ? "-o" : NULL;
    return callsys(cc[0], compose(cc, ifiles, ofile, options));
}

int main(int argc, char **argv)
{
    int ret = EXIT_SUCCESS;
    const char *tmpdir;
    size_t fails = 0;

    progname = argv[0];
    setup_sys();
    init_env();
    parse_opts(argc, argv);

    bool partial = opts.E || opts.ast_dump || opts.ir_dump || opts.S || opts.c;

    if (argc == 1) {
        usage();
        return EXIT_SUCCESS;
    } else if (vec_empty(inputs)) {
        fprintf(stderr, "no input file.\n");
        return EXIT_FAILURE;
    } else if (output && vec_len(inputs) > 1 && partial) {
        fprintf(stderr,
                "7cc: cannot specify -o when generating multiple output files\n");
        return EXIT_FAILURE;
    }

    if (!(tmpdir = mktmpdir()))
        die("Can't make temporary directory.");

    struct vector *objects = vec_new();

    // construct compiler options
    struct vector *cc_options = opts.cc_options;

    for (int i = 0; i < vec_len(inputs); i++) {
        const char *ifile = vec_at(inputs, i);
        const char *iname = basename(xstrdup(ifile));
        const char *ofile = NULL;
        const char *suffix = file_suffix(ifile);
        int ret;
        if (opts.E || opts.ast_dump || opts.ir_dump) {
            if (output)
                ofile = output;
            ret = translate(ifile, ofile, cc_options);
        } else if (opts.S) {
            if (output)
                ofile = output;
            else
                ofile = replace_suffix(iname, "s");
            ret = translate(ifile, ofile, cc_options);
        } else if (opts.c) {
            if (output)
                ofile = output;
            else
                ofile = replace_suffix(iname, "o");
            // base on suffix
            if (suffix && !strcmp(suffix, "s")) {
                ret = assemble(ifile, ofile);
            } else {
                const char *sfile = tempname(tmpdir, replace_suffix(ifile, "s"));
                ret = translate(ifile, sfile, cc_options);
                if (ret == 0)
                    ret = assemble(sfile, ofile);
            }
        } else {
            // base on suffix
            if (suffix && !strcmp(suffix, "o")) {
                vec_push(objects, (char *)ifile);
                ret = EXIT_SUCCESS;
            } else if (suffix && !strcmp(suffix, "s")) {
                ofile = tempname(tmpdir, replace_suffix(ifile, "o"));
                ret = assemble(ifile, ofile);
                vec_push(objects, (char *)ofile);
            } else {
                const char *sfile = tempname(tmpdir, replace_suffix(ifile, "s"));
                ret = translate(ifile, sfile, cc_options);
                if (ret == 0) {
                    ofile = tempname(tmpdir, replace_suffix(ifile, "o"));
                    ret = assemble(sfile, ofile);
                    vec_push(objects, (char *)ofile);
                }
            }
        }
        if (ret == EXIT_FAILURE)
            fails++;
    }

    if (fails) {
        ret = EXIT_FAILURE;
        fprintf(stderr, "%lu succeed, %lu failed.\n",
                vec_len(inputs) - fails, fails);
    } else if (!partial) {
        // link
        ret = link(objects, output, opts.ld_options);
    }

    if (tmpdir)
        rmdir(tmpdir);
    return ret;
}
