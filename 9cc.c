/*
 * 9cc
 * driver of the c compiler
 */
#include "config.h"
#include "compat.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <time.h>
#include <ctype.h>
#include "libutils.h"

static char *ld[];
static char *as[];
static char *cc[];
static char **inputs;
static char *output;
static char cflag;
static char Eflag;
static char Sflag;
static char ast_dump;
static char **ld_options;
static char **cc_options;
static const char *tmpdir;
static const char *progname = "9cc";

static void error(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    fprintf(stderr, "%s: ", progname);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    va_end(ap);
    exit(EXIT_FAILURE);
}

static void usage(void)
{
    fprintf(stderr,
            "OVERVIEW: %s - A Standard C Compiler v%s\n\n"
            "USAGE: %s [options] <files>\n\n"
            "OPTIONS:\n", progname, VERSION, progname);
    fprintf(stderr,
            "  -ansi           Strict type checking\n"
            "  -ast-dump       Only print abstract syntax tree\n"
            "  -c              Only run preprocess, compile and assemble steps\n"
            "  -debugX         Enable debug option X\n"
            "  -Dname          Define a macro\n"
            "  -Dname=value    Define a macro with value\n"
            "  -E              Only run the preprocessor\n"
            "  -h, --help      Display available options\n"
            "  -Idir           Add dir to include search path\n"
            "  -Ldir           Add dir to library search path\n"
            "  -lx             Search for library x\n"
            "  -o <file>       Write output to <file>\n"
            "  -S              Only run preprocess and compilation steps\n"
            "  -Uname          Undefine a macro\n"
            "  -v, --version   Display version and options\n"
            "  -Wall           Enable all warnings\n"
            "  -Werror         Treat warnings as errors\n");
}

static void parse_opts(int argc, char *argv[])
{
    struct list *ilist = NULL;
    struct list *clist = NULL;
    struct list *dlist = NULL;

    for (int i = 1; i < argc; i++) {
        char *arg = argv[i];
        if (!strcmp(arg, "-o")) {
            if (++i >= argc)
                error("missing file name after '-o'");
            output = argv[i];
        } else if (!strcmp(arg, "-h") || !strcmp(arg, "--help") ||
                   !strcmp(arg, "-v") || !strcmp(arg, "--version")) {
            usage();
            exit(EXIT_FAILURE);
        } else if (!strcmp(arg, "-c")) {
            cflag = true;
        } else if (!strcmp(arg, "-S")) {
            Sflag = true;
        } else if (!strcmp(arg, "-E")) {
            Eflag = true;
            clist = list_append(clist, arg);
        } else if (!strcmp(arg, "-ast-dump")) {
            ast_dump = true;
            clist = list_append(clist, arg);
        } else if (!strcmp(arg, "-Wall") ||
                   !strcmp(arg, "-Werror") ||
                   !strncmp(arg, "-I", 2) ||
                   !strncmp(arg, "-D", 2) ||
                   !strncmp(arg, "-U", 2) ||
                   !strcmp(arg, "-g") ||
                   !strncmp(arg, "-std=", 5) ||
                   !strncmp(arg, "-O", 2) ||
                   !strcmp(arg, "-ansi") ||
                   !strncmp(arg, "-debug", 6)) {
            clist = list_append(clist, arg);
        } else if (!strncmp(arg, "-l", 2) ||
                   !strncmp(arg, "-L", 2)) {
            dlist = list_append(dlist, arg);
        } else if (arg[0] == '-') {
            error("unknown option: %s", arg);
        } else {
            ilist = list_append(ilist, arg);
        }
    }

#ifdef CONFIG_DARWIN
    clist = list_append(clist, "-fleading_underscore");
#endif

    inputs = ltoa(&ilist, PERM);
    cc_options = ltoa(&clist, PERM);
    ld_options = ltoa(&dlist, PERM);
}

static char *tempname(const char *dir, const char *hint)
{
    static long index;
    const char *base = basename(strdup(hint));
    const char *name = base;
    const char *path;

 beg:
    path = join(dir, name);
    if (fexists(path)) {
        name = format("%d.%s", index++, base);
        goto beg;
    }
    return (char *)path;
}

static char **compose(char *argv[], char *ifiles[],
                      char *ofile, char *options[])
{
    int argc = length(argv);
    int nifiles = length(ifiles);
    int noptions = length(options);
    int ac = argc + nifiles + noptions;
    char **av = xmalloc(ac * sizeof(char *));
    int j = 0;
    for (int i = 0; i < argc; i++) {
        char *arg = argv[i];
        if (arg[0] == '$' && isdigit(arg[1])) {
            int k = arg[1] - '0';
            assert(k >= 0 && k <= 2);
            if (k == 0) {
                av[j++] = ofile ? ofile : "a.out";
            } else if (k == 1) {
                for (int i = 0; i < nifiles; i++)
                    av[j++] = ifiles[i];
            } else {
                for (int i = 0; i < noptions; i++)
                    av[j++] = options[i];
            }
        } else {
            av[j++] = arg;
        }
    }
    assert(j < ac);
    av[j] = NULL;
    return av;
}

static int lnk(char *ifiles[], char *ofile, char *options[])
{
    return proc(ld[0], compose(ld, ifiles, ofile, options));
}

static int assemble(char *ifile, char *ofile)
{
    struct list *ilist = list_append(NULL, ifile);
    char **ifiles = ltoa(&ilist, PERM);
    return proc(as[0], compose(as, ifiles, ofile, NULL));
}

static int translate(char *ifile, char *ofile, char *options[])
{
    struct list *ilist = list_append(NULL, ifile);
    char **ifiles = ltoa(&ilist, PERM);
    cc[3] = ofile ? "-o" : NULL;
    return proc(cc[0], compose(cc, ifiles, ofile, options));
}

static void doexit(void)
{
    if (tmpdir)
        rmdir(tmpdir);
}

int main(int argc, char **argv)
{
    int ret = EXIT_SUCCESS;
    int fails = 0;
    int partial;
    int ninputs;
    struct list *objects = NULL;

    atexit(doexit);
    parse_opts(argc, argv);
    partial = cflag || Sflag || Eflag || ast_dump;
    ninputs = length(inputs);

    if (argc == 1) {
        usage();
        exit(EXIT_FAILURE);
    } else if (ninputs == 0) {
        error("no input file.");
    } else if (output && ninputs > 1 && partial) {
        error("cannot specify -o when generating multiple output files");
    }

    if (!(tmpdir = mktmpdir()))
        error("Can't make temporary directory");

    for (int i = 0; i < ninputs; i++) {
        char *ifile = inputs[i];
        char *iname = basename(strdup(ifile));
        char *ofile = NULL;
        const char *suffix = fsuffix(ifile);
        int r = EXIT_SUCCESS;
        if (Eflag || ast_dump) {
            if (output)
                ofile = output;
            r = translate(ifile, ofile, cc_options);
        } else if (Sflag) {
            if (output)
                ofile = output;
            else
                ofile = (char *)resuffix(iname, "s");
            r = translate(ifile, ofile, cc_options);
        } else if (cflag) {
            if (output)
                ofile = output;
            else
                ofile = (char *)resuffix(iname, "o");
            // base on suffix
            if (suffix && !strcmp(suffix, "s")) {
                r = assemble(ifile, ofile);
            } else {
                char *sfile = tempname(tmpdir, resuffix(ifile, "s"));
                if ((r = translate(ifile, sfile, cc_options)) == 0)
                    r = assemble(sfile, ofile);
            }
        } else {
            // base on suffix
            if (suffix && !strcmp(suffix, "s")) {
                ofile = tempname(tmpdir, resuffix(ifile, "o"));
                r = assemble(ifile, ofile);
                objects = list_append(objects, ifile);
            } else if (suffix && !strcmp(suffix, "c")) {
                char *sfile = tempname(tmpdir, resuffix(ifile, "s"));
                if ((r = translate(ifile, sfile, cc_options)) == 0) {
                    ofile = tempname(tmpdir, resuffix(ifile, "o"));
                    r = assemble(sfile, ofile);
                    objects = list_append(objects, ifile);
                }
            } else {
                objects = list_append(objects, ifile);
            }
        }
        if (r == EXIT_FAILURE) {
            fails++;
            if (ofile)
                rmfile(ofile);
        }
    }

    if (fails)
        error("%lu succeed, %lu failed.", ninputs - fails, fails);
    else if (!partial)
        // link
        ret = lnk(ltoa(&objects, PERM), output, ld_options);

    return ret;
}

/**
 * $0: output file
 * $1: input files
 * $2: additional options
 */
#ifdef CONFIG_LINUX

#ifdef CONFIG_LIB64
#define CRT_DIR  "/usr/lib64/"
#else
#define CRT_DIR  "/usr/lib/x86_64-linux-gnu/"
#endif

static char *ld[] = {
    "ld",
    "-A", "x86_64",
    "-o", "$0",
    "-dynamic-linker", "/lib64/ld-linux-x86-64.so.2",
    CRT_DIR "crt1.o",
    CRT_DIR "crti.o",
    "$1", "$2",
    "-lc", "-lm",
    CRT_DIR "crtn.o",
    NULL
};
static char *as[] = { "as", "-o", "$0", "$1", "$2", NULL };
static char *cc[] = { _9CC_LIB_DIR "/cc1", "$1", "$2", "-o", "$0", NULL };

#elif defined (CONFIG_DARWIN)

static char *ld[] = {
    "ld",
    "-o", "$0",
    "$1", "$2",
    "-lc", "-lm",
    "-macosx_version_min", OSX_SDK_VERSION,
    "-arch", "x86_64",
    NULL
};
static char *as[] = { "as", "-o", "$0", "$1", "$2", NULL };
static char *cc[] = { _9CC_LIB_DIR "/cc1", "$1", "$2", "-o", "$0", NULL };

#else
#error "unknown platform"
#endif
