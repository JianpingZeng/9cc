/*
 * mcc
 * frontend of the c compiler
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "mcc.h"

static const char *cpp[] = {"cpp", "-o", "$out", "$in", 0};
static const char *cc[] = {"cc", "-o", "$out", "$in", 0};
static const char *as[] = {0};
static const char *ld[] = {0};
static char *tmpdir;
static const char *output_file;
static const char *input_file;
static char outpath[1024];

static void usage()
{
    fprintf(stderr, "Usage: mcc [-Ec] [-o target] source\n");
}

static char *tempname(const char *hint)
{
    static unsigned long id;
    hint = hint ? hint : "tmp";
    snprintf(outpath, sizeof(outpath), "%s/mcc.%lu.%s", tmpdir, id, hint);
    id++;
    return outpath;
}

static void preprocess()
{
    char *ofile = tempname("cpp.i");
    cpp[2] = ofile;
    cpp[3] = input_file;
    if (callsys(cpp[0], cpp) != EXIT_SUCCESS) {
	fprintf(stderr, "preprocess failed.\n");
	exit(EXIT_FAILURE);
    }
}

static void compile()
{
    /*
    if (callsys(cc[0], cc) != EXIT_SUCCESS) {
	exit(EXIT_FAILURE);
    }
    */
}

static void assemble()
{
    
}

static void link()
{

}

int main(int argc, char *argv[])
{
    int ret = EXIT_SUCCESS;
    
    tmpdir = mk_temp_dir();
    if (!tmpdir) {
	fprintf(stderr, "Can't make temporary directory.\n");
	return EXIT_FAILURE;
    }
    for (int i=1; i < argc; i++) {
	char *arg = argv[i];
	if (!strcmp(arg, "-h") || !strcmp(arg, "--help")) {
	    usage();
	    exit(EXIT_FAILURE);
	}
	else if (!strcmp(arg, "-o")) {
	    if (++i >= argc) {
		fprintf(stderr, "missing target file while -o option given.\n");
		usage();
		exit(EXIT_FAILURE);
	    }
	    output_file = argv[i];
	    continue;
	}
	else if (arg[0] == '-') {
	    continue;
	}

	input_file = arg;
    }

    if (!input_file) {
	fprintf(stderr, "no input file.\n");
	exit(EXIT_FAILURE);
    }

    if (!file_exists(input_file)) {
	fprintf(stderr, "input file '%s' not exists.\n", input_file);
	exit(EXIT_FAILURE);
    }

    preprocess();
    compile();
    assemble();
    link();
    
    return ret;
}
