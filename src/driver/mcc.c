/*
 * mcc
 * frontend of the c compiler
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "mcc.h"

static const char *cpp[] = {"cpp", "$in", "-o", "$out", 0};
static const char *cc[] = {"cc", "$in", "-o", "$out", 0};
static const char *as[] = {0};
static const char *ld[] = {0};
static char *tmpdir;
static const char *output_file;
static struct configs {
    unsigned option_E : 1;
    unsigned option_c : 1;
    unsigned option_s : 1;
} config;
// temporary files
static const char *ifile;
static const char *sfile;
static const char *ofile;
// options
static Vector *optionlist;
static unsigned fails;
static unsigned unit;

static void usage()
{
    fprintf(stderr, "Usage: mcc [-Ec] [-o target] source\n");
}

static void append(Vector *v, const char *str)
{
    char *p = malloc(strlen(str)+1);
    memcpy(p, str, strlen(str));
    p[strlen(str)] = 0;
    vector_push(v, p);
}

static char *tempname(const char *hint)
{
    unsigned len = strlen(tmpdir)+strlen(hint)+128;
    void *p = malloc(len);
    hint = hint ? hint : "tmp";
    snprintf(p, len, "%s/mcc.%u.%s", tmpdir, unit, hint);
    return p;
}

static int preprocess(const char *inputfile)
{
    int ret;
    Vector *v = new_vector();
    const char **argv;
    if (config.option_E) {
	if (output_file) {
	    cpp[3] = output_file;
	}
	else {
	    cpp[2] = NULL;
	}
    }
    else {
	if (ifile) free(ifile);
	ifile = tempname("cpp.i");
	cpp[3] = ifile;
    }
    cpp[1] = inputfile;
    vector_add_from_array(v, cpp);
    vector_add_from_vector(v, optionlist);
    argv = vector_to_array(v);
    ret = callsys(cpp[0], argv);
    free(argv);
    return ret;
}

static int compile(const char *inputfile, const char *orig_input_file)
{
    int ret;
    Vector *v = new_vector();
    const char **argv;
    const char *outfile = NULL;
    if (config.option_s) {
	if (output_file) {
	    cc[3] = output_file;
	}
	else {
	    outfile = replace_suffix(orig_input_file, "s"); 
	    cc[3] = outfile;
	}
    }
    else {
	if (sfile) free(sfile);
	sfile = tempname("cc.s");
	cc[3] = sfile;
    }
    cc[1] = inputfile;
    vector_add_from_array(v, cc);
    vector_add_from_vector(v, optionlist);
    argv = vector_to_array(v);
    ret = callsys(cc[0], argv);
    free(argv);
    if (outfile) free(outfile);
    return ret;
}

static int assemble(const char *inputfile)
{
    return EXIT_FAILURE;
}

static int link()
{
    return EXIT_FAILURE;
}

static void translate(void *inputfile)
{
    unit++;
    
    if (!file_exists(inputfile)) {
	fprintf(stderr, "input file '%s' not exists.\n", inputfile);
	fails++;
	return;
    }

    if (preprocess(inputfile) == EXIT_FAILURE) {
	fails++;
	return;
    }
    if (config.option_E) {
	return;
    }
    
    if (compile(ifile, inputfile) == EXIT_FAILURE) {
	fails++;
	return;
    }
    if (config.option_s) {
	return;
    }
    
    if (assemble(sfile) == EXIT_FAILURE) {
	fails++;
	return;
    }

    if (ifile) free(ifile);
    if (sfile) free(sfile);
}

int main(int argc, char *argv[])
{
    int ret = EXIT_SUCCESS;
    Vector *inputlist = new_vector();
    optionlist = new_vector();
    
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
	}
	else if (!strcmp(arg, "-E")) {
	    config.option_E = 1;
	}
	else if (!strcmp(arg, "-s")) {
	    config.option_s = 1;
	}
	else if (!strcmp(arg, "-c")) {
	    config.option_c = 1;
	}
	else if (arg[0] == '-') {
	    append(optionlist, arg);
	}
	else {
	    append(inputlist, arg);
	}
    }

    if (vector_length(inputlist) == 0) {
	fprintf(stderr, "no input file.\n");
	return EXIT_FAILURE;
    }
    
    vector_foreach(inputlist, translate);

    if (!config.option_E && !config.option_s && !config.option_c && fails == 0) {
	// link
	
    }
    
    purge_vector(inputlist);
    purge_vector(optionlist);
    
    return ret;
}
