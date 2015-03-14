/*
 * mcc
 * frontend of the c compiler
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "mcc.h"

extern int cpp_main(int argc, char **argv);
extern int cc_main(int argc, char **argv);

// configs
static struct configs {
    unsigned option_E : 1;
    unsigned option_c : 1;
    unsigned option_s : 1;
} config;

static char *tmpdir;
static char *output_file;

// temporary files
static char *ifile;
static char *sfile;

// options
static struct vector *optionlist;
static unsigned fails;
static unsigned unit;

static void usage()
{
    fprintf(stderr, "Usage: mcc [-Ec] [-o target] source\n");
}

static void append(struct vector *v, const char *str)
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
    static const char *cpp[] = {"cpp", "$in", "-o", "$out", 0};
    int ret;
    struct vector *v = new_vector();
    char **argv;
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
    vector_add_from_array(v, (void **)cpp);
    vector_add_from_vector(v, optionlist);
    argv = (char **) vector_to_array(v);
    ret = cpp_main(array_length((void **)argv), argv);
    free(argv);
    return ret;
}

static int compile(const char *inputfile, const char *orig_input_file)
{
    static const char *cc[] = {"cc", "$in", "-o", "$out", 0};
    int ret;
    struct vector *v = new_vector();
    char **argv;
    char *outfile = NULL;
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
    vector_add_from_array(v, (void **)cc);
    vector_add_from_vector(v, optionlist);
    argv = (char **) vector_to_array(v);
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

static void translate(void *inputfile, void *context)
{
    unit++;
    
    if (!file_exists(inputfile)) {
	fprintf(stderr, "input file '%s' not exists.\n", (char *)inputfile);
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

int main(int argc, char **argv)
{
    int ret = EXIT_SUCCESS;
    struct vector *inputlist = new_vector();
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
	} else if (!strcmp(arg, "-o")) {
	    if (++i >= argc) {
		fprintf(stderr, "missing target file while -o option given.\n");
		usage();
		exit(EXIT_FAILURE);
	    }
	    output_file = argv[i];
	} else if (!strcmp(arg, "-E")) {
	    config.option_E = 1;
	} else if (!strcmp(arg, "-s")) {
	    config.option_s = 1;
	} else if (!strcmp(arg, "-c")) {
	    config.option_c = 1;
	} else if (arg[0] == '-') {
	    if (arg[1] == 'I') {
		char *abs = expanduser(arg+2);
		if (abs) {
		    int len = strlen(abs);
		    char *ioption = malloc(len+3);
		    strncpy(ioption, "-I", 2);
		    strncpy(ioption+2, abs, len);
		    ioption[len+2] = 0;
		    free(abs);
		    vector_push(optionlist, ioption);
		}
	    } else {
		append(optionlist, arg);
	    }
	} else {
	    append(inputlist, arg);
	}
    }

    if (vector_length(inputlist) == 0) {
	fprintf(stderr, "no input file.\n");
	return EXIT_FAILURE;
    }
    
    vector_foreach(inputlist, translate, NULL);

    if (!config.option_E && !config.option_s && !config.option_c && fails == 0) {
	// link
	ret = link();
    }
    
    purge_vector(inputlist);
    purge_vector(optionlist);

    rmdir(tmpdir);
    
    return ret;
}
