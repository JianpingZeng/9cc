#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "cpp.h"

static void usage()
{
    fprintf(stderr, "Usage: cpp [-E] [-o target] source\n");
}

static const char ** concat(int argc, char **argv)
{
    char *input_file = NULL;
    char *output_file = NULL;
    const char **p = NULL;
    Vector *v = new_vector();
    Vector *options = new_vector();
    
    for (int i=1; i < argc; i++) {
	char *arg = argv[i];
	if (!strcmp(arg, "-h") || !strcmp(arg, "--help")) {
	    usage();
	    exit(EXIT_FAILURE);
	}
	else if (!strcmp(arg, "-o")) {
	    if (++i >= argc) {
		fprintf(stderr, "missing target file while -o option is given.\n");
		usage();
		exit(EXIT_FAILURE);
	    }
	    output_file = argv[i];
	}
	else if (arg[0] == '-') {
	    vector_push(options, arg);
	}
	else {
	    input_file = arg;
	}
    }
    if (!input_file) {
	fprintf(stderr, "no input file.\n");
	usage();
	exit(EXIT_FAILURE);
    }
    if (!file_exists(input_file)) {
	fprintf(stderr, "input file '%s' not exist.\n", input_file);
	perror("n");
	exit(EXIT_FAILURE);
    }

    vector_push(options, input_file);
    if (output_file) {
	vector_push(options, "-o");
	vector_push(options, output_file);
    }

    vector_add_from_array(v, (void **)cpp);
    vector_add_from_vector(v, options);
    free_vector(options);
    p = (const char **) vector_to_array(v);
    
    return p;
}

int main(int argc, char **argv)
{
    const char **options;
    int ret = EXIT_SUCCESS;
    options = concat(argc, argv);
    ret = callsys(cpp[0], options);
    free(options);
    return ret;
}
