#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "cpp.h"

static void usage()
{
    fprintf(stderr, "Usage: cpp [-E] [-o target] source\n");
}

static const char ** concat(int argc, char *argv[])
{
    char *input_file = NULL;
    char *output_file = NULL;
    const char **p = NULL;
    int cpp_len = 0;
    const char **cpp_p = cpp;
    
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
	    continue;
	}
	else if (arg[0] == '-') {
	    continue;
	}
	input_file = arg;
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

    while(*cpp_p++) {
	cpp_len++;
    }
    
    if (output_file) {
	int len = cpp_len - 1 + 3 + 1;
	int j = 0;
	p = malloc(sizeof(char *) * len);
	assert(p);
	for (int i=0; i < cpp_len; i++) {
	    p[j++] = cpp[i]; 
	}
	p[j++] = input_file;
	p[j++] = "-o";
	p[j++] = output_file;
	p[j++] = 0;
    }
    else {
	int len = cpp_len - 1 + 3 + 1;
	int j = 0;
	p = malloc(sizeof(char *) * len);
        assert(p);
	for (int i=0; i < cpp_len; i++) {
	    p[j++] = cpp[i]; 
	}
	p[j++] = input_file;
	p[j++] = 0;
    }
    
    return p;
}

int main(int argc, char *argv[])
{
    const char **options;
    int ret;
    
    options = concat(argc, argv);
    ret = execv_cpp(cpp[0], options);
    free(options);
    
    return ret;
}
