#include "mcc.h"

static char ** parse_opts(int argc, char **argv)
{
    char *input_file = NULL;
    char *output_file = NULL;
    char **p = NULL;
    struct vector *v = new_vector();
    struct vector *options = new_vector();
    
    for (int i=1; i < argc; i++) {
	char *arg = argv[i];
        if (!strcmp(arg, "-o")) {
	    if (++i >= argc) {
		fprintf(stderr, "missing target file while -o option is given.\n");
		exit(EXIT_FAILURE);
	    }
	    output_file = argv[i];
	}
	else if (arg[0] == '-') {
	    vec_push(options, arg);
	}
	else {
	    input_file = arg;
	}
    }
    if (!input_file) {
	fprintf(stderr, "no input file.\n");
	exit(EXIT_FAILURE);
    }
    if (!file_exists(input_file)) {
	fprintf(stderr, "input file '%s' not exist.\n", input_file);
	exit(EXIT_FAILURE);
    }

    vec_push(options, input_file);
    if (output_file) {
	vec_push(options, "-o");
	vec_push(options, output_file);
    }

    vec_add_from_array(v, (void **)cpp);
    vec_add_from_vector(v, options);
    free_vector(options);
    p = (char **) vtoa(v);
    
    return p;
}

static void free_cpp()
{

}

int cpp_main(int argc, char **argv)
{
    char **options = parse_opts(argc, argv);
    int ret = callsys(cpp[0], options);
    free_cpp();
    return ret;
}
