#include "cc.h"

static const char *input_file;
static const char *output_file;

static void cc_init()
{
    setlocale(LC_ALL, "");
    symbol_init();
    type_init();
    lexer_init();
}

int cc_main(int argc, const char * argv[])
{
    FILE *fp;
    struct decl *n;
    
    for (int i=1; i < argc; i++) {
	const char *arg = argv[i];
        if (!strcmp(arg, "-o")) {
	    if (++i >= argc) {
		fprintf(stderr, "missing target file while -o option given.\n");
		return EXIT_FAILURE;
	    }
	    output_file = argv[i];
	} else if (arg[0] == '-') {
	    // options
	} else {
	    input_file = arg;
	}
    }

    if (!input_file || !output_file) {
	return EXIT_FAILURE;
    }

    fp = freopen(input_file, "r", stdin);
    if (fp == NULL) {
	perror(input_file);
	return EXIT_FAILURE;
    }
    
    cc_init();
    n = translation_unit();
    print_tree(NODE(n));
    
    fclose(fp);
    free_cc();
	
    return errors > 0 ? EXIT_FAILURE : EXIT_SUCCESS;
}

