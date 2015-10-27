#include "cc.h"

static const char *ifile;
static const char *ofile;
static struct vector *options;
static bool cpp_only;

static void parseopts(int argc, const char *argv[])
{
    options = vec_new();
    
    for (int i=1; i < argc; i++) {
        const char *arg = argv[i];
        if (!strcmp(arg, "-o")) {
            if (++i >= argc)
                die("missing target file while -o option given");
            ofile = argv[i];
        } else if (arg[0] == '-') {
	    if (!strcmp(arg, "-E"))
		cpp_only = true;
	    else
		vec_push(options, (void *)arg);
        } else {
            ifile = arg;
        }
    }
}

static FILE * outfp(void)
{
    if (ofile) {
	FILE *fp = fopen(ofile, "w");
	if (fp == NULL) {
	    perror(ofile);
	    die("Can't open file %s", ofile);
	}
	return fp;
    }
    return stdout;
}

static void translate(void)
{
    node_t *tree;
    tree = translation_unit();
    print_tree(tree);
    if (errors == 0)
	gen(tree, outfp());
}

static void preprocess(void)
{
    FILE *fp = outfp();
    
    struct vector *v = all_pptoks();
    for (int i = 0; i < vec_len(v); i++) {
    	struct token *t = vec_at(v, i);
        fprintf(fp, "%s", t->name);
    }
}

static void cc_exit(void)
{
    print_alloc_stat();
}

int cc_main(int argc, const char * argv[])
{
    atexit(cc_exit);
    parseopts(argc, argv);
    input_init(ifile);
    cpp_init(options);
    type_init();
    symbol_init();

    if (cpp_only)
	preprocess();
    else
	translate();
    
    return errors > 0 ? EXIT_FAILURE : EXIT_SUCCESS;
}
