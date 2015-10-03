#include "cc.h"

static const char *ifile;
static const char *ofile;
static FILE *fp;
static struct vector *options;

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
            vec_push(options, arg);
        } else {
            ifile = arg;
        }
    }
    
    fp = freopen(ifile, "r", stdin);
    if (fp == NULL) {
        perror(ifile);
        die("Can't open input file");
    }
}

static void translate(void)
{
    node_t *tree;
    tree = translation_unit();
    print_tree(tree);
    if (errors == 0) {
    	simplify(tree);
    	println("\nSimplified:");
    	print_tree(tree);
    	gen(tree, ofile);
    }
}

static void preprocess(void)
{
    
}

int cc_main(int argc, const char * argv[])
{
    parseopts(argc, argv);
    cpp_init(ifile, options);
    input_init();
    type_init();
    symbol_init();
    preprocess();
    // translate();
    fclose(fp);
    
    return errors > 0 ? EXIT_FAILURE : EXIT_SUCCESS;
}
