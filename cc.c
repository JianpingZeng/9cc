#include "cc.h"

static const char *ifile;
static const char *ofile;
static FILE *fp;

static void parseopts(int argc, const char *argv[])
{
    for (int i=1; i < argc; i++) {
        const char *arg = argv[i];
        if (!strcmp(arg, "-o")) {
            if (++i >= argc)
                die("missing target file while -o option given");
            ofile = argv[i];
        } else if (arg[0] == '-') {
            // options
        } else {
            ifile = arg;
        }
    }
    
    if (!ifile || !ofile)
        die("input/output file not specified");
    
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
	gen(tree, ofile);
    }
}

int cc_main(int argc, const char * argv[])
{
    parseopts(argc, argv);
    input_init();
    type_init();
    symbol_init();
    translate();
    fclose(fp);
    
    return errors > 0 ? EXIT_FAILURE : EXIT_SUCCESS;
}
