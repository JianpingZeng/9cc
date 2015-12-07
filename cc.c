#include "cc.h"

static FILE *outfp;

static void cc_init(const char *ifile, const char *ofile)
{
    if (ofile) {
        outfp = fopen(ofile, "w");
        if (outfp == NULL) {
            perror(ofile);
            die("Can't open file %s", ofile);
        }
    } else {
        outfp = stdout;
    }
}

static void translate(void)
{
    node_t *tree;
    tree = translation_unit();
    if (opts.ast_dump) {
        print_tree(tree);
    } else {
        if (errors == 0)
            gen(simplify(tree), outfp);
    }
}

static void preprocess(void)
{
    struct vector *v = all_pptoks();
    for (int i = 0; i < vec_len(v); i++) {
        struct token *t = vec_at(v, i);
        fprintf(outfp, "%s", t->name);
    }
}

static void cc_exit(void)
{
    if (outfp != stdout)
        fclose(outfp);
}

int cc_main(const char *ifile, const char *ofile)
{
    atexit(cc_exit);
    cc_init(ifile, ofile);
    input_init(ifile);
    cpp_init(opts.cpp_options);
    type_init();
    symbol_init();

    if (opts.E)
        preprocess();
    else
        translate();

    return errors > 0 ? EXIT_FAILURE : EXIT_SUCCESS;
}
