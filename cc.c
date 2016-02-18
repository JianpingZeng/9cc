#include "cc.h"

static FILE *outfp;

static void cc_init(const char *ifile, const char *ofile)
{
    if (ofile) {
        outfp = fopen(ofile, "w");
        if (outfp == NULL) {
            perror(ofile);
            exit(EXIT_FAILURE);
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
        if (errors == 0) {
            struct externals *exts = ir(tree);
            if (opts.ir_dump)
                print_ir(exts);
            else
                gen(exts, outfp);
        }
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
    symbol_init();
    type_init();
    cc_init(ifile, ofile);
    input_init(ifile);
    cpp_init(opts.cpp_options);
    builtin_init();
    
    if (opts.E)
        preprocess();
    else
        translate();

    return errors > 0 ? EXIT_FAILURE : EXIT_SUCCESS;
}
