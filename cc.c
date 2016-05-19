#include "cc.h"

static FILE *outfp;
static const char *ifile, *ofile;
struct cc_options opts;

static void parse_opts(int argc, char *argv[])
{
    opts.cpp_options = vec_new();
    for (int i = 1; i < argc; i++) {
        const char *arg = argv[i];
        if (!strcmp(arg, "-o")) {
            if (++i >= argc)
                die("missing target file while -o option given");
            ofile = argv[i];
        } else if (arg[0] == '-') {
            if (!strncmp(arg, "-I", 2) ||
                !strncmp(arg, "-D", 2) ||
                !strncmp(arg, "-U", 2))
                vec_push(opts.cpp_options, (char *)arg);
            else if (!strcmp(arg, "-ast-dump"))
                opts.ast_dump = true;
            else if (!strcmp(arg, "-ir-dump"))
                opts.ir_dump = true;
            else if (!strcmp(arg, "-Werror"))
                opts.Werror = true;
            else if (!strcmp(arg, "-Wall"))
                opts.Wall = true;
            else if (!strcmp(arg, "-E"))
                opts.E = true;
            else if (!strcmp(arg, "-fleading_underscore"))
                opts.fleading_underscore = true;
            else if (!strncmp(arg, "-fversion=", 10))
                opts.version = atoi(arg+10);
        } else {
            ifile = arg;
        }
    }
}

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

int main(int argc, char *argv[])
{
    parse_opts(argc, argv);
    atexit(cc_exit);
    symbol_init();
    type_init();
    cc_init(ifile, ofile);
    input_init(ifile);
    cpp_init(opts.cpp_options);
    
    if (opts.E)
        preprocess();
    else
        translate();

    return errors > 0 ? EXIT_FAILURE : EXIT_SUCCESS;
}
