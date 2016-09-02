#include "cc.h"
#include "utils/sys.h"

struct cc_options opts;

static void parse_opts(int argc, char *argv[])
{
    const char *ifile = NULL, *ofile = NULL;

    for (int i = 1; i < argc; i++) {
        const char *arg = argv[i];
        if (!strcmp(arg, "-o")) {
            if (++i >= argc)
                die("missing target file while -o option given");
            ofile = argv[i];
        } else if (!strcmp(arg, "-ast-dump")) {
            opts.ast_dump = true;
        } else if (!strncmp(arg, "-ir-dump", 8)) {
            opts.ir_dump = true;
            if (strlen(arg) > 8)
                opts.ir_dump_level = atoi(arg+8);
        } else if (!strcmp(arg, "-Werror")) {
            opts.Werror = true;
        } else if (!strcmp(arg, "-Wall")) {
            opts.Wall = true;
        } else if (!strcmp(arg, "-E")) {
            opts.E = true;
        } else if (!strcmp(arg, "-fleading_underscore")) {
            opts.fleading_underscore = true;
        } else if (!strcmp(arg, "-ansi")) {
            opts.ansi = true;
        } else if (arg[0] != '-' || !strcmp(arg, "-")) {
            if (ifile == NULL)
                ifile = arg;
            else if (ofile == NULL)
                ofile = arg;
        }
    }

    // reopen stdin
    if (ifile && strcmp(ifile, "-")) {
        if (freopen(ifile, "r", stdin) == NULL)
            die("can't read file: %s", ifile);
    }
    // reopen stdout
    if (ofile && strcmp(ofile, "-")) {
        if (freopen(ofile, "w", stdout) == NULL)
            die("can't write file: %s", ofile);
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
                gen(exts);
        }
    }
    finalize();
}

static void preprocess(void)
{
    struct token *t = get_pptok(cpp_file);
    for (; t->id != EOI; t = get_pptok(cpp_file))
        printf("%s", tok2s(t));
}

int main(int argc, char *argv[])
{
    setup_sys();
    parse_opts(argc, argv);
    IM->progbeg(argc, argv);
    ir_init();
    symbol_init();
    type_init();
    cpp_init(argc, argv);

    if (opts.E)
        preprocess();
    else
        translate();
    
    IM->progend();

    return errors > 0 ? EXIT_FAILURE : EXIT_SUCCESS;
}
