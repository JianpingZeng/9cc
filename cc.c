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

static void define_builtin_func(const char *name, node_t *rtype, struct vector *ptypes)
{
    node_t *ftype = func_type();
    _TYPE_TYPE(ftype) = rtype;
    struct vector *params = vec_new();
    cc_assert(SCOPE == GLOBAL);
    enter_scope();
    for (int i = 0; i < vec_len(ptypes); i++) {
        node_t *ptype = vec_at(ptypes, i);
        node_t *param = anonymous(&identifiers, SCOPE);
        SYM_TYPE(param) = ptype;
        vec_push(params, param);
    }
    exit_scope();
    _TYPE_PARAMS(ftype) = (node_t **)vtoa(params);
    node_t *sym = install(name, &identifiers, GLOBAL);
    SYM_TYPE(sym) = ftype;
    SYM_DEFINED(sym) = true;
}

static void builtin_init(void)
{
    struct vector *voidptr = vec_new1(ptr_type(chartype));
    struct vector *voidptr2 = vec_new();
    vec_push(voidptr2, ptr_type(voidtype));
    vec_push(voidptr2, ptr_type(voidtype));
    // __builtin_va_list
    define_builtin_func(BUILTIN_VA_START, voidtype, voidptr2);
    define_builtin_func(BUILTIN_VA_END, voidtype, voidptr);
    define_builtin_func(BUILTIN_VA_COPY, voidtype, voidptr2);
    define_builtin_func(BUILTIN_VA_ARG, voidtype, voidptr2);
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
