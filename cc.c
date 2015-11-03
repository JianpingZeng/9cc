#include "cc.h"
#include "sys.h"

static const char *ifile;
static const char *ofile;
static FILE *outfp;
static struct vector *options;
static bool cpp_only;
struct interface *IR;

static void init_IR(void)
{
    IR = zmalloc(sizeof(struct interface));
                       // size  align  rank
#ifdef CONFIG_X32
    IR->boolmetrics        = (struct metrics){1,  1,  10};
    IR->charmetrics        = (struct metrics){1,  1,  20};
    IR->shortmetrics       = (struct metrics){2,  2,  30};
    IR->wcharmetrics       = (struct metrics){4,  4,  40};
    IR->intmetrics         = (struct metrics){4,  4,  40};
    IR->longmetrics        = (struct metrics){4,  4,  50};
    IR->longlongmetrics    = (struct metrics){8,  8,  60};
    IR->floatmetrics       = (struct metrics){4,  4,  70};
    IR->doublemetrics      = (struct metrics){8,  8,  80};
    IR->longdoublemetrics  = (struct metrics){8,  8,  90};
    IR->ptrmetrics         = (struct metrics){4,  4};
    IR->zerometrics        = (struct metrics){0,  1};
#elif defined CONFIG_X64
    IR->boolmetrics        = (struct metrics){1,  1,  10};
    IR->charmetrics        = (struct metrics){1,  1,  20};
    IR->shortmetrics       = (struct metrics){2,  2,  30};
    IR->wcharmetrics       = (struct metrics){4,  4,  40};
    IR->intmetrics         = (struct metrics){4,  4,  40};
    IR->longmetrics        = (struct metrics){8,  8,  50};
    IR->longlongmetrics    = (struct metrics){8,  8,  60};
    IR->floatmetrics       = (struct metrics){4,  4,  70};
    IR->doublemetrics      = (struct metrics){8,  8,  80};
    IR->longdoublemetrics  = (struct metrics){16, 16, 90};
    IR->ptrmetrics         = (struct metrics){8,  8};
    IR->zerometrics        = (struct metrics){0,  1};
#else
#error "architecture not defined."
#endif
    IR->uname = get_uname();
}

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
    print_tree(tree);
    if (errors == 0)
	gen(tree, outfp, ifile);
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
    print_alloc_stat();
    if (outfp != stdout)
	fclose(outfp);
}

int cc_main(int argc, const char * argv[])
{
    atexit(cc_exit);
    init_IR();
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
