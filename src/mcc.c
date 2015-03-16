/*
 * mcc
 * frontend of the c compiler
 */
#include "mcc.h"

// configs
static struct configs {
    unsigned option_E : 1;
    unsigned option_c : 1;
    unsigned option_S : 1;
} config;

static char *tmpdir;
static char *output_file;

// options
static unsigned fails;
static unsigned unit;

static const char *version = "0.0";

static void print_opt(const char *opt, const char *message)
{
    fprintf(stderr, "  %-20s%s\n", opt, message);
}

static void usage()
{
    fprintf(stderr,
	    "OVERVIEW: mcc - A Standard C Compiler v%s\n\n"
	    "USAGE: mcc [options] <inputs>\n\n"
	    "OPTIONS:\n", version);
    print_opt("-c",              "Only run preprocess, compile, and assemble steps");
    print_opt("-I <dir>",        "Add directory to include search path");
    print_opt("-h, --help",      "Display available options");
    print_opt("-E",              "Only run the preprocessor");
    print_opt("-o <file>",       "Write output to <file>");
    print_opt("-S",              "Only run preprocess and compilation steps");
    print_opt("-v, --version",   "Display version and options");
}

static void append(struct vector *v, const char *str)
{
    struct string *s = new_string();
    str_cats(s, str);
    vec_push(v, str_to_array(s));
}

static char *tempname(const char *hint)
{
    unsigned len = strlen(tmpdir)+strlen(hint)+128;
    void *p = cc_malloc(len);
    hint = hint ? hint : "tmp";
    snprintf(p, len, "%s/mcc.%u.%s", tmpdir, unit, hint);
    return p;
}

static void translate(void *elem, void *context)
{
    char *inputfile = (char *)elem;
    struct vector *options = (struct vector *)context;
    struct vector *v;
    int argc;
    char **argv;
    static const char *cpp[] = {"cpp", "$in", "-o", "$out", 0};
    static const char *cc[] = {"cc", "$in", "-o", "$out", 0};
    char *ifile, *sfile;
    
    unit++;
    
    if (!file_exists(inputfile)) {
	fprintf(stderr, "input file '%s' not exists.\n", inputfile);
	fails++;
	return;
    }

    // preprocess
    if (config.option_E) {
	if (output_file)
	    cpp[3] = output_file;
	else 
	    cpp[2] = 0;
    } else {
	ifile = tempname("cpp.i");
	cpp[3] = ifile;
    }
    cpp[1] = inputfile;
    v = new_vector();
    vec_add_from_array(v, (void **)cpp);
    vec_add_from_vector(v, options);
    argc = vec_len(v);
    argv = (char **) vtoa(v);
    if (cpp_main(argc, argv) == EXIT_FAILURE) {
	fails++;
	return;
    }
    if (config.option_E)
	return;

    // compile
    if (config.option_S) {
	if (output_file) {
	    cc[3] = output_file;
	}
	else {
	    char *new_file = replace_suffix(inputfile, "s");
	    sfile = cc_malloc(strlen(new_file)+1);
	    strcpy(sfile, new_file);
	    free(new_file);
	    cc[3] = sfile;
	}
    }
    else {
	sfile = tempname("cc.s");
	cc[3] = sfile;
    }
    cc[1] = ifile;
    v = new_vector();
    vec_add_from_array(v, (void **)cc);
    vec_add_from_vector(v, options);
    argc = vec_len(v);
    argv = (char **) vtoa(v);
    if (cc_main(argc, argv) == EXIT_FAILURE) {
	fails++;
	return;
    }

    if (ifile)
	cc_free(ifile);
    if (sfile)
	cc_free(sfile);

    free_unit();
}

int main(int argc, char **argv)
{
    int ret = EXIT_SUCCESS;
    struct vector *inputlist = new_vector();
    struct vector *optionlist = new_vector();
    
    tmpdir = mk_temp_dir();
    if (!tmpdir)
	die("Can't make temporary directory.");
    
    for (int i=1; i < argc; i++) {
	char *arg = argv[i];
	if (!strcmp(arg, "-h") || !strcmp(arg, "--help") ||
	    !strcmp(arg, "-v") || !strcmp(arg, "--version")) {
	    usage();
	    exit(EXIT_FAILURE);
	} else if (!strcmp(arg, "-o")) {
	    if (++i >= argc) {
		fprintf(stderr, "missing target file while -o option specified.\n");
		usage();
		exit(EXIT_FAILURE);
	    }
	    output_file = argv[i];
	} else if (!strcmp(arg, "-E")) {
	    config.option_E = 1;
	} else if (!strcmp(arg, "-c")) {
	    config.option_c = 1;
	} else if (!strcmp(arg, "-S")) {
	    config.option_S = 1;
	} else if (arg[0] == '-') {
	    if (arg[1] == 'I') {
		char *abs = expanduser(arg+2);
		if (abs) {
		    struct string *s = new_string();
		    str_cats(s, "-I");
		    str_cats(s, abs);
		    vec_push(optionlist, str_to_array(s));
		    free(abs);
		}
	    } else {
		append(optionlist, arg);
	    }
	} else {
	    append(inputlist, arg);
	}
    }

    if (vec_len(inputlist) == 0) {
	fprintf(stderr, "no input file.\n");
	return EXIT_FAILURE;
    }
    
    vec_foreach(inputlist, translate, optionlist);
    
    purge_vector(inputlist);
    purge_vector(optionlist);
    rmdir(tmpdir);
    
    return ret;
}
