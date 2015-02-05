#include "cc.h"

CCOptions cc_options;

static const char *input_file;
static const char *output_file;

static void test()
{
    unsigned long n = 0;
    unsigned long kws = 0;
    unsigned long cons = 0;
    unsigned long ids = 0;
    unsigned long others = 0;
    while (gettok() != EOI) {
    	n++;
    	// if (token->id == ICONSTANT) {
    	//     if (token->v.type->op == INT) {
    	// 	logv("%s:%d: [%s] <%t>[0x%llx, %llu, 0%llo] %k",
    	// 	     src.file, src.line, tname(token->id), token->v.type, token->v.u.i, token->v.u.i, token->v.u.i, token);
    	//     } else if (token->v.type->op == UNSIGNED) {
    	// 	logv("%s:%d: [%s] <%t>[0x%llx, %llu, 0%llo] %k",
    	// 	     src.file, src.line, tname(token->id), token->v.type, token->v.u.u, token->v.u.i, token->v.u.i, token);
    	//     } else {
    	// 	assert(0);
    	//     }
    	// } else if (token->id == FCONSTANT) {
    	//     if (token->v.type == longdoubletype) {
    	// 	logv("%s:%d: [%s] <%t> [%Lf] %k",
    	// 	     src.file, src.line, tname(token->id), token->v.type, token->v.u.ld, token);
    	//     } else {
    	// 	logv("%s:%d: [%s] <%t> [%f] %k",
    	// 	     src.file, src.line, tname(token->id), token->v.type, token->v.u.d, token);
    	//     }
    	// } else {
    	//     logv("%s:%d: [%s] %k", src.file, src.line, tname(token->id), token);
    	// }
    	Log.i("%k", token);
	if (token->id == ID)
	    ids++;
	else if (token->id >= ICONSTANT && token->id <= SCONSTANT)
	    cons++;
	else if (token->id >= AUTO && token->id <= WHILE)
	    kws++;
	else
	    others++;
			     
    }
    Log.i("%lu tokens, %lu errors, %lu warnings", n, errors, warnings);
    // Log.v("%lu keywords, %lu constants, %lu identifiers, %lu others",
    // 	  kws, cons, ids, others);
}

static void cc_init()
{
    setlocale(LC_ALL, "");
    register_print_function('k', token_print_function);
    register_print_function('t', type_print_function);
    register_print_function('n', node_print_function);
    init_type();
    init_input();
}

int main(int argc, const char * argv[])
{
    FILE *fp;
    
    for (int i=1; i < argc; i++) {
	const char *arg = argv[i];
        if (!strcmp(arg, "-o")) {
	    if (++i >= argc) {
		fprintf(stderr, "missing target file while -o option given.\n");
		return EXIT_FAILURE;
	    }
	    output_file = argv[i];
	} else if (arg[0] == '-') {
	    // options
	} else {
	    input_file = arg;
	}
    }

    if (!input_file || !output_file) {
	return EXIT_FAILURE;
    }

    fp = freopen(input_file, "r", stdin);
    if (fp == NULL) {
	perror(input_file);
	return EXIT_FAILURE;
    }
    
    cc_init();
    test();

    fclose(fp);
	
    return errors > 0 ? EXIT_FAILURE : EXIT_SUCCESS;
}

