#include "cc.h"
#include "string.h"

static void test()
{
    
}

int cc_option_std;
int cc_option_w;

static void at_begin(int argc, const char **argv)
{
    
}

static void at_end(int argc, const char **argv)
{
    
}

static void cc_init()
{
    register_print_function('k', token_print_function);
    register_print_function('t', type_print_function);
    register_print_function('n', node_print_function);
    init_type();
    init_input();
}

int main(int argc, const char * argv[])
{
    for (int i=0; i < argc; i++) {
	printf("argv[%d]: %s\n", i, argv[i]);
    }
    return EXIT_FAILURE;
    
    at_begin(argc, argv);

    cc_init();
    TranslationUnitDecl *node = translation_unit();
    log("Node:\n%n", node);

    at_end(argc, argv);
//    string *str = new(String);
//    printf("%s\n", classof(string)->name);
//	const char *filename;
//    FILE *fp;
//	
//	if (argc < 2) {
//		fprintf(stderr, "Usage: %s <filename>\n", argv[0]);
//		return EXIT_FAILURE;
//	}
//	filename = argv[1];
//	
//	if ((fp = freopen(filename, "r", stdin)) == NULL) {
//		perror(filename);
//		return EXIT_FAILURE;
//	}
//    
//    log("open %s ok.\n", filename);
//    
//	init_type();
//	init_input();
//    
//    if (DEBUG) {
//        test();
//    }
//    else {
//        translation_unit();
//    }
//    
//    fclose(fp);
//	
   return errors > 0;
}

