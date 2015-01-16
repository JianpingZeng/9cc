#include "c.h"
#include "string.h"

static void test()
{
    
}

int cc_option_std;
int cc_option_w;

int main(int argc, const char * argv[])
{
    char c;
    printf("%p\n", &c);
    print("%p\n", &c);
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
//    return errcnt > 0;
}

