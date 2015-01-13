#include "c.h"

static void test()
{
    tok = gettok();
    expr();
//    tok = gettok();
//	int t = lookahead();
//    for (; tok != EOI ; ) {
//        const char *tokname = tname(tok);
//        printf("[Line %d] %s %s\n", lineno, tokname, tname(t));
//        tok = gettok();
//		assert(t == tok);
//		t = lookahead();
//		int tt = lookahead();
//		assert(t == tt);
//    }
//    printf("[Line %d]\n", lineno);
}

int cc_option_std;
int cc_option_w;

int main(int argc, const char * argv[])
{
	const char *filename;
    FILE *fp;
	
	if (argc < 2) {
		fprintf(stderr, "Usage: %s <filename>\n", argv[0]);
		return EXIT_FAILURE;
	}
	filename = argv[1];
	
	if ((fp = freopen(filename, "r", stdin)) == NULL) {
		perror(filename);
		return EXIT_FAILURE;
	}
    
    log("open %s ok.\n", filename);
    
	init_type();
	init_input();
    
    if (DEBUG) {
        test();
    }
    else {
        translation_unit();
    }
    
    fclose(fp);
	
    return errcnt > 0;
}

