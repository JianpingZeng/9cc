#include "mcc.h"

static void warning(const char *fmt, ...);
static void error(const char *fmt, ...);
static void cpp_init();
static void cpp_exit();
static void input_init();
static void preprocess();
static void parse_opts(int argc, char **argv);
static void fillbuf();

static struct vector *search_path;
static FILE *ifp;
static FILE *ofp;
static int errors;

#define EOI         (-1)
#define LBUFSIZE    1024
#define RBUFSIZE    4096
static char *ibuf;
static char *pc;
static char *pe;
static long bread;

struct token {
    int id;
    char *name;
};

int cpp_main(int argc, char **argv)
{
    cpp_init();
    parse_opts(argc, argv);
    input_init();
    preprocess();
    cpp_exit();
    return errors > 0 ? EXIT_FAILURE : EXIT_SUCCESS;
}

static void cpp_init()
{
    search_path = new_vector();
    ifp = NULL;
    ofp = NULL;
    ibuf = cc_malloc(LBUFSIZE + RBUFSIZE + 1);
}

static void cpp_exit()
{
    free_vector(search_path);
    if (ifp)
        fclose(ifp);
    if (ofp)
        fclose(ofp);
    cc_free(ibuf);
}

static void input_init()
{
    pc = pe = ibuf + LBUFSIZE;
    bread = -1;
    fillbuf();
}

static void fillbuf()
{
    if (bread == 0) {
        if (pc > pe)
            pc = pe;
        return;
    }
    
    if (pc >= pe) {
        pc = ibuf + LBUFSIZE;
    } else {
        long n;
        char *dst, *src;
        
        // copy
        n = pe - pc;
        dst = ibuf + LBUFSIZE - n;
        src = pc;
        while (src < pe)
            *dst++ = *src++;
        
        pc = ibuf + LBUFSIZE - n;
    }
    
    if (feof(stdin))
        bread = 0;
    else
        bread = fread(ibuf + LBUFSIZE, 1, RBUFSIZE, stdin);
    
    if (bread < 0)
        die("read error");
    
    pe = ibuf + LBUFSIZE + bread;
    *pe = '\n';
}

static int gettok()
{
    register char *rpc;
    
    
}

static void preprocess()
{
    
}

static void add_search_path(const char *path)
{
    assert(path);
    const char *abspath = expanduser(path);
    if (is_directory(abspath)) {
        
    } else {
        warning("%s: not a directory", path);
    }
    free(abspath);
}

static void parse_opts(int argc, char **argv)
{
    const char *input_file = NULL;
    const char *output_file = NULL;
    
    for (int i=1; i < argc; i++) {
        const char *arg = argv[i];
        if (!strcmp(arg, "-o")) {
            if (++i >= argc)
                die("missing file name after '-o'");
            output_file = argv[i];
        } else if (!strncmp(arg, "-D", 2)) {
            
        } else if (!strncmp(arg, "-U", 2)) {
            
        } else if (!strncmp(arg, "-I", 2)) {
            if (strlen(arg) > 2)
                add_search_path(arg+2);
            else if (++i < argc)
                add_search_path(argv[i]);
            else
                die("missing path after '-I'");
        } else if (arg[0] != '-') {
            input_file = arg;
        }
    }
    
    if (input_file && !file_exists(input_file))
        die("input file '%s' not exist.", input_file);
    
    ifp = freopen(input_file, "r", stdin);
    if (ifp == NULL) {
        perror(input_file);
        exit(EXIT_FAILURE);
    }
    
    if (output_file) {
        ofp = freopen(output_file, "w", stdout);
        if (ofp == NULL) {
            perror(output_file);
            exit(EXIT_FAILURE);
        }
    }
}

static void warning(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    fprintf(stderr, "warning: ");
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    va_end(ap);
}

static void error(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    fprintf(stderr, "error: ");
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    va_end(ap);
    errors++;
}
