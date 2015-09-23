#include "internal.h"

struct context {
    const char *code;
    const char *type;
    const char *output;
};

static int subprocess(void *context)
{
    struct context *con = (struct context *)context;
    const char *code = con->code;
    const char *type = con->type;
    const char *output = con->output;
    node_t *n = compile(code);
    node_t *n1 = DECL_EXTS(n)[0];
    node_t *ty = SYM_TYPE(DECL_SYM(n1));
    const char *ret;
    const char *p1, *p2;
    
    if (strcmp(TYPE_NAME(unqual(ty)), type))
        fail("type not equal: %s != %s", TYPE_NAME(unqual(ty)), type);
    
    ret = type2s(ty);
    
    p1 = ret;
    p2 = output;
    
    for (;;) {
        while (isspace((unsigned char)*ret))
            ret++;
        
        while (isspace((unsigned char)*output))
            output++;
        
        if (*ret == '\0' || *output == '\0')
            break;
        
        if (*ret != *output)
            fail("'%s' != '%s' at '%c' != '%c'", p1, p2, *ret, *output);
        
        ret++;
        output++;
    }
    
    if (*ret != '\0' || *output != '\0')
        fail("'%s' != '%s'", p1, p2);
    
    return EXIT_SUCCESS;
}

static void expect2(const char *code, const char *type, const char *output)
{
    int ret;
    struct context context = { code, type, output };
    
    ret = subprocess(&context);
    if (ret != EXIT_SUCCESS)
        exit(1);
}

static void test_type2s()
{
    expect2("int (* (f (char *))) (float);",
            "function",
            "int (* (char *)) (float)");
    
    expect2("int (*f) (char *, int);",
            "pointer",
            "int (*) (char *, int)");
    
    expect2("const int a;",
            "int",
            "const int");
    
    expect2("const int * restrict b;",
            "pointer",
            "const int * restrict");
    
    expect2("int *a[10];",
            "array",
            "int *[10]");
    
    expect2("void (*a[10]) (int *a, const char *c);",
            "array",
            "void (*[10]) (int *, const char *)");
    
    expect2("void (* ((*f) (char *))) (int);",
            "pointer",
            "void (* (*) (char *)) (int)");
    
    expect2("void (* (* f(char *))) (int);",
            "function",
            "void (** (char *)) (int)");
}

const char *testname()
{
    return "type2s";
}

void testmain()
{
    test_type2s();
}
