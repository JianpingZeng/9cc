#include "internal.h"

static int size_code(const char *code)
{
    node_t *n = compile(code);
    node_t *n1 = DECL_EXTS(n)[0];
    node_t *ty1 = AST_TYPE(n1);
    return TYPE_SIZE(ty1);
}

static const char * gcc_code(const char *code)
{
    // code for gcc
    struct strbuf *s = strbuf_new();
    strbuf_cats(s, "#include <stdio.h>\n");
    strbuf_cats(s, code);
    strbuf_cats(s, "\n");
    strbuf_cats(s, "int main(int argc, char *argv[]) {\n");
    strbuf_cats(s, "printf(\"%ld\\n\", sizeof (struct S));\n");
    strbuf_cats(s, "}\n");

    return s->str;
}

static void expect3(int i, const char *code)
{
    int size1 = size_code(code);
    const char *ret = gcc_compile(gcc_code(code));
    int size2 = atoi(ret);
    free((void *)ret);
    
    expecti(size1, size2);
    expecti(size1, i);
}

#define xx(i, s)       expect3(i, CODE(s))

static void test_struct1()
{
    xx(1,
       struct S {
	   char c;
       };
    );

    xx(4,
       struct S {
	   char a;
	   short b;
       };
    );

    xx(4,
       struct S {
	   short a;
	   char b;
       };
    );

    xx(3,
       struct S {
	   char a;
	   char b;
	   char c;
       };
    );
}

void testmain()
{
    START("typesize ...");
    test_struct1();
}
