#include "internal.h"

static int size_code(const char *code)
{
    node_t *n = compile(code);
    node_t *n1 = DECL_EXTS(n)[0];
    node_t *ty1 = AST_TYPE(n1);
    return TYPE_SIZE(ty1);
}

static void expect1(const char *code)
{
    int size1 = size_code(code);

    // code for gcc
    struct strbuf *s = strbuf_new();
    strbuf_cats(s, "#include <stdio.h>\n");
    strbuf_cats(s, code);
    strbuf_cats(s, "\n");
    strbuf_cats(s, "int main(int argc, char *argv[]) {\n");
    strbuf_cats(s, "printf(\"%ld\\n\", sizeof (struct S));\n");
    strbuf_cats(s, "}\n");
    const char *ret = gcc_compile(s->str);
    int size2 = atoi(ret);
    free((void *)ret);
    
    expecti(size1, size2);
}

#define xx(s)    expect1(CODE(s))

static void test_struct1()
{
    xx(
       struct S {
	   char c;
       };
    );

    xx(
       struct S {
	   char a;
	   short b;
       };
    );
}

void testmain()
{
    START("typesize ...");
    test_struct1();
}
