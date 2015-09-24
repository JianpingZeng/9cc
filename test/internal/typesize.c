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

static void expect2(const char *code)
{
    int size1 = size_code(code);
    const char *ret = gcc_compile(gcc_code(code));
    int size2 = atoi(ret);
    free((void *)ret);

    expecti(size1, size2);
}

#define xx(i, s)       expect3(i, CODE(s))
#define yy(s)          expect2(CODE(s))

static void test_struct()
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

    xx(2,
       struct S {
	   char a:1;
	   short b:1;
       };
       );

    xx(2,
       struct S {
	   short a:1;
	   char b:1;
       };
       );

    xx(4,
       struct S {
	   char a:1;
	   short b:1;
	   int c:1;
       };
       );

    xx(4,
       struct S {
	   char a:1;
	   short b:1;
	   char :0;
	   int c:1;
       };
       );

    xx(4,
       struct S {
	   char a:1;
	   short b:1;
	   short :0;
	   int c:1;
       };
       );

    xx(8,
       struct S {
	   char a:1;
	   short b:1;
	   int :0;
	   int c:1;
       };
       );

     xx(8,
       struct S {
	   char a:1;
	   short b:1;
	   unsigned :0;
	   int c:1;
       };
       );

     xx(3,
       struct S {
	   char a:1;
	   short :0;
	   char c:1;
       };
       );

     xx(2,
       struct S {
	   char a:1;
	   short b:1;
	   char :0;
       };
       );

     xx(2,
       struct S {
	   short a:1;
	   char b:1;
	   char :0;
       };
       );

     yy(
       struct S {
     	   short a:6;
     	   char b:5;
     	   char :0;
       };
       );
     
     xx(24,
       struct S {
	   double d;
	   char c[10];
       };
       );

     xx(12,
	struct S {
	    char a;
	    short b;
	    int c;
	    char d;
	};
	);

     xx(8,
	struct S {
	    float x;
	    char n[1];
	};
	);

     xx(6,
	struct S {
	    short s;
	    char n[3];
	};
       );
}

static void test_union()
{

}

static void test_array()
{

}

static void test_others()
{

}

void testmain()
{
    START("typesize ...");
    test_struct();
    test_union();
    test_array();
    test_others();
}
