#include "internal.h"

static int size_code(const char *code)
{
    node_t *n = compile(code);
    node_t **exts = DECL_EXTS(n);
    node_t *n1 = exts[array_len((void **)exts)-1];
    node_t *ty1 = AST_TYPE(n1);
    return TYPE_SIZE(ty1);
}

static void expect3(int i, const char *code, int size2)
{
    int size1 = size_code(code);

    if (size1 != size2)
	fail("expect %ld, but got %ld, code:\n" RED("%s"), size2, size1, code);
    if (size1 != i)
	fail("both got %ld, but guess %ld, code:\n" RED("%s"), size1, i, code);
}

static void test_struct()
{
#define xx(i, s)  \
    { \
	s;				 \
	expect3(i, CODE(s), sizeof (struct S)); \
    }
    
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

     xx(2,
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

     xx(2,
	struct S {
	    char x;
	    char :0;
	    char y;
	};
	);

     xx(5,
	struct S {
	    char a;
	    int :0;
	    char b;
	};
	);

     xx(8,
	struct S {
	    int a:1;
	    short b;
	    char c;
	};
       );

     xx(4,
	struct S {
	    int a:1;
	    char b;
	};
       );

#undef xx
}

static void test_union()
{

}

static void test_array()
{

}

static void test_others()
{
#define xx(i, s)  \
    { \
	s;				 \
	expect3(i, CODE(s), sizeof (e)); \
    }

    xx(4,
       enum E {
	   E1
       };
       enum E e;
       );

    xx(1,
       _Bool e;
       );

    xx(1,
       char e;
       );

    xx(2,
       short e;
       );

    xx(4,
       int e;
       );

    xx(4,
       unsigned e;
       );

#ifdef CONFIG_X32
    xx(4,
       long e;
       );
#elif defined (CONFIG_X64)
    xx(8,
       long e;
       );
#endif

    xx(8,
       long long e;
       );

    xx(4,
       float e;
       );

    xx(8,
       double e;
       );
    
#ifdef CONFIG_X32
    xx(8,
       long double e;
       );

    xx(4,
       void *e;
       );

    xx(4,
       char *e;
       );
#elif defined (CONFIG_X64)
    xx(16,
       long double e;
       );

    xx(8,
       void *e;
       );

    xx(8,
       char *e;
       );
#endif

#undef xx
}

void testmain()
{
    START("typesize ...");
    test_struct();
    test_union();
    test_array();
    test_others();
}
