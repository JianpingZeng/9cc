#include "internal.h"

static int size_code(const char *code)
{
    node_t *n = compile(code);
    node_t **exts = DECL_EXTS(n);
    node_t *n1 = exts[LIST_LEN(exts)-1];
    node_t *ty1 = AST_TYPE(n1);
    return TYPE_SIZE(ty1);
}

static void expect3(int i, const char *prefix, const char *code, int size2)
{
    const char *code1;
    if (prefix)
	code1 = format("%s %s", prefix, code);
    else
	code1 = code;
    
    int size1 = size_code(code1);

    if (size1 != size2)
	fail("expect %d, but got %d, code:\n" RED("%s"), size2, size1, code);
    if (size1 != i)
	fail("both got %d, but guess %d, code:\n" RED("%s"), size1, i, code);
}

#define xx(i, s)				\
    {						\
	s;					\
	expect3(i, NULL, CODE(s), sizeof (e));	\
    }

static void test_basic()
{    
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
}


/* Test cases for structure.
 *
 */

#undef xx
#define xx(i1, i2, s)						\
    {								\
	struct S s;						\
	expect3(i1, "struct S", CODE(s), sizeof (struct S));	\
    }								\
    {								\
	union S s;						\
	expect3(i2, "union S", CODE(s), sizeof (union S));	\
    }

#undef yy
#define yy(i1, i2, pre, s)						\
    {									\
	pre;								\
	struct S s;							\
        expect3(i1, format("%s; struct S", CODE(pre)), CODE(s), sizeof (struct S)); \
    }									\
    {									\
	pre;								\
	union S s;							\
	expect3(i2, format("%s; union S", CODE(pre)), CODE(s), sizeof (union S)); \
    }


// ONLY non-bitfields
static void test_struct1()
{
    // 1 field basic type
    
    xx(1, 1,
       {
	   _Bool a;
       };
       );
    
    xx(1, 1,
       {
	   char a;
       };
       );

    xx(1, 1,
       {
	   signed char a;
       };
       );

    xx(1, 1,
       {
	   unsigned char a;
       };
       );

    xx(2, 2,
       {
	   short a;
       };
       );

    xx(2, 2,
       {
	   unsigned short a;
       };
       );

    xx(4, 4,
       {
	   int a;
       };
       );

    xx(4, 4,
       {
	   unsigned a;
       };
       );

#ifdef CONFIG_X32
    xx(4, 4,
       {
	   long a;
       };
       );

    xx(4, 4,
       {
	   unsigned long a;
       };
       );
#elif defined (CONFIG_X64)
    xx(8, 8,
       {
	   long a;
       };
       );

    xx(8, 8,
       {
	   unsigned long a;
       };
       );
#endif

    xx(8, 8,
       {
	   long long a;
       };
       );

    xx(8, 8,
       {
	   unsigned long long a;
       };
       );

    xx(4, 4,
       {
	   float a;
       };
       );

    xx(8, 8,
       {
	   double a;
       };
       );

#ifdef CONFIG_X32
    xx(8, 8,
       {
	   long double a;
       };
       );

    xx(4, 4,
       {
	   void *p;
       };
       );

    xx(4, 4,
       {
	   char *p;
       };
       );
#elif defined (CONFIG_X64)
    xx(16, 16,
       {
	   long double a;
       };
       );

    xx(8, 8,
       {
	   void *p;
       };
       );

    xx(8, 8,
       {
	   char *p;
       };
       );
#endif


    xx(4, 2,
       {
	   char a;
	   short b;
       };
       );

    xx(4, 2,
       {
	   short a;
	   char b;
       };
       );

    xx(3, 1,
       {
	   char a;
	   char b;
	   char c;
       };
       );

    xx(24, 16,
       {
	   double d;
	   char c[10];
       };
       );

    xx(12, 4,
        {
	    char a;
	    short b;
	    int c;
	    char d;
	};
	);

    xx(8, 4,
        {
	    float x;
	    char n[1];
	};
	);

    xx(6, 4,
        {
	    short s;
	    char n[3];
	};
       );

    xx(16, 16,
       {
	   struct {
	       int b;
	       double d;
	   }s;
       };
       );

    xx(4, 4,
       {
    	   union {
    	       char a[3];
    	       short b;
    	   }u;
       };
       );

    xx(24, 16,
       {
	   char a;
	   struct {
	       int b;
	       double d;
	   }s;
       };
       );

    xx(24, 16,
       {
	   struct {
	       int b;
	       double d;
	   }s;
	   char a;
       };
       );

    xx(32, 24,
       {
	   struct C {
	       int b;
	       char a[1];
	   }c;

	   struct C array[3];
       };
       );

    yy(28, 24,
       struct C {
    	   int b;
    	   char a[1];
       };
       ,
       {
    	   char a;
    	   struct C array[3];
       };
       );
}

// ONLY bitfields
static void test_struct2()
{
    xx(2, 2,
       {
	   char a:1;
	   short b:1;
       };
       );

    xx(2, 2,
       {
	   short a:1;
	   char b:1;
       };
       );

    xx(4, 4,
       {
	   char a:1;
	   short b:1;
	   int c:1;
       };
       );

    xx(4, 4,
       {
	   char a:1;
	   short b:1;
	   char :0;
	   int c:1;
       };
       );

    xx(4, 4,
       {
	   char a:1;
	   short b:1;
	   short :0;
	   int c:1;
       };
       );

    xx(8, 4,
       {
	   char a:1;
	   short b:1;
	   int :0;
	   int c:1;
       };
       );

    xx(8, 4,
       {
	   char a:1;
	   short b:1;
	   unsigned :0;
	   int c:1;
       };
       );

    xx(3, 1,
       {
	   char a:1;
	   short :0;
	   char c:1;
       };
       );

    xx(2, 2,
       {
	   char a:1;
	   short b:1;
	   char :0;
       };
       );

    xx(2, 2,
       {
	   short a:1;
	   char b:1;
	   char :0;
       };
       );

    xx(2, 2,
       {
     	   short a:6;
     	   char b:5;
     	   char :0;
       };
       );
}

// both
static void test_struct3()
{
    xx(2, 1,
        {
	    char x;
	    char :0;
	    char y;
	};
	);

    xx(5, 1,
        {
	    char a;
	    int :0;
	    char b;
	};
	);

    xx(8, 4,
        {
	    int a:1;
	    short b;
	    char c;
	};
       );

    xx(4, 4,
        {
	    int a:1;
	    char b;
	};
       );
}

static void test_struct()
{
    test_struct1();
    test_struct2();
    test_struct3();
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
    test_basic();
    test_struct();
    test_array();
    test_others();
}
