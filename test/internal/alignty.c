#include "internal.h"

static node_t * get_ty(const char *code)
{
    node_t *n = compile(code);
    node_t **exts = DECL_EXTS(n);
    node_t *n1 = exts[array_len((void **)exts)-1];
    node_t *ty1 = AST_TYPE(n1);
    return ty1;
}

static void expect2(node_t *ty1, node_t **ty22, const char *code)
{
    node_t *ty2 = *ty22;
    int align1 = TYPE_ALIGN(ty1);
    int align2 = TYPE_ALIGN(ty2);
    if (align1 != align2)
	fail("expect %ld ('%s'), but got %ld ('%s'), code:\n" RED("%s"),
	     align2, type2s(ty2), align1, type2s(ty1), code);
}

/* Pass pointer to pointer.
 * Otherwise variables like inttype
 * may be NULL.
 */

#define xx(ty, s)    expect2(get_ty(CODE(s)), &ty##type, CODE(s))

static void test_alignty()
{
    xx(int,
       int a;
       );

    xx(int,
       const int a;
       );

    xx(short,
       struct S {
	   char a;
	   struct C {
	       short a[10];
	       char b;
	   } b;
       };
       );

    xx(double,
       struct S {
	   double d;
	   char c[10];
       };

       struct S array[2];
       );

    xx(double,
       struct S {
	   double d;
	   char c[10];
       };

       struct C {
	   struct S a[2];
	   int b;
       };
       );

    xx(double,
       struct S {
	   double d;
	   char c[10];
       };

       struct C {
	   struct S a[2];
	   int b;
       };

       struct C array[2];
       );

    xx(double,
       struct S {
	   char d;
	   double c[5][3];
       };

       struct C {
	   struct S a[2];
	   int b;
       };
       );
}

void testmain()
{
    START("alignty ...");
    test_alignty();
}
