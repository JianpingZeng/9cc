#include "internal.h"

static void test_qual()
{
    struct type *ty1 = qual(CONST, inttype);
    struct type *ty2 = qual(CONST, ty1);
    
    expectp(unqual(ty1), inttype);
    expectp(unqual(ty2), inttype);
    expecti(kind(ty1), INT);
    expecti(ty1->kind, 0);
    expecti(ty1->q.is_const, true);
}

static void test_eq()
{
    
}

void testmain()
{
    print("type");
    type_init();
    test_qual();
    test_eq();
}