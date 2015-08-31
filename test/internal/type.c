#include "test.h"
#include "cc.h"

static void test_qual()
{
    struct type *ty1 = qual(CONST, inttype);
    struct type *ty2 = qual(CONST, ty1);
    
    expectp(unqual(ty1), inttype);
    expectp(unqual(ty2), inttype);
    expecti(kind(ty1), INT);
    expecti(ty1->kind, CONST);
    expecti(isconst(ty1), true);
}

static void test_eq()
{
    struct type *ty1, *ty2;
    struct type *ty3, *ty4;
    
    ty1 = qual(CONST, inttype);
    expecti(eqtype(ty1, inttype), false);
    ty2 = qual(CONST, inttype);
    expecti(eqtype(ty1, ty2), true);
    ty2 = qual(VOLATILE, ty2);
    expecti(eqtype(ty1, ty2), false);
    
    ty3 = ptr_type(inttype);
    ty3 = qual(CONST, ty3);
    expecti(eqtype(ty2, ty3), false);
    ty4 = ptr_type(inttype);
    expecti(eqtype(ty3, ty4), false);
    ty4 = qual(CONST, ty4);
    expecti(eqtype(ty3, ty4), true);
    
    expecti(kind(ty3), POINTER);
    expecti(ty3->kind, CONST);
    
}

const char *testname()
{
    return "type";
}

void testmain()
{
    type_init();
    test_qual();
    test_eq();
}