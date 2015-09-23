#include "internal.h"

static void test_qual()
{
    node_t *ty1 = qual(CONST, inttype);
    node_t *ty2 = qual(CONST, ty1);
    
    expectp(unqual(ty1), inttype);
    expectp(unqual(ty2), inttype);
    expecti(TYPE_KIND(ty1), INT);
    expecti(_TYPE_KIND(ty1), CONST);
    expecti(isconst(ty1), true);
}

static void test_eq()
{
    node_t *ty1, *ty2;
    node_t *ty3, *ty4;
    
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
    
    expecti(TYPE_KIND(ty3), POINTER);
    expecti(_TYPE_KIND(ty3), CONST);
    
}

void testmain()
{
    START("type ...");
    type_init();
    test_qual();
    test_eq();
}
