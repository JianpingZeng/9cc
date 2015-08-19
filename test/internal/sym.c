#include "internal.h"

static void test_lookup()
{
    struct symbol *sym;
    
    sym = lookup("var1", identifiers);
    expectp(sym, NULL);
}

static void test_scope()
{
    
}

void testmain()
{
    print("symbol");
    test_lookup();
    test_scope();
}