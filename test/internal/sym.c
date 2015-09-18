#include "test.h"
#include "cc.h"

static void test_lookup()
{
    union node *sym;
    union node *sym2;
    
    const char *var1 = strs("var1");
    sym = lookup(var1, identifiers);
    expectp(sym, NULL);
    
    expecti(SCOPE, GLOBAL);
    
    const char *name1 = strs("name1");
    sym = install(name1, &identifiers, SCOPE);
    expects(SYM_NAME(sym), "name1");
    
    sym2 = lookup(name1, identifiers);
    expectp(sym, sym2);
}

static void test_scope()
{
    expecti(SCOPE, GLOBAL);
    enter_scope();
    expecti(SCOPE, GLOBAL+1);
    
    const char *name1 = strs("name1");
    union node *sym = lookup(name1, identifiers);
    expecti(SYM_SCOPE(sym), GLOBAL);
    
    union node *sym2 = install(name1, &identifiers, SCOPE);
    expectb(sym != sym2);
    expectp(identifiers->up->up, NULL);
    expecti(SYM_SCOPE(sym2), SYM_SCOPE(sym)+1);
    
    exit_scope();
    expecti(SCOPE, GLOBAL);
    union node *sym3 = lookup(name1, identifiers);
    expectp(sym3, sym);
}

const char *testname()
{
    return "symbol";
}

void testmain()
{
    test_lookup();
    test_scope();
}
