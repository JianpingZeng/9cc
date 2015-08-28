#include "test.h"
#include "cc.h"

static void test_lookup()
{
    struct symbol *sym;
    struct symbol *sym2;
    
    const char *var1 = strs("var1");
    sym = lookup(var1, identifiers);
    expectp(sym, NULL);
    
    expecti(SCOPE, GLOBAL);
    
    const char *name1 = strs("name1");
    sym = install(name1, &identifiers, SCOPE);
    expects(sym->name, "name1");
    
    sym2 = lookup(name1, identifiers);
    expectp(sym, sym2);
}

static void test_scope()
{
    expecti(SCOPE, GLOBAL);
    enter_scope();
    expecti(SCOPE, GLOBAL+1);
    
    const char *name1 = strs("name1");
    struct symbol *sym = lookup(name1, identifiers);
    expecti(sym->scope, GLOBAL);
    
    struct symbol *sym2 = install(name1, &identifiers, SCOPE);
    expectb(sym != sym2);
    expectp(identifiers->up->up, NULL);
    expecti(sym2->scope, sym->scope+1);
    
    exit_scope();
    expecti(SCOPE, GLOBAL);
    struct symbol *sym3 = lookup(name1, identifiers);
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