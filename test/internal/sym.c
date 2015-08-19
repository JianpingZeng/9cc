#include "internal.h"

static void test_lookup()
{
    struct symbol *sym;
    struct symbol *sym2;
    
    const char *var1 = strings("var1");
    sym = lookup(var1, identifiers);
    expectp(sym, NULL);
    
    expecti(SCOPE, GLOBAL);
    
    const char *name1 = strings("name1");
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
    
    const char *name1 = strings("name1");
    struct symbol *sym = lookup(name1, identifiers);
    expecti(sym->scope, GLOBAL);
}

void testmain()
{
    print("symbol");
    test_lookup();
    test_scope();
}