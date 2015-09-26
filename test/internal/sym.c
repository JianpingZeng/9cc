#include "internal.h"

static void test_lookup()
{
    node_t *sym;
    node_t *sym2;
    
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
    node_t *sym = lookup(name1, identifiers);
    expecti(SYM_SCOPE(sym), GLOBAL);
    
    node_t *sym2 = install(name1, &identifiers, SCOPE);
    expectb(sym != sym2);
    expectp(identifiers->up->up, NULL);
    expecti(SYM_SCOPE(sym2), SYM_SCOPE(sym)+1);
    
    exit_scope();
    expecti(SCOPE, GLOBAL);
    node_t *sym3 = lookup(name1, identifiers);
    expectp(sym3, sym);
}

void testmain()
{
    START("symbol ...");
    symbol_init();
    test_lookup();
    test_scope();
}
