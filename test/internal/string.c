#include "internal.h"

static void test_strip()
{
    struct strbuf *s1 = strbuf_new();
    
    strbuf_cats(s1, " abv d  ");
    strbuf_strip(s1);
    expects("abv d", strs(s1->str));
    
    s1 = strbuf_new();
    strbuf_cats(s1, "   ");
    strbuf_strip(s1);
    expecti(0, s1->len);
    expectp(NULL, strs(s1->str));
}

const char *testname()
{
    return "string";
}

void testmain()
{
    test_strip();
}
