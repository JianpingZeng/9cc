#include "test.h"
#include "cc.h"

static void test_strip()
{
    struct str *s1 = new_str();
    
    str_cats(s1, " abv d  ");
    str_strip(s1);
    expects("abv d", stoa(s1));
    
    s1 = new_str();
    str_cats(s1, "   ");
    str_strip(s1);
    expecti(0, s1->len);
    expectp(NULL, stoa(s1));
}

const char *testname()
{
    return "string";
}

void testmain()
{
    test_strip();
}