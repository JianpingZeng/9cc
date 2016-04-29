#include "test.h"
#include "../utils/utils.h"

void test1()
{
    int a = 1, b = 2, c = 3;
    void *pa = &a, *pb = &b, *pc = &c;
    
    struct set *set1 = set_new();
    assert_true(set_len(set1) == 0);

    struct set *set2 = set_new();
    set_add(set1, pa);
    set_add(set2, pb);
}

void testmain()
{
    printf("test_set...");
    test1();
}
