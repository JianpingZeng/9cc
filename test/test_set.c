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

    assert_true(set_len(set1) == 1);
    assert_true(set_len(set2) == 1);

    struct set *set3 = set_union(set1, set2);
    assert_true(set_len(set3) == 2);

    set_add(set1, pa);
    assert_true(set_len(set1) == 1);
}

void testmain()
{
    printf("test_set...");
    test1();
}
