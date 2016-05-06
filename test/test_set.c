#include "test.h"
#include "../utils/utils.h"

void test1()
{
    int a = 1, b = 2, c = 3;
    void *pa = &a, *pb = &b, *pc = &c;
    
    struct set *set1 = set_new();
    assert_true(set_size(set1) == 0);
    set_add(set1, pa);
    assert_true(set_size(set1) == 1);
    
    struct set *set2 = set_new();
    set_add(set2, pb);
    assert_true(set_size(set2) == 1);

    struct set *set3 = set_union(set1, set2);
    assert_true(set_size(set3) == 2);

    set_add(set1, pa);
    assert_true(set_size(set1) == 1);

    struct set *set4 = set_intersection(set3, set1);
    assert_true(set_size(set4) == 1);
}

void testmain()
{
    printf("test_set...");
    test1();
}
