#include "test.h"
#include "../utils/utils.h"

static int sort(const void *val1, const void *val2)
{
    const char *c1 = *(const char **) val1;
    const char *c2 = *(const char **) val2;
    println("hehe %s %s", c1, c2);
    return strcmp(c1, c2);
}

void test1()
{
    const char *c1 = ".t1235";
    const char *c2 = ".t1233";
    const char *c3 = ".t1234";
    struct vector *v1 = vec_new();
    vec_push(v1, c1);
    vec_push(v1, c2);
    vec_push(v1, c3);

    struct vector *v2 = vec_sort(v1, sort);
    for (int i=0; i < vec_len(v2); i++)
        printf("[%d] %s\n", i, (char *)vec_at(v2, i));
}

void testmain()
{
    printf("test_vector...");
    test1();
}
