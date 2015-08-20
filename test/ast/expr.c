#include "ast.h"

static void test_expr1()
{
    const char *code = CODE(
        void f() {
            int a;
            short b;
            a = b + 10;
        }
    );
    compile(code);
}

void testmain()
{
    print("ast expr");
    test_expr1();
}