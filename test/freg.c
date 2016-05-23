#include <stdio.h>

static void f(double a, double b)
{
    printf("a: %f, b: %f\n", a, b);
}

int main()
{
    double a = 1.6;
    double b = 2.2;
    double c;
    f(a+b, a-b);
    c = a + b;
    printf("%f\n", c);
    return 0;
}
