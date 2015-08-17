
int * (*f) (float a, int , int (*) (int));

;;;

void G()
{
    int a;
    f = 1;
    a = 10;
    struct S {
    };
    (10 ? a : f)  = 10;
    struct S s;
    -s;
}
