#include <stdio.h>

static int a[] = { 10, -9, 8, 2, 6, 1, 7, 0 };

static void buble_sort()
{
    int len = sizeof(a) / sizeof(a[0]);
    for (int i = 0; i < len; i++) {
        for (int j = i+1; j < len; j++) {
            if (a[i] > a[j]) {
                int tmp = a[i];
                a[i] = a[j];
                a[j] = tmp;
            }
        }
    }
}

int main()
{
    int len = sizeof(a) / sizeof(a[0]);
    buble_sort();
    printf("buble sort: ");
    for (int i = 0; i < len; i++)
        printf("%d ", a[i]);
    printf("\n");
}
