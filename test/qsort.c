#include <stdio.h>

static int a[] = { 10, -9, 8, 2, 6, 1, 7, 0 };

static void quicksort(int m, int n)
{
    int i, j;
    int v, x;
    if (n <= m) return;
    i = m - 1; j = n; v = a[n];
    while (1) {
        do i = i+1; while (a[i] < v);
        do j = j-1; while (a[j] > v);
        if (i >= j) break;
        x = a[i]; a[i] = a[j]; a[j] = x;
    }
    x = a[i]; a[i] = a[n]; a[n] = x;
    quicksort(m, j); quicksort(i+1, n);
}

int main()
{
    int len = sizeof(a) / sizeof(a[0]);
    quicksort(0, len-1);
    printf("quick sort: ");
    for (int i = 0; i < len; i++)
        printf("%d ", a[i]);
    printf("\n");
}
