#include <stddef.h>

struct S {
    int a;
};

void f() {
#define CC (sizeof(struct S) - offsetof(struct S, a))
    char c[CC];
}
