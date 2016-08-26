#include "utils.h"

struct bucket {
    struct bucket *next;
    char *cur;
    char *limit;
};

union align {
    long l;
    double d;
    void (*f) (void);
};

union header {
    struct bucket b;
    union align a;
};

static struct bucket first[] = {
    { NULL }, { NULL }, { NULL }
};
static struct bucket *area[] = {
    &first[0], &first[1], &first[2]
};
static struct bucket *freebuckets;

void *allocate(size_t n, int a)
{
    struct bucket *p;
    assert(a < ARRAY_SIZE(area));
    
    p = area[a];
    n = ROUNDUP(n, sizeof(union align));
    while (n > p->limit - p->cur) {
        if ((p->next = freebuckets) != NULL) {
            freebuckets = freebuckets->next;
            p = p->next;
        } else {
            size_t m = sizeof(union header) + n + ROUNDUP(10*1024, sizeof(union align));
            p->next = xmalloc(m);
            p = p->next;
            p->limit = (char *)p + m;
        }

        p->cur = (char *)((union header *)p + 1);
        p->next = NULL;
        area[a] = p;
    }

    p->cur += n;
    return p->cur - n;
}

void *newarray(size_t n, int m, int a)
{
    return allocate(n*m, a);
}

void deallocate(int a)
{
    assert(a < ARRAY_SIZE(area));
    area[a]->next = freebuckets;
    freebuckets = first[a].next;
    first[a].next = NULL;
    area[a] = &first[a];
}
