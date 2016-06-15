#include "utils.h"

#define BLOCKING    1024

struct alloc_state {
    int count;              // total number of nodes allocated
    int nr;                 // number of nodes left in current allocation
    void *p;                // first free node in current allocation
};

static void *do_alloc_object(struct alloc_state *s, size_t size)
{
    void *ret;

    if (!s->nr) {
        s->nr = BLOCKING;
        s->p = zmalloc(BLOCKING * size);
    }
    s->nr--;
    s->count++;
    ret = s->p;
    s->p = (char *)s->p + size;
    return ret;
}

static struct alloc_state set_state;
void *alloc_set(void)
{
    return do_alloc_object(&set_state, sizeof(struct set));
}

static struct alloc_state map_state;
void *alloc_map(void)
{
    return do_alloc_object(&map_state, sizeof(struct map));
}

static struct alloc_state vector_state;
void *alloc_vector(void)
{
    return do_alloc_object(&vector_state, sizeof(struct vector));
}

static struct alloc_state map_entry_state;
void *alloc_map_entry(void)
{
    return do_alloc_object(&map_entry_state, sizeof(struct map_entry));
}

static struct alloc_state hideset_state;
void *alloc_hideset(void)
{
    return do_alloc_object(&hideset_state, sizeof(struct hideset));
}

struct alloc_buf {
    struct alloc_buf *next;
    void *base, *cur, *limit;
};

static struct alloc_buf bufs[] = { {NULL}, {NULL}};
static struct alloc_buf *area[] = { &bufs[0], &bufs[1]};
#define MIN_BUF_SIZE 8000

void *allocate(size_t size, int n)
{
    void *result;
    struct alloc_buf *ap = area[n];
    if (size > ap->limit - ap->cur) {
        size_t len = size;
        size_t hlen = sizeof(struct alloc_buf);
        struct alloc_buf *head;
        char *base;
        if (len < MIN_BUF_SIZE)
            len = MIN_BUF_SIZE;
        head = xmalloc(hlen + len);
        base = (char *)head + hlen;
        head->base = base;
        head->cur = base;
        head->limit = base + len;
        head->next = ap;
        area[n] = ap = head;
    }
    result = ap->cur;
    ap->cur = (char *)ap->cur + size;
    return result;
}
