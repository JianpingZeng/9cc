#include "cc.h"

#define BLOCKING    1024

struct alloc_state {
    int count;			// total number of nodes allocated
    int nr;			// number of nodes left in current allocation
    void *p;			// first free node in current allocation
};

static inline void * do_alloc_object(struct alloc_state *s, size_t size)
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

static struct alloc_state node_state;
void * alloc_node(void)
{
    return do_alloc_object(&node_state, sizeof(node_t));
}

static struct alloc_state token_state;
void * alloc_token(void)
{
    return do_alloc_object(&token_state, sizeof(struct token));
}

static struct alloc_state macro_state;
void * alloc_macro(void)
{
    return do_alloc_object(&macro_state, sizeof(struct macro));
}
