#include "cc.h"

#define BLOCKING    1024

struct alloc_state {
    int count;			// total number of nodes allocated
    int nr;			// number of nodes left in current allocation
    void *p;			// first free node in current allocation
};

static inline void * do_alloc_node(struct alloc_state *s, size_t size)
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
    return do_alloc_node(&node_state, sizeof(node_t));
}

void * alloc_symbol(void)
{
    node_t *n = alloc_node();
    AST_ID(n) = SYMBOL_NODE;
    return n;
}

void * alloc_type(void)
{
    node_t *n = alloc_node();
    AST_ID(n) = TYPE_NODE;
    return n;
}

void * alloc_field(void)
{
    node_t *n = alloc_node();
    AST_ID(n) = FIELD_NODE;
    return n;
}
