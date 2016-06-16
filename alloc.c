#include "cc.h"

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

static struct alloc_state node_state;
void *alloc_node(void)
{
    return do_alloc_object(&node_state, sizeof(node_t));
}

static struct alloc_state token_state;
void *alloc_token(void)
{
    return do_alloc_object(&token_state, sizeof(struct token));
}

static struct alloc_state macro_state;
void *alloc_macro(void)
{
    return do_alloc_object(&macro_state, sizeof(struct macro));
}

static struct alloc_state operand_state;
void *alloc_operand(void)
{
    return do_alloc_object(&operand_state, sizeof(struct operand));
}

static struct alloc_state tac_state;
void *alloc_tac(void)
{
    return do_alloc_object(&tac_state, sizeof(struct tac));
}

static struct alloc_state basic_block_state;
void *alloc_basic_block(void)
{
    return do_alloc_object(&basic_block_state, sizeof(struct basic_block));
}

static struct alloc_state reladdr_state;
void *alloc_reladdr(void)
{
    return do_alloc_object(&reladdr_state, sizeof(struct reladdr));
}

static struct alloc_state opcode_state;
void *alloc_opcode(void)
{
    return do_alloc_object(&opcode_state, sizeof(struct opcode));
}

static struct alloc_state hideset_state;
void *alloc_hideset(void)
{
    return do_alloc_object(&hideset_state, sizeof(struct hideset));
}
