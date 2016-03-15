#include "cc.h"

static struct basic_block * new_basic_block(void)
{
    struct basic_block *block = alloc_basic_block();
    block->label = gen_block_label();
    return block;
}

bool is_basic_block_empty(struct basic_block *block)
{
    return block->head == NULL;
}

struct basic_block * construct_basic_blocks(struct tac *head)
{
    struct basic_block *start = new_basic_block();
    struct basic_block *end = new_basic_block();
    struct basic_block *current = start;
    // map: label => basic_block
    struct map *map = map_new();
    map->cmpfn = nocmp;
    
    for (struct tac *tac = head; tac; tac = tac->next) {
        if (tac == head) {
            struct basic_block *block = new_basic_block();
            block->head = tac;
            // update current
            current->successors[0] = block;
            current = block;
        } else if (tac->op == IR_IF_F ||
                   tac->op == IR_IF_I ||
                   tac->op == IR_IF_FALSE_F ||
                   tac->op == IR_IF_FALSE_I ||
                   tac->op == IR_GOTO) {
            current->tail = tac;
            struct basic_block *block = new_basic_block();
            block->head = tac->next;
            // update current
            current->successors[0] = block;
            current = block;
        } else if (tac->op == IR_RETURNI ||
                   tac->op == IR_RETURNF) {

        } else if (tac->op == IR_LABEL) {
            // new block
            struct basic_block *block = new_basic_block();
            block->head = tac;
            while (tac && tac->op == IR_LABEL)
                tac = tac->next;
        } else if (tac->op == IR_CALL) {
            // function call
        } else {
            // do nothing
        }
    }
}
