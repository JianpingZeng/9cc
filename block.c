#include "cc.h"

struct block * new_block(void)
{
    struct block *blk = zmalloc(sizeof(struct block));
    blk->label = gen_block_label();
    return blk;
}

struct block * convert2blocks(struct tac *head)
{
    struct block *blk = new_bblock();
    
    for (struct tac *tac = head; tac; tac = tac->next) {
        if (tac == head) {
            struct block *block = new_block();
            blk->branches[0] = block;
            blk = block;
            blk->head = tac;
        } else if (tac->op == IR_IF_F ||
                   tac->op == IR_IF_I ||
                   tac->op == IR_IF_FALSE_F ||
                   tac->op == IR_IF_FALSE_I ||
                   tac->op == IR_GOTO ||
                   tac->op == IR_RETURNI ||
                   tac->op == IR_RETURNF) {
            blk->tail = tac;
            struct block *block = new_bblock();
            blk->branches[0] = block;
            blk = block;
        } else if (tac->op == IR_LABEL) {
            // new block
            struct 
        } else if (tac->op == IR_CALL) {
            
        } else {
            // do nothing
        }
    }
}
