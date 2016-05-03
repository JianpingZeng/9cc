#include "cc.h"

static struct basic_block * new_basic_block(void)
{
    struct basic_block *block = alloc_basic_block();
    block->label = gen_block_label();
    return block;
}

void construct_basic_blocks(node_t *decl, struct tac *head)
{
    const char *start_label = SYM_X_LABEL(DECL_SYM(decl));
    const char *end_label = STMT_X_NEXT(DECL_BODY(decl));

    struct basic_block *start = alloc_basic_block();
    start->label = start_label;
    start->tag = BLOCK_START;
    struct basic_block *end = alloc_basic_block();
    end->label = end_label;
    end->tag = BLOCK_END;

    // map: label => basic_block
    struct map *map = map_new();
    map->cmpfn = nocmp;
    map_put(map, end_label, end);

    struct vector *branch_tacs = vec_new();
    struct vector *branch_blks = vec_new();
    struct basic_block **current = &start;

    for (struct tac **ptac = &head; *ptac; ) {
        struct tac *tac = *ptac;
        struct basic_block *entry = *current;
        
        if (tac == head) {
            struct basic_block *block = new_basic_block();
            block->head = tac;
            // update current
            entry->successors[0] = block;
            current = & entry->successors[0];
            entry = *current;
        }

        if (tac->op == IR_IF_F ||
            tac->op == IR_IF_I ||
            tac->op == IR_IF_FALSE_F ||
            tac->op == IR_IF_FALSE_I ||
            tac->op == IR_GOTO ||
            tac->op == IR_RETURNI ||
            tac->op == IR_RETURNF) {

            vec_push(branch_tacs, tac);
            vec_push(branch_blks, entry);
            
            struct tac *next = tac->next;
            tac->next = NULL;
            if (next) {
                struct basic_block *block = new_basic_block();
                block->head = next;
                // update current
                entry->successors[0] = block;
                current = & entry->successors[0];
            }
            ptac = & next;
        } else if (tac->op == IR_LABEL) {
            // new block
            *ptac = NULL;
            if (entry->head != tac) {
                struct basic_block *block = new_basic_block();
                // update current
                entry->successors[0] = block;
                current = & entry->successors[0];
                entry = *current;
            }
            while (tac && tac->op == IR_LABEL) {
                const char *label = SYM_X_LABEL(tac->operands[0]->sym);
                map_put(map, label, entry);
                tac = tac->next;
            }
            entry->head = tac;
            ptac = & tac;
        } else {
            // do nothing, just goto next
            ptac = & tac->next;
        }
    }

    for (int i = 0; i < vec_len(branch_tacs); i++) {
        struct tac *tac = vec_at(branch_tacs, i);
        struct basic_block *blk = vec_at(branch_blks, i);
        const char *label = SYM_X_LABEL(tac->operands[0]->sym);
        struct basic_block *block = map_get(map, label);
        tac->operands[0]->sym = make_label_sym(block->label);
        block->tag = BLOCK_JUMPING_DEST;
        blk->successors[1] = block;
    }

    struct basic_block *entry = *current;
    if (!entry->head && entry != start)
        *current = end;
    else
        entry->successors[0] = end;
    DECL_X_BASIC_BLOCK(decl) = start;
}
