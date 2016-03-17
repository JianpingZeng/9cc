#include "cc.h"

static void analyze_sym(node_t *sym, struct dict *dict, struct basic_block *blk)
{
    // tmp sym
    if (SYM_X_KIND(sym) != SYM_KIND_TMP)
        return;
    
    struct vector *blks = dict_get(dict, sym);
    if (!blks) {
        blks = vec_new();
        dict_put(dict, sym, blks);
    }
    bool found = false;
    for (int i = 0; i < vec_len(blks); i++) {
        struct basic_block *block = vec_at(blks, i);
        if (block == blk) {
            found = true;
            break;
        }
    }
    if (!found)
        vec_push(blks, blk);
}

static void analyze_tmps(struct basic_block *head)
{
    const char *start_label = head->label;
    
    struct dict *dict = dict_new();
    dict->map->cmpfn = nocmp;
    
    for (struct basic_block *blk = head; blk; blk = blk->successors[0]) {
        for (struct tac *tac = blk->head; tac; tac = tac->next) {
            for (int i = 0; i < ARRAY_SIZE(tac->operands); i++) {
                struct operand *operand = tac->operands[i];
                if (operand) {
                    analyze_sym(operand->sym, dict, blk);
                    if (operand->index)
                        analyze_sym(operand->index, dict, blk);
                }
            }
        }
    }

    for (int i = 0; i < vec_len(dict->keys); i++) {
        node_t *sym = vec_at(dict->keys, i);
        struct vector *blks = dict_get(dict, sym);
        if (vec_len(blks) > 1) {
            println("%s: %s: ", start_label, SYM_X_LABEL(sym));
            for (int i = 0; i < vec_len(blks); i++) {
                struct basic_block *blk = vec_at(blks, i);
                println("%s", blk->label);
            }
        }
    }
}

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
