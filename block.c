#include "cc.h"

static struct basic_block * new_basic_block(void)
{
    struct basic_block *block = alloc_basic_block();
    block->label = gen_block_label();
    block->use = set_new();
    block->def = set_new();
    block->in = set_new();
    block->out = set_new();
    return block;
}

/**
 * def[B]: the set of variables defined (i.e., definitely assigned values) in B
 *         prior to any use of that variable in B.
 * use[B]: the set of variables whose values may be used in B prior to any
 *         definition of that variable.
 */
static void calculate_use_def(struct basic_block *block)
{
    FOR_EACH_BB(block) {
        for (struct tac *tac = block->head; tac; tac = tac->next) {
            for (int i = 0; i < ARRAY_SIZE(tac->operands); i++) {
                struct operand *operand = tac->operands[i];
                if (operand) {
                    if (i == 0) {
                        // result
                        if (operand->sym && REF_SYM(operand->sym)) {
                            if (!set_has(block->use, operand->sym))
                                set_add(block->def, operand->sym);
                        }
                        if (operand->index && REF_SYM(operand->index)) {
                            if (!set_has(block->def, operand->index))
                                set_add(block->use, operand->index);
                        }
                    } else {
                        if (operand->sym && REF_SYM(operand->sym)) {
                            if (!set_has(block->def, operand->sym))
                                set_add(block->use, operand->sym);
                        }
                        if (operand->index && REF_SYM(operand->index)) {
                            if (!set_has(block->def, operand->index))
                                set_add(block->use, operand->index);
                        }
                    }
                }
            }
        }
    }
}

/**
 * Algorithm of IN[B] and OUT[B]
 *
 * IN[EXIT] = ∅;
 * for (each basic block except EXIT) IN[B] = ∅;
 * while (any IN[B] has changed) {
 *     for (each basic block except EXIT) {
 *         OUT[B] = ∪ IN[S], while S is a successor of B;
 *         IN[B] = use[B] ∪ (OUT[B] - def[B]);
 *     }
 * }
 */
static void calculate_in_out(struct basic_block *block)
{
    bool changed;
    do {
        changed = false;
        FOR_EACH_BB(block) {
            if (block->tag == BLOCK_END)
                break;
            struct set *outs = NULL;
            for (int i = 0; i < ARRAY_SIZE(block->successors); i++) {
                struct basic_block *successor = block->successors[i];
                if (successor)
                    outs = set_union(outs, successor->in);
            }
            struct set *set1 = set_substract(outs, block->def);
            struct set *ins = set_union(block->use, set1);
            if (!changed) {
                changed = !set_equal(block->in, ins);
            }
            block->out = outs;
            block->in = ins;
        }
    } while (changed);
}

/**
 * Next-use information
 */
static void mark_die(node_t *sym)
{
    if (REF_SYM(sym)) {
        SYM_X_USES(sym).live = false;
        SYM_X_USES(sym).next = NULL;
    }
}

static void mark_live(node_t *sym, struct tac *tac)
{
    if (REF_SYM(sym)) {
        SYM_X_USES(sym).live = true;
        SYM_X_USES(sym).next = tac;
    }
}

static void init_use(node_t *sym)
{
    if (SYM_X_KIND(sym) == SYM_KIND_GREF ||
        SYM_X_KIND(sym) == SYM_KIND_LREF) {
        SYM_X_USES(sym).live = true;
        SYM_X_USES(sym).next = NULL;
    } else if (SYM_X_KIND(sym) == SYM_KIND_TMP) {
        SYM_X_USES(sym).live = false;
        SYM_X_USES(sym).next = NULL;
    }
}

static void init_next_use(struct basic_block *block)
{
    for (struct tac *tac = block->head; tac; tac = tac->next) {
        for (int i = 0; i < ARRAY_SIZE(tac->operands); i++) {
            struct operand *operand = tac->operands[i];
            if (operand) {
                if (operand->sym)
                    init_use(operand->sym);
                if (operand->index)
                    init_use(operand->index);
            }
        }
    }
}

static void scan_next_use(struct basic_block *block)
{
    struct tac *tail;
    for (tail = block->head; tail; tail = tail->next) {
        if (tail->next == NULL)
            break;
    }
    for (struct tac *tac = tail; tac; tac = tac->prev) {
        // set
        for (int i = 0; i < ARRAY_SIZE(tac->operands); i++) {
            struct operand *operand = tac->operands[i];
            if (operand) {
                if (operand->sym)
                    tac->uses[i*2] = SYM_X_USES(operand->sym);
                if (operand->index)
                    tac->uses[i*2+1] = SYM_X_USES(operand->index);
            }
        }
        
        // mark
        for (int i = 0; i < ARRAY_SIZE(tac->operands); i++) {
            struct operand *operand = tac->operands[i];
            if (operand) {
                if (i == 0) {
                    // die
                    if (operand->sym) {
                        if (operand->op == IR_SUBSCRIPT)
                            mark_live(operand->sym, tac);
                        else
                            mark_die(operand->sym);
                    }
                    if (operand->index)
                        mark_live(operand->index, tac);
                } else {
                    // live at tac
                    if (operand->sym)
                        mark_live(operand->sym, tac);
                    if (operand->index)
                        mark_live(operand->index, tac);
                }
            }
        }
    }
}

static void calculate_next_use(struct basic_block *block)
{
    FOR_EACH_BB(block) {
        init_next_use(block);
        scan_next_use(block);
    }
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

    // calculate use[B] and def[B]
    calculate_use_def(start);
    // calculate IN[B] and OUT[B]
    calculate_in_out(start);
    // calculate next use information
    calculate_next_use(start);
}
