#include <assert.h>
#include "cc.h"

struct field *alloc_field(void)
{
    return NEWS0(struct field, PERM);
}

struct field *new_indirect_field(struct field *field)
{
    assert(field);
    assert(field->name);
    struct field *p = alloc_field();
    p->indir = field;
    return p;
}

struct expr *ast_expr(int op, struct type *ty, struct expr *l, struct expr *r)
{
    assert(OPINDEX(op) > OPNONE && OPINDEX(op) < OPEND);
    struct expr *expr = NEWS0(struct expr, FUNC);
    expr->op = op;
    expr->type = ty;
    expr->kids[0] = l;
    expr->kids[1] = r;
    return expr;
}

struct stmt *ast_stmt(int id)
{
    assert(id >= LABEL && id <= RET);
    struct stmt *stmt = NEWS0(struct stmt, FUNC);
    stmt->id = id;
    return stmt;
}

const char *gen_tmpname(void)
{
    static size_t i;
    return format(".T%llu", i++);
}

const char *gen_compound_label(void)
{
    static size_t i;
    return format("__compound_literal.%llu", i++);
}

int genlabel(int count)
{
    static int lab = 1;
    assert(count > 0);
    lab += count;
    return lab - count;
}

struct desig *new_desig(int id)
{
    struct desig *d = NEWS0(struct desig, FUNC);
    d->id = id;
    return d;
}

struct desig *new_desig_name(const char *name, struct source src)
{
    struct desig *d = new_desig(DESIG_FIELD);
    d->u.name = name;
    d->src = src;
    return d;
}

struct desig *new_desig_index(long index, struct source src)
{
    struct desig *d = new_desig(DESIG_INDEX);
    d->u.index = index;
    d->src = src;
    return d;
}

struct desig *new_desig_field(struct field *field, struct source src)
{
    struct desig *d = new_desig(DESIG_FIELD);
    d->u.field = field;
    d->type = field->type;
    d->src = src;
    return d;
}

// copy designator list
struct desig *copy_desig(struct desig *desig)
{
    struct desig *ret = NULL;
    struct desig **pp = &ret;
    
    for (struct desig *s = desig; s; s = s->prev) {
        *pp = NEWS(struct desig, FUNC);
        *(*pp) = *s;
        pp = &(*pp)->prev;
    }

    return ret;
}
