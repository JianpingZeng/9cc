#include <assert.h>
#include "cc.h"

struct code *code0, *code1;

int genlabel(int count)
{
    static int lab = 1;
    assert(count > 0);
    lab += count;
    return lab - count;
}

static struct code *alloc_code(int id)
{
    struct code *code = zmalloc(sizeof(struct code));
    code->id = id;
    return code;
}

void branch(struct expr *expr, int tlab, int flab)
{
    
}

void jmpto(int label)
{
    
}

void ret(struct expr *expr)
{
    
}

void label(int label)
{
    
}

void gen(struct expr *expr)
{
    
}

