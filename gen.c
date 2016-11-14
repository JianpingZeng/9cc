#include "cc.h"

static unsigned int freemask[2];

struct symbol *mkreg(const char *name, int index, int kind)
{
    struct symbol *s;

    s = NEWS0(struct symbol, PERM);
    s->name = s->x.name = name;
    s->x.reg.index = index;
    s->x.reg.kind = kind;
    s->x.reg.mask = 1<<index;
    return s;
}

struct symbol *mksreg(const char *name)
{
    struct symbol *s;

    s = NEWS0(struct symbol, PERM);
    s->name = s->x.name = name;
    return s;
}

void gen(struct symbol *s)
{
    
}

void emit(struct symbol *s)
{
    
}
