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

static void rewrite(void)
{
    
}

void gen(struct symbol *s)
{
    
}

void emit(struct symbol *s)
{
    
}

static void geninit_struct(struct symbol *s)
{
    // TODO: 
}

static void geninit_array(struct symbol *s)
{
    // TODO:
    struct tree *init = s->u.init;
    struct type *ty = s->type;
    size_t size = TYPE_SIZE(ty);

    if (isstring(ty) && issliteral(init)) {
        IR->defstring(init->s.sym->name, size);
    } else {
        error_at(s->src, "illegal %s initializer for '%s'",
                 TYPE_NAME(ty), s->name);
        IR->defzero(size);
    }
}

static void geninit_ptr(struct symbol *s)
{
    struct tree *init = s->u.init;

    if (OPKIND(init->op) == CNST) {
        IR->defconst(OPTYPE(init->op), TYPE_SIZE(s->type), init->s.value);
    } else if (opid(init->op) == ADDRG+P) {
        IR->defaddress(init->s.sym->x.name, 0);
    } else if (opid(init->op) == ADD+P &&
               opid(init->kids[0]->op) == ADDRG+P &&
               OPKIND(init->kids[1]->op) == CNST) {
        IR->defaddress(init->kids[0]->s.sym->x.name,
                       init->kids[1]->s.value.i);
    } else if (opid(init->op) == SUB+P &&
               opid(init->kids[0]->op) == ADDRG+P &&
               OPKIND(init->kids[1]->op) == CNST) {
        IR->defaddress(init->kids[0]->s.sym->x.name,
                       -init->kids[1]->s.value.i);
    } else {
        if (debug['A'])
            error_at(s->src, "illegal initializer for '%s'", s->name);
        IR->defzero(TYPE_SIZE(s->type));
    }
}

static void geninit_arith(struct symbol *s)
{
    struct tree *init = s->u.init;

    if (OPKIND(init->op) == CNST) {
        IR->defconst(OPTYPE(init->op), TYPE_SIZE(s->type), init->s.value);
    } else {
        if (debug['A'])
            error_at(s->src, "illegal initializer for '%s'", s->name);
        IR->defzero(TYPE_SIZE(s->type));
    }
}

void genglobal(struct symbol *s)
{
    int seg = s->u.init ? DATA : BSS;

    if (seg == DATA && s->sclass != STATIC)
        IR->export(s);
    else if (seg == BSS && s->sclass == STATIC)
        IR->local(s);
    IR->segment(seg);
    IR->defvar(s);
    if (s->u.init) {
        if (isarith(s->type))
            geninit_arith(s);
        else if (isptr(s->type))
            geninit_ptr(s);
        else if (isarray(s->type))
            geninit_array(s);
        else if (isstruct(s->type))
            geninit_struct(s);
        else
            CC_UNAVAILABLE();
    }
}

void genstring(struct symbol *s)
{
    IR->segment(RODATA);
    IR->defvar(s);
    IR->defstring(s->name, -1);
}
