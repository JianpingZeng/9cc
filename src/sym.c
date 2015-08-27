#include "cc.h"

static struct table _identifiers = { .scope = GLOBAL };
static struct table _constants = { .scope = CONSTANT };
static struct table _tags = { .scope = GLOBAL };

struct table * identifiers = &_identifiers;
struct table * constants = &_constants;
struct table * tags = &_tags;

static int level = GLOBAL;

static struct table * new_table(struct table *up, int scope)
{
    struct table *t = NEWS(table);
    t->up = up;
    t->scope = scope;
    t->map = new_map(always_notequal);
    return t;
}

int scopelevel()
{
    return level;
}

void enter_scope()
{
    level++;
}

void exit_scope()
{
    if (tags->scope == level) {
        tags = tags->up;
    }
    if (identifiers->scope == level) {
        identifiers = identifiers->up;
    }
    assert(level >= GLOBAL);
    level--;
}

struct symbol * anonymous(struct table **tpp, int scope)
{
    static long i;
    return install(stringd(i++), tpp, scope);
}

struct symbol * lookup(const char *name, struct table *table)
{
    assert(name);
    struct symbol *s = NULL;
    
    for (struct table *t = table; t; t = t->up) {
        if ((s = map_get(t->map, name)))
            return s;
    }

    return s;
}

struct symbol * install(const char *name, struct table **tpp, int scope)
{
    struct symbol *sym;
    struct table *tp = *tpp;
    
    if (scope > tp->scope) {
        tp = *tpp = new_table(tp, scope);
    } else {
        while (scope != tp->scope)
            tp = tp->up;
    }
    
    sym = NEWS(symbol);
    sym->scope = scope;
    sym->name = strings(name);
    map_put(tp->map, sym->name, sym);

    return sym;
}

bool issymnamed(struct symbol *sym)
{
    return sym->name == NULL || is_digit(sym->name[0]);
}
