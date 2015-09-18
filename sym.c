#include "cc.h"

#define EMPTY_MAP   ((struct map){ .cmpfn = nocmp })

struct table * identifiers = &(struct table){ GLOBAL, .map = &EMPTY_MAP };
struct table * constants = &(struct table){ CONSTANT, .map = &EMPTY_MAP };
struct table * tags = &(struct table){ GLOBAL, .map = &EMPTY_MAP };

static int level = GLOBAL;

static struct table * new_table(struct table *up, int scope)
{
    struct table *t = zmalloc(sizeof(struct table));
    t->up = up;
    t->scope = scope;
    t->map = map_new(nocmp);
    return t;
}

static void free_table(struct table *t)
{
    map_free(t->map);
    free(t);
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
	struct table *up = tags->up;
	free_table(tags);
        tags = up;
    }
    if (identifiers->scope == level) {
	struct table *up = identifiers->up;
	free_table(identifiers);
        identifiers = up;
    }
    CCAssert(level >= GLOBAL);
    level--;
}

union node * anonymous(struct table **tpp, int scope)
{
    static long i;
    return install(strd(i++), tpp, scope);
}

union node * lookup(const char *name, struct table *table)
{
    CCAssert(name);
    union node *s = NULL;
    
    for (struct table *t = table; t; t = t->up) {
        if ((s = map_get(t->map, name)))
            return s;
    }

    return s;
}

union node * install(const char *name, struct table **tpp, int scope)
{
    union node *sym;
    struct table *tp = *tpp;
    
    if (scope > tp->scope) {
        tp = *tpp = new_table(tp, scope);
    } else {
        while (scope != tp->scope)
            tp = tp->up;
    }
    
    sym = alloc_symbol();
    SYM_SCOPE(sym) = scope;
    SYM_NAME(sym) = name;
    map_put(tp->map, SYM_NAME(sym), sym);

    return sym;
}

bool issymnamed(union node *sym)
{
    return SYM_NAME(sym) == NULL || isanonymous(SYM_NAME(sym));
}
