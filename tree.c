#include <assert.h>
#include <stdint.h>
#include "cc.h"

struct expr *addrof(struct expr *expr)
{
    //TODO:
    return NULL;
}

static void init(int argc, char *argv[])
{
}

static void finalize(void)
{
}

static void defvar(struct symbol *sym)
{
}

static void defun(struct symbol *sym)
{
}

struct interface *IR = &(struct interface) {
    .defvar = defvar,
    .defun = defun,
    .init = init,
    .finalize = finalize,
};
