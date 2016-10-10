#include <assert.h>
#include "cc.h"

/// constant expression evaluation

/*
 * Constant expressions in C:
 *
 * 1. interger
 * 2. floating
 * 3. address (address of a static extent)
 * 4. initializer (combination of the aboves)
 */

struct expr *eval(struct expr * expr, struct type * ty)
{
    // TODO:
    return cnsti(1, ty);
}
