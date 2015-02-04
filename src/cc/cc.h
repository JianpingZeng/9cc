#ifndef cc_cc_h
#define cc_cc_h

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <memory.h>
#include <stdarg.h>
#include <errno.h>
#include <limits.h>
#include <locale.h>
#include <float.h>

#include "config.h"
#include "lib.h"

#define twos(size)  (size)>=sizeof(unsigned long long) ? ~0ULL : ~((~0ULL)<<(CHAR_BIT*size))
#define bits(type)  (CHAR_BIT * (type)->size)

union value {
    long long i;
    unsigned long long u;
    double d;
    long double ld;
    void *p;
    void (*g) ();
};

// cc modules
#include "type.h"
#include "lex.h"
#include "sym.h"
#include "ast.h"
// error
#include "error.h"

//kind
enum {
    SCLASS_SPEC = 01, TYPE_QUAL = 02, TYPE_SPEC = 04,
    FUNC_SPEC = 010,
};

#endif
