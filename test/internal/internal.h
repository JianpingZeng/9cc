#ifndef _INTERNAL_H
#define _INTERNAL_H

#include "test.h"
#include "cc.h"
#include "sys.h"
#include <ctype.h>

#define STRING(str)   #str
#define CODE(code)    STRING(code)

// mcc compile
extern node_t * compile(const char *code);

// gcc compile
extern const char * gcc_compile(const char *code);

#endif
