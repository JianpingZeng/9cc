// Copyright 2015 Guiyang Huang. Released under the MIT license.

#include "internal.h"

#define STRINGIZE(x)    #x
#define CODE(x)    STRINGIZE(x)

extern struct node *compile(const char *code);