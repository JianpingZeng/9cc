/**
 * Copyright (C) Guiyang Huang, 2015
 *
 * ISO/IEC 9899:1999
 * <stddef.h>
 */
#ifndef __STDDEF_H
#define __STDDEF_H

#define NULL ((void *)0)

typedef long ptrdiff_t;
typedef unsigned long size_t;
typedef unsigned int wchar_t;

#define offsetof(type, member)  ((size_t)&((type *)0)->member)

#endif
