/**
 * Copyright (C) Guiyang Huang, 2015
 *
 * ISO/IEC 9899:1999
 * <stdarg.h>
 */

#ifndef __STDARG_H
#define __STDARG_H

// TODO
typedef void *__gnuc_va_list;
typedef __gnuc_va_list va_list;

#define va_start(v, l)
#define va_end(v)   ((void)0)
#define va_arg(v, l)

#endif
