/**
 * Copyright (C) Guiyang Huang, 2015
 *
 * ISO/IEC 9899:1999
 * <stdarg.h>
 */

#ifndef __STDARG_H
#define __STDARG_H

// See also: http://www.x86-64.org/documentation/abi.pdf

typedef struct {
    unsigned int gp_offset;
    unsigned int fp_offset;
    void *overflow_arg_area;
    void *reg_save_area;
} __builtin_va_list[1];

typedef __builtin_va_list va_list;

#define va_start(ap, param)   __builtin_va_start(ap, param)
#define va_end(ap)            __builtin_va_end(ap)
#define va_arg(ap, type)      __builtin_va_arg(ap, type)
#define va_copy(dest, src)    __builtin_va_copy(dest, src)

// GNU
#define __va_copy(d, s)       __builtin_va_copy(d, s)

#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST 1
#endif
typedef __builtin_va_list __gnuc_va_list;

#endif
