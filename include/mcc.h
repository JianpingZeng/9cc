/**
 * Copyright (C) Guiyang Huang, 2015
 *
 * This file contains standard predeined macros.
 * None of these predefined macros may be undefined
 * (#undef) or redefined(#define) by the programmer.
 */

/**
 * The demical constant 1 if and only if the
 * compiler is an ISO-conforming implementation.
 */
#define __STDC__            1

/**
 * If the implementation conforms to:
 *
 *   standard    value
 *
 *   C89         199409L
 *   C99         199901L
 *   C11         201112L
 *
 * Otherwise, its value is not defined.
 */
#define __STDC_VERSION__    199901L

/**
 * Defined as 1 if the implementation is a
 * hosted implementation, 0 if it is a freestanding
 * implementation.
 *
 * A hosted environment has the complete facilities
 * of the standard C library available.
 */
#define __STDC_HOSTED__     1

/**
 *  Architecture relative macros
 */
#define __x86_64__    1
#define __LP64__      1

// Standard C source
#define _ISOC99_SOURCE 1


// mcc builtins
typedef struct __builtin_va_list_tag {
    unsigned int gp_offset;
    unsigned int fp_offset;
    void *overflow_arg_area;
    void *reg_save_area;
} __builtin_va_list[1];

void __builtin_va_start(__builtin_va_list, ...);
void * __builtin_va_arg_p(__builtin_va_list, ...);

#define __builtin_va_copy(dst, src)  ((dst)[0] = (src)[0])
#define __builtin_va_end(ap)
#define __builtin_va_arg(ap, type)  (*(type *)__builtin_va_arg_p(ap, (type *)0))
