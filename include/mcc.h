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
