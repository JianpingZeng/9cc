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
 * If the implementation conforms to C99,
 * then the macro has the value 199901L.
 * Otherwise, its value is not defined.
 */
#define __STDC_VERSION__    199901L

/**
 * Defined as 1 if the implementation is a
 * hosted implementation, 0 if it is a freestanding
 * implementation.
 */
#define __STDC_HOSTED__     1

