/**
 * Copyright (C) Guiyang Huang, 2015
 *
 * ISO/IEC 9899:1999
 * <float.h>
 */

#ifndef __FLOAT_H
#define __FLOAT_H

/**
 *              | sign | exponent | fraction | total bits | exponent bias |
 * float:          1      8          23          32            127
 * double:         1      11         52          64            1023
 * long double:    1      15         64          80            16383
 */

#define DECIMAL_DIG     21
#define FLT_ROUNDS      1
#define FLT_RADIX       2
#define FLT_EVAL_METHOD 0

#define FLT_DIG         6
#define FLT_EPSILON     1.19209290e-7F
#define FLT_MANT_DIG    24
#define FLT_MIN         1.17549435e-38F
#define FLT_MIN_EXP     -125
#define FLT_MIN_10_EXP  -37
#define FLT_MAX         3.40282347e+38F
#define FLT_MAX_EXP     128
#define FLT_MAX_10_EXP  38

#define DBL_DIG         15
#define DBL_EPSILON     2.2204460492503131e-16
#define DBL_MANT_DIG    53
#define DBL_MIN         2.2250738585072014e-308
#define DBL_MIN_EXP     -1021
#define DBL_MIN_10_EXP  -307
#define DBL_MAX         1.7976931348623157e+308
#define DBL_MAX_EXP     1024
#define DBL_MAX_10_EXP  308

#define LDBL_DIG        18
#define LDBL_EPSILON    1.08420217248550443401e-19L
#define LDBL_MANT_DIG   64
#define LDBL_MIN        3.36210314311209350626e-4932L
#define LDBL_MIN_EXP    -16381
#define LDBL_MIN_10_EXP -4931
#define LDBL_MAX        1.18973149535723176502e+4932L
#define LDBL_MAX_EXP    16384
#define LDBL_MAX_10_EXP 4932

#endif
