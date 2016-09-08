#ifndef _UTILS_H
#define _UTILS_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>

#define ONES(size)  (size)>=sizeof(unsigned long long) ? ~0ULL : ~((~0ULL)<<(CHAR_BIT*size))

#define ARRAY_SIZE(array)    (sizeof(array) / sizeof((array)[0]))

#define FIELD_SIZEOF(st, f)  (sizeof(((st*)0)->f))

#define FLEX_ARRAY                /* flexible array */

#define ALIGN_SIZE          (sizeof (long long))
#define ROUNDUP(x, align)   (((x)+((align)-1))&(~((align)-1)))

#define MAX(x, y)    (((x) > (y)) ? (x) : (y))
#define MIN(x, y)    ((x) < (y) ? (x) : (y))

// wrapper.c
extern void die(const char *fmt, ...);
extern void println(const char *fmt, ...);
extern void dlog(const char *fmt, ...);
extern void dwarning(const char *fmt, ...);
extern void derror(const char *fmt, ...);
extern void *xmalloc(size_t size);
extern void *xcalloc(size_t count, size_t size);
extern void *xrealloc(void *ptr, size_t size);
extern void *zmalloc(size_t size);
extern int log2i(size_t i);
extern size_t length(void *array);

// string.c
extern unsigned strhash(const char *s);
extern unsigned strhashn(const char *s, size_t len);
extern char *strs(const char *str);
extern char *strn(const char *src, size_t len);
extern char *strd(long long n);
extern char *stru(unsigned long long n);
extern char *vformat(const char *fmt, va_list ap);
extern char *format(const char *fmt, ...);
extern char *xstrdup(const char *str);
extern char *xstrndup(const char *str, size_t n);
extern bool has_prefix(const char *s, const char *prefix);

// alloc.c
extern void *allocate(size_t n, unsigned int a);
extern void *newarray(size_t n, unsigned int m, unsigned int a);
extern void deallocate(unsigned int a);
#define NEW(n, a)  allocate((n), (a))
#define NEW0(n, a)  memset(allocate((n), (a)), 0, (n))
#define NEWS(s, a)  NEW(sizeof(s), a)
#define NEWS0(s, a)  NEW0(sizeof(s), a)
enum { PERM = 0, FUNC };

// vector.c
#include "vector.h"
// map.c
#include "map.h"
// strbuf.c
#include "strbuf.h"
// set.c
#include "set.h"
// list.c
#include "list.h"

#endif
