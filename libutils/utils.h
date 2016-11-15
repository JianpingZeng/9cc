#ifndef UTILS_H
#define UTILS_H

// for bool
#include <stdbool.h>
// for size_t
#include <stddef.h>
// for va_list
#include <stdarg.h>
// for memset
#include <string.h>
// for CHAR_BIT
#include <limits.h>

#define ONES(size)  (size)>=sizeof(unsigned long) ? ~0UL : ~((~0UL)<<(CHAR_BIT*size))

#define ARRAY_SIZE(array)    (sizeof(array) / sizeof((array)[0]))

#define FIELD_SIZEOF(st, f)  (sizeof(((st*)0)->f))

#define FLEX_ARRAY                /* flexible array */

#define ALIGN_SIZE          (sizeof (long))
#define ROUNDUP(x, align)   (((x)+((align)-1))&(~((align)-1)))

#define BITS(bytes)     (CHAR_BIT * (bytes))
#define BYTES(bits)     ((ROUNDUP(bits, CHAR_BIT)) / (CHAR_BIT))

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
extern int log2i(size_t i);
extern size_t length(void *array);
#define isempty(array)  ((array)[0] == NULL)
#define zmalloc(size)  memset(xmalloc(size), 0, (size))

// string.c
extern unsigned int strhash(const char *s);
extern unsigned int strhashn(const char *s, size_t len);
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
// strbuf.c
#include "strbuf.h"
// list.c
#include "list.h"
// sys.c
#include "sys.h"

#endif
