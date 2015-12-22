#ifndef _UTILS_H
#define _UTILS_H

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

// string.c
extern unsigned strhash(const char *s);
extern char *strs(const char *str);
extern char *strn(const char *src, size_t len);
extern char *strd(long long n);
extern char *stru(unsigned long long n);
extern char *format(const char *fmt, ...);
extern char *xstrdup(const char *str);
extern char *xstrndup(const char *str, size_t n);
extern bool starts_with(const char *s, const char *prefix);

// map.c
#include "map.h"
// vector.c
#include "vector.h"
// strbuf.c
#include "strbuf.h"
// hideset.c
#include "hideset.h"
// dict.c
#include "dict.h"

#endif
