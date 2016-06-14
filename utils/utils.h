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

// string.c
struct str_table {
    struct str_bucket {
        char *str;
        size_t len;
        struct str_bucket *next;
    } *buckets[1024];
};

#define HASHSTEP(r, c)  ((r) * 67 + ((c) - 133))
#define HASHFINISH(r, len)  ((r) + (len))

extern unsigned strhash(const char *s);
extern char *strs(const char *str);
extern char *strnh(const char *src, size_t len, unsigned int hash);
extern char *strn(const char *src, size_t len);
extern char *strd(long long n);
extern char *stru(unsigned long long n);
extern char *vformat(const char *fmt, va_list ap);
extern char *format(const char *fmt, ...);
extern char *xstrdup(const char *str);
extern char *xstrndup(const char *str, size_t n);
extern bool has_prefix(const char *s, const char *prefix);

// alloc.c
extern void *alloc_set(void);
extern void *alloc_map(void);
extern void *alloc_map_entry(void);
extern void *alloc_vector(void);
extern void *alloc_hideset(void);

// vector.c
#include "vector.h"
// map.c
#include "map.h"
// strbuf.c
#include "strbuf.h"
// hideset.c
#include "hideset.h"
// set.c
#include "set.h"

#endif
