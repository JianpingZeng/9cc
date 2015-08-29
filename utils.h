#ifndef _UTILS_H
#define _UTILS_H

#define TWOS(size)  (size)>=sizeof(unsigned long long) ? ~0ULL : ~((~0ULL)<<(CHAR_BIT*size))

#define ARRAY_SIZE(array)    (sizeof(array) / sizeof((array)[0]))

#define FIELD_SIZEOF(st, f)  (sizeof(((st*)0)->f))

#define FLEX_ARRAY      /* flexible array */

// wrapper.c
extern void die(const char *fmt, ...);
extern void println(const char *fmt, ...);
extern void * xmalloc(size_t size);
extern void * xcalloc(size_t count, size_t size);
extern void * xrealloc(void *ptr, size_t size);
extern void * zmalloc(size_t size);

// string.c
extern unsigned strhash(const char *s);
extern char *strs(const char *str);
extern char *strn(const char *src, size_t len);
extern char *strd(long n);
extern char *format(const char *fmt, ...);

#endif
