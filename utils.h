#ifndef _UTILS_H
#define _UTILS_H

// wrapper.c
extern void die(const char *fmt, ...);
extern void println(const char *fmt, ...);
extern void * xmalloc(size_t size);
extern void * xcalloc(size_t count, size_t size);
extern void * xrealloc(void *ptr, size_t size);
extern void * zmalloc(size_t size);

// string.c
extern char *strs(const char *str);
extern char *strn(const char *src, size_t len);
extern char *strd(long n);
extern char * stoa(struct str *s);
extern char *format(const char *fmt, ...);

#endif