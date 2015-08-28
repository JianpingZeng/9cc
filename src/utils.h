#ifndef _UTILS_H
#define _UTILS_H

extern void die(const char *fmt, ...);
extern void println(const char *fmt, ...);
extern void * xmalloc(size_t size);
extern void * xcalloc(size_t count, size_t size);
extern void * xrealloc(void *ptr, size_t size);
extern void * zmalloc(size_t size);

#endif