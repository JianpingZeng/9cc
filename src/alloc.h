#ifndef _ALLOC_H
#define _ALLOC_H

extern void * xmalloc(size_t size);

extern void * zmalloc(size_t size);

extern void *cc_alloc(size_t size);

extern void cc_drain(void);

#define NEW0(size)  cc_alloc(size)

#define NEWS(tag)   ((struct tag *)NEW0(sizeof(struct tag)))

#endif