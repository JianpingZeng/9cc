#ifndef _utils_h
#define _utils_h

#include "macros.h"

// alloc.c
#define NEW0(size)      cc_alloc(size)
#define NEWS(tag)       ((struct tag *)NEW0(sizeof(struct tag)))

// alloc memory of size
extern void *cc_alloc(size_t size);

// free all allocated memory
extern void cc_drain(void);


// string.c
struct string {
    char    *str;
    size_t  len;
    size_t  nalloc;
    size_t  reserve;
};
extern struct string * new_string();

extern size_t str_len(struct string *s);
extern void str_cats(struct string *s, const char *src);
extern void str_catn(struct string *s, const char *src, int len);
extern void str_catd(struct string *s, long d);
// string to array
extern char * stoa(struct string *s);

// char* from char*/integer
extern char *strings(const char *str);
extern char *stringn(const char *src, int len);
extern char *stringd(long n);

extern char *format(const char *fmt, ...);


// vector.c
struct vector {
    void    **mem;
    size_t  elemsize;
    size_t  len;
    size_t  nalloc;
    size_t  reserve;
};
extern struct vector *new_vector();

extern void * vec_at(struct vector *v, int index);
extern void vec_push(struct vector *v, void *elem);
extern size_t vec_len(struct vector *v);
extern void * vec_head(struct vector *v);
extern void * vec_tail(struct vector *v);
extern void vec_add_from_array(struct vector *v, void **array);
extern void vec_add_from_vector(struct vector *v, struct vector *v2);
extern void vec_foreach(struct vector *v, void (*func)(void *elem, void *context), void *context);

// vector to array
extern void ** vtoa(struct vector *v);

extern int array_len(void **array);


// misc.c
extern void die(const char *fmt, ...);
extern void log(const char *fmt, ...);

#endif
