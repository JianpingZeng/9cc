#ifndef _utils_h
#define _utils_h

#include "macros.h"

// alloc.c
extern void * xmalloc(size_t size);
extern void * zmalloc(size_t size);

extern void *cc_alloc(size_t size);
extern void cc_drain(void);
#define NEW0(size)  cc_alloc(size)
#define NEWS(tag)   ((struct tag *)NEW0(sizeof(struct tag)))

// map.c
extern unsigned strhash(const char *s);
extern struct map * new_map(int (*cmpfn) (const char *, const char *));
extern void free_map(struct map *map);
extern void *map_get(struct map *map, const char *key);
extern void map_put(struct map *map, const char *key, void *value);
extern int always_notequal(const char *key1, const char *key2);
extern int always_equal(const char *key1, const char *key2);

// string.c
struct string {
    char    *str;
    size_t  len;
    size_t  alloc;
};
extern struct string * new_string();
extern void free_string(struct string *s);
extern size_t str_len(struct string *s);
extern void str_add(struct string *s, struct string *s2);
extern void str_cats(struct string *s, const char *src);
extern void str_catn(struct string *s, const char *src, size_t len);
extern void str_catd(struct string *s, long d);
extern void str_lstrip(struct string *s);
extern void str_rstrip(struct string *s);
extern void str_strip(struct string *s);

// vector.c
struct vector {
    void    **mem;
    size_t  len;
    size_t  alloc;
};
extern struct vector *new_vector();
extern void free_vector(struct vector *v);
extern void * vec_at(struct vector *v, int index);
extern void vec_set(struct vector *v, int index, void *val);
extern void vec_push(struct vector *v, void *val);
extern size_t vec_len(struct vector *v);
extern void * vec_head(struct vector *v);
extern void * vec_tail(struct vector *v);
extern void vec_add(struct vector *v, struct vector *v2);

// array.c
extern char * stoa(struct string *s);
extern void ** vtoa(struct vector *v);
extern void vec_add_from_array(struct vector *v, void **array);
extern int array_len(void **array);
extern char *format(const char *fmt, ...);
extern char *strings(const char *str);
extern char *stringn(const char *src, int len);
extern char *stringd(long n);

// misc.c
extern void die(const char *fmt, ...);
extern void println(const char *fmt, ...);

#endif
