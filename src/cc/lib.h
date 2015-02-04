#ifndef cc_lib_h
#define cc_lib_h

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <limits.h>

#define ARRAY_SIZE(array)    (sizeof(array) / sizeof((array)[0]))
#define FIELD_SIZEOF(st, f)  (sizeof(((st*)0)->f))

// print
typedef const char * (*PrintFunc) (void *data);

extern void print(const char *fmt, ...);
extern void fprint(FILE *f, const char *fmt, ...);
extern void vfprint(FILE *f, const char *fmt, va_list ap);
extern void register_print_function(char c, PrintFunc p);
extern void die(const char *fmt, ...);

// alloc
enum {
    ALLOC_TEMP,    /* =0: temporarily useage */
    ALLOC_NODE,    /* =1: nodes, types, symbols */
    ALLOC_STRING,  /* =2: strings */
};
extern void * allocate(size_t size, int flags);
extern void deallocate(void *p);
#define alloc_temp(size) allocate(size, ALLOC_TEMP)
#define alloc_node(st)   allocate(sizeof(st), ALLOC_NODE)

// string
extern const char *strings(const char *str);
extern const char *stringn(const char *src, int len);
extern const char *stringd(long n);

struct string {
    char     *str;
    unsigned size;
    unsigned capelems;
    unsigned reserve;
};
extern struct string * new_string();
extern unsigned string_length(struct string *s);
extern void string_concats(struct string *s, char *src);
extern void string_concatn(struct string *s, char *src, int len);
extern void string_concatd(struct string *s, long d);
extern char * string_to_array(struct string *s);
extern void free_string(struct string *s);

// vector (container of pointers)
struct vector {
    void        **mem;
    unsigned    elemsize;
    unsigned    elems;
    unsigned    capelems;
    unsigned    reserve;
};
extern struct vector *new_vector();
extern void * vector_at(struct vector *v, unsigned index);
extern void vector_push(struct vector *v, void *elem);
extern void *vector_pop(struct vector *v);
extern void vector_insert(struct vector *v, unsigned index, void *elem);
extern void free_vector(struct vector *v);
extern void purge_vector(struct vector *v);
extern unsigned vector_length(struct vector *v);
extern void *vector_front(struct vector *v);
extern void *vector_back(struct vector *v);
extern void vector_foreach(struct vector *v, void (*func) (void *elem));
extern void ** vector_to_array(struct vector *v);
extern void vector_add_from_array(struct vector *v, void **array);
extern void vector_add_from_vector(struct vector *v, struct vector *v2);

#endif
