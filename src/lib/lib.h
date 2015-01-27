#ifndef cc_lib_h
#define cc_lib_h

#include <stdio.h>
#include <stdarg.h>

// print
typedef const char * (*PrintFunc) (void *data);

extern void print(const char *fmt, ...);
extern void fprint(FILE *f, const char *fmt, ...);
extern void vfprint(FILE *f, const char *fmt, va_list ap);
extern void register_print_function(char c, PrintFunc p);

// alloc
extern void * allocate(unsigned long size, int flags);
extern void deallocate(void *p);
#define new(t)      ((t *) allocate(sizeof (t), 0))
#define delete(p)   deallocate(p)

// string
extern const char *strings(const char *str);
extern const char *stringn(const char *src, int len);
extern const char *stringd(long n);
extern void appendstring(const char **string, const char *src, int len);
typedef struct {
    char     *str;
    unsigned size;
    unsigned capelems;
    unsigned reserve;
} String;
extern String * new_string();
extern void string_concat_s(String *s, char *src);
extern void string_concat_n(String *s, char *src, int len);
extern void string_concat_d(String *s, long d);
extern char * string_to_array(String *s);
extern void free_string(String *s);

// vector (container of pointers)
typedef struct {
    void        **mem;
    unsigned    elemsize;
    unsigned    elems;
    unsigned    capelems;
    unsigned    reserve;
} Vector;
extern Vector *new_vector();
extern void * vector_at(Vector *v, unsigned index);
extern void vector_push(Vector *v, void *elem);
extern void *vector_pop(Vector *v);
extern void vector_insert(Vector *v, unsigned index, void *elem);
extern void free_vector(Vector *v);
extern void purge_vector(Vector *v);
extern unsigned vector_length(Vector *v);
extern void *vector_front(Vector *v);
extern void *vector_back(Vector *v);
extern void vector_foreach(Vector *v, void (*func) (void *elem));
extern void ** vector_to_array(Vector *v);
extern void vector_add_from_array(Vector *v, void **array);
extern void vector_add_from_vector(Vector *v, Vector *v2);

#endif
