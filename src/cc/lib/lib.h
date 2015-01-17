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
extern const char *strings(const char *str);
extern const char *stringn(const char *src, int len);
extern const char *stringd(long n);
void appendstring(const char **string, const char *src, int len);
#define new(t)      ((t *) allocate(sizeof (t), 0))
#define delete(p)   deallocate(p)

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
extern unsigned vector_length(Vector *v);
extern void *vector_front(Vector *v);
extern void *vector_back(Vector *v);
extern void vector_foreach(Vector *v, void (*func) (void *elem));
extern void ** vector_to_array(Vector *v);
extern void vector_add_from_array(Vector *v, void **array);

#endif
