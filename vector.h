#ifndef _VECTOR_H
#define _VECTOR_H

struct vector {
    void    **mem;
    size_t  len;
    size_t  alloc;
};

extern struct vector *vec_new();

extern void vec_free(struct vector *v);

extern void * vec_at(struct vector *v, int index);

extern void vec_set(struct vector *v, int index, void *val);

extern void vec_push(struct vector *v, void *val);

extern size_t vec_len(struct vector *v);

extern void * vec_head(struct vector *v);

extern void * vec_tail(struct vector *v);

extern void vec_add(struct vector *v, struct vector *v2);

extern void ** vtoa(struct vector *v);

extern void vec_add_array(struct vector *v, void **array);

extern int array_len(void **array);

#endif
