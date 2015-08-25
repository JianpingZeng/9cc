#include <string.h>
#include <assert.h>
#include "utils.h"

static void vec_grow(struct vector *v)
{
    if (v->len == v->nalloc) {
        void *mem = NEW0((v->len + v->reserve) * v->elemsize);
        memcpy(mem, v->mem, v->len * v->elemsize);
        v->mem = mem;
        v->nalloc += v->reserve;
    }
}

struct vector *new_vector()
{
    struct vector *v = NEWS(vector);
    v->reserve = 10;
    v->elemsize = sizeof(void *);
    v->len = 0;
    v->nalloc = v->reserve;
    v->mem = NEW0(v->elemsize * v->nalloc);
    return v;
}

void * vec_at(struct vector *v, int index)
{
    assert(index >= 0 && index < v->len);
    return v->mem[index];
}

void vec_set(struct vector *v, int index, void *val)
{
    assert(index >= 0 && index < v->len);
    v->mem[index] = val;
}

void * vec_head(struct vector *v)
{
    assert(v->len > 0);
    return v->mem[0];
}

void * vec_tail(struct vector *v)
{
    assert(v->len > 0);
    return v->mem[v->len - 1];
}

void vec_push(struct vector *v, void *elem)
{
    if (elem == NULL) return;
    vec_grow(v);
    v->mem[v->len++] = elem;
}

size_t vec_len(struct vector *v)
{
    return v->len;
}

// Add elements to vector from a null-terminated array
void vec_add_from_array(struct vector *v, void **array)
{
    if (array == NULL) return;
    for (int i=0; array[i]; i++)
        vec_push(v, array[i]);
}

void vec_add_from_vector(struct vector *v, struct vector *v2)
{
    for (int i=0; i < vec_len(v2); i++)
        vec_push(v, vec_at(v2, i));
}

void vec_foreach(struct vector *v, void (*func) (void *elem, void *context), void *context)
{
    assert(func);
    for (int i=0; i < vec_len(v); i++) {
        void *p = vec_at(v, i);
        func(p, context);
    }
}

void ** vtoa(struct vector *v)
{
    void **array = NULL;
    int vlen = vec_len(v);
    if (vlen > 0) {
        array = NEW0((vlen+1) * v->elemsize);
        memcpy(array, v->mem, vlen * v->elemsize);
    }
    return array;
}

int array_len(void **array)
{
    int i;
    if (array == NULL) return 0;
    for (i=0; array[i]; i++)
        ;
    return i;
}
