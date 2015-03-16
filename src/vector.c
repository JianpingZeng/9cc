#include <string.h>
#include <assert.h>
#include "lib.h"

static void vec_grow(struct vector *v)
{
    assert(v->elems == v->capelems);
    void *mem = cc_malloc((v->elems + v->reserve) * v->elemsize);
    memcpy(mem, v->mem, v->elems * v->elemsize);
    cc_free(v->mem);
    v->mem = mem;
    v->capelems += v->reserve;
}

static void vec_insert(struct vector *v, int index, void *elem)
{
    assert(v);
    assert(elem && index >= 0 && index <= v->elems);
    if (v->elems == v->capelems) {
        vec_grow(v);
    }
    if (index < v->elemsize) {
        // maybe overlap
        memmove(v->mem + v->elemsize * (index+1), v->mem + v->elemsize * index, (v->elems - index) * v->elemsize);
    }
    v->mem[index] = elem;
    v->elems++;
}

struct vector *new_vector()
{
    struct vector *v = cc_malloc(sizeof(struct vector));
    v->reserve = 10;
    v->elemsize = sizeof(void *);
    v->elems = 0;
    v->capelems = v->reserve;
    v->mem = cc_malloc(v->elemsize * v->capelems);
    return v;
}

void free_vector(struct vector *v)
{
    assert(v);
    cc_free(v->mem);
    cc_free(v);
}

void purge_vector(struct vector *v)
{
    assert(v);
    for (int i=0; i < vec_len(v); i++) {
        void *p = vec_at(v, i);
        cc_free(p);
    }
    free_vector(v);
}

void * vec_at(struct vector *v, int index)
{
    assert(v);
    assert(index >= 0 && index < v->elems);
    return v->mem[index];
}

void vec_push(struct vector *v, void *elem)
{
    assert(v);
    if (elem)
        vec_insert(v, v->elems, elem);
}

int vec_len(struct vector *v)
{
    if (v)
        return v->elems;
    else
        return 0;
}

// Add elements to vector from a null-terminated array
void vec_add_from_array(struct vector *v, void **array)
{
    assert(v);
    if (array == NULL)
        return;
    for (int i=0; array[i]; i++) {
        vec_push(v, array[i]);
    }
}

void vec_add_from_vector(struct vector *v, struct vector *v2)
{
    assert(v);
    for (int i=0; i < vec_len(v2); i++) {
        vec_push(v, vec_at(v2, i));
    }
}

void vec_foreach(struct vector *v, void (*func) (void *elem, void *context), void *context)
{
    assert(func);
    for (int i=0; i < vec_len(v); i++) {
        void *p = vec_at(v, i);
        func(p, context);
    }
}

/**
 * vec_to_array is different from vtoa,
 * the caller needs to free the memory returned.
 */
void ** vec_to_array(struct vector *v)
{
    void **array = NULL;
    int vlen = vec_len(v);
    if (vlen > 0) {
        array = cc_malloc((vlen+1) * v->elemsize);
        memcpy(array, v->mem, vlen * v->elemsize);
    }
    free_vector(v);
    return array;
}
