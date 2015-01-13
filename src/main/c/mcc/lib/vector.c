#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <assert.h>
#include "lib.h"

struct vector {
    void        **mem;
    unsigned    elemsize;
    unsigned    elems;
    unsigned    capelems;
    unsigned    reserve;
};

static void vector_grow(Vector v)
{
    assert(v->elems == v->capelems);
    void *mem = allocate((v->elems + v->reserve) * v->elemsize, 0);
    memcpy(mem, v->mem, v->elems * v->elemsize);
    deallocate(v->mem);
    v->mem = mem;
    v->capelems += v->reserve;
}

Vector new_vector(unsigned elemsize)
{
    Vector v = (Vector) allocate(sizeof(struct vector), 0);
    v->reserve = 10;
    v->elemsize = sizeof(void *);
    v->elems = 0;
    v->capelems = v->reserve;
    v->mem = allocate(v->elemsize * v->capelems, 0);
    return v;
}

void * vector_at(Vector v, unsigned index)
{
    if (!v) {
        return NULL;
    }
    assert(index < v->elems);
    return v->mem[index];
}

void vector_push_back(Vector v, void *elem)
{
    if (!v) {
        return;
    }
    vector_insert(v, v->elems, elem);
}

void vector_insert(Vector v, unsigned index, void *elem)
{
    if (!v) {
        return;
    }
    assert(elem && index <= v->elems);
    if (v->elems == v->capelems) {
        vector_grow(v);
    }
    if (index < v->elemsize) {
        // maybe overlap
        memmove(v->mem + v->elemsize * (index+1), v->mem + v->elemsize * index, (v->elems - index) * v->elemsize);
    }
    v->mem[index] = elem;
    v->elems++;
}

void free_vector(Vector v)
{
    if (v) {
        deallocate(v->mem);
        deallocate(v);
    }
}

unsigned vector_length(Vector v)
{
    if (!v) {
        return 0;
    }
    else {
        return v->elems;
    }
}

// Convert a vector to a null-terminated array
void ** vector_to_array(Vector v)
{
    int vlen = vector_length(v);
    void **array = allocate((vlen+1) * sizeof(array[0]), 0);
    array[vlen] = 0;
    if (v) {
        memcpy(array, v->mem, v->elems * v->elemsize);
        free_vector(v);
    }
    
    return array;
}

// Add elements to vector from a null-terminated array
void vector_add_from_array(Vector v, void **array)
{
    if (!v || !array) {
        return;
    }
    for (int i=0; array[i]; i++) {
        vector_push_back(v, array[i]);
    }
}
