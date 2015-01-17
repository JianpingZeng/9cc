#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <assert.h>
#include "lib.h"

static void vector_grow(Vector *v)
{
    assert(v->elems == v->capelems);
    void *mem = allocate((v->elems + v->reserve) * v->elemsize, 0);
    memcpy(mem, v->mem, v->elems * v->elemsize);
    deallocate(v->mem);
    v->mem = mem;
    v->capelems += v->reserve;
}

Vector *new_vector()
{
    Vector *v = new(Vector);
    v->reserve = 10;
    v->elemsize = sizeof(void *);
    v->elems = 0;
    v->capelems = v->reserve;
    v->mem = allocate(v->elemsize * v->capelems, 0);
    return v;
}

void * vector_at(Vector *v, unsigned index)
{
    assert(v);
    assert(index < v->elems);
    return v->mem[index];
}

void vector_push(Vector v, void *elem)
{
    assert(v);
    vector_insert(v, v->elems, elem);
}

void *vector_pop(Vector *v)
{
    assert(v);
    if (v->elems == 0) {
	return NULL;
    }
    else {
	void *p = v->mem[v->elems-1];
	v->elems--;
	return p;
    }
}

void vector_insert(Vector v, unsigned index, void *elem)
{
    assert(v);
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

void free_vector(Vector *v)
{
    assert(v);
    deallocate(v->mem);
    deallocate(v);
}

unsigned vector_length(Vector *v)
{
    assert(v);
    return v->elems;
}

// Convert a vector to a null-terminated array
void ** vector_to_array(Vector *v)
{
    int vlen = vector_length(v);
    void **array = allocate((vlen+1) * v->elemsize, 0);
    array[vlen] = 0;
    memcpy(array, v->mem, vlen * v->elemsize);
    free_vector(v);
    return array;
}

// Add elements to vector from a null-terminated array
void vector_add_from_array(Vector *v, void **array)
{
    assert(v && array);
    for (int i=0; array[i]; i++) {
        vector_push_back(v, array[i]);
    }
}

void *vector_front(Vector *v)
{
    return vector_at(v, 0);
}

void *vector_back(Vector *v)
{
    return vector_at(v, vector_length(v)-1);
}

void vector_foreach(Vector *v, void (*func) (void *elem))
{
    if (!func) return;
    for (int i=0; i < vector_length(v); i++) {
	void *p = vector_at(v, i);
	func(p);
    }
}
