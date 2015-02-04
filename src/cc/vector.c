#include "cc.h"

static void vector_grow(struct vector *v)
{
    assert(v->elems == v->capelems);
    void *mem = cc_malloc((v->elems + v->reserve) * v->elemsize);
    memcpy(mem, v->mem, v->elems * v->elemsize);
    cc_free(v->mem);
    v->mem = mem;
    v->capelems += v->reserve;
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

void * vector_at(struct vector *v, unsigned index)
{
    assert(v);
    assert(index < v->elems);
    return v->mem[index];
}

void vector_push(struct vector *v, void *elem)
{
    assert(v);
    vector_insert(v, v->elems, elem);
}

void *vector_pop(struct vector *v)
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

void vector_insert(struct vector *v, unsigned index, void *elem)
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

void free_vector(struct vector *v)
{
    assert(v);
    cc_free(v->mem);
    cc_free(v);
}

void purge_vector(struct vector *v)
{
    assert(v);
    for (int i=0; i < vector_length(v); i++) {
	void *p = vector_at(v, i);
	cc_free(p);
    }
    free_vector(v); 
}

unsigned vector_length(struct vector *v)
{
    assert(v);
    return v->elems;
}

// Convert a vector to a null-terminated array
void ** vector_to_array(struct vector *v)
{
    int vlen = vector_length(v);
    void **array = cc_malloc((vlen+1) * v->elemsize);
    array[vlen] = 0;
    memcpy(array, v->mem, vlen * v->elemsize);
    free_vector(v);
    return array;
}

// Add elements to vector from a null-terminated array
void vector_add_from_array(struct vector *v, void **array)
{
    assert(v && array);
    for (int i=0; array[i]; i++) {
        vector_push(v, array[i]);
    }
}

void vector_add_from_vector(struct vector *v, struct vector *v2)
{
    assert(v && v2);
    for (int i=0; i < vector_length(v2); i++) {
        vector_push(v, vector_at(v2, i));
    }
}

void *vector_front(struct vector *v)
{
    return vector_at(v, 0);
}

void *vector_back(struct vector *v)
{
    return vector_at(v, vector_length(v)-1);
}

void vector_foreach(struct vector *v, void (*func) (void *elem))
{
    if (!func) return;
    for (int i=0; i < vector_length(v); i++) {
	void *p = vector_at(v, i);
	func(p);
    }
}
