#include <assert.h>
#include <stdlib.h>
#include "utils.h"

#define VEC_INIT_SIZE   64

static void vec_grow(struct vector *v)
{
    v->alloc <<= 1;
    v->mem = xrealloc(v->mem, v->alloc * sizeof(void *));
}

void vec_free(struct vector *v)
{
    free(v->mem);
    free(v);
}

struct vector *vec_new(void)
{
    struct vector *v = zmalloc(sizeof(struct vector));
    v->len = 0;
    v->alloc = VEC_INIT_SIZE;
    v->mem = xmalloc(v->alloc * sizeof(void *));
    return v;
}

struct vector *vec_new1(void *val)
{
    struct vector *v = vec_new();
    vec_push(v, val);
    return v;
}

struct vector *vec_newn(size_t capacity)
{
    struct vector *v = zmalloc(sizeof(struct vector));
    v->len = 0;
    v->alloc = MAX(VEC_INIT_SIZE, capacity);
    v->mem = xmalloc(v->alloc * sizeof(void *));
    return v;
}

void *vec_at(struct vector *v, int index)
{
    assert(index >= 0 && index < v->len);
    return v->mem[index];
}

void *vec_at_safe(struct vector *v, int index)
{
    if (index >= 0 && index < v->len)
        return v->mem[index];
    else
        return NULL;
}

void vec_set(struct vector *v, int index, void *val)
{
    assert(index >= 0 && index < v->len);
    v->mem[index] = val;
    if (val == NULL)
        v->len = index;
}

void vec_set_safe(struct vector *v, int index, void *val)
{
    if (val)
        vec_set(v, index, val);
}

void *vec_head(struct vector *v)
{
    if (v->len == 0)
        return NULL;
    return v->mem[0];
}

void *vec_tail(struct vector *v)
{
    if (v->len == 0)
        return NULL;
    return v->mem[v->len - 1];
}

void vec_add(struct vector *v, struct vector *v2)
{
    size_t len2 = vec_len(v2);
    for (size_t i = 0; i < len2; i++)
        vec_push(v, v2->mem[i]);
}

void vec_push(struct vector *v, void *val)
{
    assert(val);
    if (v->len == v->alloc)
        vec_grow(v);
    v->mem[v->len++] = val;
}

void vec_push_safe(struct vector *v, void *val)
{
    if (val)
        vec_push(v, val);
}

void vec_push_front(struct vector *v, void *val)
{
    assert(val);
    if (v->len == v->alloc)
        vec_grow(v);
    memmove(v->mem + 1, v->mem, v->len * sizeof(void *));
    v->mem[0] = val;
    v->len++;
}

void *vec_pop(struct vector *v)
{
    if (v->len == 0)
        return NULL;
    void *r = v->mem[v->len - 1];
    v->len--;
    return r;
}

void *vec_pop_front(struct vector *v)
{
    if (v->len == 0)
        return NULL;
    void *r = v->mem[0];
    v->len--;
    memmove(v->mem, v->mem + 1, v->len * sizeof(void *));
    return r;
}

void vec_clear(struct vector *v)
{
    v->len = 0;
}

int vec_empty(struct vector *v)
{
    if (v == NULL)
        return 1;
    return v->len == 0;
}

size_t vec_len(struct vector *v)
{
    if (v == NULL)
        return 0;
    return v->len;
}

struct vector *vec_reverse(struct vector *v)
{
    struct vector *r = vec_new();
    for (int i = vec_len(v) - 1; i >= 0; i--)
        vec_push(r, vec_at(v, i));
    return r;
}

struct vector *vec_copy(struct vector *v)
{
    if (v == NULL)
        return NULL;
    struct vector *copy = vec_new();
    vec_add(copy, v);
    return copy;
}

struct vector *vec_sort(struct vector *v, int (*sort) (const void *val1, const void *val2))
{
    if (vec_empty(v))
        return NULL;
    struct vector *r = vec_copy(v);
    qsort(r->mem, r->len, sizeof(void *), sort);
    return r;
}

// Add elements to vector from a null-terminated array
void vec_add_array(struct vector *v, void *array)
{
    if (array == NULL)
        return;
    void **a = (void **)array;
    for (int i = 0; a[i]; i++)
        vec_push(v, a[i]);
}

void *vtoa(struct vector *v, unsigned int area)
{
    int i = 0;
    size_t len = vec_len(v);
    void **array = newarray(sizeof(array[0]), len + 1, area);

    if (v) {
        for (; i < len; i++)
            array[i] = vec_at(v, i);
    }

    array[i] = NULL;
    return array;
}
