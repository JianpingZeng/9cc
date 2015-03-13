#ifndef MCC_LIB_H
#define MCC_LIB_H

// vector.c
struct vector {
    void   **mem;
    int    elemsize;
    int    elems;
    int    capelems;
    int    reserve;
};
struct vector *new_vector();
void * vector_at(struct vector *v, int index);
void vector_push(struct vector *v, void *elem);
void * vector_pop(struct vector *v);
void vector_insert(struct vector *v, int inex, void *elem);
void free_vector(struct vector *v);
void purge_vector(struct vector *v);
int vector_length(struct vector *v);
void ** vector_to_array(struct vector *v);
void vector_add_from_array(struct vector *v, void **array);
void vector_add_from_vector(struct vector *v, struct vector *v2);
void * vector_front(struct vector *v);
void * vector_back(struct vector *v);
void vector_foreach(struct vector *v, void (*func)(void *elem));
int array_length(void **array);

#endif
