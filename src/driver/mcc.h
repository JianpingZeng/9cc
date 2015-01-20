#ifndef driver_mcc_h
#define driver_mcc_h

extern char *mk_temp_dir();
extern int file_exists(const char *path);
extern int callsys(const char *path, char *argv[]);

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
