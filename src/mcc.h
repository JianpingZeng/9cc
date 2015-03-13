#ifndef driver_mcc_h
#define driver_mcc_h

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

extern const char *cpp[];
extern int cpp_main(int argc, char **argv);

extern char *mk_temp_dir();
extern int file_exists(const char *path);
extern int callsys(const char *path, char **argv);
extern char *replace_suffix(const char *path, const char *suffix);

extern int rmdir(const char *dir);
extern char *expanduser(char *path);

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
