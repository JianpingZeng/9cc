#ifndef _LIB_H
#define _LIB_H

#define TWOS(size)  (size)>=sizeof(unsigned long long) ? ~0ULL : ~((~0ULL)<<(CHAR_BIT*size))
#define BITS(type)  (CHAR_BIT * (type)->size)
#define ARRAY_SIZE(array)    (sizeof(array) / sizeof((array)[0]))
#define FIELD_SIZEOF(st, f)  (sizeof(((st*)0)->f))

// util.c
struct bucket_info {
    void *p;			// free position
    void *limit;		// end position
    struct bucket_info *next;	// next bucket
};
extern void * alloc_bucket(size_t size);
extern void free_bucket(struct bucket_info *s);
extern void * alloc_table(size_t size);
extern void drop_table(void *table);
extern void * alloc_table_entry(void *table, size_t size);
extern void * alloc_for_size(struct bucket_info *s, size_t size);

extern const char *strings(const char *str);
extern const char *stringn(const char *src, int len);
extern const char *stringd(long n);

extern void print_bucket(struct bucket_info *s, const char *name);
extern void print_table(void *table, const char *name);
extern void unit_exit();

extern void * cc_malloc(size_t size);
extern void cc_free(void *p);
extern void die(const char *fmt, ...);
extern int array_len(void **array);

// string.c
struct string {
    char     *str;
    unsigned size;
    unsigned capelems;
    unsigned reserve;
};
extern struct string * new_string();
extern void free_string(struct string *s);

extern unsigned str_len(struct string *s);
extern void str_cats(struct string *s, const char *src);
extern void str_catn(struct string *s, const char *src, int len);
extern void str_catd(struct string *s, long d);
extern char * str_to_array(struct string *s);

// vector.c
struct vector {
    void   **mem;
    int    elemsize;
    int    elems;
    int    capelems;
    int    reserve;
};
extern struct vector *new_vector();
extern void free_vector(struct vector *v);
extern void purge_vector(struct vector *v);

extern void * vec_at(struct vector *v, int index);
extern void vec_push(struct vector *v, void *elem);
extern int vec_len(struct vector *v);
extern void vec_add_from_array(struct vector *v, void **array);
extern void vec_add_from_vector(struct vector *v, struct vector *v2);
extern void vec_foreach(struct vector *v, void (*func)(void *elem, void *context), void *context);
extern void ** vec_to_array(struct vector *v);

// string to array
extern char * stoa(struct string *s);
// vector to array
extern void ** vtoa(struct vector *v);

#endif
