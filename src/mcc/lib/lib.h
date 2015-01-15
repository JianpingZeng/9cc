#ifndef cc_lib_h
#define cc_lib_h

// error
extern int errcnt;
extern int warncnt;
extern void printreport();
extern void fatal(const char *fmt, ...);
extern void errorl(const char *file, int line, int lineno, const char *fmt, ...);
extern void warningl(const char *file, int line, int lineno, const char *fmt, ...);

// alloc
extern void * allocate(unsigned long size, int flags);
extern void deallocate(void *p);
extern const char *strings(const char *str);
extern const char *stringn(const char *src, int len);
extern const char *stringd(long n);
void appendstring(const char **string, const char *src, int len);
#define NEW(p)    ((p) = allocate(sizeof *(p), 0), memset((p), 0, sizeof *(p)))
#define FREE(p)   deallocate(p)

// vector
typedef struct vector *Vector;
extern Vector new_vector();
extern void * vector_at(Vector v, unsigned index);
extern void vector_push_back(Vector v, void *elem);
extern void vector_insert(Vector v, unsigned index, void *elem);
extern void free_vector(Vector v);
extern unsigned vector_length(Vector v);
extern void ** vector_to_array(Vector v);
extern void vector_add_from_array(Vector v, void **array);

#define log(fmt, ...)   fprintf(stderr, fmt, ##__VA_ARGS__)

#endif
