#ifndef CPP_INTERNAL_H
#define CPP_INTERNAL_H

// for bool/size_t
#include <stdbool.h>
#include <stddef.h>

// hideset.c
struct hideset {
    const char *name;
    struct hideset *next;
};

extern struct hideset *hideset_add(struct hideset *, const char *);
extern bool hideset_has(struct hideset *, const char *);
extern struct hideset *hideset_union(struct hideset *,
                                     struct hideset *);
extern struct hideset *hideset_intersection(struct hideset *,
                                            struct hideset *);

// idtab
enum idtab_lookup_option { ID_SEARCH = 0, ID_CREATE };

struct idtab_entry {
    struct ident *ident;
    struct idtab_entry *link;
};

struct idtab {
    struct idtab_entry **table;
    unsigned int nslots;        // number of slots
    unsigned int nelements;     // number of elements
    unsigned int searches;
    unsigned int collisions;
    struct ident * (*alloc_ident) (struct idtab *);
};

extern struct idtab *idtab_new(unsigned int);
extern void idtab_free(struct idtab *);
extern struct ident *idtab_lookup(struct idtab *,
                                  const char *, size_t,
                                  enum idtab_lookup_option);
extern struct ident *idtab_lookup_with_hash(struct idtab *,
                                            const char *, size_t,
                                            unsigned int,
                                            enum idtab_lookup_option);
typedef int (*idtab_cb) (struct idtab *, struct ident *, const void *);
extern void idtab_foreach(struct idtab *, idtab_cb, const void *);
extern void idtab_dump(struct idtab *t);

// error.c
enum { WRN = 1, ERR, FTL };

struct source;

extern void cpp_warning_at(struct source, const char *, ...);
extern void cpp_error_at(struct source, const char *, ...);
extern void cpp_fatal_at(struct source, const char *, ...);

#define SAVE_ERRORS    unsigned int __err = cpp_file->errors
#define NO_ERROR       (__err == cpp_file->errors)
#define HAS_ERROR      (__err != cpp_file->errors)

#define cpp_warning(...)  cpp_warning_at(source, __VA_ARGS__)
#define cpp_error(...)    cpp_error_at(source, __VA_ARGS__)
#define cpp_fatal(...)    cpp_fatal_at(source, __VA_ARGS__)

// expr.c
extern bool eval_cpp_const_expr(void);

// sys.c
extern struct vector *sys_include_dirs(void);

// dump
extern void strtab_dump(void);

#endif
