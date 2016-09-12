#ifndef _CPP_INTERNAL_H
#define _CPP_INTERNAL_H

// for bool/size_t
#include <stdbool.h>
#include <stddef.h>

// hideset.c
struct hideset {
    const unsigned char *name;
    struct hideset *next;
};

extern struct hideset *hideset_add(struct hideset *s, const unsigned char *name);
extern bool hideset_has(struct hideset *s, const unsigned char *name);
extern struct hideset *hideset_union(struct hideset *a, struct hideset *b);
extern struct hideset *hideset_intersection(struct hideset *a,
                                            struct hideset *b);

// imap.c
/*
  An identifier hash map.

  ACKNOWLEDGE:

  imap is modified from cpp_hash_table in GCC.
 */

#define IMAP_HASHSTEP(h, c)  ((h) * 67 + ((c) - 133))
#define IMAP_HASHFINISH(h, len)  ((h) + (len))

enum imap_lookup_option { IMAP_SEARCH = 0, IMAP_CREATE };

// An identifier hash table for lexer.
struct imap {
    struct ident **table;
    unsigned int nslots;        // number of slots
    unsigned int nelements;     // number of elements
    // Callback, allocate an entry
    struct ident * (*alloc_entry) (struct imap *);
    // Statistics
    unsigned int searches;
    unsigned int collisions;
};

extern struct imap *imap_new(unsigned int cap);
extern void imap_free(struct imap *imap);
extern struct ident *imap_lookup(struct imap *imap,
                                 const unsigned char *str,
                                 size_t len,
                                 enum imap_lookup_option opt);
extern struct ident *imap_lookup_with_hash(struct imap *imap,
                                           const unsigned char *str,
                                           size_t len,
                                           unsigned int hash,
                                           enum imap_lookup_option opt);
typedef int (*imap_cb) (struct imap *imap, struct ident *id, const void *context);
extern void imap_foreach(struct imap *imap, imap_cb cb, const void *context);
extern void imap_dump(struct imap *imap);

// error.c
enum { WRN = 1, ERR, FTL };

extern unsigned int cpp_errors;
extern void cpp_warningf(const char *file, unsigned int line, unsigned int column,
                         const char *fmt, ...);
extern void cpp_errorf(const char *file, unsigned int line, unsigned int column,
                       const char *fmt, ...);
extern void cpp_fatalf(const char *file, unsigned int line, unsigned int column,
                       const char *fmt, ...);

#define SAVE_ERRORS    unsigned int __err = cpp_errors
#define NO_ERROR       (__err == cpp_errors)
#define HAS_ERROR      (__err != cpp_errors)

#define cpp_warning_at(s, ...)   cpp_warningf(s.file, s.line, s.column, __VA_ARGS__)
#define cpp_error_at(s, ...)     cpp_errorf(s.file, s.line, s.column, __VA_ARGS__)
#define cpp_fatal_at(s, ...)     cpp_fatalf(s.file, s.line, s.column, __VA_ARGS__)
#define cpp_warning(...)         cpp_warning_at(source, __VA_ARGS__)
#define cpp_error(...)           cpp_error_at(source, __VA_ARGS__)
#define cpp_fatal(...)           cpp_fatal_at(source, __VA_ARGS__)

// expr.c
extern bool eval_cpp_const_expr(void);

#endif
