#ifndef _IMAP_H
#define _IMAP_H

/*
  An identifier hash map.

  ACKNOWLEDGE:

  imap is modified from cpp_hash_table in GCC.
 */

#define IMAP_HASHSTEP(h, c)  ((h) * 67 + ((c) - 133))
#define IMAP_HASHFINISH(h, len)  ((h) + (len))

enum imap_lookup_option {
    IMAP_SEARCH = 0,
    IMAP_CREATE
};

// An identifier
struct ident {
    unsigned int hash;
    unsigned int len;
    const unsigned char *name;
};

// An identifier hash table for lexer.
struct imap {
    struct ident **table;
    unsigned int nslots;        // number of slots
    unsigned int nelements;     // number of elements
    // Callback, allocate an entry
    struct ident * (*alloc_entry) (struct imap *);
    // Callback, allocate data attatched on an entry
    void * (*alloc_subobject) (size_t);
    // Statistics
    unsigned int searches;
    unsigned int collisions;
};

extern struct imap *imap_new(unsigned int cap);
extern void imap_free(struct imap *map);
extern struct ident *imap_lookup(struct imap *imap,
                                 const unsigned char *name,
                                 size_t len,
                                 enum imap_lookup_option opt);
extern struct ident *imap_lookup_with_hash(struct imap *imap,
                                           const unsigned char *name,
                                           size_t len,
                                           unsigned int hash,
                                           enum imap_lookup_option opt);
typedef int (*imap_cb) (struct imap *imap, struct ident *id, const void *context);
extern void imap_foreach(struct imap *imap, imap_cb cb, const void *context);
extern void imap_dump(struct imap *imap);

#endif
