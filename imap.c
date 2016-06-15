#include <stdio.h>
#include <stdlib.h>
#include "imap.h"

struct imap *imap_new(unsigned int cap)
{
    unsigned int nslots = 1 << cap;
}

void imap_free(struct imap *map)
{
    
}

struct ident *imap_lookup(struct imap *imap,
                          const unsigned char *name,
                          size_t len,
                          enum imap_lookup_option opt)
{
    
}

struct ident *imap_lookup_with_hash(struct imap *imap,
                                    const unsigned char *name,
                                    size_t len,
                                    unsigned int hash,
                                    enum imap_lookup_option opt)
{
    
}

void imap_foreach(struct imap *imap, imap_cb cb, const void *context)
{
    
}

void imap_dump(struct imap *imap)
{
    
}
