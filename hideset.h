#ifndef _HIDESET_H
#define _HIDESET_H

struct hideset {
    const unsigned char *name;
    struct hideset *next;
};

extern struct hideset *hideset_add(struct hideset *s, const unsigned char *name);

extern bool hideset_has(struct hideset *s, const unsigned char *name);

extern struct hideset *hideset_union(struct hideset *a, struct hideset *b);

extern struct hideset *hideset_intersection(struct hideset *a,
                                            struct hideset *b);

#endif
