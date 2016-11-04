#ifndef GEN_H
#define GEN_H

// forward declaration
struct expr;

struct xface {
    void (*label) (struct expr *);
};

#endif
