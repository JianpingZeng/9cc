#ifndef _EVENTS_H
#define _EVENTS_H

// events
struct event {
    void (*include) (const char *file);
    void (*comment) (void);
    void (*dcltype) (void *node);
    void (*dclvar) (void *node);
    void (*dclfun) (void *node);
    void (*defun) (void *node);
    void (*end) (void);
};
extern struct event events;

#endif

