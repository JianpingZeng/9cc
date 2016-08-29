#ifndef _LIST_H
#define _LIST_H

struct list {
    void *x;
    struct list *link;
};

extern struct list *list_append(struct list *list, void *x);

extern size_t list_length(struct list *list);

extern void *ltov(struct list **list, unsigned int area);

#endif
