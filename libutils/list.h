#ifndef LIST_H
#define LIST_H

struct list {
    void *x;
    struct list *link;
};

extern struct list *list_append(struct list *list, void *x);

extern size_t list_length(struct list *list);

extern void *ltoa(struct list **list, unsigned int area);

#endif
