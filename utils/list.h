#ifndef _LIST_H
#define _LIST_H

struct list {
    void *x;
    struct list *link;
};

extern struct list *list_append(struct list *list, void *x);

extern struct list *list_concat(struct list *list1, struct list *list2);

extern size_t list_length(struct list *list);

extern void *ltoa(struct list **list, unsigned int area);

#endif
