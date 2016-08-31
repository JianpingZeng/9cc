#include "utils.h"

static struct list *freelists;

struct list *list_append(struct list *list, void *x)
{
    struct list *new;

    if ((new = freelists) != NULL)
        freelists = freelists->link;
    else
        new = xmalloc(sizeof(struct list));

    if (list) {
        new->link = list->link;
        list->link = new;
    } else {
        new->link = new;
    }
    new->x = x;
    return new;
}

struct list *list_concat(struct list *list1, struct list *list2)
{
    struct list *p;

    if (!list1)
        return list2;
    if (!list2)
        return list1;

    p = list2->link;
    list2->link = list1->link;
    list1->link = p;

    return list2;
}

size_t list_length(struct list *list)
{
    size_t n = 0;

    if (list) {
        struct list *p = list;
        do
            n++;
        while ((p = p->link) != list);
    }
    
    return n;
}

void *ltoa(struct list **list, unsigned int area)
{
    int i = 0;
    void **array = newarray(sizeof(array[0]), list_length(*list) + 1, area);

    if (*list) {
        struct list *p = *list;
        do {
            p = p->link;
            array[i++] = p->x;
        } while (p != *list);

        p = (*list)->link;
        (*list)->link = freelists;
        freelists = p;
    }
    
    *list = NULL;
    array[i] = NULL;
    return array;
}
