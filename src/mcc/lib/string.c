#include "string.h"

static void * string_constructor(void *self, va_list *app)
{
    return self;
}

static void * string_dectructor(void *self)
{
    return NULL;
}

static void * string_clone(const void *self)
{
    return NULL;
}

const struct class class_string = {
    &class_object,                   /* super class */
    "string",               /* name */
    sizeof(string),  /* size */
    string_constructor,     /* constructor */
    string_dectructor,      /* destructor */
    string_clone            /* clone */
};

const Class String = &class_string;