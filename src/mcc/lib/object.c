#include "object.h"
#include "lib.h"
#include <assert.h>

static void * object_constructor(void *self, va_list *app)
{
    return self;
}

static void * object_dectructor(void *self)
{
    return self;
}

static void * object_clone(const void *self)
{
    return NULL;
}

BOOL isKindOfClass(void *_self, Class _class)
{
    return classof(_self) == _class;
}

BOOL isMemberOfClass(void *_self, Class _class)
{
    Class self = _self;
    while (self) {
        if (classof(self) == _class) {
            return YES;
        }
        self = self->super_class;
    }
    return NO;
}

const struct class class_object = {
    NULL,                   /* super class */
    "object",               /* name */
    sizeof(struct object),  /* size */
    object_constructor,     /* constructor */
    object_dectructor,      /* destructor */
    object_clone,            /* clone */
    isKindOfClass,
    isMemberOfClass
};

const Class Object = &class_object;

void * new(const struct class *_class, ...)
{
    void *p = allocate(_class->size, 0);
    assert(p);
    * (const struct class **) p = _class;
    if (_class->constructor) {
        va_list ap;
        va_start(ap, _class);
        p = _class->constructor(p, &ap);
        va_end(ap);
    }
    return p;
}

void delete(void *_self)
{
    const struct class **cp = _self;
    if (_self && *cp && (*cp)->destructor) {
        _self = (*cp)->destructor(_self);
    }
    deallocate(_self);
}
