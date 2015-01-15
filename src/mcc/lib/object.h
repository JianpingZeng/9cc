#ifndef cc_object_h
#define cc_object_h

#include <stdarg.h>
#include <sys/types.h>
#include <stddef.h>

typedef char BOOL;
#define YES 1
#define NO  0

typedef struct class *Class;

struct class {
    Class super_class;
    const char *name;
    size_t size;
    void * (*constructor) (void *_self, va_list *app);
    void * (*destructor) (void *_self);
    void * (*clone) (const void *_self);
    BOOL (*isKindOfClass) (void *_self, Class _class);
    BOOL (*isMemberOfClass) (void *_self, Class _class);
};

struct object {
    Class isa;
};

extern const struct class class_object;
extern const Class Object;

extern void * new(const struct class *_class, ...);
extern void delete(void *_self);

#define classof(p)  (*((Class *)(p)))
#define object(p)   ((struct object *)(p))

#define CLASS(self, super)  \
                             typedef struct _##self {\
                                struct super _;

#define END(self)   }self;

#endif