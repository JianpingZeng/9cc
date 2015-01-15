#ifndef __mcc__string__
#define __mcc__string__

#include "object.h"

CLASS(string, object)
    const char *text;
END(string)

extern const struct class class_string;
extern const Class String;

#endif /* defined(__mcc__string__) */
