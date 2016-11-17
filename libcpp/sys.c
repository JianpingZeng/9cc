#include "config.h"
#include "internal.h"
#include "libutils.h"

#ifdef CONFIG_LINUX

struct vector *sys_include_dirs(void)
{
    struct vector *v = vec_new();
    vec_push(v, _7CC_LIB_DIR "/include");
    vec_push(v, "/usr/include");
    vec_push(v, "/usr/include/linux");
    vec_push(v, "/usr/include/x86_64-linux-gnu");
    return v;
}

#elif defined (CONFIG_DARWIN)

struct vector *sys_include_dirs(void)
{
    struct vector *v = vec_new();
    vec_push(v, _7CC_LIB_DIR "/include");
    vec_push(v, XCODE_DIR "/usr/include");
    return v;
}

#else
#error "unknown platform"
#endif
