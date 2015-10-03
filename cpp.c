#include "cc.h"

static struct map *defines;
struct source source;

static void parseopts(struct vector *options)
{

}

void cpp_init(const char *file, struct vector *options)
{
    parseopts(options);
    defines = map_new(nocmp);
}
