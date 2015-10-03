#include "cc.h"

static struct map *defines;
struct source source;

static void parseopts(struct vector *options)
{

}

void cpp_init(struct vector *options)
{
    defines = map_new(nocmp);
    parseopts(options);
}
