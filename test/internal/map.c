#include "test.h"
#include "cc.h"
#include "utils.h"

static void test_map()
{
    struct map *map = new_map();
    
    expectp(map_get(map, "name"), NULL);
    
    map_put(map, "name", "Bill");
    expects(map_get(map, "name"), "Bill");
    
    map_put(map, "name", NULL);
    expectp(map_get(map, "name"), NULL);
    
    map_put(map, "os", "mac os x");
    map_put(map, "os", "ubuntu");
    expects(map_get(map, "os"), "ubuntu");
    
    map_put(map, "os", NULL);
    expectp(map_get(map, "os"), NULL);
    
    map_put(map, "op", "delete");
    expects(map_get(map, "op"), "delete");
    
    free_map(map);
}

void testmain()
{
    print("map");
    test_map();
}