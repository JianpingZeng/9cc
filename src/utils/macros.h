#ifndef _macro_h
#define _macro_h

#define TWOS(size)  (size)>=sizeof(unsigned long long) ? ~0ULL : ~((~0ULL)<<(CHAR_BIT*size))
#define BITS(type)  (CHAR_BIT * (type)->size)
#define ARRAY_SIZE(array)    (sizeof(array) / sizeof((array)[0]))
#define FIELD_SIZEOF(st, f)  (sizeof(((st*)0)->f))

#endif
