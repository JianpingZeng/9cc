#include "lib.h"

static void string_grow(struct string *s, int len)
{
    char *mem = alloc_temp(len + s->reserve);
    memcpy(mem, s->str, s->size);
    deallocate(s->str);
    s->str = mem;
    s->capelems = len + s->reserve;
}

struct string * new_string()
{
    struct string *s = alloc_temp(sizeof(struct string));
    s->reserve = 128;
    s->size = 0;
    s->capelems = s->reserve;
    s->str = allocate(s->capelems, 0);
    return s;
}

unsigned string_length(struct string *s)
{
    assert(s);
    return s->size;
}

void string_concats(struct string *s, char *src)
{
    string_concatn(s, src, strlen(src));
}

void string_concatn(struct string *s, char *src, int len)
{
    assert(s);
    if (s->size + len >= s->capelems) {
	string_grow(s, s->size+len);
    }

    char *dst = s->str + s->size;
    int size = len;
    while (size-- > 0) {
	*dst++ = *src++;
    }
    s->size += len;
}

void string_concatd(struct string *s, long n)
{
    assert(s);
    char str[32], *ps = str + sizeof (str);
    unsigned long m;
    
    if (n == LONG_MIN)
	m = (unsigned long)LONG_MAX + 1;
    else if (n < 0)
	m = -n;
    else
	m = n;
    
    do {
	*--ps = m%10 + '0';
    } while ((m /= 10) != 0);
    
    if (n < 0) {
	*--ps = '-';
    }
    return string_concatn(s, ps, str + sizeof (str) - ps);
}

char * string_to_array(struct string *s)
{
    assert(s);
    char *str = allocate(s->size+1, 0);
    memcpy(str, s->str, s->size);
    free_string(s);
    return str;
}

void free_string(struct string *s)
{
    assert(s);
    deallocate(s->str);
    deallocate(s);
}
