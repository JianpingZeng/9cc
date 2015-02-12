#include "cc.h"

#define RESERVED_SIZE 8192

#define ALIGN_SIZE    (sizeof(unsigned long long))
#define ROUNDUP(size) ((size+(ALIGN_SIZE - 1))&(~(ALIGN_SIZE - 1)))

void * cc_malloc(size_t size)
{
    void *p = malloc(size);
    if (!p)
	die("Can't malloc");
    memset(p, 0, size);
    return p;
}

void cc_free(void *p)
{
    free(p);
}

struct alloc_state {
    int nr;			// number of free nodes left
    void *p;			// first free node position
};

static inline void * alloc_node(struct alloc_state *s, size_t size)
{
    void *ret;
    
    if (!s->nr) {
	s->nr = RESERVED_SIZE;
	s->p = cc_malloc(size*s->nr);
    }

    s->nr--;
    ret = s->p;
    s->p = (char *)s->p + size;
    return ret;
}

static struct alloc_state node_state;
void * alloc_node_node()
{
    void *ret = alloc_node(&node_state, sizeof(struct node));
    return ret;
}

static struct alloc_state expr_state;
void * alloc_expr_node()
{
    void *ret = alloc_node(&expr_state, sizeof(struct expr));
    return ret;
}

static struct alloc_state stmt_state;
void * alloc_stmt_node()
{
    void *ret = alloc_node(&stmt_state, sizeof(struct stmt));
    return ret;
}

static struct alloc_state decl_state;
void * alloc_decl_node()
{
    void *ret = alloc_node(&decl_state, sizeof(struct decl));
    return ret;
}

static struct alloc_state type_state;
void * alloc_type_node()
{
    void *ret = alloc_node(&type_state, sizeof(struct type));
    return ret;
}

static struct alloc_state symbol_state;
void * alloc_symbol_node()
{
    void *ret = alloc_node(&symbol_state, sizeof(struct symbol));
    return ret;
}

/*
 * alloc_bucket: for table-like structures
 *
 * A table-like structure has a bucket array and
 * each bucket points to a linked list of content
 * entry.
 */

struct alloc_bucket {
    void *p;			// free position
    void *limit;		// end position
    struct alloc_bucket *next;	// next bucket
};

#define ALLOC_BUCKET_FOR(table)  ((struct alloc_bucket *)table - 1)

static void * alloc_bucket(size_t size)
{
    struct alloc_bucket *pb;
    size = ROUNDUP(size);
    
    pb = cc_malloc(sizeof(struct alloc_bucket) + size + RESERVED_SIZE);
    pb->p = pb + 1;
    pb->limit = (char *)pb->p + size + RESERVED_SIZE;
    return pb;
}

static inline void * alloc_for_bucket(struct alloc_bucket *s, size_t size)
{
    void *ret;
    size = ROUNDUP(size);

    while (s->next)
	s = s->next;

    if ((char *)s->p + size > (char *)s->limit) {
	struct alloc_bucket *pb = alloc_bucket(size);
	s->next = pb;
	s = pb;
    }

    ret = s->p;
    s->p = (char *)s->p + size;
    return ret;
}

void * alloc_table(size_t size)
{
    struct alloc_bucket *s = alloc_bucket(size);
    return alloc_for_bucket(s, size);
}

void drop_table(void *table)
{
    struct alloc_bucket *s = ALLOC_BUCKET_FOR(table);
    do {
	struct alloc_bucket *c = s;
	s = c->next;
	cc_free(c);
    } while(s);
}

void * alloc_table_entry(void *table, size_t size)
{
    struct alloc_bucket *s = ALLOC_BUCKET_FOR(table);
    return alloc_for_bucket(s, size);
}


struct string_table {
    struct string_bucket {
	char *str;
	int len;
	struct string_bucket *next;
    } *buckets[1024];
};

const char *stringn(const char *src, int len)
{
    static struct string_table *string_table;
    struct string_bucket *ps;
    register unsigned int hash;
    register unsigned char *p;
    const char *end = src + len;
    
    if (src == NULL || len < 0)
        return NULL;

    if (!string_table)
	string_table = alloc_table(sizeof(struct string_table));
    
    for(hash = 0, p = (unsigned char *)src; *p ; p++)
        hash = 31 * hash + *p;
    
    hash %= ARRAY_SIZE(string_table->buckets) - 1;
    for (ps = string_table->buckets[hash]; ps; ps = ps->next) {
        if (ps->len == len) {
            const char *s1 = src;
            char *s2 = ps->str;
            do {
                if (s1 == end)
                    return ps->str;
            } while (*s1++ == *s2++);
        }
    }
    
    // alloc
    {
	char *dst = alloc_table_entry(string_table, len+1);
	ps = alloc_table_entry(string_table, sizeof(struct string_bucket));
        ps->len = len;
        for (ps->str = dst; src < end; )
            *dst++ = *src++;
        *dst++ = 0;
        ps->next = string_table->buckets[hash];
        string_table->buckets[hash] = ps;

        return ps->str;
    }
}

const char *strings(const char *str)
{
    const char *s = str;
    while (*s)
        s++;
    return stringn(str, s - str);
}

const char *stringd(long n)
{
    char str[25], *s = str + sizeof (str);
    unsigned long m;
    
    if (n == LONG_MIN)
	m = (unsigned long)LONG_MAX + 1;
    else if (n < 0)
	m = -n;
    else
	m = n;
    
    do {
	*--s = m%10 + '0';
    } while ((m /= 10) != 0);
    
    if (n < 0)
	*--s = '-';
    
    return stringn(s, str + sizeof (str) - s);
}
