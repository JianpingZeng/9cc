#include "cc.h"

#define RESERVED_SIZE 8192

struct bucket_info {
    void *p;			// free position
    void *limit;		// end position
    struct bucket_info *next;	// next bucket
};

#define BUCKET_INFO(table)  ((struct bucket_info *)table - 1)

static void * alloc_bucket(size_t size)
{
    struct bucket_info *pb;
    
    pb = cc_malloc(sizeof(struct bucket_info) + size);
    pb->p = pb + 1;
    pb->limit = (char *)pb->p + size;
    return pb;
}

static void free_bucket(struct bucket_info *s)
{
    while (s) {
	struct bucket_info *c = s;
	s = c->next;
	cc_free(c);
    }
}

static void *table_alloc_bucket(size_t size)
{
    return alloc_bucket(size + RESERVED_SIZE);
}

static void *node_alloc_bucket(size_t size)
{
    return alloc_bucket(size * RESERVED_SIZE);
}

static inline void * alloc_for_size(struct bucket_info *s, size_t size, void *(alloc_bucket_func)(size_t size))
{
    void *ret;

    while (s->next)
	s = s->next;

    if ((char *)s->p + size > (char *)s->limit) {
	struct bucket_info *pb = alloc_bucket_func(size);
	s->next = pb;
	s = pb;
    }

    ret = s->p;
    s->p = (char *)s->p + size;
    return ret;
}

void * alloc_table(size_t size)
{
    struct bucket_info *s = table_alloc_bucket(size);
    return alloc_for_size(s, size, table_alloc_bucket);
}

void drop_table(void *table)
{
    struct bucket_info *s = BUCKET_INFO(table);
    free_bucket(s);
}

void * alloc_table_entry(void *table, size_t size)
{
    struct bucket_info *s = BUCKET_INFO(table);
    return alloc_for_size(s, size, table_alloc_bucket);
}

// alloc nodes
static void * alloc_node(struct bucket_info **s, size_t size)
{
    if (!*s)
	*s = node_alloc_bucket(size);
    return alloc_for_size(*s, size, node_alloc_bucket);
}

static struct bucket_info *node_info;
void * alloc_node_node()
{
    void *ret = alloc_node(&node_info, sizeof(struct node));
    return ret;
}

static struct bucket_info *expr_info;
void * alloc_expr_node()
{
    void *ret = alloc_node(&expr_info, sizeof(struct expr));
    return ret;
}

static struct bucket_info *stmt_info;
void * alloc_stmt_node()
{
    void *ret = alloc_node(&stmt_info, sizeof(struct stmt));
    return ret;
}

static struct bucket_info *decl_info;
void * alloc_decl_node()
{
    void *ret = alloc_node(&decl_info, sizeof(struct decl));
    return ret;
}

static struct bucket_info *type_info;
void * alloc_type_node()
{
    void *ret = alloc_node(&type_info, sizeof(struct type));
    return ret;
}

static struct bucket_info *symbol_info;
void * alloc_symbol_node()
{
    void *ret = alloc_node(&symbol_info, sizeof(struct symbol));
    return ret;
}

struct string_table {
    struct string_bucket {
	char *str;
	int len;
	struct string_bucket *next;
    } *buckets[1024];
};

static struct string_table *string_table;
const char *stringn(const char *src, int len)
{
    struct string_bucket *ps;
    register unsigned int hash;
    register unsigned char *p;
    const char *end = src + len;
    
    if (src == NULL || len <= 0)
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
    if (!str)
	return NULL;
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

// for debug
static void print_bucket(struct bucket_info *s, const char *name)
{
    int nr = 0;
    fprintf(stderr, "%s: ", name);
    while (s) {
	nr++;
	s = s->next;
    }
    fprintf(stderr, "%d buckets allocated.\n", nr);
}

static void print_table(void *table, const char *name)
{
    struct bucket_info *s = BUCKET_INFO(table);
    print_bucket(s, name);
}

void print_alloc_info()
{
    print_bucket(node_info, "node_info");
    print_bucket(expr_info, "expr_info");
    print_bucket(stmt_info, "stmt_info");
    print_bucket(decl_info, "decl_info");
    print_bucket(type_info, "type_info");
    print_bucket(symbol_info, "symbol_info");
    print_table(string_table, "string_table");
}

/**
 * free all allocated memory
 */
void free_cc()
{
    free_bucket(node_info);
    free_bucket(expr_info);
    free_bucket(stmt_info);
    free_bucket(decl_info);
    free_bucket(type_info);
    free_bucket(symbol_info);
    drop_table(string_table);
}
