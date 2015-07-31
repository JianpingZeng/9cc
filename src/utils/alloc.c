#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include <limits.h>

/**
 * Add alignment to make the compiler happy.
 */
#define ALIGN_SIZE          sizeof(long double)
#define ROUNDUP(x)          (((x)+((ALIGN_SIZE)-1))&(~((ALIGN_SIZE)-1)))
#define HEAD_SIZE           ROUNDUP(sizeof(struct bucket_info))
#define BUCKET_INFO(table)  ((struct bucket_info *)((char *)table - HEAD_SIZE))
#define RESERVED_SIZE       8192

struct bucket_info {
    void *p;			// free position
    void *limit;		// end position
    struct bucket_info *next;	// next bucket
};

static struct bucket_info *first_bucket;
static struct bucket_info *current_bucket;

static void * cc_malloc(size_t size)
{
    void *p = malloc(size);
    if (!p) {
        fprintf(stderr, "Can't malloc\n");
        exit(EXIT_FAILURE);
    }
    memset(p, 0, size);
    return p;
}

static void cc_free(void *p)
{
    free(p);
}

static void * new_bucket(size_t size)
{
    struct bucket_info *pb;
    size = ROUNDUP(size + RESERVED_SIZE);
    
    pb = cc_malloc(HEAD_SIZE + size);
    pb->p = (char *)pb + HEAD_SIZE;
    pb->limit = (char *)pb->p + size;
    return pb;
}

void * cc_alloc(size_t size)
{
    void *ret;
    
    if (!first_bucket)
        current_bucket = first_bucket = new_bucket(size);
    
    size = ROUNDUP(size);
    if ((char *)current_bucket->p + size > (char *)current_bucket->limit) {
        struct bucket_info *pb = new_bucket(size);
        current_bucket->next = pb;
        current_bucket = pb;
    }
    
    ret = current_bucket->p;
    current_bucket->p = (char *)current_bucket->p + size;
    return ret;
}

void cc_drain(void)
{
    for (struct bucket_info *s=first_bucket; s;) {
        struct bucket_info *n = s->next;
        cc_free(s);
        s = n;
    }
    first_bucket = NULL;
    current_bucket = NULL;
}
