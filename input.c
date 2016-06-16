#include "cc.h"

struct file *cpp_file;

static struct buffer *new_buffer(void)
{
    struct buffer *pb = zmalloc(sizeof(struct buffer));
    pb->bol = true;
    pb->need_line = true;
    pb->line = 1;
    pb->column = 0;
    pb->ifstubs = vec_new();
    pb->ungets = vec_new();
    return pb;
}

static void close_buffer(struct buffer *pb)
{
    free((void *)pb->buf);
    free(pb);
}

struct buffer *with_file(const char *file, const char *name)
{
    struct buffer *pb = new_buffer();
    pb->kind = BK_REGULAR;
    pb->file = file;
    pb->name = name;
    
    FILE *fp = fopen(file, "r");
    if (fp == NULL)
        die("%s: %s", file, strerror(errno));
    // read the content
    fseek(fp, 0, SEEK_END);
    long size = ftell(fp);
    fseek(fp, 0, SEEK_SET);
    pb->buf = xmalloc(size + 1);

    unsigned char *d = (unsigned char *)pb->buf;
    if (fread(d, size, 1, fp) != 1)
        die("%s: %s", file, strerror(errno));
    fclose(fp);

    pb->cur = pb->line_base = pb->next_line = pb->buf;
    /**
     * Add a newline character to the end if the
     * file doesn't have one, thus the include
     * directive would work well.
     */
    d[size] = '\n';
    pb->limit = &pb->buf[size];
    return pb;
}

struct buffer *with_string(const char *input, const char *name)
{
    /**
     * NOTE:
     * If it's a temp buffer:
     * _NOT_ add newline at the end
     * Otherwise the buffer will generate an
     * additional newline when expanding a macro.
     */
    struct buffer *pb = new_buffer();
    size_t len = strlen(input);
    pb->kind = BK_STRING;
    pb->name = name ? name : "<anonymous-string>";
    pb->buf = (const unsigned char *)xstrdup(input);
    pb->cur = pb->line_base = pb->next_line = pb->buf;
    unsigned char *d = (unsigned char *)pb->buf;
    d[len] = '\n';
    pb->limit = &pb->buf[len];
    return pb;
}

struct buffer *with_tokens(struct vector *v, struct buffer *cur)
{
    struct buffer *pb = new_buffer();
    pb->kind = BK_TOKEN;
    pb->name = cur->name;
    pb->line = cur->line;
    pb->column = cur->column;
    pb->need_line = false;
    vec_add(pb->ungets, v);
    return pb;
}

void buffer_sentinel(struct file *pfile, struct buffer *pb,
                   enum buffer_sentinel_option opt)
{
    if (opt == BS_STUB)
        pb->stub = true;
    vec_push(pfile->buffers, pb);
    pfile->current = pb;
}

void buffer_unsentinel(struct file *pfile)
{
    struct buffer *pb = vec_pop(pfile->buffers);
    close_buffer(pb);
    pfile->current = vec_tail(pfile->buffers);
    // reset current 'bol'
    if (pfile->current)
        pfile->current->bol = true;
}

void if_sentinel(struct file *pfile, struct ifstub *i)
{
    struct ifstub *ic = zmalloc(sizeof(struct ifstub));
    memcpy(ic, i, sizeof(struct ifstub));
    vec_push(pfile->current->ifstubs, ic);
}

void if_unsentinel(struct file *pfile)
{
    vec_pop(pfile->current->ifstubs);
}

struct ifstub *current_ifstub(struct file *pfile)
{
    return vec_tail(pfile->current->ifstubs);
}

bool is_original_file(struct file *pfile, const char *file)
{
    if (!strcmp(pfile->file, file))
        return true;
    else
        return false;
}

static struct file *new_file(const char *file)
{
    struct file *pfile = zmalloc(sizeof(struct file));
    pfile->file = file;
    pfile->buffers = vec_new();
    pfile->tokens = vec_new();
    pfile->macros = map_new();
    pfile->std_include_paths = vec_new();
    pfile->usr_include_paths = vec_new();
    return pfile;
}

void input_init(const char *file)
{
    cpp_file = new_file(file);
    buffer_sentinel(cpp_file, with_file(file, file), BS_NONE);
}
