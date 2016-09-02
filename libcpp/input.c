#include "cpp.h"

struct file *cpp_file;

static struct buffer *new_buffer(void)
{
    struct buffer *pb = zmalloc(sizeof(struct buffer));
    pb->bol = true;
    pb->need_line = true;
    pb->line = 1;
    pb->column = 0;
    pb->ungets = vec_new();
    return pb;
}

static void free_buffer(struct buffer *pb)
{
    free((void *)pb->buf);
    free(pb);
}

struct buffer *with_stdin(const char *file)
{
    FILE *fp = stdin;
    struct buffer *pb = new_buffer();
    pb->kind = BK_REGULAR;
    pb->name = file;

    // read the content
    fseek(fp, 0, SEEK_END);
    long size = ftell(fp);
    fseek(fp, 0, SEEK_SET);
    char *d = xmalloc(size + 1);
    if (fread(d, size, 1, fp) != 1)
        die("Can't read file: %s", file);
    fclose(fp);

    /**
     * Add a newline character to the end if the
     * file doesn't have one, thus the include
     * directive would work well.
     */
    d[size] = '\n';

    pb->buf = (const unsigned char *)d;
    pb->cur = pb->line_base = pb->next_line = pb->buf;
    pb->limit = &pb->buf[size];
    return pb;
}

struct buffer *with_file(const char *file)
{
    struct buffer *pb = new_buffer();
    pb->kind = BK_REGULAR;
    pb->name = file;

    //NOTE: must be binary in Windows
    FILE *fp = fopen(file, "rb");
    if (fp == NULL)
        die("Can't open file: %s", file);
    // read the content
    long size = file_size(file);
    char *d = xmalloc(size + 1);
    if (fread(d, size, 1, fp) != 1)
        die("Can't read file: %s", file);
    fclose(fp);

    /**
     * Add a newline character to the end if the
     * file doesn't have one, thus the include
     * directive would work well.
     */
    d[size] = '\n';

    pb->buf = (const unsigned char *)d;
    pb->cur = pb->line_base = pb->next_line = pb->buf;
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
    pb->buf = xmalloc(len + 1);
    char *d = (char *)pb->buf;
    memcpy(d, input, len);
    d[len] = '\n';
    pb->cur = pb->line_base = pb->next_line = pb->buf;
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
    if (opt == BS_RETURN_EOI)
        pb->return_eoi = true;
    pb->prev = pfile->buffer;
    pfile->buffer = pb;
}

void buffer_unsentinel(struct file *pfile)
{
    struct buffer *prev = pfile->buffer->prev;
    free_buffer(pfile->buffer);
    pfile->buffer = prev;
    // reset current 'bol'
    if (pfile->buffer)
        pfile->buffer->bol = true;
}

void if_sentinel(struct file *pfile, struct ifstack *i)
{
    struct ifstack *ic = xmalloc(sizeof(struct ifstack));
    memcpy(ic, i, sizeof(struct ifstack));
    ic->prev = pfile->buffer->ifstack;
    pfile->buffer->ifstack = ic;
}

void if_unsentinel(struct file *pfile)
{
    struct ifstack *prev = pfile->buffer->ifstack->prev;
    pfile->buffer->ifstack = prev;
}

bool is_original_file(struct file *pfile, const char *file)
{
    if (!strcmp(pfile->file, file))
        return true;
    else
        return false;
}

static inline struct ident *alloc_cpp_ident_entry(struct imap *imap)
{
    return NEWS(struct cpp_ident, PERM);
}

struct file *new_cpp_file(const char *file)
{
    struct file *pfile = zmalloc(sizeof(struct file));
    pfile->file = file;
    pfile->tokens = vec_new();
    pfile->std_include_paths = vec_new();
    pfile->usr_include_paths = vec_new();
    // 2^13: 8k slots
    pfile->imap = imap_new(13);
    pfile->imap->alloc_entry = alloc_cpp_ident_entry;
    // tokenrun
    pfile->tokenrun = next_tokenrun(NULL, 1024);
    pfile->cur_token = pfile->tokenrun->base;
    return pfile;
}
