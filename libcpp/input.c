#include "compat.h"
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <limits.h>
#include "internal.h"
#include "libutils.h"

struct file *cpp_file;

static struct buffer *new_buffer(void)
{
    struct buffer *pb = zmalloc(sizeof(struct buffer));
    pb->bol = true;
    pb->need_line = true;
    pb->line = 1;
    pb->column = 1;
    pb->ungets = vec_new();
    return pb;
}

static void free_buffer(struct buffer *pb)
{
    free((void *)pb->buf);
    free(pb);
}

struct buffer *with_file(const char *file, const char *name)
{
    int fd;
    struct buffer *pb;
    struct stat st;
    bool regular;
    ssize_t size, total, count;
    char *buf;

    if (file[0] == '\0')
        fd = 0;
    else
        fd = open(file, O_RDONLY | O_NOCTTY, 0666);

    if (fd == -1)
        die("Can't open file: %s (%s)", file, strerror(errno));

    if (fstat(fd, &st) == -1)
        die("Can't get file stat: %s (%s)", file, strerror(errno));

    if (S_ISDIR(st.st_mode))
        die("%s is a directory", file);

    if (S_ISBLK(st.st_mode))
        die("%s is a block device", file);
    
    regular = S_ISREG(st.st_mode) != 0;
    if (regular)
        size = st.st_size;
    else
        // 8k
        size = 8 * 1024;

    buf = xmalloc(size + 1);
    total = 0;
    while ((count = read(fd, buf + total, size - total)) > 0) {
        total += count;

        if (total == size) {
            if (regular)
                break;
            size *= 2;
            buf = xrealloc(buf, size + 1);
        }
    }

    if (count < 0)
        die("Can't read file: %s (%s)", file, strerror(errno));

    close(fd);
    pb = new_buffer();
    pb->kind = BK_REGULAR;
    pb->name = file;
    
    /**
     * Add a newline character to the end if the
     * file doesn't have one, thus the include
     * directive would work well.
     */
    buf[total] = '\n';
    
    pb->buf = (const unsigned char *)buf;
    pb->cur = pb->line_base = pb->next_line = pb->buf;
    pb->limit = &pb->buf[total];
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

static inline struct ident *alloc_cpp_ident(struct idtab *t)
{
    return NEWS0(struct ident, PERM);
}

struct file *input_init(const char *file)
{
    struct file *pfile = zmalloc(sizeof(struct file));
    pfile->file = file;
    pfile->tokens = vec_new();
    pfile->std_include_paths = vec_new();
    pfile->usr_include_paths = vec_new();
    // 2^13: 8k slots
    pfile->idtab = idtab_new(13);
    pfile->idtab->alloc_ident = alloc_cpp_ident;
    // tokenrun
    pfile->tokenrun = next_tokenrun(NULL, 1024);
    pfile->cur_token = pfile->tokenrun->base;

    buffer_sentinel(pfile, with_file(file, file), BS_CONTINUOUS);
    return pfile;
}

void cpp_dump(struct file *pfile)
{
    idtab_dump(pfile->idtab);
    strtab_dump();
}
