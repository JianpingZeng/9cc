#include "cc.h"

#define LBUFSIZE     64
#define RBUFSIZE     4096

static struct vector *files;

enum {
    FILE_KIND_REGULAR = 1,
    FILE_KIND_STRING,
};

#define HISTS    (FIELD_SIZEOF(struct file, hists) / sizeof(struct cc_char))
#define NEXT(p)  (((p)+1)%HISTS)
#define PREV(p)  (((p)-1+HISTS)%HISTS)

static bool file_eof(struct file *fs)
{
    if (fs->kind == FILE_KIND_REGULAR)
	return fs->fp ? feof(fs->fp) : true;
    else
	return fs->file ? fs->pos == strlen(fs->file) : true;
}

static size_t file_read(void *ptr, size_t size, size_t nitems, struct file *fs)
{
    if (fs->kind == FILE_KIND_REGULAR) {
	return fread(ptr, size, nitems, fs->fp);
    } else {
	size_t reqs = size * nitems;
	size_t left = strlen(fs->file) - fs->pos;
	size_t bytes = MIN(reqs, left);
	strncpy(ptr, fs->file + fs->pos, bytes);
	fs->pos += bytes;
	return bytes;
    }
}

static void fillbuf(struct file *fs)
{
    if (fs->bread == 0) {
        if (fs->pc > fs->pe)
            fs->pc = fs->pe;
        return;
    }
    
    if (fs->pc >= fs->pe) {
        fs->pc = &fs->buf[LBUFSIZE];
    } else {
        long n;
        char *dst, *src;
        
        // copy
        n = fs->pe - fs->pc;
        dst = &fs->buf[LBUFSIZE] - n;
        src = fs->pc;
        while (src < fs->pe)
            *dst++ = *src++;
        
        fs->pc = &fs->buf[LBUFSIZE] - n;
    }

    if (file_eof(fs))
        fs->bread = 0;
    else
        fs->bread = file_read(&fs->buf[LBUFSIZE], 1, RBUFSIZE, fs);
    
    if (fs->bread < 0)
        die("read error: %s", strerror(errno));
    
    fs->pe = &fs->buf[LBUFSIZE] + fs->bread;

    /* Add a newline character to the end if the
     * file doesn't have one, thus the include
     * directive would work well.
     */
    if (fs->pe < &fs->buf[LBUFSIZE+RBUFSIZE]) {
	if (fs->pe == fs->pc || fs->pe[-1] != '\n') {
	    *fs->pe++ = '\n';
	    /**
	     * warning only if it's really a file.
	     */
	    if (fs->kind == FILE_KIND_REGULAR)
		fprintf(stderr, "%s: " PURPLE("warning: ") "No newline at end of file\n", fs->name);
	}
    }
    *fs->pe = 0;
}

static int get(void)
{
    struct file *fs = current_file();
    if (fs->pe - fs->pc < LBUFSIZE)
	fillbuf(fs);
    if (fs->pc == fs->pe)
	return EOI;
    if (*fs->pc == '\n') {
	fs->line++;
	fs->column = 0;
    } else {
	fs->column++;
    }
    // convert to unsigned char first
    return (unsigned char) (*fs->pc++);
}

/**
 * 'histp' points at current history item.
 * 'charp' ponits at next available slot.
 */

static void history(int c, unsigned line, unsigned column)
{
    struct file *fs = current_file();
    fs->histp = NEXT(fs->histp);
    fs->hists[fs->histp] = (struct cc_char)
	{.dirty = true, .ch = c, .line = line, .column = column};
}

static void unwind_history(int c)
{
    struct file *fs = current_file();
    if (fs->hists[fs->histp].ch != c)
	fatal("an unbufferred character '\\0%o'", c);
    if (!fs->hists[PREV(fs->histp)].dirty)
	fatal("unwind history overflow '\\0%o'", c);
    fs->hists[fs->histp].dirty = false;
    fs->histp = PREV(fs->histp);
    fs->line = fs->hists[fs->histp].line;
    fs->column = fs->hists[fs->histp].column;
}

void unreadc(int c)
{
    struct file *fs = current_file();
    if (c == EOI)
	return;
    if (fs->charp >= ARRAY_SIZE(fs->chars))
	fatal("too many unreadc '\\0%o'", c);
    unsigned line = fs->line;
    unsigned column = fs->column;
    unwind_history(c);
    fs->chars[fs->charp++] = (struct cc_char)
	{.ch = c, .line = line, .column = column};
}

int readc(void)
{
    struct file *fs = current_file();
    int c;
    unsigned line, column;

    if (fs->charp) {
	struct cc_char ch = fs->chars[--fs->charp];
	history(ch.ch, ch.line, ch.column);
	fs->line = ch.line;
	fs->column = ch.column;
	return ch.ch;
    }
    
    for (;;) {
        c = get();
	line = fs->line;
	column = fs->column;
    	if (c == EOI || c != '\\') {
	    history(c, line, column);
    	    goto end;
	}
    	int c2 = get();
    	if (c2 == '\n')
    	    continue;
    	// cache
	history(c, line, column);
	history(c2, fs->line, fs->column);
	unreadc(c2);
    end:
	return c;
    }
}

static struct file * new_file(int kind)
{
    struct file *fs = zmalloc(sizeof(struct file));
    fs->kind = kind;
    fs->buf = xmalloc(LBUFSIZE+RBUFSIZE+1);
    fs->pc = fs->pe = &fs->buf[LBUFSIZE];
    fs->bread = -1;
    fs->line = 1;
    fs->column = 0;
    fs->bol = true;
    fs->ifstubs = vec_new();
    fs->buffer = vec_new();
    fs->tokens = vec_new();
    fs->hists[0] = (struct cc_char)
	    {.dirty = true, .ch = EOI, .line = fs->line, .column = fs->column};
    return fs;
}

static struct file * open_file(int kind, const char *file)
{
    struct file *fs = new_file(kind);
    if (kind == FILE_KIND_REGULAR) {
	FILE *fp = fopen(file, "r");
	if (fp == NULL) {
	    perror(file);
	    die("Cannot open file %s", file);
	}
	fs->fp = fp;
	fs->name = file;
    }
    fs->file = file;
    
    return fs;
}

static void close_file(struct file *fs)
{
    if (fs->kind == FILE_KIND_REGULAR)
	fclose(fs->fp);
    free(fs->buf);
    vec_free(fs->ifstubs);
    vec_free(fs->buffer);
    vec_free(fs->tokens);
    free(fs);
    // reset current 'bol'
    if (current_file())
	current_file()->bol = true;
}

struct file * current_file(void)
{
    return vec_tail(files);
}

void file_sentinel(struct file *fs)
{
    vec_push(files, fs);
}

void file_unsentinel(void)
{
    close_file(vec_pop(files));
}

void file_stub(struct file *fs)
{
    fs->stub = true;
    file_sentinel(fs);
}

void file_unstub(void)
{
    file_unsentinel();
}

struct file * with_string(const char *input, const char *name)
{
    struct file *fs = open_file(FILE_KIND_STRING, input);
    fs->name = name ? name : "<anonymous-string>";
    return fs;
}

struct file * with_file(const char *file, const char *name)
{
    struct file *fs = open_file(FILE_KIND_REGULAR, file);
    fs->name = name ? name : "<anonymous-file>";
    return fs;
}

struct file * with_buffer(struct vector *v)
{
    struct file *fs = new_file(FILE_KIND_STRING);
    fs->name = current_file()->name;
    vec_add(fs->buffer, v);
    return fs;
}

struct ifstub * new_ifstub(struct ifstub *i)
{
    struct ifstub *ic = zmalloc(sizeof(struct ifstub));
    memcpy(ic, i, sizeof(struct ifstub));
    return ic;
}

void if_sentinel(struct ifstub *i)
{
    vec_push(current_file()->ifstubs, i);
}

void if_unsentinel(void)
{
    vec_pop(current_file()->ifstubs);
}

struct ifstub * current_ifstub(void)
{
    return vec_tail(current_file()->ifstubs);
}

void input_init(const char *file)
{
    files = vec_new();
    file_sentinel(with_file(file, file));
}
