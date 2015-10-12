#include "cc.h"

static struct vector *files;

static bool file_eof(struct file *fs)
{
    if (fs->kind == FILE_KIND_REGULAR)
	return feof(fs->fp);
    else
	return fs->pos == strlen(fs->file);
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
	if (fs->pe > fs->pc && fs->pe[-1] != '\n')
	    *fs->pe++ = '\n';
    }
    *fs->pe = 0;
}

static int get(void)
{
    struct file *fs = current_file();
    if (fs->pe - fs->pc < MAXTOKEN)
	fillbuf(fs);
    if (fs->pc == fs->pe)
	return EOI;
    if (*fs->pc == '\n') {
	fs->line++;
	fs->column = 0;
    } else {
	fs->column++;
    }
    return *fs->pc++;
}

static inline struct cc_char * newch(int c, unsigned line, unsigned column)
{
    struct cc_char *ch =  zmalloc(sizeof(struct cc_char));
    ch->ch = c;
    ch->line = line;
    ch->column = column;
    return ch;
}

void unreadc(struct cc_char * ch)
{
    vec_push(current_file()->chars, ch);
}

struct cc_char * readc(void)
{
    struct file *fs = current_file();
    
    if (vec_len(fs->chars))
	return vec_pop(fs->chars);
    
    for (;;) {
        int c = get();
	unsigned line = fs->line;
	unsigned column = fs->column;
	if (c == EOI || c != '\\')
	    goto end;
	int c2 = get();
	if (c2 == '\n')
	    continue;
	// cache
	struct cc_char *ch2 = newch(c2, fs->line, fs->column);
	unreadc(ch2);
    end:
	return newch(c, line, column);
    }
}

static struct file * new_file(int kind)
{
    struct file *fs = xmalloc(sizeof(struct file));
    fs->kind = kind;
    fs->pc = fs->pe = &fs->buf[LBUFSIZE];
    fs->bread = -1;
    fs->chars = vec_new();
    fs->line = 1;
    fs->column = 0;
    fs->fp = NULL;
    fs->file = NULL;
    fs->pos = 0;
    return fs;
}

struct file * current_file(void)
{
    return vec_tail(files);
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
    }
    fs->file = file;
    
    return fs;
}

static void close_file(struct file *file)
{
    if (file->kind == FILE_KIND_REGULAR) {
	fclose(file->fp);
	struct file *fs = current_file();
	genlineno(fs->line, fs->file);
    }
    vec_free(file->chars);
    free(file);
}

void file_stub(struct file *f)
{
    vec_push(files, f);
}

void file_unstub(void)
{
    close_file(vec_pop(files));
}

struct file * with_temp_string(const char *input, const char *name)
{
    struct file *fs = open_file(FILE_KIND_STRING, input);
    fs->name = name ? name : "<temp>";
    return fs;
}

struct file * with_temp_file(const char *file, const char *name)
{
    struct file *fs = open_file(FILE_KIND_REGULAR, file);
    fs->name = name ? name : fs->file;
    genlineno(1, fs->name);
    return fs;
}

void input_init(const char *file)
{
    files = vec_new();
    vec_push(files, open_file(FILE_KIND_REGULAR, file));
    genlineno(1, file);
}
