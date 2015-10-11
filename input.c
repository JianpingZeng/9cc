#include "cc.h"

static struct vector *files;
static struct vector *fstubs;

static void open_file(const char *file);
static void close_file(struct file *file);

static bool file_eof(struct file *fs)
{
    if (fs->kind == FILE_KIND_REGULAR)
	return feof(fs->u.fp);
    else
	return fs->u.s.pos == strlen(fs->u.s.input);
}

static size_t file_read(void *ptr, size_t size, size_t nitems, struct file *fs)
{
    if (fs->kind == FILE_KIND_REGULAR) {
	return fread(ptr, size, nitems, fs->u.fp);
    } else {
	size_t reqs = size * nitems;
	size_t left = strlen(fs->u.s.input) - fs->u.s.pos;
	size_t bytes = MIN(reqs, left);
	strncpy(ptr, fs->u.s.input + fs->u.s.pos, bytes);
	fs->u.s.pos += bytes;
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

static char get(void)
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

static inline struct cc_char * newch(char c, unsigned line, unsigned column)
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

static void unreads(const char *s)
{
    for (int i = strlen(s) - 1; i >= 0; i--) {
	struct cc_char *ch = newch(s[i], 0, i+1);
	unreadc(ch);
    }
}

struct cc_char * readc(void)
{
    struct file *fs;

beg:
    fs = current_file();
    if (vec_len(fs->chars))
	return vec_pop(fs->chars);
    
    for (;;) {
        char c = get();
	unsigned line = fs->line;
	unsigned column = fs->column;
	if (c == EOI) {
	    if (vec_len(files) == 1) {
		goto end;
	    } else {
		close_file(vec_pop(files));
		goto beg;
	    }
	}
	if (c != '\\')
	    goto end;
	char c2 = get();
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
    fs->u.fp = NULL;
    fs->u.s.input = NULL;
    fs->u.s.pos = 0;
    return fs;
}

static struct file * do_open_file(int kind, const char *file)
{
    struct file *fs = new_file(kind);
    if (kind == FILE_KIND_REGULAR) {
	FILE *fp = fopen(file, "r");
	if (fp == NULL) {
	    perror(file);
	    die("Cannot open file %s", file);
	}
	fs->u.fp = fp;
	fs->file = file;
    } else {
	fs->u.s.input = file;
    }
    
    return fs;
}

static void genlineno(unsigned line, const char *file)
{
    unreads(format("# %u \"%s\"\n", line, file));
}

struct file * current_file(void)
{
    return vec_tail(files);
}

static void open_file(const char *file)
{
    struct file *fs = do_open_file(FILE_KIND_REGULAR, file);
    vec_push(files, fs);
    genlineno(1, file);
}

static void close_file(struct file *file)
{
    println("close %s", file->file);
    if (file->kind == FILE_KIND_REGULAR) {
	fclose(file->u.fp);
	struct file *fs = current_file();
	genlineno(fs->line, fs->file);
    }
    vec_free(file->chars);
    free(file);
}

void include_file(const char *file)
{
    open_file(file);
}

void file_stub(struct file *f)
{
    vec_push(fstubs, files);
    struct vector *v = vec_new();
    vec_push(v, f);
    files = v;
}

void file_unstub(void)
{
    files = vec_pop(fstubs);
}

struct file * with_temp_string(const char *input, const char *name)
{
    struct file *fs = do_open_file(FILE_KIND_STRING, input);
    fs->file = name ? name : "<temp>";
    return fs;
}

struct file * with_temp_file(const char *file, const char *name)
{
    struct file *fs = do_open_file(FILE_KIND_REGULAR, file);
    fs->file = name ? name : fs->file;
    genlineno(1, fs->file);
    return fs;
}

void input_init(const char *file)
{
    fstubs = vec_new();
    files = vec_new();
    open_file(file);
}
