#include "internal.h"

static const char *tmpdir;

static const char * write_str(const char *str)
{
    struct strbuf *s = strbuf_new();
    FILE *fp;
    size_t len;

    tmpdir = mktmpdir();
    if (tmpdir == NULL)
	fail("Can't mktmpdir");
    
    strbuf_cats(s, join(tmpdir, "1.c"));
    if (!(fp = fopen(s->str, "w")))
        fail("Can't open file");
    
    len = fwrite(str, strlen(str), 1, fp);
    if (len != 1)
        fail("Can't write file");
    
    fclose(fp);
    return s->str;
}

node_t * compile(const char *code)
{
    node_t *n;
    const char *ifile = write_str(code);
    FILE *fp = freopen(ifile, "r", stdin);
    if (fp == NULL)
        fail("Can't open input file");

    input_init();
    type_init();
    symbol_init();
    n = translation_unit();
    fclose(fp);
    rmdir(tmpdir);
    if (errors)
        fail("Compile error:\n" RED("%s"), code);
    return n;
}

const char * gcc_compile(const char *code)
{
    const char *ifile = write_str(code);
    const char *ofile = join(tmpdir, "a.out");
    const char *argv[] = { "/usr/bin/gcc", ifile, "-o", ofile, NULL};
    callsys(argv[0], (char **)argv);
    if (!file_exists(ofile))
	fail("gcc compile failed:\n" RED("%s"), code);

    const char *rfile = join(tmpdir, "res");
    const char *cmd = format("%s > %s", ofile, rfile);
    const char *argv2[] = { "/bin/sh", "-c", cmd,  NULL };
    callsys(argv2[0], (char **)argv2);
    if (!file_exists(rfile))
	fail("run binary failed");

    FILE *fp = fopen(rfile, "r");
    if (fp == NULL)
	fail("Cannot open result file");
    int size = file_size(rfile);
    if (size < 0)
	fail("Cannot get file size");
    if (size == 0)
	return NULL;

    char *buf = malloc(size+1);
    if (fread(buf, 1, size, fp) != size)
	fail("Cannot read file");
    fclose(fp);
    buf[size] = 0;
    
    rmdir(tmpdir);
    return buf;
}
