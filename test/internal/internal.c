#include "internal.h"

static const char * write_str(const char *str)
{
    struct strbuf *s = strbuf_new();
    const char *tmpdir = mktmpdir();
    FILE *fp;
    size_t len;
    
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
    n = translation_unit();
    fclose(fp);
    if (errors)
        fail("Compile error");
    return n;
}
