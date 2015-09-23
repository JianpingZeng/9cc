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

static void cleanup(void)
{
    identifiers = zmalloc(sizeof(struct table));
    identifiers->scope = GLOBAL;
    identifiers->map = map_new(nocmp);

    constants = zmalloc(sizeof(struct table));
    constants->scope = CONSTANT;
    constants->map = map_new(nocmp);

    tags = zmalloc(sizeof(struct table));
    tags->scope = GLOBAL;
    tags->map = map_new(nocmp);
}

node_t * compile(const char *code)
{
    node_t *n;
    const char *ifile = write_str(code);
    FILE *fp = freopen(ifile, "r", stdin);
    if (fp == NULL)
        fail("Can't open input file");

    cleanup();
    lex_init();
    type_init();
    n = translation_unit();
    fclose(fp);
    rmdir(tmpdir);
    if (errors)
        fail("Compile error");
    return n;
}
