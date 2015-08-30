#include "test.h"
#include "cc.h"
#include "sys.h"
#include <ctype.h>

static const char * write_str(const char *str)
{
    struct strbuf *s = strbuf_new();
    const char *tmpdir = mktmpdir();
    FILE *fp;
    size_t len;

    strbuf_cats(s, tmpdir);
    strbuf_cats(s, "/1.c");
    if (!(fp = fopen(s->str, "w")))
	fail("Can't open file");

    len = fwrite(str, strlen(str), 1, fp);
    if (len != 1)
	fail("Can't write file");

    fclose(fp);
    return s->str;
}

static union node * compile(const char *code)
{
    union node *n;
    const char *ifile = write_str(code);
    FILE *fp = freopen(ifile, "r", stdin);
    if (fp == NULL)
        fail("Can't open input file");

    input_init();
    type_init();
    gettok();
    n = translation_unit();
    fclose(fp);
    if (errors)
	fail("Compile error");
    return n;
}

static int getlen(const char **src)
{
    
}

static void expect1(const char *code, const char *output)
{
    union node *n = compile(code);
    union node *n1 = DECL_EXTS(n)[0];
    struct type *ty = DECL_SYM(n1)->type;
    const char *ret = type2s(ty);
    for (;;) {
	int len;
	if (*code == '\0' || *output == '\0')
	    break;
	if (isspace((unsigned char)*code))
	    code++;
	len = getlen(&code);
	
	if (isspace((unsigned char)*output))
	    output++;

	if (strncmp(code, output, len))
	    fail(format("%c != %c", *code, *output));
	code += len;
	output += len;
    }

    if (*code != '\0' || *output != '\0')
	fail(format("%c != %c", *code, *output));
}

static void test_type2s()
{
    expect1("int (* (f (char *))) (float);", "int (* ((char *))) (float)");
}

const char *testname()
{
    return "type2s";
}

void testmain()
{
    test_type2s();
}
