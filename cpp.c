#include "cc.h"

static struct token * get_pptok(void);

static struct map *defines;
struct source source;
static struct token *pptoken = &(struct token){};

static void if_section(void)
{
}

static void ifdef_section(void)
{
}

static void ifndef_section(void)
{
}

static void include_line(void)
{
}

static void define_line(void)
{
}

static void undef_line(void)
{
}

static void line_line(void)
{
}

static void error_line(void)
{
}

static void pragma_line(void)
{
}

static void warning_line(void)
{
}

static void read_directive(void)
{
    struct token *t = get_pptok();
    if (t->kind == TNEWLINE)
	return;
    if (t->kind == TNUMBER) {
	line_line();
	return;
    }
    if (t->kind != TIDENTIFIER) {
	skipline();
	return;
    }
    if (!strcmp(t->name, "if")) if_section();
    else if (!strcmp(t->name, "ifdef")) ifdef_section();
    else if (!strcmp(t->name, "ifndef")) ifndef_section();
    else if (!strcmp(t->name, "include")) include_line();
    else if (!strcmp(t->name, "define")) define_line();
    else if (!strcmp(t->name, "undef")) undef_line();
    else if (!strcmp(t->name, "line")) line_line();
    else if (!strcmp(t->name, "error")) error_line();
    else if (!strcmp(t->name, "pragma")) pragma_line();
    else if (!strcmp(t->name, "warning")) warning_line();
    else skipline();
}

static struct token * get_pptok(void)
{
beg:
    pptoken = lex();
    if (pptoken->kind == TSPACE)
	goto beg;
    
    static bool linebeg = true;
    for (;;) {
        pptoken = lex();
	switch (tok->kind) {
	case TSPACE:
	    continue;
	case TPUNCTUATOR:
	    if (linebeg && pptoken->id == '#')
	case TNEWLINE:
	case TIDENTIFIER:
	case TCHARCONST:
	case TSTRING:
	default:
	    return pptoken;
	}
    }
}

static void parseopts(struct vector *options)
{
}

void cpp_init(struct vector *options)
{
    defines = map_new(nocmp);
    parseopts(options);
}
