#include "cc.h"

static struct map *defines;
struct source source;

static void parseopts(struct vector *options)
{

}

static struct token * pptok(void)
{
    struct token *tok = lex();
    switch (tok->kind) {
    case TSPACE:
	break;
    case TNEWLINE:
	break;
    case TPUNCTUATOR:
	break;
    case TIDENTIFIER:
	break;
    case TCONSTANT:
	break;
    default:
	break;
    }
}

void cpp_init(struct vector *options)
{
    defines = map_new(nocmp);
    parseopts(options);
}
