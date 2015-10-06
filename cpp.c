#include "cc.h"

/* ACKNOWLEDGE
 *
 * Base on Dave Prosser's algorithm describe
 * in this doc: https://github.com/rui314/8cc/wiki/cpp.algo.pdf
 *
 * Also thanks to Rui Ueyama, from whom I found
 * this document.
 */

static struct token * read_expand(void);

static struct map *macros;
struct source source;
static struct vector *putbacks;

// macro kind
enum {
    MACRO_OBJ,
    MACRO_FUNC,
    MACRO_SPECIAL
};

struct macro {
    int kind;
    struct vector *body;
    struct vector *params;
    bool vararg;
};

static struct macro * new_macro(int kind)
{
    struct macro *m = zmalloc(sizeof (struct macro));
    m->kind = kind;
    m->body = vec_new();
    return m;
}

static inline void putback1(struct token *t)
{
    vec_push(putbacks, t);
}

static inline void putbackv(struct vector *v)
{
    for (int i = vec_len(v)-1; i >= 0; i--)
	vec_push(putbacks, vec_at(v, i));
}

static inline struct token * skip_spaces(void)
{
    struct token *t;
 beg:
    if (vec_len(putbacks))
	t = vec_pop(putbacks);
    else
	t = lex();
    if (t->kind == TSPACE)
	goto beg;
    return t;
}

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

static void define_obj_macro(const char *name)
{
    struct macro *m = new_macro(MACRO_OBJ);
    for (;;) {
	struct token *tok = skip_spaces();
	if (tok->kind == TNEWLINE)
	    break;
	vec_push(m->body, tok);
    }
    map_put(macros, name, m);
}

static void define_funclike_macro(const char *name)
{
}

static struct token * read_identifier(void)
{
     struct token *id = skip_spaces();
     if (id->kind != TIDENTIFIER) {
	 error("expect identifier");
	 return NULL;
     }
     return id;
}

static void define_line(void)
{
    struct token *id = read_identifier();
    if (id == NULL) {
	skipline(true);
	return;
    }
    struct token *t = lex();
    if (t->id == '(') {
	define_funclike_macro(id->name);
    } else {
	putback1(t);
	define_obj_macro(id->name);
    }
}

static void undef_line(void)
{
    struct token *id = read_identifier();
    if (id == NULL) {
	skipline(true);
	return;
    }
    map_put(macros, id->name, NULL);
    id = skip_spaces();
    if (id->kind != TNEWLINE)
	error("extra tokens");
    skipline(true);
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
    struct token *t = skip_spaces();
    if (t->kind == TNEWLINE)
	return;
    if (t->kind == TNUMBER) {
	line_line();
	return;
    }
    if (t->kind != TIDENTIFIER) {
	skipline(true);
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
    else skipline(true);
}

static struct vector * hsadd(struct vector *r, struct set *hideset)
{
    for (int i = 0; i < vec_len(r); i++) {
	struct token *t = vec_at(r, i);
	t->hideset = set_union(t->hideset, hideset);
    }
    return r;
}

static struct vector * subst(struct macro *m, struct vector *args, struct set *hideset)
{
    struct vector *r = vec_new();
    for (int i = 0; i < vec_len(m->body); i++) {
	struct token *t = vec_at(m->body, i);

	vec_push(r, t);
    }
    return hsadd(r, hideset);
}

static struct token * do_read_expand(void)
{
    struct token *t = skip_spaces();
    if (t->kind != TIDENTIFIER)
	return t;

    const char *name = t->name;
    struct macro *m = map_get(macros, name);
    if (m == NULL || set_has(t->hideset, name))
	return t;

    switch (m->kind) {
    case MACRO_OBJ:
	{
	    struct set *hdset = set_add(t->hideset, name);
	    struct vector *v = subst(m, NULL, hdset);
	    putbackv(v);
	    return read_expand();
	}
    case MACRO_FUNC:
	break;
    case MACRO_SPECIAL:
	break;
    default:
	die("internal error: unknown macro type: %d", m->kind);
    }
}

static struct token * read_expand(void)
{
    return do_read_expand();
}

static struct token * conv2cc(struct token *t)
{
    return t;
}

struct token * get_pptok(void)
{
    for (;;) {
        struct token *t = read_expand();
	if (t->id == '#') {
	    read_directive();
	    continue;
	}
	return conv2cc(t);
    }
}

static void parseopts(struct vector *options)
{
}

void cpp_init(struct vector *options)
{
    macros = map_new(nocmp);
    putbacks = vec_new();
    parseopts(options);
}
