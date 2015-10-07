#include "cc.h"

/* ACKNOWLEDGE
 *
 * Base on Dave Prosser's algorithm describe
 * in this doc: https://github.com/rui314/8cc/wiki/cpp.algo.pdf
 *
 * Also thanks to Rui Ueyama, from whom I found
 * this document.
 */

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
    void (*fn) (struct token *t); // special macro handler
};

static struct token * expand(void);
static struct map *macros;

static struct macro * new_macro(int kind)
{
    struct macro *m = zmalloc(sizeof (struct macro));
    m->kind = kind;
    return m;
}

static inline void ungetv(struct vector *v)
{
    for (int i = vec_len(v)-1; i >= 0; i--)
	unget(vec_at(v, i));
}

static void ensure_macro_def(struct macro *m)
{
    for (int i = 0; i < vec_len(m->body); i++) {
	struct token *t = vec_at(m->body, i);
	if (t->id == SHARPSHARP) {
	    if (i == 0)
		error("'##' cannot appear at the beginning of a replacement list");
	    else if (i == vec_len(m->body) - 1)
		error("'##' cannot appear at the end of a replacement list");
	} else if (t->id == '#' && m->kind != MACRO_FUNC) {
	    error("'#' must be followed by the name of a macro formal parameter");
	}
    }
}

static struct token * skip_spaces(void)
{
    struct token *t;
 beg:
    t = lex();
    if (IS_SPACE(t))
	goto beg;
    return t;
}

static int inparams(struct token *t, struct vector *params)
{
    if (t->id != ID)
	return -1;
    for (int i = 0; i < vec_len(params); i++) {
	struct token *p = vec_at(params, i);
	if (t->name == p->name)
	    return i;
    }
    return -1;
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
    struct vector *body = m->body;
    struct vector *params = m->params;
    for (int i = 0; i < vec_len(body); i++) {
	struct token *t = vec_at(body, i);
	int index = inparams(t, params);
	if (index >= 0) {
	    struct vector *is = vec_at(args, index);
	    
	} else {
	    vec_push(r, t);
	}
    }
    return hsadd(r, hideset);
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

static struct token * read_identifier(void)
{
     struct token *id = skip_spaces();
     if (id->id != ID) {
	 error("expect identifier");
	 return NULL;
     }
     return id;
}

static struct vector * arg(void)
{
    struct vector *v = vec_new();
    int parens = 0;
    struct token *t;
    for (;;) {
	t = skip_spaces();
	if (((t->id == ',' || t->id == ')') && parens == 0) ||
	    t->id == EOI)
	    break;
	if (t->id == '(')
	    parens++;
	else if (t->id == ')')
	    parens--;
	vec_push(v, t);
    }
    if (t->id != ',' && t->id != ')')
	error("invalid arguments");
    return v;
}

static struct vector * arguments(struct macro *m)
{
    struct vector *v = vec_new();
    for (;;) {
	if (token->id == ')' || token->id == EOI)
	    break;
	struct vector *r = arg();
	if (vec_len(r))
	    vec_push(v, r);
    }
    // check args and params
    
    return v;
}

static void parameters(struct macro *m)
{
    struct token *t = skip_spaces();
    if (t->id == ')') {
	// empty
    } else if (t->id == ELLIPSIS) {
	m->vararg = true;
	t = skip_spaces();
	if (t->id != ')')
	    error("expect ')'");
    } else if (t->id == ID) {
	struct vector *v = vec_new();
	for (;;) {
	    if (t->id == ')' || t->id == ELLIPSIS)
		break;
	    vec_push(v, t);
	    t = skip_spaces();
	}
	if (t->id == ELLIPSIS) {
	    m->vararg = true;
	    t = skip_spaces();
	    if (t->id != ')')
		error("expect ')'");
	}
	m->params = v;
    } else {
	error("expect identifier list or ')' or ...");
	skipline(true);
    }
}

static struct vector * replacement_list(void)
{
    struct vector *v = vec_new();
    for (;;) {
	struct token *tok = skip_spaces();
	if (IS_NEWLINE(tok))
	    break;
	vec_push(v, tok);
    }
    return v;
}

static void define_obj_macro(const char *name)
{
    struct macro *m = new_macro(MACRO_OBJ);
    m->body = replacement_list();
    ensure_macro_def(m);
    map_put(macros, name, m);
}

static void define_funclike_macro(const char *name)
{
    struct macro *m = new_macro(MACRO_FUNC);
    parameters(m);
    m->body = replacement_list();
    ensure_macro_def(m);
    map_put(macros, name, m);
}

static void define_line(void)
{
    struct token *id = read_identifier();
    if (id == NULL) {
	skipline(true);
	return;
    }
    struct macro *m = map_get(macros, id->name);
    if (m)
	error("redefinition of macro '%s'", id->name);
    struct token *t = lex();
    if (t->id == '(') {
	define_funclike_macro(id->name);
    } else {
	unget(t);
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
    if (!IS_NEWLINE(id))
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
    if (IS_NEWLINE(t))
	return;
    if (t->id == ICONSTANT) {
	line_line();
	return;
    }
    if (t->id != ID) {
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

static struct token * doexpand(void)
{
    struct token *t = lex();
    if (t->id != ID)
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
	    ungetv(v);
	    return expand();
	}
    case MACRO_FUNC:
	{
	}
	break;
    case MACRO_SPECIAL:
	{
	}
    default:
	break;
    }
}

static struct token * expand(void)
{
    return doexpand();
}

static struct token * conv2cc(struct token *t)
{
    return t;
}

struct token * get_pptok(void)
{
    for (;;) {
        struct token *t = expand();
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
    parseopts(options);
}
