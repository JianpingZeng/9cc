#include "cc.h"

/* ACKNOWLEDGE
 *
 * Base on Dave Prosser's algorithm describe
 * in this doc: https://github.com/rui314/8cc/wiki/cpp.algo.pdf
 *
 * Also thanks to Rui Ueyama, from whom I found
 * this document.
 */

// IS kind
enum {
    IS_INPUT,
    IS_VECTOR,
};

struct IS {
    int kind;
    struct token * (*get) (struct IS *is);
    struct token * (*peek) (struct IS *is);
    bool (*eoi) (struct IS *is);
    void (*add) (struct IS *is, struct vector *v);
    struct token *token;
    struct vector *v;
};

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

static struct token * read_expand(void);
static void expand(struct IS *IS, struct vector *OS);
static struct token *iget(struct IS *is);
static struct token *ipeek(struct IS *is);
static bool ieoi(struct IS *is);
static void iadd(struct IS *is, struct vector *v);
static struct vector * arguments(struct macro *m);

static struct map *macros;
struct source source;
static struct vector *putbacks;
static struct token *pptoken;

static struct IS * inputIS = &(struct IS){
    .kind = IS_INPUT,
    .get = iget,
    .peek = ipeek,
    .eoi = ieoi,
    .add = iadd,
};

static struct IS * new_IS(struct vector *v)
{
    struct IS *is = xmalloc(sizeof (struct IS));
    is->kind = IS_VECTOR;
    is->get = iget;
    is->peek = ipeek;
    is->eoi = ieoi;
    is->add = iadd;
    is->v = v;
    return is;
}

static struct macro * new_macro(int kind)
{
    struct macro *m = zmalloc(sizeof (struct macro));
    m->kind = kind;
    return m;
}

static inline void putback1(struct token *t)
{
    vec_push(putbacks, t);
}

static inline void putbackv(struct vector *v)
{
    vec_addr(putbacks, v);
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

static inline struct token * skip_spaces(void)
{
 beg:
    if (vec_len(putbacks))
	pptoken = vec_pop(putbacks);
    else
	pptoken = lex();
    if (pptoken->kind == TSPACE)
	goto beg;
    return pptoken;
}

static struct token *iget(struct IS *is)
{
    if (is->kind == IS_INPUT) {
	 is->token = skip_spaces();
    } else if (is->kind == IS_VECTOR) {
	if (vec_len(is->v))
	    is->token = vec_pop(is->v);
	else
	    is->token = eoi_token;
    } else {
	die("unkown IS type: %d", is->kind);
    }
    return is->token;
}

static struct token *ipeek(struct IS *is)
{
    struct token *t;
    if (is->kind == IS_INPUT) {
	t = skip_spaces();
	putback1(t);
    } else if (is->kind == IS_VECTOR) {
	if (vec_len(is->v))
	    t = vec_tail(is->v);
	else
	    t = eoi_token;
    } else {
	die("unkown IS type: %d", is->kind);
	t = eoi_token;
    }
    return t;
}

static bool ieoi(struct IS *is)
{
    return is->token->kind == TEOI;
}

static void iadd(struct IS *is, struct vector *v)
{
    if (is->kind == IS_INPUT)
	putbackv(v);
    else if (is->kind == IS_VECTOR)
	vec_addr(is->v, v);
    else
	die("unkown IS type: %d", is->kind);
}

static int inparams(struct token *t, struct vector *params)
{
    if (t->kind != TIDENTIFIER)
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
	    struct vector *os = vec_new();
	    expand(is, os);
	} else {
	    vec_push(r, t);
	}
    }
    return hsadd(r, hideset);
}

static void expand(struct IS *IS, struct vector *OS)
{
    if (IS->eoi(IS))
	return;

    struct token *t = IS->get(IS);
    if (t->kind != TIDENTIFIER)
	goto end;

    const char *name = t->name;
    struct macro *m = map_get(macros, name);
    if (m == NULL || set_has(t->hideset, name))
	goto end;

    switch (m->kind) {
    case MACRO_OBJ:
	{
	    struct set *hdset = set_add(t->hideset, name);
	    struct vector *v = subst(m, NULL, hdset);
	    IS->add(IS, v);
	    return expand(IS, OS);
	}
    case MACRO_FUNC:
	{
	    struct token *t2 = IS->peek(IS);
	    if (t2->id != '(')
		goto end;
	    struct vector *args = arguments(m);
	    if (pptoken->id != ')')
		goto end;
	    struct set *hdset = set_add(set_intersection(t->hideset, pptoken->hideset), name);
	    struct vector *v = subst(m, args, hdset);
	    putbackv(v);
	    return read_expand();
	}
	break;
    case MACRO_SPECIAL:
	{

	}
    default:
	break;
    }
    
 end:
    vec_push(OS, t);
    expand(IS, OS);
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
     if (id->kind != TIDENTIFIER) {
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
	if (pptoken->id == ')' || pptoken->id == EOI)
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
    } else if (t->kind == TIDENTIFIER) {
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
	if (tok->kind == TNEWLINE)
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

static struct token * read_expand(void)
{
    
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
