#include "cc.h"
#include "sys.h"

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
static struct vector * expandv(struct vector *v);
static struct map *macros;
static struct vector *std;
static struct vector *usr;
static struct vector *cpplines;

static struct macro * new_macro(int kind)
{
    struct macro *m = zmalloc(sizeof (struct macro));
    m->kind = kind;
    return m;
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

static inline void ungetv(struct vector *v)
{
    for (int i = vec_len(v)-1; i >= 0; i--)
	unget(vec_at(v, i));
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

static struct token * peek(void)
{
    struct token *t = skip_spaces();
    unget(t);
    return t;
}

static int inparams(struct token *t, struct vector *params)
{
    if (t->id != ID || !params)
	return -1;
    for (int i = 0; i < vec_len(params); i++) {
	struct token *p = vec_at(params, i);
	if (t->name == p->name)
	    return i;
    }
    return -1;
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

static const char * find_header(const char *name, bool isstd)
{
    if (name == NULL)
	return NULL;
    
    struct vector *v1, *v2;
    if (isstd) {
        v1 = std;
	v2 = usr;
    } else {
	v1 = usr;
	v2 = std;
    }
    for (int i = 0; i < vec_len(v1); i++) {
	const char *dir = vec_at(v1, i);
	const char *file = join(dir, name);
	if (file_exists(file))
	    return file;
    }
    for (int i = 0; i < vec_len(v2); i++) {
	const char *dir = vec_at(v2, i);
	const char *file = join(dir, name);
	if (file_exists(file))
	    return file;
    }
    return NULL;
}

static const char * tokens2s(struct vector *v)
{
    struct strbuf *s = strbuf_new();
    for (int i = 0; i < vec_len(v); i++) {
	struct token *t = vec_at(v, i);
	if (t->name)
	    strbuf_cats(s, t->name);
    }
    return strbuf_str(s);
}

static void do_include(const char *name, bool isstd, struct vector *tokens)
{
    const char *file = find_header(name, isstd);
    if (file) {
	include_file(file);
    } else {
	if (tokens) {
	    struct source src = ((struct token *)vec_head(tokens))->src;
	    const char *macro = tokens2s(tokens);
	    if (name)
		fatalf(src, "'%s' file not found, expanded from macro '%s'", name, macro);
	    else
		errorf(src, "empty filename, expanded from macro '%s'", macro);
	} else {
	    if (name)
		fatal("'%s' file not found", name);
	    else
		error("empty filename");
	}
    }
}

static const char *unwrap(const char *name)
{
    struct strbuf *s = strbuf_new();
    
    if (name[0] == '"')
	strbuf_catn(s, name+1, strlen(name)-2); 
    else
	strbuf_catn(s, name+2, strlen(name)-3);

    return strbuf_str(s);
}

static void include_line(void)
{
    struct token *t = header_name();
    if (t) {
        do_include(t->name, t->kind == '<', NULL);
    } else {
	// pptokens
	struct source src = source;
	struct vector *v = vec_new();
	for (;;) {
	    struct token *t = skip_spaces();
	    if (IS_NEWLINE(t) || t->id == EOI)
		break;
	    vec_push(v, t);
	}
	
	if (vec_len(v)) {
	    struct vector *r = expandv(v);
	    struct token *tok = vec_head(r);
	    if (tok->id == SCONSTANT) {
		do_include(unwrap(tok->name), false, v);
	    } else if (tok->id == '<') {

	    } else {
		errorf(src, "invalid filename at '%s'", tok->name);
	    }
	} else {
	    errorf(src, "missing filename");
	}
    }
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
	    if (t->id == ID)
		vec_push(v, t);
	    else
		error("expect identifier");
	    t = skip_spaces();
	    if (t->id != ',')
		break;
	    t = skip_spaces();
	}
	if (t->id == ELLIPSIS) {
	    m->vararg = true;
	    t = skip_spaces();
	    if (t->id != ')')
		error("expect ')'");
	} else if (t->id != ')') {
	    error("expect ')'");
	}
	m->params = v;
    } else {
	error("expect identifier list or ')' or ...");
	skipline();
    }
}

static struct vector * replacement_list(void)
{
    struct vector *v = vec_new();
    struct token *t;
    for (;;) {
	t = skip_spaces();
	if (IS_NEWLINE(t))
	    break;
	vec_push(v, t);
    }
    if (IS_NEWLINE(t))
	unget(t);
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
	skipline();
	return;
    }
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
	skipline();
	return;
    }
    map_put(macros, id->name, NULL);
    id = skip_spaces();
    if (!IS_NEWLINE(id))
	error("extra tokens");
    skipline();
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

static void directive(void)
{
    struct token *t = skip_spaces();
    if (IS_NEWLINE(t))
	return;
    if (t->id == ICONSTANT) {
	unget(t);
	line_line();
	return;
    }
    if (t->id != ID) {
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

static struct vector * hsadd(struct vector *r, struct set *hideset)
{
    for (int i = 0; i < vec_len(r); i++) {
	struct token *t = vec_at(r, i);
	t->hideset = set_union(t->hideset, hideset);
    }
    return r;
}

static struct vector * expandv(struct vector *v)
{
    struct vector *r = vec_new();
    struct vector *iv = vec_new();
    vec_add(iv, v);
    push_buffer(iv);
    for (;;) {
	struct token *t = expand();
	if (t->id == EOI)
	    break;
	vec_push(r, t);
    }
    pop_buffer();
    vec_free(iv);
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
	    struct vector *iv = vec_at(args, index);
	    struct vector *ov = expandv(iv);
	    vec_add(r, ov);
	} else {
	    vec_push(r, t);
	}
    }
    return hsadd(r, hideset);
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
	    if (peek()->id != '(') {
		error("invalid func macro");
		return t;
	    }
	    skip_spaces();
	    struct vector *args = arguments(m);
	    CCAssert(token->id == ')');
	    struct set *hdset = set_add(set_intersection(t->hideset, token->hideset), name);
	    struct vector *v = subst(m, args, hdset);
	    ungetv(v);
	    return expand();
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

void gen_cpp_line(unsigned line, const char *file)
{
    const char *message = strs(format("# %u \"%s\"\n", line, file));
    struct token *t = new_token(&(struct token){.id = SCONSTANT, .name = message});
    vec_push(cpplines, t);
}

struct token * get_pptok(void)
{
    for (;;) {
        struct token *t = expand();
	if (vec_len(cpplines)) {
	    unget(t);
	    return vec_pop(cpplines);
	}
	// TODO: and t must be the first non-white-space token of a line
	if (t->id == '#') {
	    directive();
	    continue;
	}
	return conv2cc(t);
    }
}

static inline void add_include(struct vector *v, const char *name)
{
    const char *path = abspath(name);
    vec_push(v, (void *)strs(path));
    free((void *)path);
}

static void init_include(void)
{
    std = vec_new();
    usr = vec_new();
    
#ifdef CONFIG_LINUX
    
    add_include(std, "/usr/include");
    
#elif defined (CONFIG_DARWIN)
    
    add_include(std, XCODE_DIR "/usr/include");
    
#endif
}

static void parseopts(struct vector *options)
{
    for (int i = 0; i < vec_len(options); i++) {
	const char *arg = vec_at(options, i);
	if (strlen(arg) < 3)
	    continue;
	if (!strncmp(arg, "-I", 2)) {
	    add_include(usr, arg+2);
	} else if (!strncmp(arg, "-D", 2)) {
	    
	} else if (!strncmp(arg, "-U", 2)) {

	}
    }
}

void cpp_init(struct vector *options)
{
    macros = map_new(nocmp);
    cpplines = vec_new();
    init_include();
    parseopts(options);
}
