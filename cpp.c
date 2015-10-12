#include "cc.h"
#include "sys.h"

/* ACKNOWLEDGE
 *
 * Base on Dave Prosser's algorithm described in this document:
 *
 * http://www.spinellis.gr/blog/20060626/x3J11-86-196.pdf
 *
 */

// macro kind
enum {
    MACRO_OBJ,
    MACRO_FUNC,
    MACRO_SPECIAL
};

typedef void SpecialFn(struct token *t);

struct macro {
    int kind;
    struct vector *body;
    struct vector *params;
    bool vararg;
    SpecialFn *fn; // special macro handler
};

struct ifstub {
    const char *name;
    struct source src;
    bool b;
};

static struct token * expand(void);
static struct vector * expandv(struct vector *v);
static inline void include_file(const char *file);
static struct map *macros;
static struct vector *std;
static struct vector *usr;
static struct tm now;

static struct ifstub * new_ifstub(struct ifstub *i)
{
    struct ifstub *ic = zmalloc(sizeof(struct ifstub));
    memcpy(ic, i, sizeof(struct ifstub));
    return ic;
}

static void if_stub(struct ifstub *i)
{
    vec_push(current_file()->ifstubs, i);
}

static void if_unstub(void)
{
    vec_pop(current_file()->ifstubs);
}

static struct ifstub * current_ifstub(void)
{
    if (vec_len(current_file()->ifstubs))
	return vec_tail(current_file()->ifstubs);
    else
	return NULL;
}

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

static int inparams(struct token *t, struct macro *m)
{
    struct vector *params = m->params;
    if (t->id != ID)
	return -1;
    if (!strcmp(t->name, "__VA_ARGS__") && m->vararg)
	return vec_len(params);
    if (!params)
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

static void do_ifdef_section(const char *name, bool def)
{
    struct source src = source;
    struct token *t = skip_spaces();
    if (t->id != ID)
	fatal("expect identifier");
    bool b = map_get(macros, t->name);
    t = skip_spaces();
    if (!IS_NEWLINE(t)) {
	error("extra tokens in '%s' directive", name);
	skipline();
    } else {
	unget(t);
    }
    if_stub(new_ifstub(&(struct ifstub){.name = name, .src = src, .b = b}));
    bool skip = def ? !b : b;
    if (skip)
	skip_if_cond();
}

static void ifdef_section(void)
{
    do_ifdef_section("ifdef", true);
}

static void ifndef_section(void)
{
    do_ifdef_section("ifndef", false);
}

static void elif_group(void)
{
    if (current_ifstub() == NULL)
	error("#elif without #if");
}

static void else_group(void)
{
    if (current_ifstub() == NULL)
	error("#else without #if");
}

static void endif_line(void)
{
    if (current_ifstub())
	if_unstub();
    else
	error("#endif without #if");
    struct token *t = skip_spaces();
    if (!IS_NEWLINE(t)) {
	error("extra tokens in #endif");
	skipline();
    } else {
	unget(t);
    }
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
		do_include(unwrap_scon(tok->name), false, v);
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
	t = lex();
	if (((t->id == ',' || t->id == ')') && parens == 0) ||
	    t->id == EOI)
	    break;
	if (t->id == '(')
	    parens++;
	else if (t->id == ')')
	    parens--;
	vec_push(v, t);
    }
    unget(t);
    return v;
}

static struct vector * arguments(struct macro *m)
{
    struct vector *v = vec_new();
    struct vector *commas = vec_new();
    struct token *t;
    for (;;) {
	struct vector *r = arg();
	vec_push(v, r);
	t = lex();
	if (t->id == ')' || t->id == EOI)
	    break;
	vec_push(commas, t);
    }
    if (t->id == ')')
	unget(t);
    else
	error("expect ')'");
    // check args and params
    if (vec_len(v) < vec_len(m->params)) {
	error("too few arguments provided to function-like macro invocation");
    } else if (vec_len(v) > vec_len(m->params)) {
	if (m->vararg) {
	    // merge 'variable arguments'
	    struct vector *v2 = vec_new();
	    for (int i = vec_len(m->params); i < vec_len(v); i++) {
		vec_add(v2, vec_at(v, i));
		if (i != vec_len(v) - 1)
		    vec_push(v2, vec_at(commas, i));
	    }
	    int i = vec_len(v) - vec_len(m->params);
	    while (i--)
		vec_pop(v);
	    vec_push(v, v2);
	} else {
	    error("too many arguments provided to function-like macro invocation");
	}
    }
    // remove space
    if (vec_len(v)) {
	struct vector *v1 = vec_head(v);
	struct vector *v2 = vec_tail(v);
	while (vec_len(v1) && IS_SPACE(vec_head(v1)))
	    vec_pop_front(v1);
	while (vec_len(v2) && IS_SPACE(vec_tail(v2)))
	    vec_pop(v2);
    }
    return v;
}

static void parameters(struct macro *m)
{
    struct token *t = skip_spaces();
    if (t->id == ')') {
	// ()
    } else if (t->id == ELLIPSIS) {
	// (...)
	m->vararg = true;
	t = skip_spaces();
	if (t->id != ')')
	    error("expect ')'");
    } else if (t->id == ID) {
	// (a,b,c,...)
	struct vector *v = vec_new();
	for (;;) {
	    if (t->id == ID) {
		vec_push(v, t);
	    } else if (t->id == ELLIPSIS) {
		m->vararg = true;
		t = skip_spaces();
		break;
	    } else {
		error("expect identifier or ...");
	    }
	    t = skip_spaces();
	    if (t->id != ',')
		break;
	    t = skip_spaces();
	}
	if (t->id != ')')
	    error("expect ')'");
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
    unget(t);
    return v;
}

static void define_obj_macro(const char *name)
{
    struct macro *m = new_macro(MACRO_OBJ);
    SAVE_ERRORS;
    m->body = replacement_list();
    ensure_macro_def(m);
    if (NO_ERROR)
	map_put(macros, name, m);
}

static void define_funclike_macro(const char *name)
{
    struct macro *m = new_macro(MACRO_FUNC);
    SAVE_ERRORS;
    parameters(m);
    m->body = replacement_list();
    ensure_macro_def(m);
    if (NO_ERROR)
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
	warning("extra tokens at the end of #undef directive");
    skipline();
}

static void line_line(void)
{
    struct token *t = skip_spaces();
    struct token *t2 = skip_spaces();
    for (;;) {
	struct token *t = skip_spaces();
	if (IS_NEWLINE(t))
	    break;
    }

    const char *name = format("# %s %s\n", t->name, t2->name);
    t = new_token(&(struct token){.id = LINENO, .name = name});
    unget(t);
}

static const char * tokens2msg(struct vector *v)
{
    struct strbuf *s = strbuf_new();
    for (int i = 0; i < vec_len(v); i++) {
	struct token *t = vec_at(v, i);
	if (IS_SPACE(t))
	    strbuf_cats(s, " ");
	else if (t->name)
	    strbuf_cats(s, t->name);
    }
    const char *ret = strbuf_str(strbuf_strip(s));
    return ret ? ret : "";
}

static struct vector * pptokens(void)
{
    struct vector *v = vec_new();
    struct token *t;
    for (;;) {
	t = lex();
	if (IS_NEWLINE(t))
	    break;
	vec_push(v, t);
    }
    unget(t);
    return v;
}

static void error_line(void)
{
    struct source src = source;
    struct vector *v = pptokens();
    const char *message = tokens2msg(v);
    errorf(src, message);
}

static void warning_line(void)
{
    struct source src = source;
    struct vector *v = pptokens();
    const char *message = tokens2msg(v);
    warningf(src, message);
}

static void pragma_line(void)
{
    struct source src = source;
    struct token *t;
    for (;;) {
	t = skip_spaces();
	if (IS_NEWLINE(t))
	    break;
    }
    unget(t);
    warningf(src, "pragma directive not supported yet");
}

static void directive(void)
{
    struct token *t = skip_spaces();
    if (IS_NEWLINE(t)) {
	unget(t);
	return;
    }
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
    else if (!strcmp(t->name, "elif")) elif_group();
    else if (!strcmp(t->name, "else")) else_group();
    else if (!strcmp(t->name, "endif")) endif_line();
    else if (!strcmp(t->name, "include")) include_line();
    else if (!strcmp(t->name, "define")) define_line();
    else if (!strcmp(t->name, "undef")) undef_line();
    else if (!strcmp(t->name, "line")) line_line();
    else if (!strcmp(t->name, "error")) error_line();
    else if (!strcmp(t->name, "pragma")) pragma_line();
    else if (!strcmp(t->name, "warning")) warning_line();
    else skipline();
}

static struct token * stringize(struct vector *v)
{
    struct strbuf *s = strbuf_new();
    strbuf_cats(s, "\"");
    for (int i = 0; i < vec_len(v); i++) {
	struct token *t = vec_at(v, i);
	if (IS_SPACE(t))
	    strbuf_cats(s, " "); // only one space
	else
	    strbuf_cats(s, t->name);
    }
    strbuf_cats(s, "\"");
    return new_token(&(struct token){.id = SCONSTANT, .name = strs(s->str)});
}

static struct vector * expandv(struct vector *v)
{
    struct vector *r = vec_new();
    struct vector *iv = vec_new();
    vec_add(iv, v);

    buffer_stub(iv);
    for (;;) {
	struct token *t = expand();
	if (t->id == EOI)
	    break;
	vec_push(r, t);
    }
    buffer_unstub();
    
    vec_free(iv);
    struct vector *r2 = vec_reverse(r);
    vec_free(r);
    return r2;
}

// paste last of left side with first of right side
static struct vector * glue(struct vector *ls, struct vector *rs)
{
    struct token *l = vec_pop(ls);
    struct token *r = vec_pop_front(rs);
    const char *str = format("%s%s", l->name, r->name);
    struct token *t = with_temp_lex(str);
    t->hideset = set_intersection(l->hideset, r->hideset);
    vec_push(ls, t);
    vec_add(ls, rs);
    return ls;
}

static struct vector * hsadd(struct vector *r, struct set *hideset)
{
    for (int i = 0; i < vec_len(r); i++) {
	struct token *t = vec_at(r, i);
	t->hideset = set_union(t->hideset, hideset);
    }
    return r;
}

static struct vector * select(struct vector *ap, int index)
{
    if (index < 0 || index >= vec_len(ap))
	return NULL;
    return vec_at(ap, index);
}

static struct vector * remove_spaces(struct vector *v)
{
    struct vector *r = vec_new();
    for (int i = 0; i < vec_len(v); i++) {
	struct toke *t = vec_at(v, i);
	if (IS_SPACE(t))
	    continue;
	vec_push(r, t);
    }
    return r;
}

static struct vector * subst(struct macro *m, struct vector *args, struct set *hideset)
{
    struct vector *r = vec_new();
    struct vector *body = m->body;
    
    for (int i = 0; i < vec_len(body); i++) {
	struct token *t0 = vec_at(body, i);
	struct token *t1 = i+1 < vec_len(body) ? vec_at(body, i+1) : NULL;
	int index;

	if (t0->id == '#' && (index = inparams(t1, m)) >= 0) {

	    struct vector *iv = select(args, index);
	    struct token *ot = stringize(iv);
	    vec_push_safe(r, ot);
	    i++;
	    
	} else if (t0->id == SHARPSHARP && (index = inparams(t1, m)) >= 0) {

	    struct vector *iv = select(args, index);
	    if (iv && vec_len(iv))
		r = glue(r, remove_spaces(iv));
	    i++;
	    
	} else if (t0->id == SHARPSHARP && t1) {

	    hideset = t1->hideset;
	    r = glue(r, vec_new1(t1));
	    i++;
	    
	} else if ((index = inparams(t0, m)) >= 0 && (t1 && t1->id == SHARPSHARP) ) {
	    hideset = t1->hideset;
	    struct vector *iv = select(args, index);
	    iv = remove_spaces(iv);

	    if (iv && vec_len(iv)) {
		vec_add(r, iv);
	    } else {
		struct token *t2 = i+2 < vec_len(body) ? vec_at(body, i+2) : NULL;
		int index2 = inparams(t2, m);
		if (index2 >= 0) {
		    struct vector *iv2 = select(args, index2);
		    vec_add(r, remove_spaces(iv2));
		    i++;
		}
		i++;
	    }
	    
	} else if ((index = inparams(t0, m)) >= 0) {
	    
	    struct vector *iv = select(args, index);
	    struct vector *ov = expandv(iv);
	    vec_add(r, ov);
	    
	} else {
	    vec_push(r, t0);
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
	    if (peek()->id != '(')
		return t;
	    SAVE_ERRORS;
	    skip_spaces();
	    struct vector *args = arguments(m);
	    if (NO_ERROR) {
		struct token *rparen = skip_spaces();
		struct set *hdset = set_add(set_intersection(t->hideset, rparen->hideset), name);
		struct vector *v = subst(m, args, hdset);
		ungetv(v);
		return expand();
	    } else {
		return t;
	    }
	}
	break;
    case MACRO_SPECIAL:
        m->fn(t);
	return expand();
    default:
	die("unkown macro type %d", m->kind);
	return t;
    }
}

static struct token * expand(void)
{
    return doexpand();
}

static void file_handler(struct token *t)
{
    const char *file = t->src.file;
    const char *name = strs(format("\"%s\"", file));
    struct token *tok = new_token(&(struct token){.id = SCONSTANT, .name = name, .src = t->src});
    unget(tok);
}

static void line_handler(struct token *t)
{
    unsigned line = t->src.line;
    const char *name = strd(line);
    struct token *tok = new_token(&(struct token){.id = ICONSTANT, .name = name, .src = t->src});
    unget(tok);
}

static void date_handler(struct token *t)
{
    // mmm dd yyyy
    char ch[20];
    strftime(ch, sizeof(ch), "%b %e %Y", &now);
    const char *name = strs(format("\"%s\"", ch));
    struct token *tok = new_token(&(struct token){.id = SCONSTANT, .name = name, .src = t->src});
    unget(tok);
}

static void time_handler(struct token *t)
{
    // hh:mm:ss
    char ch[10];
    strftime(ch, sizeof(ch), "%T", &now);
    const char *name = strs(format("\"%s\"", ch));
    struct token *tok = new_token(&(struct token){.id = SCONSTANT, .name = name, .src = t->src});
    unget(tok);
}

static void define_special(const char *name, SpecialFn *fn)
{
    struct macro *m = new_macro(MACRO_SPECIAL);
    m->fn = fn;
    map_put(macros, strs(name), m);
}

static inline void add_include(struct vector *v, const char *name)
{
    const char *path = abspath(name);
    vec_push(v, (void *)strs(path));
    free((void *)path);
}

struct token * get_pptok(void)
{
    for (;;) {
    	struct token *t = expand();
    	if (t->id == EOI) {
	    struct ifstub *stub = current_ifstub();
	    if (stub)
		errorf(stub->src, "unterminated conditional directive");
    	    return t;
	}
        if (t->id == '#' && t->bol) {
    	    directive();
    	    continue;
    	}
	return t;
    }
}

static struct vector * preprocess(void)
{
    struct vector *v = vec_new();
    for (;;) {
	struct token *t = get_pptok();
	if (t->id == EOI)
	    break;
	vec_push(v, t);
    }
    // remove unnecessary spaces and newlines
    while (vec_len(v) && (IS_NEWLINE(vec_tail(v)) || IS_SPACE(vec_tail(v))))
	vec_pop(v);
    if (vec_len(v) && !IS_NEWLINE(vec_tail(v)) && !IS_LINENO(vec_tail(v)))
	vec_push(v, newline_token);
    return v;
}

static void include_alias(const char *file, const char *alias)
{
    file_stub(with_temp_file(file, alias));
    struct vector *v = preprocess();
    file_unstub();
    ungetv(v);
}

static inline void include_file(const char *file)
{
    include_alias(file, file);
}

static inline void include_builtin(const char *file)
{
    include_alias(file, "<built-in>");
}

static void include_command_line(const char *command)
{
    file_stub(with_temp_string(command, "<command-line>"));
    struct vector *v = preprocess();
    file_unstub();
    ungetv(v);
}

static void builtin_macros(void)
{
    define_special("__FILE__", file_handler);
    define_special("__LINE__", line_handler);
    define_special("__DATE__", date_handler);
    define_special("__TIME__", time_handler);

    include_builtin(BUILD_DIR "/include/mcc.h");
}

static void init_env(void)
{
    setlocale(LC_ALL, "C");
    time_t t = time(NULL);
    localtime_r(&t, &now);
}

static void init_include(void)
{
    std = vec_new();
    usr = vec_new();

    add_include(std, BUILD_DIR "/include");
    
#ifdef CONFIG_LINUX
    
    add_include(std, "/usr/include");
    
#elif defined (CONFIG_DARWIN)
    
    add_include(std, XCODE_DIR "/usr/include");
    
#endif
}

static void parseopts(struct vector *options)
{
    struct strbuf *s = strbuf_new();
    
    for (int i = 0; i < vec_len(options); i++) {
	const char *arg = vec_at(options, i);
	if (strlen(arg) < 3)
	    continue;
	if (!strncmp(arg, "-I", 2)) {
	    add_include(usr, arg+2);
	} else if (!strncmp(arg, "-D", 2)) {
	    const char *content = arg + 2;
	    char *ptr = strchr(content, '=');
	    if (ptr) {
		char *name = strndup(content, ptr - content);
		if (ptr - content < strlen(content) - 1) {
		    char *value = strdup(ptr + 1);
		    strbuf_cats(s, format("#define %s %s\n", name, value));
		    free(value);
		} else {
		    strbuf_cats(s, format("#define %s\n", name));
		}
		free(name);
	    } else {
		strbuf_cats(s, format("#define %s\n", content));
	    }
	} else if (!strncmp(arg, "-U", 2)) {
	    strbuf_cats(s, format("#undef %s\n", arg+2));
	}
    }

    if (strbuf_len(s))
	include_command_line(s->str);
    
    strbuf_free(s);
}

void cpp_init(struct vector *options)
{
    macros = map_new(nocmp);
    init_env();
    init_include();
    builtin_macros();
    parseopts(options);
}

struct vector * all_pptoks(void)
{
    return preprocess();
}
