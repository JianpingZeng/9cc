#include "cc.h"
#include "sys/sys.h"

/* ACKNOWLEDGE
 *
 * Based on Dave Prosser's algorithm described in this document:
 *
 * http://www.spinellis.gr/blog/20060626/x3J11-86-196.pdf
 *
 */

static struct token *expand(void);
static struct vector *expandv(struct vector *v);
static void include_file(const char *file, bool std);
static struct map *macros;
static struct vector *std_include_paths;
static struct vector *usr_include_paths;
static struct tm now;
static struct token *token_zero = &(struct token){.id = NCONSTANT,.name = "0" };
static struct token *token_one = &(struct token){.id = NCONSTANT,.name = "1" };

static struct token *lineno0;

static struct macro *new_macro(int kind)
{
    struct macro *m = alloc_macro();
    m->kind = kind;
    return m;
}

static bool defined(const char *name)
{
    return map_get(macros, name);
}

static struct token *skip_spaces(void)
{
    struct token *t;
 beg:
    t = lex();
    if (IS_SPACE(t))
        goto beg;
    return t;
}

static void skipline(void)
{
    struct token *t;
 beg:
    t = lex();
    if (!IS_NEWLINE(t))
        goto beg;
    unget(t);
}

static struct token *peek(void)
{
    struct token *t = skip_spaces();
    unget(t);
    return t;
}

static void ungetv(struct vector *v)
{
    for (int i = vec_len(v) - 1; i >= 0; i--)
        unget(vec_at(v, i));
}

static struct token *defined_op(struct token *t)
{
    /* 'defined' operator:
     *
     * 1. defined identifier
     * 2. defined ( identifier )
     */
    struct token *t1 = skip_spaces();
    if (t1->id == ID) {
        return defined(t1->name) ? token_one : token_zero;
    } else if (t1->id == '(') {
        struct token *t2 = skip_spaces();
        struct token *t3 = skip_spaces();
        if (t2->id == ID && t3->id == ')') {
            return defined(t2->name) ? token_one : token_zero;
        } else {
            errorf(t->src,
                   "expect 'identifier )' after 'defined ('");
            unget(t3);
            unget(t2);
            return t;
        }
    } else {
        errorf(t->src, "expect identifier or ( after defined operator");
        return t;
    }
}

// read _expanded_ tokens
static struct vector *read_if_tokens(void)
{
    struct vector *v = vec_new();
    struct token *t;
    for (;;) {
        t = expand();
        if (IS_NEWLINE(t) || t->id == EOI)
            break;
        if (IS_SPACE(t))
            continue;
        if (t->id == ID && !strcmp(t->name, "defined"))
            vec_push(v, defined_op(t));
        else if (t->id == ID)
            // C99 6.10.1.3 says that remaining identifiers
            // should be replaced with pp-number 0.
            // Replace with 0 to inhibit parser fail.
            vec_push(v, token_zero);
        else
            vec_push(v, t);
    }
    unget(t);
    return v;
}

static bool eval_constexpr(void)
{
    SAVE_ERRORS;
    struct vector *tokens = read_if_tokens();
    if (HAS_ERROR)
        return false;

    // save parser context
    struct token *saved_token = token;
    struct token *saved_ahead = ahead_token;

    // create a temp file
    // so that get_pptok will not
    // generate 'unterminated conditional directive'
    file_stub(with_buffer(vec_reverse(tokens)));
    bool ret = eval_cpp_cond();
    file_unstub();

    // restore context
    token = saved_token;
    ahead_token = saved_ahead;

    return ret;
}

static void if_section(void)
{
    struct source src = source;
    bool b = eval_constexpr();
    if_sentinel(new_ifstub(&(struct ifstub) {
                .id = IF,.src = src,.b = b}));
    if (!b)
        skip_ifstub();
}

static void elif_group(void)
{
    struct ifstub *stub = current_ifstub();
    if (stub == NULL)
        error("#elif without #if");
    bool b = eval_constexpr();
    if (stub) {
        if (stub->b || !b)
            skip_ifstub();
        else
            stub->b = true;
    } else if (!b) {
        skip_ifstub();
    }
}

static void else_group(void)
{
    struct ifstub *stub = current_ifstub();
    if (stub == NULL)
        error("#else without #if");
    struct token *t = skip_spaces();
    if (!IS_NEWLINE(t)) {
        error("extra tokens in #else directive");
        skipline();
        t = skip_spaces();
    }
    unget(t);
    if (stub) {
        if (stub->b)
            skip_ifstub();
        else
            stub->b = true;
    }
}

static void endif_line(void)
{
    if (current_ifstub())
        if_unsentinel();
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

static void do_ifdef_section(int id)
{
    struct source src = source;
    struct token *t = skip_spaces();
    if (t->id != ID)
        fatal("expect identifier");
    bool b = defined(t->name);
    t = skip_spaces();
    if (!IS_NEWLINE(t)) {
        error("extra tokens in '%s' directive", id2s(id));
        skipline();
        t = skip_spaces();
    }
    unget(t);
    bool skip = id == IFDEF ? !b : b;
    if_sentinel(new_ifstub(&(struct ifstub) {
                .id = id,.src = src,.b = !skip}));
    if (skip)
        skip_ifstub();
}

static void ifdef_section(void)
{
    do_ifdef_section(IFDEF);
}

static void ifndef_section(void)
{
    do_ifdef_section(IFNDEF);
}

static void include_line(void)
{
    struct token *t = header_name();
    if (t) {
        include_file(t->name, t->kind == '<');
    } else {
        // # include pptokens newline
        struct source src = source;
        struct vector *v = vec_new();
        for (;;) {
            struct token *t = skip_spaces();
            if (IS_NEWLINE(t) || t->id == EOI)
                break;
            vec_push(v, t);
        }

        struct vector *r = expandv(v);
        if (vec_empty(r)) {
            errorf(src, "empty filename");
            return;
        }

        struct token *tok = vec_head(r);
        if (tok->id == SCONSTANT) {
            include_file(unwrap_scon(tok->name), false);
            for (int i = 1; i < vec_len(r); i++) {
                struct token *t = vec_at(r, i);
                if (!IS_SPACE(t)) {
                    errorf(t->src,
                           "extra tokens at end of #include directive '%s'",
                           t->name);
                    break;
                }
            }
        } else if (tok->id == '<') {
            struct token *tail = vec_tail(r);
            struct strbuf *s = strbuf_new();
            for (int i = 1; i < vec_len(r) - 1; i++) {
                struct token *t = vec_at(r, i);
                strbuf_cats(s, t->name);
            }
            strbuf_strip(s);
            if (tail->id != '>')
                errorf(src,
                       "expected \"FILENAME\" or <FILENAME>");
            else if (strbuf_len(s) == 0)
                errorf(src, "empty filename");
            else
                include_file(s->str, true);
        } else {
            errorf(src, "expected \"FILENAME\" or <FILENAME>");
        }
    }
}

static struct vector *arg(void)
{
    struct vector *v = vec_new();
    int parens = 0;
    struct token *t;
    bool space = false;
    for (;;) {
        /**
         * Merge multiple spaces to one,
         * treat newline as space here.
         */
        t = lex();
        if (IS_SPACE(t) || IS_NEWLINE(t)) {
            space = true;
            continue;
        }
        if (space)
            vec_push(v, space_token);
        if (((t->id == ',' || t->id == ')') && parens == 0) ||
            t->id == EOI)
            break;
        if (t->id == '(')
            parens++;
        else if (t->id == ')')
            parens--;
        vec_push(v, t);
        space = false;
    }
    unget(t);
    return v;
}

static struct vector *arguments(struct macro *m)
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
        assert(t->id == ',');
        vec_push(commas, t);
    }
    if (t->id != ')')
        error("unterminated function-like macro invocation");
    else
        unget(t);

    // remove leading and trailing space
    if (vec_len(v)) {
        struct vector *v1 = vec_head(v);
        struct vector *v2 = vec_tail(v);
        while (vec_len(v1) && IS_SPACE(vec_head(v1)))
            vec_pop_front(v1);
        while (vec_len(v2) && IS_SPACE(vec_tail(v2)))
            vec_pop(v2);
        // if the only arg is empty, then remove it
        if (vec_len(v) == 1 && vec_empty(vec_head(v)))
            vec_pop(v);
    }
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

    return v;
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
        if (!strcmp(t->name, p->name))
            return i;
    }
    return -1;
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
        if (t->id != ')') {
            error("expect ')'");
            unget(t);
        }
    } else if (t->id == ID) {
        // (a,b,c,...)
        struct vector *v = vec_new();
        for (;;) {
            if (t->id == ID) {
                for (int i = 0; i < vec_len(v); i++) {
                    struct token *t1 = vec_at(v, i);
                    if (!strcmp(t->name, t1->name)) {
                        error("duplicate macro paramter name '%s'",
                             t->name);
                        break;
                    }
                }
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
        if (t->id != ')') {
            errorf(t->src, "unterminated macro parameter list");
            unget(t);
        }
        m->params = v;
    } else {
        error("expect identifier list or ')' or ...");
        unget(t);
        skipline();
    }
    // create an empty vector if params == 0
    if (!m->params)
        m->params = vec_new();
}

static void add_macro(const char *name, struct macro *m)
{
    static const char *builtins[] = {
        "__STDC__",
        "__STDC_VERSION__",
        "__STDC_HOSTED__"
    };
    for (int i = 0; i < ARRAY_SIZE(builtins); i++) {
        if (!strcmp(name, builtins[i]))
            m->builtin = true;
    }
    map_put(macros, name, m);
}

static void remove_macro(const char *name)
{
    struct macro *m = map_get(macros, name);
    if (m && m->builtin)
        error("Can't undefine predefined macro '%s'", name);
    else
        map_put(macros, name, NULL);
}

static void ensure_macro_def(struct token *t, struct macro *m)
{
    // check redefinition
    const char *name = t->name;
    struct macro *m1 = map_get(macros, name);
    if (m1) {
        if (m1->builtin) {
            errorf(t->src, "Can't redefine predefined macro '%s'",
                   name);
        } else {
            // compare definition
            if (m->kind != m1->kind ||
                m->vararg != m1->vararg ||
                vec_len(m->params) != vec_len(m1->params) ||
                vec_len(m->body) != vec_len(m1->body))
                goto redef;

            for (int i = 0; i < vec_len(m->params); i++) {
                struct token *t1 = vec_at(m->params, i);
                struct token *t2 = vec_at(m1->params, i);
                if (strcmp(t1->name, t2->name))
                    goto redef;
            }

            for (int i = 0; i < vec_len(m->body); i++) {
                struct token *t1 = vec_at(m->body, i);
                struct token *t2 = vec_at(m1->body, i);
                if (strcmp(t1->name, t2->name))
                    goto redef;
            }
            // equal definition
            return;

        redef:
            errorf(t->src,
                   "'%s' macro redefinition, previous definition at %s:%u:%u",
                   name, m1->src.file, m1->src.line,
                   m1->src.column);
        }
        return;
    }

    if (!strcmp(name, "defined"))
        errorf(t->src, "'defined' cannot be used as a macro name");

    for (int i = 0; i < vec_len(m->body); i++) {
        struct token *t = vec_at(m->body, i);
        if (t->id == SHARPSHARP) {
            if (i == 0)
                errorf(t->src,
                       "'##' cannot appear at the beginning of a replacement list");
            else if (i == vec_len(m->body) - 1)
                errorf(t->src,
                       "'##' cannot appear at the end of a replacement list");
        } else if (t->id == '#') {
            struct token *t1 = vec_at_safe(m->body, i + 1);
            if (m->kind != MACRO_FUNC || t1 == NULL
                || inparams(t1, m) < 0)
                errorf(t->src,
                       "'#' is not followed by a macro parameter");
        }
    }
}

static struct vector *replacement_list(void)
{
    struct vector *v = vec_new();
    struct token *t = skip_spaces();
    bool space = false;
    for (;;) {
        if (IS_NEWLINE(t) || t->id == EOI)
            break;
        t->space = space;
        vec_push(v, t);
        // skip spaces and record
        space = false;
    beg:
        t = lex();
        if (IS_SPACE(t)) {
            space = true;
            goto beg;
        }
    }
    unget(t);
    return v;
}

static void define_objlike_macro(struct token *t)
{
    struct macro *m = new_macro(MACRO_OBJ);
    SAVE_ERRORS;
    m->src = t->src;
    m->body = replacement_list();
    ensure_macro_def(t, m);
    if (NO_ERROR)
        add_macro(t->name, m);
}

static void define_funclike_macro(struct token *t)
{
    struct macro *m = new_macro(MACRO_FUNC);
    SAVE_ERRORS;
    m->src = t->src;
    parameters(m);
    m->body = replacement_list();
    ensure_macro_def(t, m);
    if (NO_ERROR)
        add_macro(t->name, m);
}

static struct token *read_identifier(void)
{
    struct token *t = skip_spaces();
    if (t->id != ID) {
        error("expect identifier at '%s'", t->name);
        unget(t);
    }
    return t;
}

static void define_line(void)
{
    struct token *id = read_identifier();
    if (id->id != ID) {
        skipline();
        return;
    }
    struct token *t = lex();
    if (t->id == '(') {
        define_funclike_macro(id);
    } else {
        unget(t);
        define_objlike_macro(id);
    }
}

static void undef_line(void)
{
    struct token *t = read_identifier();
    if (t->id != ID) {
        skipline();
        return;
    }
    remove_macro(t->name);
    t = skip_spaces();
    if (!IS_NEWLINE(t)) {
        warning("extra tokens at the end of #undef directive");
        skipline();
    } else {
        unget(t);
    }
}

static void line_line(void)
{
    /* line directive:
     *
     * standard format:
     *
     * 1. # line digit-sequence new-line
     * 2. # line digit-sequence "s-char-sequence(opt)" new-line
     * 3. # line pp-tokens new-line
     *
     * extended format:
     *
     * 4. # digit-sequence new-line
     * 5. # digit-sequence "s-char-sequence(opt)" new-line
     */
    // TODO: 3rd format

    struct token *t = skip_spaces();
    // TODO: must be an integer, not floating
    if (t->id != NCONSTANT) {
        error("expect integer constant");
        unget(t);
        skipline();
        return;
    }
    const char *name;
    struct token *t2 = skip_spaces();
    if (t2->id == SCONSTANT) {
        name = format("# %s %s\n", t->name, t2->name);
    } else {
        name = format("# %s \"%s\"\n", t->name, current_file()->name);
        unget(t2);
    }
    skipline();
    unget(new_token(&(struct token) {
                .id = LINENO,.name = name}));
}

static const char *tokens2s(struct vector *v)
{
    struct strbuf *s = strbuf_new();
    for (int i = 0; i < vec_len(v); i++) {
        struct token *t = vec_at(v, i);
        strbuf_cats(s, t->name);
    }
    const char *ret = strbuf_str(strbuf_strip(s));
    return ret ? ret : "";
}

static void do_message_line(int level)
{
    struct source src = source;
    struct vector *v = vec_new();
    struct token *t;
    for (;;) {
        t = lex();
        if (IS_NEWLINE(t) || t->id == EOI)
            break;
        vec_push(v, t);
    }
    unget(t);

    const char *message = tokens2s(v);
    if (level == WRN)
        warningf(src, message);
    else
        errorf(src, message);
}

static void error_line(void)
{
    do_message_line(ERR);
}

static void warning_line(void)
{
    do_message_line(WRN);
}

static void pragma_line(void)
{
    struct source src = source;
    struct token *t;
    for (;;) {
        t = skip_spaces();
        if (IS_NEWLINE(t) || t->id == EOI)
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
    // TODO: must be an integer, not floating
    if (t->id == NCONSTANT) {
        unget(t);
        line_line();
        return;
    }
    if (t->id != ID)
        goto err;
    if (!strcmp(t->name, "if"))
        if_section();
    else if (!strcmp(t->name, "ifdef"))
        ifdef_section();
    else if (!strcmp(t->name, "ifndef"))
        ifndef_section();
    else if (!strcmp(t->name, "elif"))
        elif_group();
    else if (!strcmp(t->name, "else"))
        else_group();
    else if (!strcmp(t->name, "endif"))
        endif_line();
    else if (!strcmp(t->name, "include"))
        include_line();
    else if (!strcmp(t->name, "define"))
        define_line();
    else if (!strcmp(t->name, "undef"))
        undef_line();
    else if (!strcmp(t->name, "line"))
        line_line();
    else if (!strcmp(t->name, "error"))
        error_line();
    else if (!strcmp(t->name, "pragma"))
        pragma_line();
    else if (!strcmp(t->name, "warning"))
        warning_line();
    else
        goto err;
    return;
 err:
    warning("unknown preprocess directive '%s'", t->name);
    skipline();
}

static struct vector *expandv(struct vector *v)
{
    struct vector *r = vec_new();

    // create a temp file
    // so that get_pptok will not
    // generate 'unterminated conditional directive'
    file_stub(with_buffer(vec_reverse(v)));
    for (;;) {
        struct token *t = expand();
        if (t->id == EOI)
            break;
        vec_push(r, t);
    }
    file_unstub();

    return r;
}

// parse the input string to a token
static struct token *with_temp_lex(const char *input)
{
    struct source src = source;
    file_stub(with_string(input, "lex"));
    struct token *t = lex();
    struct token *t1 = lex();
    if (!IS_NEWLINE(t1))
        unget(t1);
    if (peek()->id != EOI) {
        struct token *t2 = lex();
        errorf(src,
               "pasting formed '%s%s', an invalid preprocessing token",
               t->name, t2->name);
    }
    file_unstub();
    return t;
}

static struct vector *hsadd(struct vector *r, struct hideset *hideset)
{
    for (int i = 0; i < vec_len(r); i++) {
        struct token *t = vec_at(r, i);
        t->hideset = hideset_union(t->hideset, hideset);
    }
    return r;
}

/**
 * Paste last of left side with first of right side.
 * The 'rs' is selected with no leading spaces and trailing spaces.
 */
static struct vector *glue(struct vector *ls, struct vector *rs)
{
    struct vector *r = vec_new();

    while (vec_len(ls) && IS_SPACE(vec_tail(ls)))
        vec_pop(ls);

    if (vec_empty(ls)) {
        vec_add(r, rs);
        return r;
    } else if (vec_empty(rs)) {
        vec_add(r, ls);
        return r;
    }

    struct token *ltok = vec_pop(ls);
    struct token *rtok = vec_pop_front(rs);
    const char *str = format("%s%s", ltok->name, rtok->name);
    struct token *t = with_temp_lex(str);
    t->hideset = hideset_intersection(ltok->hideset, rtok->hideset);

    vec_add(r, ls);
    vec_push(r, t);
    vec_add(r, rs);
    return r;
}

static const char *backslash(const char *name)
{
    struct strbuf *s = strbuf_new();
    for (int i = 0; i < strlen(name); i++) {
        char c = name[i];
        if (c == '"' || c == '\\')
            strbuf_catc(s, '\\');
        strbuf_catc(s, c);
    }
    return s->str;
}

/**
 * Stringify tokens.
 * The 'v' is selected with no leading and trailing spaces.
 */
static struct token *stringize(struct vector *v)
{
    struct strbuf *s = strbuf_new();
    strbuf_cats(s, "\"");
    for (int i = 0; i < vec_len(v); i++) {
        struct token *t = vec_at(v, i);
        if (t->id == SCONSTANT ||
            (t->id == NCONSTANT
             && (t->name[0] == '\'' || t->name[0] == 'L')))
            // Any embedded quotation or backslash characters
            // are preceded by a backslash character to preserve
            // their meaning in the string.
            strbuf_cats(s, backslash(t->name));
        else
            strbuf_cats(s, t->name);
    }
    strbuf_cats(s, "\"");
    return new_token(&(struct token) {
            .id = SCONSTANT,.name = s->str});
}

/**
 * Select an argument for expansion.
 * Remove the leading and trailing spaces.
 */
static struct vector *select(struct vector *args, int index)
{
    struct vector *v = vec_new();
    vec_add(v, vec_at_safe(args, index));
    while (vec_len(v) && IS_SPACE(vec_head(v)))
        vec_pop_front(v);
    while (vec_len(v) && IS_SPACE(vec_tail(v)))
        vec_pop(v);
    return v;
}

static struct vector *subst(struct macro *m, struct vector *args,
                            struct hideset *hideset)
{
    struct vector *r = vec_new();
    struct vector *body = m->body;

#define PUSH_SPACE(r, t)    if (t->space) vec_push(r, space_token)

    for (int i = 0; i < vec_len(body); i++) {
        struct token *t0 = vec_at(body, i);
        struct token *t1 = vec_at_safe(body, i + 1);
        int index;

        if (t0->id == '#' && (index = inparams(t1, m)) >= 0) {

            struct vector *iv = select(args, index);
            struct token *ot = stringize(iv);
            PUSH_SPACE(r, t0);
            vec_push(r, ot);
            i++;

        } else if (t0->id == SHARPSHARP
                   && (index = inparams(t1, m)) >= 0) {

            struct vector *iv = select(args, index);
            if (vec_len(iv))
                r = glue(r, iv);
            i++;

        } else if (t0->id == SHARPSHARP && t1) {

            hideset = t1->hideset;
            r = glue(r, vec_new1(t1));
            i++;

        } else if ((index = inparams(t0, m)) >= 0
                   && (t1 && t1->id == SHARPSHARP)) {

            hideset = t1->hideset;
            struct vector *iv = select(args, index);
            if (vec_len(iv)) {
                PUSH_SPACE(r, t0);
                vec_add(r, iv);
            } else {
                // add a space
                vec_push(r, space_token);

                struct token *t2 = vec_at_safe(body, i + 2);
                int index2 = inparams(t2, m);
                if (index2 >= 0) {
                    struct vector *iv2 =
                        select(args, index2);
                    vec_add(r, iv2);
                    i++;
                }
                i++;
            }

        } else if ((index = inparams(t0, m)) >= 0) {

            struct vector *iv = select(args, index);
            struct vector *ov = expandv(iv);
            PUSH_SPACE(r, t0);
            vec_add(r, ov);

        } else {
            PUSH_SPACE(r, t0);
            vec_push(r, t0);
        }
    }
    return hsadd(r, hideset);
}

static struct token *expand(void)
{
    struct token *t = lex();
    if (t->id != ID)
        return t;

    const char *name = t->name;
    struct macro *m = map_get(macros, name);
    if (m == NULL || hideset_has(t->hideset, name))
        return t;

    switch (m->kind) {
    case MACRO_OBJ:
        {
            struct hideset *hdset = hideset_add(t->hideset, name);
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
                assert(rparen->id == ')');
                struct hideset *hdset =
                    hideset_add(hideset_intersection
                                (t->hideset, rparen->hideset),
                                name);
                struct vector *v = subst(m, args, hdset);
                ungetv(v);
                return expand();
            } else {
                return t;
            }
        }
        break;
    case MACRO_SPECIAL:
        m->handler(t);
        return expand();
    default:
        die("unkown macro type %d", m->kind);
        return t;
    }
}

static void file_handler(struct token *t)
{
    const char *file = current_file()->name;
    const char *name = format("\"%s\"", file);
    struct token *tok = new_token(&(struct token){.id = SCONSTANT,.name =
                name,.src = t->src });
    unget(tok);
}

static void line_handler(struct token *t)
{
    unsigned line = current_file()->line;
    const char *name = strd(line);
    struct token *tok = new_token(&(struct token){.id = NCONSTANT,.name =
                name,.src = t->src });
    unget(tok);
}

static void date_handler(struct token *t)
{
    // mmm dd yyyy
    char ch[20];
    strftime(ch, sizeof(ch), "%b %e %Y", &now);
    const char *name = format("\"%s\"", ch);
    struct token *tok = new_token(&(struct token){.id = SCONSTANT,.name =
                name,.src = t->src });
    unget(tok);
}

static void time_handler(struct token *t)
{
    // hh:mm:ss
    char ch[10];
    strftime(ch, sizeof(ch), "%T", &now);
    const char *name = format("\"%s\"", ch);
    struct token *tok = new_token(&(struct token){.id = SCONSTANT,.name =
                name,.src = t->src });
    unget(tok);
}

static void define_special(const char *name, void (*handler) (struct token *))
{
    struct macro *m = new_macro(MACRO_SPECIAL);
    m->handler = handler;
    add_macro(name, m);
}

static void add_include(struct vector *v, const char *name)
{
    vec_push(v, (char *)abspath(name));
}

static struct token *lineno(unsigned line, const char *file)
{
    const char *name = format("# %u \"%s\"\n", line, file);
    struct token *t = new_token(&(struct token){.id = LINENO,.name =
                name,.src.file = "<built-in>" });
    return t;
}

static const char *find_header(const char *name, bool isstd)
{
    if (name == NULL)
        return NULL;

    struct vector *paths = vec_new();
    if (isstd) {
        vec_add(paths, std_include_paths);
    } else {
        vec_add(paths, usr_include_paths);
        // try current path
        /**
         * NOTE!!!
         * The 'dirname()' manual page says:
         * Both dirname() and basename() may modify
         * the contents of path, so it may be desirable
         * to pass a copy when calling one of these functions.
         */
        vec_push(paths, dirname(xstrdup(current_file()->name)));
        vec_add(paths, std_include_paths);
    }
    for (int i = 0; i < vec_len(paths); i++) {
        const char *dir = vec_at(paths, i);
        const char *file = join(dir, name);
        if (file_exists(file))
            return file;
    }
    return NULL;
}

static void do_include_file(const char *file, const char *name, bool std)
{
    const char *path = find_header(file, std);
    if (path) {
        file_sentinel(with_file(path, name ? name : path));
        unget(lineno(1, current_file()->name));
    } else {
        if (file)
            fatal("'%s' file not found", file);
        else
            error("empty filename");
    }
}

static void include_file(const char *file, bool std)
{
    do_include_file(file, NULL, std);
}

static void include_builtin(const char *file)
{
    do_include_file(file, "<built-in>", true);
}

static void include_command_line(const char *command)
{
    file_sentinel(with_string(command, "<command-line>"));
    unget(lineno(1, current_file()->name));
}

static void builtin_macros(void)
{
    define_special("__FILE__", file_handler);
    define_special("__LINE__", line_handler);
    define_special("__DATE__", date_handler);
    define_special("__TIME__", time_handler);
    include_builtin(BUILTIN_HEADER);
}

static void init_env(void)
{
    setlocale(LC_ALL, "C");
    time_t t = time(NULL);
    set_localtime(&t, &now);
}

static void init_include(void)
{
    std_include_paths = vec_new();
    usr_include_paths = vec_new();
    // add system include paths
    struct vector *sys_include_paths = sys_include_dirs();
    for (int i = 0; i < vec_len(sys_include_paths); i++) {
        const char *dir = vec_at(sys_include_paths, i);
        add_include(std_include_paths, dir);
    }
}

static void parseopts(struct vector *options)
{
    struct strbuf *s = strbuf_new();

    for (int i = 0; i < vec_len(options); i++) {
        const char *arg = vec_at(options, i);
        if (strlen(arg) < 3)
            continue;
        if (!strncmp(arg, "-I", 2)) {
            add_include(usr_include_paths, arg + 2);
        } else if (!strncmp(arg, "-D", 2)) {
            const char *content = arg + 2;
            char *ptr = strchr(content, '=');
            if (ptr) {
                char *name = xstrndup(content, ptr - content);
                if (ptr - content < strlen(content) - 1) {
                    char *value = xstrdup(ptr + 1);
                    strbuf_cats(s, format("#define %s %s\n", name, value));
                } else {
                    strbuf_cats(s, format("#define %s\n", name));
                }
            } else {
                strbuf_cats(s, format("#define %s\n", content));
            }
        } else if (!strncmp(arg, "-U", 2)) {
            strbuf_cats(s, format("#undef %s\n", arg + 2));
        }
    }

    if (strbuf_len(s))
        include_command_line(s->str);
}

void cpp_init(struct vector *options)
{
    macros = map_new();
    lineno0 = lineno(1, current_file()->name);
    init_env();
    init_include();
    builtin_macros();
    parseopts(options);
}

/* Getting one expanded token.
 */
struct token *get_pptok(void)
{
    for (;;) {
        struct token *t = expand();
        if (t->id == EOI) {
            struct ifstub *stub = current_ifstub();
            if (stub)
                errorf(stub->src,
                       "unterminated conditional directive");
            if (current_file()->stub) {
                return t;
            } else {
                file_unsentinel();
                if (current_file())
                    return lineno(current_file()->line,
                                  current_file()->name);
                else
                    return t;
            }
        }
        if (t->id == '#' && t->bol) {
            directive();
            continue;
        }
        return t;
    }
}

static struct vector *pretty(struct vector *v)
{
    // remove unnecessary spaces and newlines
    while (vec_len(v) && (IS_NEWLINE(vec_tail(v)) || IS_SPACE(vec_tail(v))))
        vec_pop(v);
    if (vec_len(v) && !IS_NEWLINE(vec_tail(v)) && !IS_LINENO(vec_tail(v)))
        vec_push(v, newline_token);

    struct vector *r = vec_new();
    for (int i = 0; i < vec_len(v); i++) {
        struct token *t = vec_at(v, i);
        if (t->id != LINENO) {
            vec_push(r, t);
            continue;
        }
        int j = i + 1;
        struct token *t1 = vec_at_safe(v, j);
        while (t1 && (IS_NEWLINE(t1) || IS_SPACE(t1)))
            t1 = vec_at_safe(v, ++j);
        if (t1 && t1->id == LINENO)
            i = j - 1;
        vec_push(r, t);
    }
    return r;
}

struct vector *all_pptoks(void)
{
    struct vector *v = vec_new1(lineno0);
    for (;;) {
        struct token *t = get_pptok();
        if (t->id == EOI)
            break;
        vec_push(v, t);
    }
    return pretty(v);
}
