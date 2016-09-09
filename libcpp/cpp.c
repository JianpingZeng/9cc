#include "cpp.h"

/* ACKNOWLEDGE
 *
 * Based on Dave Prosser's algorithm described in this document:
 *
 * http://www.spinellis.gr/blog/20060626/x3J11-86-196.pdf
 *
 */

static struct token *expand(struct file *pfile);
static struct vector *expandv(struct file *pfile, struct vector *v);
static void include_file(struct file *pfile, const char *file, bool std);
static struct token *token_zero = &(struct token){.id = NCONSTANT, .value.lexeme = "0"};
static struct token *token_one = &(struct token){.id = NCONSTANT, .value.lexeme = "1"};

#define IMAP_LOOKUP_HASH(imap, id, opt)                                 \
    (struct cpp_ident *)imap_lookup_with_hash(imap, id->str, id->len, id->hash, opt)

static void do_if(struct file *);
static void do_ifdef(struct file *);
static void do_ifndef(struct file *);
static void do_elif(struct file *);
static void do_else(struct file *);
static void do_endif(struct file *);
static void do_include(struct file *);
static void do_define(struct file *);
static void do_undef(struct file *);
static void do_line(struct file *);
static void do_error(struct file *);
static void do_warning(struct file *);
static void do_pragma(struct file *);

static struct directive_table {
    const char *name;
    void (*handler) (struct file *);
} directive_table[] = {
    { "if",      do_if },
    { "ifdef",   do_ifdef },
    { "ifndef",  do_ifndef },
    { "elif",    do_elif },
    { "else",    do_else },
    { "endif",   do_endif },
    { "include", do_include },
    { "define",  do_define },
    { "undef",   do_undef },
    { "line",    do_line },
    { "error",   do_error },
    { "warning", do_warning },
    { "pragma",  do_pragma }
};

static inline bool defined(struct file *pfile, struct token *t)
{
    struct ident *id = t->value.ident;
    struct cpp_ident *ident = IMAP_LOOKUP_HASH(pfile->imap, id, IMAP_SEARCH);
    return ident && ident->type == CT_MACRO;
}

static struct token *skip_spaces(struct file *pfile)
{
    struct token *t;
    for (;;) {
        t = lex(pfile);
        if (t->id == EOI)
            break;
        if (!IS_SPACE(t))
            break;
    }
    return t;
}

static void skipline(struct file *pfile)
{
    struct token *t;
    for (;;) {
        t = lex(pfile);
        if (IS_NEWLINE(t))
            break;
        if (t->id == EOI)
            break;
    }
}

static struct token *peek(struct file *pfile)
{
    struct token *t = skip_spaces(pfile);
    unget(pfile, t);
    return t;
}

void unget(struct file *pfile, struct token *t)
{
    vec_push(pfile->buffer->ungets, t);
}

static void ungetv(struct file *pfile, struct vector *v)
{
    for (int i = vec_len(v) - 1; i >= 0; i--)
        unget(pfile, vec_at(v, i));
}

static struct token *defined_op(struct file *pfile, struct token *t)
{
    /* 'defined' operator:
     *
     * 1. defined identifier
     * 2. defined ( identifier )
     */
    struct token *t1 = skip_spaces(pfile);
    if (t1->id == ID) {
        return defined(pfile, t1) ? token_one : token_zero;
    } else if (t1->id == '(') {
        struct token *t2 = skip_spaces(pfile);
        struct token *t3 = skip_spaces(pfile);
        if (t2->id == ID && t3->id == ')') {
            return defined(pfile, t2) ? token_one : token_zero;
        } else {
            error_at(t->src,
                     "expect 'identifier )' after 'defined ('");
            unget(pfile, t3);
            unget(pfile, t2);
            return t;
        }
    } else {
        error_at(t->src, "expect identifier or ( after defined operator");
        return t;
    }
}

// read _expanded_ tokens
static struct vector *read_if_tokens(struct file *pfile)
{
    struct vector *v = vec_new();
    struct token *t;
    for (;;) {
        t = expand(pfile);
        if (IS_NEWLINE(t) || t->id == EOI)
            break;
        if (IS_SPACE(t))
            continue;
        if (t->id == ID && !strcmp(TOK_ID_STR(t), "defined"))
            vec_push(v, defined_op(pfile, t));
        else if (t->id == ID)
            // C99 6.10.1.3 says that remaining identifiers
            // should be replaced with pp-number 0.
            // Replace with 0 to inhibit parser fail.
            vec_push(v, token_zero);
        else
            vec_push(v, t);
    }
    return v;
}

static bool eval_constexpr(struct file *pfile)
{
    SAVE_ERRORS;
    struct vector *tokens = read_if_tokens(pfile);
    if (HAS_ERROR)
        return false;

    // save parser context
    struct token *saved_token = token;
    struct token *saved_ahead = ahead_token;

    // create a temp file
    // so that get_pptok will not
    // generate 'unterminated conditional directive'
    buffer_sentinel(pfile,
                    with_tokens(vec_reverse(tokens), pfile->buffer),
                    BS_RETURN_EOI);
    bool ret = eval_cpp_cond();
    buffer_unsentinel(pfile);

    // restore context
    token = saved_token;
    ahead_token = saved_ahead;

    return ret;
}

static void do_if(struct file *pfile)
{
    struct source src = source;
    bool b = eval_constexpr(pfile);
    if_sentinel(pfile, &(struct ifstack){.id = IF,.src = src,.b = b});
    if (!b)
        skip_ifstack(pfile);
}

static void do_elif(struct file *pfile)
{
    struct ifstack *stack = pfile->buffer->ifstack;
    if (stack == NULL)
        error("#elif without #if");
    bool b = eval_constexpr(pfile);
    if (stack) {
        if (stack->b || !b)
            skip_ifstack(pfile);
        else
            stack->b = true;
    } else if (!b) {
        skip_ifstack(pfile);
    }
}

static void do_else(struct file *pfile)
{
    struct ifstack *stack = pfile->buffer->ifstack;
    if (stack == NULL)
        error("#else without #if");
    struct token *t = skip_spaces(pfile);
    if (!IS_NEWLINE(t) && t->id != EOI) {
        error("extra tokens in #else directive");
        skipline(pfile);
    }
    if (stack) {
        if (stack->b)
            skip_ifstack(pfile);
        else
            stack->b = true;
    }
}

static void do_endif(struct file *pfile)
{
    if (pfile->buffer->ifstack)
        if_unsentinel(pfile);
    else
        error("#endif without #if");
    struct token *t = skip_spaces(pfile);
    if (!IS_NEWLINE(t) && t->id != EOI) {
        error("extra tokens in #endif");
        skipline(pfile);
    }
}

static void do_ifdef_section(struct file *pfile, int id)
{
    struct source src = source;
    struct token *t = skip_spaces(pfile);
    if (t->id != ID)
        fatal("expect identifier");

    bool b = defined(pfile, t);
    bool skip = id == IFDEF ? !b : b;

    if_sentinel(pfile, &(struct ifstack){.id = id,.src = src,.b = !skip});
    
    t = skip_spaces(pfile);
    if (!IS_NEWLINE(t) && t->id != EOI) {
        error("extra tokens in '%s' directive", id2s(id));
        skipline(pfile);
    }
    if (skip)
        skip_ifstack(pfile);
}

static inline void do_ifdef(struct file *pfile)
{
    do_ifdef_section(pfile, IFDEF);
}

static inline void do_ifndef(struct file *pfile)
{
    do_ifdef_section(pfile, IFNDEF);
}

static void do_include(struct file *pfile)
{
    struct token *t = header_name(pfile);
    if (t) {
        include_file(pfile, TOK_LIT_STR(t), t->kind == '<');
    } else {
        // # include pptokens newline
        struct source src = source;
        struct vector *v = vec_new();
        for (;;) {
            struct token *t = skip_spaces(pfile);
            if (IS_NEWLINE(t) || t->id == EOI)
                break;
            vec_push(v, t);
        }

        struct vector *r = expandv(pfile, v);
        if (vec_empty(r)) {
            error_at(src, "empty filename");
            return;
        }

        struct token *tok = vec_head(r);
        if (tok->id == SCONSTANT) {
            include_file(pfile, unwrap_scon(TOK_LIT_STR(tok)), false);
            for (int i = 1; i < vec_len(r); i++) {
                struct token *t = vec_at(r, i);
                if (!IS_SPACE(t)) {
                    error_at(t->src,
                             "extra tokens at end of #include directive '%s'",
                             tok2s(t));
                    break;
                }
            }
        } else if (tok->id == '<') {
            struct token *tail = vec_tail(r);
            struct strbuf *s = strbuf_new();
            for (int i = 1; i < vec_len(r) - 1; i++) {
                struct token *t = vec_at(r, i);
                strbuf_cats(s, tok2s(t));
            }
            strbuf_strip(s);
            if (tail->id != '>')
                error_at(src,
                         "expected \"FILENAME\" or <FILENAME>");
            else if (strbuf_len(s) == 0)
                error_at(src, "empty filename");
            else
                include_file(pfile, s->str, true);
        } else {
            error_at(src, "expected \"FILENAME\" or <FILENAME>");
        }
    }
}

static struct vector *arg(struct file *pfile)
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
        t = lex(pfile);
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
    unget(pfile, t);
    return v;
}

static struct vector *arguments(struct file *pfile, struct macro *m)
{
    struct vector *v = vec_new();
    struct vector *commas = vec_new();
    struct token *t;
    for (;;) {
        struct vector *r = arg(pfile);
        vec_push(v, r);
        t = lex(pfile);
        if (t->id == ')' || t->id == EOI)
            break;
        assert(t->id == ',');
        vec_push(commas, t);
    }
    if (t->id != ')')
        error("unterminated function-like macro invocation");
    else
        unget(pfile, t);

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

    size_t lenv = vec_len(v);
    size_t lenp = m->nparams;
    // check args and params
    if (lenv < lenp) {
        error("too few arguments provided to function-like macro invocation");
    } else if (lenv > lenp) {
        if (m->varg) {
            // merge 'variable arguments'
            struct vector *v2 = vec_new();
            for (int i = lenp; i < lenv; i++) {
                vec_add(v2, vec_at(v, i));
                if (i != lenv - 1)
                    vec_push(v2, vec_at(commas, i));
            }
            int i = lenv - lenp;
            while (i--)
                vec_pop(v);
            vec_push(v, v2);
        } else {
            error("too many arguments provided to function-like macro invocation");
        }
    }

    return v;
}

static void parameters(struct file *pfile, struct macro *m)
{
    unsigned int n = 0;
    unsigned int i = 0;
    struct token **v = NULL;
    struct token *t = skip_spaces(pfile);

    switch (t->id) {
    case ')':
        // ()
        break;
    case ELLIPSIS:
        // (...)
        m->varg = true;
        t = skip_spaces(pfile);
        if (t->id != ')') {
            error("expect ')'");
            unget(pfile, t);
        }
        break;
    case ID:
        // (a,b,c,...)
        for (;;) {
            if (t->id == ID) {
                for (unsigned int j = 0; j < i; j++) {
                    struct token *t1 = v[j];
                    if (!strcmp(TOK_ID_STR(t), TOK_ID_STR(t1))) {
                        error("duplicate macro paramter name '%s'",
                              tok2s(t));
                        break;
                    }
                }
                if (i >= n) {
                    n = n * 2 + 16;
                    v = xrealloc(v, n * sizeof(struct token *));
                }
                t->param = true;
                t->pos = i;
                v[i++] = t;
            } else if (t->id == ELLIPSIS) {
                m->varg = true;
                t = skip_spaces(pfile);
                break;
            } else {
                error("expect identifier or ...");
            }
            t = skip_spaces(pfile);
            if (t->id != ',')
                break;
            t = skip_spaces(pfile);
        }
        if (t->id != ')') {
            error_at(t->src, "unterminated macro parameter list");
            unget(pfile, t);
        }
        break;
    default:
        error("expect identifier list or ')' or ...");
        unget(pfile, t);
        skipline(pfile);
        break;
    }
    m->nparams = i;
    m->params = v;
}

static struct cpp_ident *lookup_macro(struct file *pfile, struct token *t)
{
    struct ident *id = t->value.ident;
    struct cpp_ident *ident;

    ident = IMAP_LOOKUP_HASH(pfile->imap, id, IMAP_SEARCH);
    if (ident && ident->type == CT_MACRO)
        return ident;
    else
        return NULL;
}

static void add_macro(struct file *pfile, struct ident *id, struct macro *m)
{
    static const char *builtins[] = {
        "__STDC__",
        "__STDC_VERSION__",
        "__STDC_HOSTED__"
    };
    for (int i = 0; i < ARRAY_SIZE(builtins); i++) {
        if (!strcmp((const char *)id->str, builtins[i]))
            m->builtin = true;
    }
    struct cpp_ident *ident;
    ident = IMAP_LOOKUP_HASH(pfile->imap, id, IMAP_CREATE);
    ident->type = CT_MACRO;
    ident->value.macro = m;
}

static void add_macro_with_name(struct file *pfile, const char *name, struct macro *m)
{
    struct ident *id = imap_lookup(pfile->imap,
                                   (const unsigned char *)name,
                                   strlen(name), IMAP_CREATE);
    add_macro(pfile, id, m);
}

static void remove_macro(struct file *pfile, struct token *t)
{
    struct cpp_ident *ident;

    ident = lookup_macro(pfile, t);

    if (ident) {
        struct macro *m = ident->value.macro;
        if (m->builtin) {
            error("Can't undefine predefined macro '%s'", tok2s(t));
        } else {
            ident->type = 0;
            ident->value.macro = NULL;
        }
    }
}

static void ensure_macro_def(struct file *pfile, struct token *t, struct macro *m)
{
    // check redefinition
    const char *name = TOK_ID_STR(t);
    struct cpp_ident *ident = lookup_macro(pfile, t);
    struct macro *m1 = ident ? ident->value.macro : NULL;
    size_t len1p = m->nparams;
    size_t len1b = m->nbody;
    
    if (m1) {
        if (m1->builtin) {
            error_at(t->src, "Can't redefine predefined macro '%s'",
                     name);
        } else {
            size_t len2p = m1->nparams;
            size_t len2b = m1->nbody;
            
            // compare definition
            if (m->kind != m1->kind ||
                m->varg != m1->varg ||
                len1p != len2p ||
                len1b != len2b)
                goto redef;

            for (size_t i = 0; i < len1p; i++) {
                struct token *t1 = m->params[i];
                struct token *t2 = m1->params[i];
                if (strcmp(TOK_ID_STR(t1),
                           TOK_ID_STR(t2)))
                    goto redef;
            }

            for (size_t i = 0; i < len1b; i++) {
                struct token *t1 = m->body[i];
                struct token *t2 = m1->body[i];
                if (strcmp(tok2s(t1), tok2s(t2)))
                    goto redef;
            }
            // equal definition
            return;

        redef:
            error_at(t->src,
                     "'%s' macro redefinition, previous definition at %s:%u:%u",
                     name, m1->src.file, m1->src.line,
                     m1->src.column);
        }
        return;
    }

    if (!strcmp(name, "defined"))
        error_at(t->src, "'defined' cannot be used as a macro name");

    for (size_t i = 0; i < len1b; i++) {
        struct token *t = m->body[i];
        if (t->id == SHARPSHARP) {
            if (i == 0)
                error_at(t->src,
                         "'##' cannot appear at the beginning of a replacement list");
            else if (i == len1b - 1)
                error_at(t->src,
                         "'##' cannot appear at the end of a replacement list");
        } else if (t->id == '#') {
            struct token *t1 = i + 1 < len1b ? m->body[i+1] : NULL;
            if (m->kind != MACRO_FUNC ||
                t1 == NULL ||
                !t1->param)
                error_at(t->src,
                         "'#' is not followed by a macro parameter");
        }
    }
}

static void replacement_list(struct file *pfile, struct macro *m)
{
    unsigned int n = 0;
    unsigned int i = 0;
    struct token **v = NULL;
    struct token *t = skip_spaces(pfile);
    bool space = false;
    for (;;) {
        if (IS_NEWLINE(t) || t->id == EOI)
            break;
        t->space = space;
        if (i >= n) {
            n = n * 2 + 64;
            v = xrealloc(v, n * sizeof(struct token *));
        }
        if (m->kind == MACRO_FUNC && t->id == ID) {
            const char *name = TOK_ID_STR(t);
            if (m->varg && !strcmp(name, "__VA_ARGS__")) {
                t->param = true;
                t->pos = m->nparams;
            } else {
                for (size_t i = 0; i < m->nparams; i++) {
                    struct token *t0 = m->params[i];
                    if (!strcmp(name, TOK_ID_STR(t0))) {
                        t->param = true;
                        t->pos = t0->pos;
                    }
                }
            }
        }
        v[i++] = t;
        // skip spaces and record
        space = false;
    beg:
        t = lex(pfile);
        if (IS_SPACE(t)) {
            space = true;
            goto beg;
        }
    }
    m->nbody = i;
    m->body = v;
}

static void define_objlike_macro(struct file *pfile, struct token *t)
{
    struct macro *m = NEWS0(struct macro, PERM);
    SAVE_ERRORS;
    m->kind = MACRO_OBJ;
    m->src = t->src;
    replacement_list(pfile, m);
    ensure_macro_def(pfile, t, m);
    if (NO_ERROR)
        add_macro(pfile, t->value.ident, m);
}

static void define_funclike_macro(struct file *pfile, struct token *t)
{
    struct macro *m = NEWS0(struct macro, PERM);
    SAVE_ERRORS;
    m->kind = MACRO_FUNC;
    m->src = t->src;
    parameters(pfile, m);
    replacement_list(pfile, m);
    ensure_macro_def(pfile, t, m);
    if (NO_ERROR)
        add_macro(pfile, t->value.ident, m);
}

static struct token *read_identifier(struct file *pfile)
{
    struct token *t = skip_spaces(pfile);
    if (t->id != ID) {
        error("expect identifier at '%s'", tok2s(t));
        unget(pfile, t);
    }
    return t;
}

static void do_define(struct file *pfile)
{
    struct token *id = read_identifier(pfile);
    if (id->id != ID) {
        skipline(pfile);
        return;
    }
    struct token *t = lex(pfile);
    if (t->id == '(') {
        define_funclike_macro(pfile, id);
    } else {
        unget(pfile, t);
        define_objlike_macro(pfile, id);
    }
}

static void do_undef(struct file *pfile)
{
    struct token *t = read_identifier(pfile);
    if (t->id != ID) {
        skipline(pfile);
        return;
    }
    remove_macro(pfile, t);
    t = skip_spaces(pfile);
    if (!IS_NEWLINE(t) && t->id != EOI) {
        warning("extra tokens at the end of #undef directive");
        skipline(pfile);
    }
}

static void do_line(struct file *pfile)
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

    struct token *t = skip_spaces(pfile);
    // TODO: must be an integer, not floating
    if (t->id != NCONSTANT) {
        error("expect integer constant");
        unget(pfile, t);
        skipline(pfile);
        return;
    }
    const char *name;
    struct token *t2 = skip_spaces(pfile);
    if (t2->id == SCONSTANT) {
        name = format("# %s %s\n", tok2s(t), tok2s(t2));
    } else {
        name = format("# %s \"%s\"\n", tok2s(t), pfile->buffer->name);
        unget(pfile, t2);
    }
    skipline(pfile);
    unget(pfile, new_token(&(struct token) {
                .id = LINENO, .value.lexeme = name}));
}

static const char *tokens2s(struct vector *v)
{
    struct strbuf *s = strbuf_new();
    for (int i = 0; i < vec_len(v); i++) {
        struct token *t = vec_at(v, i);
        strbuf_cats(s, tok2s(t));
    }
    const char *ret = strbuf_str(strbuf_strip(s));
    return ret ? ret : "";
}

static void do_message_line(struct file *pfile, int level)
{
    struct source src = source;
    struct vector *v = vec_new();
    struct token *t;
    for (;;) {
        t = lex(pfile);
        if (IS_NEWLINE(t) || t->id == EOI)
            break;
        vec_push(v, t);
    }

    const char *message = tokens2s(v);
    if (level == WRN)
        warning_at(src, message);
    else
        error_at(src, message);
}

static inline void do_error(struct file *pfile)
{
    do_message_line(pfile, ERR);
}

static inline void do_warning(struct file *pfile)
{
    do_message_line(pfile, WRN);
}

static void do_pragma(struct file *pfile)
{
    struct source src = source;
    struct token *t;
    for (;;) {
        t = skip_spaces(pfile);
        if (IS_NEWLINE(t) || t->id == EOI)
            break;
    }
    warning_at(src, "pragma directive not supported yet");
}

static void directive(struct file *pfile)
{
    struct token *t = skip_spaces(pfile);
    if (IS_NEWLINE(t) || t->id == EOI)
        return;
    // TODO: must be an integer, not floating
    if (t->id == NCONSTANT) {
        unget(pfile, t);
        do_line(pfile);
        return;
    }
    if (t->id != ID)
        goto err;

    const char *name = TOK_ID_STR(t);
    int len = ARRAY_SIZE(directive_table);
    for (int i = 0; i < len; i++) {
        struct directive_table tb = directive_table[i];
        if (!strcmp(name, tb.name)) {
            tb.handler(pfile);
            return;
        }
    }
 err:
    warning("unknown preprocess directive '%s'", tok2s(t));
    skipline(pfile);
}

static struct vector *expandv(struct file *pfile, struct vector *v)
{
    struct vector *r = vec_new();

    // create a temp file
    // so that get_pptok will not
    // generate 'unterminated conditional directive'
    buffer_sentinel(pfile,
                    with_tokens(vec_reverse(v), pfile->buffer),
                    BS_RETURN_EOI);
    for (;;) {
        struct token *t = expand(pfile);
        if (t->id == EOI)
            break;
        vec_push(r, t);
    }
    buffer_unsentinel(pfile);

    return r;
}

// parse the input string to a token
static struct token *with_tmp_lex(struct file *pfile, const char *input)
{
    struct source src = source;

    buffer_sentinel(pfile,
                    with_string(input, "<lex>"),
                    BS_RETURN_EOI);

    struct token *t = lex(pfile);
    struct token *t1 = lex(pfile);
    if (!IS_NEWLINE(t1))
        unget(pfile, t1);
    if (peek(pfile)->id != EOI) {
        struct token *t2 = lex(pfile);
        error_at(src,
                 "pasting formed '%s%s', an invalid preprocessing token",
                 tok2s(t), tok2s(t2));
    }

    buffer_unsentinel(pfile);
    
    return t;
}

static struct vector *hsadd(struct vector *r, struct hideset *hideset)
{
    size_t len = vec_len(r);
    for (size_t i = 0; i < len; i++) {
        struct token *t = vec_at(r, i);
        t->hideset = hideset_union(t->hideset, hideset);
    }
    return r;
}

/**
 * Paste last of left side with first of right side.
 * The 'rs' is selected with no leading spaces and trailing spaces.
 */
static struct vector *glue(struct file *pfile, struct vector *ls, struct vector *rs)
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
    const char *str = format("%s%s", tok2s(ltok), tok2s(rtok));
    struct token *t = with_tmp_lex(pfile, str);
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
        const char *name = tok2s(t);
        if (t->id == SCONSTANT ||
            (t->id == NCONSTANT &&
             (name[0] == '\'' || name[0] == 'L')))
            // Any embedded quotation or backslash characters
            // are preceded by a backslash character to preserve
            // their meaning in the string.
            strbuf_cats(s, backslash(name));
        else
            strbuf_cats(s, name);
    }
    strbuf_cats(s, "\"");
    return new_token(&(struct token) {
            .id = SCONSTANT, .value.lexeme = s->str});
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

static struct vector *subst(struct file *pfile,
                            struct macro *m,
                            struct vector *args,
                            struct hideset *hideset)
{
    struct vector *r = vec_new();
    struct token **body = m->body;
    size_t len = m->nbody;

#define PUSH_SPACE(r, t)    if (t->space) vec_push(r, space_token)

    for (size_t i = 0; i < len; i++) {
        struct token *t0 = body[i];
        struct token *t1 = i + 1 < len ? body[i+1] : NULL;
        bool t0_inparams = t0->param;
        bool t1_inparams = t1 && t1->param;

        if (t0->id == '#' && t1_inparams) {
            struct vector *iv = select(args, t1->pos);
            struct token *ot = stringize(iv);
            PUSH_SPACE(r, t0);
            vec_push(r, ot);
            i++;

        } else if (t0->id == SHARPSHARP && t1_inparams) {

            struct vector *iv = select(args, t1->pos);
            if (vec_len(iv))
                r = glue(pfile, r, iv);
            i++;

        } else if (t0->id == SHARPSHARP && t1) {

            hideset = t1->hideset;
            r = glue(pfile, r, vec_new1(t1));
            i++;

        } else if (t0_inparams && (t1 && t1->id == SHARPSHARP)) {

            hideset = t1->hideset;
            struct vector *iv = select(args, t0->pos);
            if (vec_len(iv)) {
                PUSH_SPACE(r, t0);
                vec_add(r, iv);
            } else {
                // add a space
                vec_push(r, space_token);

                struct token *t2 = i + 2 < len ? body[i+2] : NULL;
                bool t2_inparams = t2 && t2->param;
                if (t2_inparams) {
                    struct vector *iv2 = select(args, t2->pos);
                    vec_add(r, iv2);
                    i++;
                }
                i++;
            }

        } else if (t0_inparams) {

            struct vector *iv = select(args, t0->pos);
            struct vector *ov = expandv(pfile, iv);
            PUSH_SPACE(r, t0);
            vec_add(r, ov);

        } else {
            PUSH_SPACE(r, t0);
            vec_push(r, t0);
        }
    }
    return hsadd(r, hideset);
}

static struct token *expand(struct file *pfile)
{
    struct token *t;
 start:
    t = lex(pfile);
    if (t->id != ID)
        return t;

    struct ident *id = t->value.ident;
    const unsigned char *name = id->str;
    struct cpp_ident *ident;

    ident = IMAP_LOOKUP_HASH(pfile->imap, id, IMAP_SEARCH);

    if (ident == NULL ||
        ident->type != CT_MACRO ||
        hideset_has(t->hideset, name))
        return t;

    struct macro *m = ident->value.macro;

    switch (m->kind) {
    case MACRO_OBJ:
        {
            struct hideset *hdset = hideset_add(t->hideset, name);
            struct vector *v = subst(pfile, m, NULL, hdset);
            ungetv(pfile, v);
            goto start;
        }
    case MACRO_FUNC:
        {
            if (peek(pfile)->id != '(')
                return t;
            SAVE_ERRORS;
            skip_spaces(pfile);
            struct vector *args = arguments(pfile, m);
            if (NO_ERROR) {
                struct token *rparen = skip_spaces(pfile);
                assert(rparen->id == ')');
                struct hideset *hdset =
                    hideset_add(hideset_intersection
                                (t->hideset, rparen->hideset),
                                name);
                struct vector *v = subst(pfile, m, args, hdset);
                ungetv(pfile, v);
                goto start;
            } else {
                return t;
            }
        }
        break;
    case MACRO_SPECIAL:
        m->handler(pfile, t);
        goto start;
    default:
        assert(0);
    }
}

static void file_handler(struct file *pfile, struct token *t)
{
    const char *file = pfile->buffer->name;
    const char *name = format("\"%s\"", file);
    struct token *tok = new_token(&(struct token){
            .id = SCONSTANT, .value.lexeme = name, .src = t->src });
    unget(pfile, tok);
}

static void line_handler(struct file *pfile, struct token *t)
{
    unsigned line = pfile->buffer->line;
    const char *name = strd(line);
    struct token *tok = new_token(&(struct token){
            .id = NCONSTANT, .value.lexeme = name, .src = t->src });
    unget(pfile, tok);
}

static void date_handler(struct file *pfile, struct token *t)
{
    struct token *tok = new_token(&(struct token){
            .id = SCONSTANT, .value.lexeme = pfile->date, .src = t->src });
    unget(pfile, tok);
}

static void time_handler(struct file *pfile, struct token *t)
{
    struct token *tok = new_token(&(struct token){
            .id = SCONSTANT, .value.lexeme = pfile->time, .src = t->src });
    unget(pfile, tok);
}

static void define_special(struct file *pfile,
                           const char *name,
                           void (*handler) (struct file *, struct token *))
{
    struct macro *m = NEWS0(struct macro, PERM);
    m->kind = MACRO_SPECIAL;
    m->handler = handler;
    add_macro_with_name(pfile, name, m);
}

static void add_include(struct vector *v, const char *name)
{
    vec_push(v, (char *)sys_abspath(name));
}

static struct token *lineno(unsigned line, const char *file)
{
    const char *name = format("# %u \"%s\"\n", line, file);
    struct token *t = new_token(&(struct token){
            .id = LINENO, .value.lexeme = name, .src.file = "<built-in>" });
    return t;
}

static const char *find_header(struct file *pfile, const char *name, bool isstd)
{
    struct vector *paths;
    if (isstd)
        paths = pfile->std_include_paths;
    else
        paths = pfile->usr_include_paths;

    size_t len = vec_len(paths);
    for (size_t i = 0; i < len; i++) {
        const char *dir = vec_at(paths, i);
        const char *file = sys_join(dir, name);
        if (file_exists(file))
            return file;
    }

    if (!isstd) {
        // try current path
        const char *curdir = sys_dirname(pfile->buffer->name);
        const char *file = sys_join(curdir, name);
        if (file_exists(file))
            return file;
    }
    return NULL;
}

static void include_file(struct file *pfile, const char *file, bool std)
{
    const char *path;

    if (!file) {
        error("empty filename");
        return;
    }
    
    path = find_header(pfile, file, std);

    if (path) {
        buffer_sentinel(pfile, with_file(path), BS_CONTINUOUS);
        unget(pfile, lineno(1, pfile->buffer->name));
    } else {
        fatal("'%s' file not found", file);
    }
}

static void include_cmdline(struct file *pfile, const char *command)
{
    buffer_sentinel(pfile,
                    with_string(command, "<command-line>"),
                    BS_CONTINUOUS);
    unget(pfile, lineno(1, pfile->buffer->name));
}

static void init_builtin_macros(struct file *pfile)
{
    define_special(pfile, "__FILE__", file_handler);
    define_special(pfile, "__LINE__", line_handler);
    define_special(pfile, "__DATE__", date_handler);
    define_special(pfile, "__TIME__", time_handler);
    include_file(pfile, BUILTIN_HEADER, true);
}

static void init_env(struct file *pfile)
{
    setlocale(LC_ALL, "C");
    time_t t = time(NULL);
    struct tm *now = localtime(&t);
    // mmm dd yyyy
    char datestr[20];
    strftime(datestr, sizeof(datestr), "%b %e %Y", now);
    pfile->date = format("\"%s\"", datestr);
    // hh:mm:ss
    char timestr[10];
    strftime(timestr, sizeof(timestr), "%T", now);
    pfile->time = format("\"%s\"", timestr);
}

static void init_include_path(struct file *pfile)
{
    // add system include paths
    struct vector *sys_include_paths = sys_include_dirs();
    for (int i = 0; i < vec_len(sys_include_paths); i++) {
        const char *dir = vec_at(sys_include_paths, i);
        add_include(pfile->std_include_paths, dir);
    }
}

void cpp_init(int argc, char *argv[])
{
    const char *ifile = NULL;
    struct strbuf *s = strbuf_new();
    struct vector *v = vec_new();

    for (int i = 1; i < argc; i++) {
        const char *arg = argv[i];
        if (!strncmp(arg, "-I", 2)) {
            if (arg[2])
                vec_push(v, (char *)arg + 2);
        } else if (!strncmp(arg, "-D", 2)) {
            if (arg[2] == 0)
                continue;
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
            if (arg[2])
                strbuf_cats(s, format("#undef %s\n", arg + 2));
        } else if (arg[0] != '-' || !strcmp(arg, "-")) {
            if (ifile == NULL)
                ifile = arg;
        }
    }

    if (ifile == NULL || !strcmp(ifile, "-"))
        ifile = "<stdin>";
    
    cpp_file = new_cpp_file(ifile);
    init_env(cpp_file);
    init_include_path(cpp_file);
    init_builtin_macros(cpp_file);

    for (int i = 0; i < vec_len(v); i++) {
        const char *dir = vec_at(v, i);
        add_include(cpp_file->usr_include_paths, dir);
    }
    
    if (strbuf_len(s))
        include_cmdline(cpp_file, s->str);
}

/* Getting one expanded token.
 */
struct token *get_pptok(struct file *pfile)
{
    for (;;) {
        struct token *t = expand(pfile);
        if (t->id == EOI) {
            struct ifstack *stack = pfile->buffer->ifstack;
            if (stack)
                error_at(stack->src, "unterminated conditional directive");
            if (pfile->buffer->return_eoi) {
                return t;
            } else {
                buffer_unsentinel(pfile);
                if (pfile->buffer)
                    return lineno(pfile->buffer->line,
                                  pfile->buffer->name);
                else
                    return t;
            }
        }
        if (t->id == '#' && t->bol) {
            directive(pfile);
            continue;
        }
        return t;
    }
}
