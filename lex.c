#include "cc.h"

static unsigned char map[256] = {
#define _a(a, b, c, d)  c,
#define _x(a, b, c, d)
#define _t(a, b, c)
#define _k(a, b, c)
#include "token.def"
    OTHER,
};

static const char *tnames[] = {
#define _a(a, b, c, d)  b,
#define _x(a, b, c, d)  b,
#define _t(a, b, c)     b,
#define _k(a, b, c)     b,
#include "token.def"
};

static struct token *eoi_token = &(struct token){.id = EOI, .lexeme = "EOI"};
static struct token *newline_token = &(struct token){.id = '\n', .lexeme = "\n"};
struct token *space_token = &(struct token){.id = ' ', .lexeme = " "};

struct source source;

#define iswhitespace(ch)  (map[ch] & BLANK)
#define isnewline(ch)     (map[ch] & NEWLINE)
#define isdigitletter(ch) (map[ch] & (DIGIT|LETTER))
#define INCLINE(fs, col)  do {                  \
        fs->line++;                             \
        fs->column = col;                       \
    } while (0)

#define MARKC(fs)  do {                         \
        source.file = fs->name;                 \
        source.line = fs->line;                 \
        source.column = fs->column;             \
    } while (0)

#define SET_COLUMN(fs, col)  fs->column = col

const char *id2s(int t)
{
    if (t < 0)
        return "(null)";
    else if (t < 128)
        return tnames[t];
    else if (t < 256)
        return "(null)";
    else if (t < TOKEND)
        return tnames[128 + t - ID];
    else
        return "(null)";
}

int isletter(int c)
{
    return map[c] & LETTER;
}

int isxalpha(int c)
{
    return map[c] & HEX;
}

static void add_line_note(struct buffer *pb, const unsigned char *pos, int type)
{
    if (pb->notes_used == pb->notes_alloc) {
        pb->notes_alloc = pb->notes_alloc * 2 + 200;
        pb->notes = xrealloc(pb->notes, pb->notes_alloc * sizeof(struct line_note));
    }
    pb->notes[pb->notes_used].pos = pos;
    pb->notes[pb->notes_used].type = type;
    pb->notes_used++;
}

static void process_line_notes(struct buffer *pb)
{    
    for (;;) {
        struct line_note *note = &pb->notes[pb->cur_note];

        if (note->pos > pb->cur)
            break;

        pb->cur_note++;
        
        if (note->type == '\\') {
            pb->line_base = note->pos;
            INCLINE(pb, 0);
        } else {
            assertf(0, "unexpected line note type:%d", note->type);
        }
    }
}

#if defined (__GNUC__) && defined (CONFIG_LINUX) && HAVE_SSE2

#include <xmmintrin.h>

static unsigned char repl_chars[2][16] __attribute__((aligned(16))) = {
    { '\n', '\n', '\n', '\n', '\n', '\n', '\n', '\n',
      '\n', '\n', '\n', '\n', '\n', '\n', '\n', '\n' },
    { '\\', '\\', '\\', '\\', '\\', '\\', '\\', '\\',
      '\\', '\\', '\\', '\\', '\\', '\\', '\\', '\\' }
};

const unsigned char *search_line_fast(const unsigned char *s,
                                      const unsigned char *limit)
{
    const __v16qi repl_nl = *(const __v16qi *)repl_chars[0];
    const __v16qi repl_bk = *(const __v16qi *)repl_chars[1];
    unsigned int mask, misalign, result;
    const __v16qi *p;
    __v16qi data, t;

    misalign = (uint64_t)s & 15;
    p = (const __v16qi *)((uint64_t)s & -16);
    data = *p;
    mask = -1u << misalign;

    goto start;
    do {
        data = *++p;
        mask = -1;
        
    start:
        // aligned
        t = __builtin_ia32_pcmpeqb128(data, repl_nl);
        t |= __builtin_ia32_pcmpeqb128(data, repl_bk);
        result = __builtin_ia32_pmovmskb128(t);
        result &= mask;
        
    } while (!result);

    result = __builtin_ctz(result);
    return (const unsigned char *)p + result;
}

#else

const unsigned char *search_line_fast(const unsigned char *s,
                                      const unsigned char *limit)
{
    while (*s != '\n' && *s != '\\')
            s++;
    return s;
}

#endif

// return an unescaped logical line.
static void next_clean_line(struct buffer *pb)
{
    const unsigned char *s;
    unsigned char *d;
    unsigned char c;
    const unsigned char *pbackslash = NULL;

    pb->cur_note = pb->notes_used = 0;
    pb->cur = pb->line_base = pb->next_line;
    pb->need_line = false;
    s = pb->next_line;
    
    while (1) {
        // search '\n', '\\'
        s = search_line_fast(s, pb->limit);

        c = *s;
        if (c == '\\')
            pbackslash = s++;
        else
            break;
    }

    // d must be '\n'
    d = (unsigned char *)s;

    if (d == pb->limit)
        goto done;
    if (pbackslash == NULL)
        goto done;
    if (d - 1 != pbackslash)
        goto done;

    // Have an escaped newline
    add_line_note(pb, d - 1, '\\');
    d -= 2;
    
    while (1) {
        c = *++s;
        *++d = c;

        if (c == '\n') {
            if (s == pb->limit)
                break;
            if (d[-1] != '\\')
                break;

            add_line_note(pb, d - 1, '\\');
            d -= 2;
        }
    }
    
 done:
    *d = '\n';
    /* a sentinel note that should never be processed. */
    add_line_note(pb, d + 1, '\n');
    pb->next_line = s + 1;
}

struct token *new_token(struct token *tok)
{
    struct token *t = alloc_token();
    memcpy(t, tok, sizeof(struct token));
    if (!tok->lexeme)
        t->lexeme = id2s(tok->id);
    return t;
}

static struct token *make_token(struct buffer *pb, int id, const char *name)
{
    struct token *t = alloc_token();
    t->id = id;
    t->lexeme = name;
    t->src = source;
    t->bol = pb->bol;
    pb->bol = false;
    return t;
}

static void line_comment(struct file *pfile)
{
    struct buffer *pb = pfile->current;
    while (*pb->cur != '\n')
        pb->cur++;
    process_line_notes(pb);
}

// fs->cur points to the initial asterisk of the comment.
static void block_comment(struct file *pfile)
{
    struct buffer *pb = pfile->current;
    const unsigned char *rpc = pb->cur;
    unsigned char ch;
    rpc++;
    
    for (;;) {
        ch = *rpc++;
        if (ch == '/' && rpc[-2] == '*') {
            break;
        } else if (ch == '\n') {
            pb->cur = rpc - 1;
            process_line_notes(pb);
            if (pb->next_line >= pb->limit) {
                error("unterminated /* comment");
                return;
            }
            next_clean_line(pb);
            INCLINE(pb, 0);
            rpc = pb->cur;
        }
    }

    pb->cur = rpc;
    process_line_notes(pb);
}

// fs->cur points at prior initial digit or dot.
static struct token *ppnumber(struct file *pfile)
{
    struct buffer *pb = pfile->current;
    const unsigned char *rpc = pb->cur - 1;
    int ch;
    for (;;) {
        ch = *pb->cur++;
        if (!isdigitletter(ch) && ch != '.') {
            pb->cur--;
            break;
        }
        bool is_float = strchr("eEpP", ch) && strchr("+-", *pb->cur);
        if (is_float)
            pb->cur++;
    }
    const char *name = xstrndup((const char *)rpc, pb->cur - rpc);
    return make_token(pb, NCONSTANT,  name);
}

static struct token *sequence(struct file *pfile, bool wide, int sep)
{
    struct buffer *pb = pfile->current;
    const unsigned char *rpc = pb->cur - 1;
    bool is_char = sep == '\'';
    if (wide) pb->cur++;
    const char *name;
    int ch;
    for (;;) {
        ch = *pb->cur++;
        if (ch == sep || isnewline(ch))
            break;
        if (ch == '\\')
            pb->cur++;
    }

    if (ch != sep) {
        char *str = xstrndup((const char *)rpc, pb->cur - rpc + 1);
        str[pb->cur - rpc] = sep;
        name = str;
        error("untermiated %s constant: %s",
              is_char ? "character" : "string", name);
    } else {
        name = xstrndup((const char *)rpc, pb->cur - rpc);
    }

    if (is_char)
        return make_token(pb, NCONSTANT, name);
    else
        return make_token(pb, SCONSTANT, name);
}

static struct ident *identifier(struct file *pfile)
{
    struct buffer *pb = pfile->current;
    const unsigned char *rpc = pb->cur - 1;
    unsigned int hash = IMAP_HASHSTEP(0, *rpc);
    unsigned int len;
    
    while (isdigitletter(*pb->cur)) {
        hash = IMAP_HASHSTEP(hash, *pb->cur);
        pb->cur++;
    }
    len = pb->cur - rpc;
    hash = IMAP_HASHFINISH(hash, len);
    return imap_lookup_with_hash(pfile->imap,
                                 rpc, len, hash, IMAP_CREATE);
}

static struct token *dolex(struct file *pfile)
{
    register const unsigned char *rpc;
    int id;
    struct token *result;
    const char *name = NULL;
    struct ident *ident = NULL;
    struct buffer *pb = pfile->current;

    if (pb->need_line)
        next_clean_line(pb);
    // pb->buf maybe NULL
    if (pb->cur >= pb->limit)
        return eoi_token;
    
 start:
    if (pb->cur >= pb->notes[pb->cur_note].pos)
        process_line_notes(pb);
    SET_COLUMN(pb, pb->cur - pb->line_base);
    rpc = pb->cur++;
    MARKC(pb);

    switch (*rpc) {
    case '\n':
        if (rpc >= pb->limit) {
            return eoi_token;
        } else {
            pb->need_line = true;
            pb->bol = true;
            newline_token->src = source;
            INCLINE(pb, 0);
            return newline_token;
        }

        // spaces
    case '\t':
    case '\v':
    case '\f':
    case '\r':
    case ' ':
        do
            rpc++;
        while (iswhitespace(*rpc));
        pb->cur = rpc;
        space_token->src = source;
        return space_token;

        // punctuators
    case '/':
        if (rpc[1] == '/') {
            line_comment(pfile);
            goto start;
        } else if (rpc[1] == '*') {
            block_comment(pfile);
            goto start;
        } else if (rpc[1] == '=') {
            pb->cur++;
            id = DIVEQ;
        } else {
            id = '/';
        }
        break;

    case '+':
        if (rpc[1] == '+') {
            pb->cur++;
            id = INCR;
        } else if (rpc[1] == '=') {
            pb->cur++;
            id = ADDEQ;
        } else {
            id = '+';
        }
        break;

    case '-':
        if (rpc[1] == '-') {
            pb->cur++;
            id = DECR;
        } else if (rpc[1] == '=') {
            pb->cur++;
            id = MINUSEQ;
        } else if (rpc[1] == '>') {
            pb->cur++;
            id = DEREF;
        } else {
            id = '-';
        }
        break;

    case '*':
        if (rpc[1] == '=') {
            pb->cur++;
            id = MULEQ;
        } else {
            id = '*';
        }
        break;

    case '=':
        if (rpc[1] == '=') {
            pb->cur++;
            id = EQ;
        } else {
            id = '=';
        }
        break;

    case '!':
        if (rpc[1] == '=') {
            pb->cur++;
            id = NEQ;
        } else {
            id = '!';
        }
        break;

    case '%':
        if (rpc[1] == '=') {
            pb->cur++;
            id = MODEQ;
        } else if (rpc[1] == '>') {
            pb->cur++;
            id = '}';
        } else if (rpc[1] == ':' && rpc[2] == '%' && rpc[3] == ':') {
            pb->cur += 3;
            id = SHARPSHARP;
        } else if (rpc[1] == ':') {
            pb->cur++;
            id = '#';
        } else {
            id = '%';
        }
        break;

    case '^':
        if (rpc[1] == '=') {
            pb->cur++;
            id = XOREQ;
        } else {
            id = '^';
        }
        break;

    case '&':
        if (rpc[1] == '=') {
            pb->cur++;
            id = BANDEQ;
        } else if (rpc[1] == '&') {
            pb->cur++;
            id = AND;
        } else {
            id = '&';
        }
        break;

    case '|':
        if (rpc[1] == '=') {
            pb->cur++;
            id = BOREQ;
        } else if (rpc[1] == '|') {
            pb->cur++;
            id = OR;
        } else {
            id = '|';
        }
        break;

    case '<':
        if (rpc[1] == '=') {
            pb->cur++;
            id = LEQ;
        } else if (rpc[1] == '<' && rpc[2] == '=') {
            pb->cur += 2;
            id = LSHIFTEQ;
        } else if (rpc[1] == '<') {
            pb->cur++;
            id = LSHIFT;
        } else if (rpc[1] == '%') {
            pb->cur++;
            id = '{';
        } else if (rpc[1] == ':') {
            pb->cur++;
            id = '[';
        } else {
            id = '<';
        }
        break;

    case '>':
        if (rpc[1] == '=') {
            pb->cur++;
            id = GEQ;
        } else if (rpc[1] == '>' && rpc[2] == '=') {
            pb->cur += 2;
            id = RSHIFTEQ;
        } else if (rpc[1] == '>') {
            pb->cur++;
            id = RSHIFT;
        } else {
            id = '>';
        }
        break;

    case '(': case ')':
    case '{': case '}':
    case '[': case ']':
    case ',': case ';':case '~': case '?':
        id = *rpc;
        break;

    case ':':
        if (rpc[1] == '>') {
            pb->cur++;
            id = ']';
        } else {
            id = ':';
        }
        break;

    case '#':
        if (rpc[1] == '#') {
            pb->cur++;
            id = SHARPSHARP;
        } else {
            id = '#';
        }
        break;

        // constants
    case '\'':
        return sequence(pfile, false, '\'');

    case '"':
        return sequence(pfile, false, '"');

    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
        return ppnumber(pfile);

    case '.':
        if (rpc[1] == '.' && rpc[2] == '.') {
            pb->cur += 2;
            id = ELLIPSIS;
        } else if (isdigit(rpc[1])) {
            return ppnumber(pfile);
        } else {
            id = '.';
        }
        break;

        // identifiers
    case 'L':
        if (rpc[1] == '\'')
            return sequence(pfile, true, '\'');
        else if (rpc[1] == '"')
            return sequence(pfile, true, '"');
        // go through
    case '_':
    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
    case 'g': case 'h': case 'i': case 'j': case 'k': case 'l':
    case 'm': case 'n': case 'o': case 'p': case 'q': case 'r':
    case 's': case 't': case 'u': case 'v': case 'w': case 'x':
    case 'y': case 'z':
    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
    case 'G': case 'H': case 'I': case 'J': case 'K':
    case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R':
    case 'S': case 'T': case 'U': case 'V': case 'W': case 'X':
    case 'Y': case 'Z':
        id = ID;
        ident = identifier(pfile);
        name = (const char *)ident->str;
        break;

    default:
        // illegal character
        if (isgraph(*rpc))
            error("illegal character '%c'", *rpc);
        else
            error("illegal character '\\0%o'", *rpc);
        goto start;
    }

    // done
    result = alloc_token();
    result->id = id;
    result->lexeme = name ? name : id2s(id);
    result->value.ident = ident;
    result->src = source;
    result->bol = pb->bol;
    pb->bol = false;
    return result;
}

static void skipline(struct file *pfile, bool over)
{
    struct buffer *pb = pfile->current;
    while (*pb->cur != '\n')
        pb->cur++;
    if (over) {
        INCLINE(pb, 0);
        next_clean_line(pb);
    }
}

static const char *hq_char_sequence(struct file *pfile, int sep)
{
    struct buffer *pb = pfile->current;
    const unsigned char *rpc = pb->cur;
    int ch;
    const char *name;

    for (;;) {
        ch = *pb->cur++;
        if (ch == sep || isnewline(ch)) {
            pb->cur--;
            break;
        }
    }

    if (ch != sep)
        error("missing '%c' in header name", sep);

    name = xstrndup((const char *)rpc, pb->cur - rpc);
    skipline(pfile, true);
    return name;
}

struct token *header_name(struct file *pfile)
{
    struct buffer *pb = pfile->current;
    
    while (iswhitespace(*pb->cur))
        pb->cur++;

    SET_COLUMN(pb, pb->cur - pb->line_base);
    char ch = *pb->cur++;

    // mark for 'error/warning etc.'
    MARKC(pb);
    if (ch == '<') {
        const char *name = hq_char_sequence(pfile, '>');
        return new_token(&(struct token) {
                .lexeme = name, .kind = ch});
    } else if (ch == '"') {
        const char *name = hq_char_sequence(pfile, '"');
        return new_token(&(struct token) {
                .lexeme = name, .kind = ch});
    } else {
        // pptokens
        pb->cur--;
        return NULL;
    }
}

static void skip_sequence(struct file *pfile, int sep)
{
    struct buffer *pb = pfile->current;
    int ch;
    for (;;) {
        ch = *pb->cur++;
        if (ch == sep || isnewline(ch))
            break;
        if (ch == '\\')
            pb->cur++;
    }
    if (ch != sep)
        pb->cur--;
}

static void skip_spaces(struct file *pfile)
{
    // skip spaces, including comments
    struct buffer *pb = pfile->current;
    int ch;

    for (;;) {
        ch = *pb->cur++;
        if (iswhitespace(ch))
            continue;
        if (ch == '/' && *pb->cur == '/') {
            line_comment(pfile);
            continue;
        }
        if (ch == '/' && *pb->cur == '*') {
            block_comment(pfile);
            continue;
        }
        break;
    }
    pb->cur--;
}

void unget(struct file *pfile, struct token *t)
{
    vec_push(pfile->current->ungets, t);
}

/* Skip part of conditional group.
 */
void skip_ifstack(struct file *pfile)
{
    struct buffer *pb = pfile->current;
    bool bol = true;
    int nest = 0;
    assert(vec_len(pb->ungets) == 0);
    for (;;) {
        if (pb->need_line)
            next_clean_line(pb);
        // skip spaces
        skip_spaces(pfile);
        int ch = *pb->cur++;
        if (pb->cur >= pb->limit)
            break;
        if (isnewline(ch)) {
            bol = true;
            pb->need_line = true;
            continue;
        }
        if (ch == '\'' || ch == '"') {
            skip_sequence(pfile, ch);
            bol = false;
            continue;
        }
        if (ch != '#' || !bol) {
            bol = false;
            continue;
        }
        struct source src = {
            .file = pb->name,
            .line = pb->line,
            .column = pb->column };
        struct token *t = lex(pfile);
        while (IS_SPACE(t))
            t = lex(pfile);
        if (t->id != ID) {
            if (IS_NEWLINE(t)) {
                bol = true;
                pb->need_line = true;
            } else {
                bol = false;
            }
            continue;
        }
        const char *name = t->lexeme;
        if (!strcmp(name, "if") ||
            !strcmp(name, "ifdef") ||
            !strcmp(name, "ifndef")) {
            nest++;
            bol = false;
            continue;
        }
        if (!nest &&
            (!strcmp(name, "elif") ||
             !strcmp(name, "else") ||
             !strcmp(name, "endif"))) {
            // found
            unget(pfile, t);
            struct token *t0 = new_token(&(struct token){
                    .id = '#', .src = src, .bol = true });
            unget(pfile, t0);
            break;
        }
        if (nest && !strcmp(name, "endif")) {
            nest--;
            bol = false;
        }
        skipline(pfile, false);
    }
}

struct token *lex(struct file *pfile)
{
    struct vector *v = pfile->current->ungets;
    struct token *t;
    if (v && v->len)
        t = vec_pop(v);
    else
        t = dolex(pfile);
    MARK(t);
    return t;
}

/* Parser tokens
 */

static struct token *one_token(struct file *pfile)
{
    if (pfile->tokens && pfile->tokens->len) {
        return vec_pop(pfile->tokens);
    } else {
        for (;;) {
            struct token *t = get_pptok(pfile);
            if (IS_SPACE(t) || IS_NEWLINE(t) || IS_LINENO(t))
                continue;
            return t;
        }
    }
}

static struct token *peek_token(struct file *pfile)
{
    struct token *t = one_token(pfile);
    vec_push(pfile->tokens, t);
    return t;
}

const char *unwrap_scon(const char *name)
{
    struct strbuf *s = strbuf_new();

    if (name[0] == '"')
        strbuf_catn(s, name + 1, strlen(name) - 2);
    else
        strbuf_catn(s, name + 2, strlen(name) - 3);

    return strbuf_str(s);
}

static struct token *combine_scons(struct vector *v, bool wide)
{
    struct token *t = new_token(vec_head(v));
    struct strbuf *s = strbuf_new();
    if (wide)
        strbuf_catc(s, 'L');
    strbuf_catc(s, '"');
    for (int i = 0; i < vec_len(v); i++) {
        struct token *ti = vec_at(v, i);
        const char *name = unwrap_scon(ti->lexeme);
        if (name)
            strbuf_cats(s, name);
    }
    strbuf_catc(s, '"');
    t->lexeme = strbuf_str(s);
    return t;
}

static struct token *do_cctoken(struct file *pfile)
{
    struct token *t = one_token(pfile);
    if (t->id == SCONSTANT) {
        struct vector *v = vec_new1(t);
        struct token *t1 = peek_token(pfile);
        bool wide = t->lexeme[0] == 'L';
        while (t1->id == SCONSTANT) {
            if (t1->lexeme[0] == 'L')
                wide = true;
            vec_push(v, one_token(pfile));
            t1 = peek_token(pfile);
        }
        if (vec_len(v) > 1)
            return combine_scons(v, wide);
    }
    return t;
}

static int kinds[] = {
#define _a(a, b, c, d)  d,
#define _x(a, b, c, d)  c,
#define _t(a, b, c)     c,
#define _k(a, b, c)     c,
#include "token.def"
};

static const char *kws[] = {
#define _a(a, b, c, d)
#define _x(a, b, c, d)
#define _t(a, b, c)
#define _k(a, b, c)  b,
#include "token.def"
};

static int kwi[] = {
#define _a(a, b, c, d)
#define _x(a, b, c, d)
#define _t(a, b, c)
#define _k(a, b, c)  a,
#include "token.def"
};

struct token *token;
struct token *ahead_token;

static int tkind(int t)
{
    if (t < 0)
        return 0;
    else if (t < 128)
        return kinds[t];
    else if (t >= ID && t < TOKEND)
        return kinds[128 + t - ID];
    else
        return 0;
}

static struct token *cctoken(struct file *pfile)
{
    struct token *t = do_cctoken(pfile);
    // keywords
    if (t->id == ID) {
        for (int i = 0; i < ARRAY_SIZE(kws); i++) {
            if (!strcmp(t->lexeme, kws[i])) {
                t->id = kwi[i];
                break;
            }
        }
    }
    // set kind finally
    t->kind = tkind(t->id);
    return t;
}

int gettok(void)
{
    if (ahead_token) {
        token = ahead_token;
        ahead_token = NULL;
    } else {
        token = cctoken(cpp_file);
    }
    MARK(token);
    return token->id;
}

struct token *lookahead(void)
{
    if (ahead_token == NULL) {
        ahead_token = cctoken(cpp_file);
        // restore source
        MARK(token);
    }
    return ahead_token;
}

void expect(int t)
{
    if (token->id == t)
        gettok();
    else
        error("expect token '%s'", id2s(t));
}

void match(int t, int follow[])
{
    if (token->id == t) {
        gettok();
    } else {
        int n;
        expect(t);
        for (n = 0; token->id != EOI; gettok()) {
            int *k;
            for (k = follow; *k && *k != token->kind; k++)
                ;        // continue
            if (*k == token->kind)
                break;
        }

        if (n > 0)
            fprintf(stderr, "%d tokens skipped.\n", n);
    }
}

int skipto(int (*test[]) (struct token *))
{
    struct token *t = token;
    int cnt;
    for (cnt = 0; token->id != EOI; cnt++, gettok()) {
        for (int i = 0; test[i]; i++)
            if (test[i](token))
                goto out;
    }
 out:
    if (cnt > 1)
        errorf(t->src,
               "invalid token '%s', %d tokens skipped",
               t->lexeme, cnt);
    else if (cnt)
        errorf(t->src,
               "invalid token '%s'",
               t->lexeme);
    else
        die("nothing skipped, may be an internal error");
    return cnt;
}
