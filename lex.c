#include "cc.h"

enum {
    BLANK = 01, NEWLINE = 02, LETTER = 04,
    DIGIT = 010, HEX = 020, OTHER = 040,
};

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

static struct token *eoi_token = &(struct token){.id = EOI,.name = "EOI" };
struct token *space_token = &(struct token){.id = ' ',.name = " " };
struct token *newline_token = &(struct token){.id = '\n',.name = "\n" };

struct source source;

#define iswhitespace(ch)  (map[ch] & BLANK)
#define isnewline(ch)     (map[ch] & NEWLINE)
#define isdigitletter(ch) (map[ch] & (DIGIT|LETTER))
#define mark(t)           source = t->src

int isletter(int c)
{
    return map[c] & LETTER;
}

int isxalpha(int c)
{
    return map[c] & HEX;
}

static struct source chsrc()
{
    struct file *fs = current_file;
    struct source src;
    src.file = fs->name;
    src.line = fs->line;
    src.column = fs->column;
    return src;
}

static void markc()
{
    struct file *fs = current_file;
    source.file = fs->name;
    source.line = fs->line;
    source.column = fs->column;
}

static void unreadc(int c)
{
    struct file *fs = current_file;
    if (c == EOI || fs->pc == fs->buf)
        return;
    if (fs->pc[-1] != c)
        fatal("an unbuffered character '\\0%o", c);
    if (fs->pc[-1] == '\n') {
        fs->line--;
    } else {
        fs->column--;
    }
    fs->pc--;
}

static int readc(void)
{
    struct file *fs = current_file;
 beg:
    if (fs->pc >= fs->pe)
        return EOI;
    if (fs->pc[0] == '\\' && fs->pc[1] == '\n') {
        fs->pc += 2;
        fs->line++;
        fs->column = 0;
        goto beg;
    } else {
        if (*fs->pc == '\n') {
            fs->line++;
            fs->column = 0;
        } else {
            fs->column++;
        }
        // convert to unsigned char first
        return (unsigned char)(*fs->pc++);
    }
}

static bool next(int c)
{
    int ch = readc();
    if (ch == c)
        return true;
    unreadc(ch);
    return false;
}

static int peek(void)
{
    int ch = readc();
    unreadc(ch);
    return ch;
}

struct token *new_token(struct token *tok)
{
    struct token *t = alloc_token();
    memcpy(t, tok, sizeof(struct token));
    if (!tok->name)
        t->name = id2s(tok->id);
    return t;
}

static struct token *make_token2(int id, const char *name)
{
    struct token *t = alloc_token();
    t->id = id;
    t->name = name ? name : id2s(id);
    t->src = source;
    t->bol = current_file->bol;
    current_file->bol = false;
    return t;
}

#define make_token(id)  make_token2(id, NULL)

static void skipline(bool over)
{
    int ch;
    for (;;) {
        ch = readc();
        if (isnewline(ch) || ch == EOI)
            break;
    }
    if (isnewline(ch) && !over)
        unreadc(ch);
}

static void line_comment(void)
{
    skipline(false);
}

static void block_comment(void)
{
    for (;;) {
        int ch = readc();
        if (ch == '*' && next('/'))
            break;
        if (ch == EOI) {
            error("unterminated /* comment");
            break;
        }
    }
}

static struct token *ppnumber(int c)
{
    struct strbuf *s = strbuf_new();
    strbuf_catc(s, c);
    for (;;) {
        int ch = readc();
        if (!isdigitletter(ch) && ch != '.') {
            unreadc(ch);
            break;
        }
        bool is_float = strchr("eEpP", ch) && strchr("+-", peek());
        if (is_float) {
            strbuf_catc(s, ch);
            ch = readc();
            strbuf_catc(s, ch);
        } else {
            strbuf_catc(s, ch);
        }
    }
    return make_token2(NCONSTANT,  strbuf_str(s));
}

static struct token *sequence(bool wide, int sep)
{
    struct strbuf *s = strbuf_new();

    if (wide)
        strbuf_catc(s, 'L');
    strbuf_catc(s, sep);

    int ch;
    for (;;) {
        ch = readc();
        if (ch == sep || isnewline(ch) || ch == EOI)
            break;
        if (ch == '\\') {
            strbuf_catc(s, '\\');
            ch = readc();
        }
        strbuf_catc(s, ch);
    }

    bool is_char = sep == '\'' ? true : false;
    const char *name = is_char ? "character" : "string";
    if (ch != sep)
        error("untermiated %s constant: %s", name, s->str);
    strbuf_catc(s, sep);

    if (is_char)
        return make_token2(NCONSTANT, strbuf_str(s));
    else
        return make_token2(SCONSTANT, strbuf_str(s));
}

static struct token *identifier(int c)
{
    struct strbuf *s = strbuf_new();
    strbuf_catc(s, c);
    for (;;) {
        int ch = readc();
        if (!isdigitletter(ch)) {
            unreadc(ch);
            break;
        }
        strbuf_catc(s, ch);
    }
    return make_token2(ID, strbuf_str(s));
}

static struct token *newline(void)
{
    current_file->bol = true;
    newline_token->src = source;
    return newline_token;
}

static struct token *spaces(int c)
{
    for (;;) {
        int ch = readc();
        if (!iswhitespace(ch)) {
            unreadc(ch);
            break;
        }
    }
    space_token->src = source;
    return space_token;
}

struct token *dolex(void)
{
    register int rpc;

    for (;;) {
        rpc = readc();
        markc();

        switch (rpc) {
        case EOI:
            return eoi_token;

        case '\n':
            return newline();

            // spaces
        case '\t':
        case '\v':
        case '\f':
        case '\r':
        case ' ':
            return spaces(rpc);

            // punctuators
        case '/':
            if (next('/')) {
                line_comment();
                continue;
            } else if (next('*')) {
                block_comment();
                continue;
            } else if (next('=')) {
                return make_token(DIVEQ);
            } else {
                return make_token(rpc);
            }

        case '+':
            if (next('+'))
                return make_token(INCR);
            else if (next('='))
                return make_token(ADDEQ);
            else
                return make_token(rpc);

        case '-':
            if (next('-'))
                return make_token(DECR);
            else if (next('='))
                return make_token(MINUSEQ);
            else if (next('>'))
                return make_token(DEREF);
            else
                return make_token(rpc);

        case '*':
            if (next('='))
                return make_token(MULEQ);
            else
                return make_token(rpc);

        case '=':
            if (next('='))
                return make_token(EQ);
            else
                return make_token(rpc);

        case '!':
            if (next('='))
                return make_token(NEQ);
            else
                return make_token(rpc);

        case '%':
            if (next('=')) {
                return make_token(MODEQ);
            } else if (next('>')) {
                return make_token('}');
            } else if (next(':')) {
                if (next('%')) {
                    if (next(':'))
                        return make_token(SHARPSHARP);
                    unreadc('%');
                }
                return make_token('#');
            } else {
                return make_token(rpc);
            }

        case '^':
            if (next('='))
                return make_token(XOREQ);
            else
                return make_token(rpc);

        case '&':
            if (next('='))
                return make_token(BANDEQ);
            else if (next('&'))
                return make_token(AND);
            else
                return make_token(rpc);

        case '|':
            if (next('='))
                return make_token(BOREQ);
            else if (next('|'))
                return make_token(OR);
            else
                return make_token(rpc);

        case '<':
            if (next('=')) {
                return make_token(LEQ);
            } else if (next('<')) {
                if (next('='))
                    return make_token(LSHIFTEQ);
                else
                    return make_token(LSHIFT);
            } else if (next('%')) {
                return make_token('{');
            } else if (next(':')) {
                return make_token('[');
            } else {
                return make_token(rpc);
            }

        case '>':
            if (next('=')) {
                return make_token(GEQ);
            } else if (next('>')) {
                if (next('='))
                    return make_token(RSHIFTEQ);
                else
                    return make_token(RSHIFT);
            } else {
                return make_token(rpc);
            }

        case '(':
        case ')':
        case '{':
        case '}':
        case '[':
        case ']':
        case ',':
        case ';':
        case '~':
        case '?':
            return make_token(rpc);

        case ':':
            if (next('>'))
                return make_token(']');
            else
                return make_token(rpc);

        case '#':
            if (next('#'))
                return make_token(SHARPSHARP);
            else
                return make_token(rpc);

            // constants
        case '\'':
            return sequence(false, '\'');

        case '"':
            return sequence(false, '"');

        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
            return ppnumber(rpc);

        case '.':
            if (next('.')) {
                if (next('.'))
                    return make_token(ELLIPSIS);
                unreadc('.');
                return make_token(rpc);
            } else if (isdigit(peek())) {
                return ppnumber(rpc);
            } else {
                return make_token(rpc);
            }

            // identifiers
        case 'L':
            if (next('\''))
                return sequence(true, '\'');
            else if (next('"'))
                return sequence(true, '"');
            // go through
        case 'a':
        case 'b':
        case 'c':
        case 'd':
        case 'e':
        case 'f':
        case 'g':
        case 'h':
        case 'i':
        case 'j':
        case 'k':
        case 'l':
        case 'm':
        case 'n':
        case 'o':
        case 'p':
        case 'q':
        case 'r':
        case 's':
        case 't':
        case 'u':
        case 'v':
        case 'w':
        case 'x':
        case 'y':
        case 'z':
        case 'A':
        case 'B':
        case 'C':
        case 'D':
        case 'E':
        case 'F':
        case 'G':
        case 'H':
        case 'I':
        case 'J':
        case 'K':
        case 'M':
        case 'N':
        case 'O':
        case 'P':
        case 'Q':
        case 'R':
        case 'S':
        case 'T':
        case 'U':
        case 'V':
        case 'W':
        case 'X':
        case 'Y':
        case 'Z':
        case '_':
            return identifier(rpc);

        default:
            // illegal character
            if (isgraph(rpc))
                error("illegal character '%c'", rpc);
            else
                error("illegal character '\\0%o'", rpc);
        }
    }
}

static const char *hq_char_sequence(int sep)
{
    struct strbuf *s = strbuf_new();
    int ch;

    for (;;) {
        ch = readc();
        if (ch == sep || isnewline(ch) || ch == EOI)
            break;
        strbuf_catc(s, ch);
    }

    if (ch != sep)
        error("missing '%c' in header name", sep);

    skipline(true);
    return strbuf_str(s);
}

struct token *header_name(void)
{
    int ch;
 beg:
    ch = readc();
    if (iswhitespace(ch))
        goto beg;

    markc();
    if (ch == '<') {
        const char *name = hq_char_sequence('>');
        return new_token(&(struct token) {
                .name = name,.kind = '<'});
    } else if (ch == '"') {
        const char *name = hq_char_sequence('"');
        return new_token(&(struct token) {
                .name = name,.kind = '"'});
    } else {
        // pptokens
        unreadc(ch);
        return NULL;
    }
}

void unget(struct token *t)
{
    vec_push(current_file->buffer, t);
}

static void skip_sequence(int sep)
{
    int ch;
    for (;;) {
        ch = readc();
        if (ch == sep || isnewline(ch) || ch == EOI)
            break;
        if (ch == '\\')
            readc();
    }
    if (ch != sep)
        unreadc(ch);
}

void skip_spaces(void)
{
    // skip spaces, including comments
    int ch;

 beg:
    ch = readc();
    if (iswhitespace(ch)) {
        goto beg;
    } else if (ch == '/') {
        if (next('/')) {
            line_comment();
            goto beg;
        } else if (next('*')) {
            block_comment();
            goto beg;
        }
    }
    unreadc(ch);
}

void skip_ifstub(void)
{
    /* Skip part of conditional group.
     */
    unsigned lines = 0;
    bool bol = true;
    int nest = 0;
    struct token *t0 = lex();
    lines++;
    assert(IS_NEWLINE(t0) || t0->id == EOI);
    for (;;) {
        // skip spaces
        skip_spaces();
        int ch = readc();
        if (ch == EOI)
            break;
        if (isnewline(ch)) {
            bol = true;
            lines++;
            continue;
        }
        if (ch == '\'' || ch == '"') {
            skip_sequence(ch);
            bol = false;
            continue;
        }
        if (ch != '#' || !bol) {
            bol = false;
            continue;
        }
        struct source src = chsrc();
        struct token *t = lex();
        while (IS_SPACE(t))
            t = lex();
        if (t->id != ID) {
            if (IS_NEWLINE(t)) {
                bol = true;
                lines++;
            } else {
                bol = false;
            }
            continue;
        }
        const char *name = t->name;
        if (!strcmp(name, "if") || !strcmp(name, "ifdef")
            || !strcmp(name, "ifndef")) {
            nest++;
            bol = false;
            continue;
        }
        if (!nest &&
            (!strcmp(name, "elif") || !strcmp(name, "else")
             || !strcmp(name, "endif"))) {
            // found
            unget(t);
            struct token *t0 = new_token(&(struct token){.id =
                        '#',.src = src,.bol =
                        true });
            unget(t0);
            break;
        }
        if (nest && !strcmp(name, "endif")) {
            nest--;
            bol = false;
        }
        skipline(false);
    }

    while (lines-- > 0)
        unget(newline_token);
}

struct token *lex(void)
{
    struct vector *v = current_file->buffer;
    struct token *t;
    if (v && v->len)
        t = vec_pop(v);
    else
        t = dolex();
    mark(t);
    return t;
}

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

static void unget_token(struct token *t)
{
    vec_push(current_file->tokens, t);
}

static struct token *do_one_token(void)
{
    for (;;) {
        struct token *t = get_pptok();
        if (IS_SPACE(t) || IS_NEWLINE(t) || IS_LINENO(t))
            continue;
        return t;
    }
}

static struct token *one_token(void)
{
    if (vec_len(current_file->tokens))
        return vec_pop(current_file->tokens);
    else
        return do_one_token();
}

static struct token *peek_token(void)
{
    struct token *t = one_token();
    unget_token(t);
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
        const char *name = unwrap_scon(ti->name);
        if (name)
            strbuf_cats(s, name);
    }
    strbuf_catc(s, '"');
    t->name = strbuf_str(s);
    return t;
}

static struct token *do_cctoken(void)
{
    struct token *t = one_token();
    if (t->id == SCONSTANT) {
        struct vector *v = vec_new1(t);
        struct token *t1 = peek_token();
        bool wide = t->name[0] == 'L';
        while (t1->id == SCONSTANT) {
            if (t1->name[0] == 'L')
                wide = true;
            vec_push(v, one_token());
            t1 = peek_token();
        }
        if (vec_len(v) > 1)
            return combine_scons(v, wide);
    }
    return t;
}

/* Parser interfaces
 *
 * 1. gettok
 * 2. lookahead
 * 3. expect
 * 4. match
 */

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

static struct token *cctoken(void)
{
    struct token *t = do_cctoken();
    // keywords
    if (t->id == ID) {
        for (int i = 0; i < ARRAY_SIZE(kws); i++) {
            if (!strcmp(t->name, kws[i])) {
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
        token = cctoken();
    }
    mark(token);
    return token->id;
}

struct token *lookahead(void)
{
    if (ahead_token == NULL) {
        ahead_token = cctoken();
        // restore source
        mark(token);
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
               t->name, cnt);
    else if (cnt)
        errorf(t->src,
               "invalid token '%s'",
               t->name);
    else
        die("nothing skipped, may be an internal error");
    return cnt;
}
