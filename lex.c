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
#define INCLINE(fs, col)  do {                  \
        fs->line++;                             \
        fs->column = col;                       \
    } while (0)

#define markc(fs)  do {                         \
        source.file = fs->name;                 \
        source.line = fs->line;                 \
        source.column = fs->column;             \
    } while (0)

#define SET_COLUMN(fs, col)  fs->column = col
#define make_token(fs, id)  make_token2(fs, id, NULL)

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

static void add_line_note(struct file *fs, const unsigned char *pos, int type)
{
    if (fs->notes_used == fs->notes_alloc) {
        fs->notes_alloc = fs->notes_alloc * 2 + 200;
        fs->notes = xrealloc(fs->notes, fs->notes_alloc * sizeof(struct line_note));
    }
    fs->notes[fs->notes_used].pos = pos;
    fs->notes[fs->notes_used].type = type;
    fs->notes_used++;
}

static void process_line_notes(struct file *fs)
{
    for (;;) {
        struct line_note *note = &fs->notes[fs->cur_note];

        if (note->pos > fs->cur)
            break;

        fs->cur_note++;
        
        if (note->type == '\\') {
            fs->line_base = note->pos;
            INCLINE(fs, 0);
        } else {
            assertf(0, "unexpected line note type:%d", note->type);
        }
    }
}

#ifdef __GNUC__

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
static void next_clean_line(struct file *fs)
{
    const unsigned char *s;
    unsigned char *d;
    unsigned char c;
    const unsigned char *pbackslash = NULL;
    
    fs->cur_note = fs->notes_used = 0;
    fs->cur = fs->line_base = fs->next_line;
    fs->need_line = false;
    s = fs->next_line;
    
    while (1) {
        // search '\n', '\\'
        s = search_line_fast(s, fs->limit);

        c = *s;
        if (c == '\\')
            pbackslash = s++;
        else
            break;
    }

    // d must be '\n'
    d = (unsigned char *)s;

    if (d == fs->limit)
        goto done;
    if (pbackslash == NULL)
        goto done;
    if (d - 1 != pbackslash)
        goto done;

    // Have an escaped newline
    add_line_note(fs, d - 1, '\\');
    d -= 2;
    
    while (1) {
        c = *++s;
        *++d = c;

        if (c == '\n') {
            if (s == fs->limit)
                break;
            if (d[-1] != '\\')
                break;

            add_line_note(fs, d - 1, '\\');
            d -= 2;
        }
    }
    
 done:
    *d = '\n';
    /* a sentinel note that should never be processed. */
    add_line_note(fs, d + 1, '\n');
    fs->next_line = s + 1;
}

struct token *new_token(struct token *tok)
{
    struct token *t = alloc_token();
    memcpy(t, tok, sizeof(struct token));
    if (!tok->name)
        t->name = id2s(tok->id);
    return t;
}

static struct token *make_token2(struct file *fs, int id, const char *name)
{
    struct token *t = alloc_token();
    t->id = id;
    t->name = name ? name : id2s(id);
    t->src = source;
    t->bol = fs->bol;
    fs->bol = false;
    return t;
}

static void line_comment(struct file *fs)
{
    while (*fs->cur != '\n')
        fs->cur++;
    process_line_notes(fs);
}

// fs->cur points to the initial asterisk of the comment.
static void block_comment(struct file *fs)
{
    const unsigned char *rpc = fs->cur;
    unsigned char ch;
    rpc++;
    
    for (;;) {
        ch = *rpc++;
        if (ch == '/' && rpc[-2] == '*') {
            break;
        } else if (ch == '\n') {
            fs->cur = rpc - 1;
            process_line_notes(fs);
            if (fs->next_line >= fs->limit) {
                error("unterminated /* comment");
                return;
            }
            next_clean_line(fs);
            INCLINE(fs, 0);
            rpc = fs->cur;
        }
    }

    fs->cur = rpc;
    process_line_notes(fs);
}

// fs->cur points at prior initial digit or dot.
static struct token *ppnumber(struct file *fs)
{
    const unsigned char *rpc = fs->cur - 1;
    int ch;
    for (;;) {
        ch = *fs->cur++;
        if (!isdigitletter(ch) && ch != '.') {
            fs->cur--;
            break;
        }
        bool is_float = strchr("eEpP", ch) && strchr("+-", *fs->cur);
        if (is_float)
            fs->cur++;
    }
    const char *name = xstrndup((const char *)rpc, fs->cur - rpc);
    return make_token2(fs, NCONSTANT,  name);
}

static struct token *sequence(struct file *fs, bool wide, int sep)
{
    const unsigned char *rpc = fs->cur - 1;
    bool is_char = sep == '\'';
    if (wide) fs->cur++;
    const char *name;
    int ch;
    for (;;) {
        ch = *fs->cur++;
        if (ch == sep || isnewline(ch))
            break;
        if (ch == '\\')
            fs->cur++;
    }

    if (ch != sep) {
        char *str = xstrndup((const char *)rpc, fs->cur - rpc + 1);
        str[fs->cur - rpc] = sep;
        name = str;
        error("untermiated %s constant: %s",
              is_char ? "character" : "string", name);
    } else {
        name = xstrndup((const char *)rpc, fs->cur - rpc);
    }

    if (is_char)
        return make_token2(fs, NCONSTANT, name);
    else
        return make_token2(fs, SCONSTANT, name);
}

static struct token *identifier(struct file *fs)
{
    const unsigned char *rpc = fs->cur - 1;
    while (isdigitletter(*fs->cur))
        fs->cur++;
    const char *name = xstrndup((const char *)rpc, fs->cur - rpc);
    return make_token2(fs, ID, name);
}

static struct token *dolex(struct file *fs)
{
    register const unsigned char *rpc;

    if (fs->need_line)
        next_clean_line(fs);
    // fs->buf maybe NULL
    if (fs->cur >= fs->limit)
        return eoi_token;
    
    for (;;) {
        if (fs->cur >= fs->notes[fs->cur_note].pos)
            process_line_notes(fs);
        SET_COLUMN(fs, fs->cur - fs->line_base);
        rpc = fs->cur++;
        markc(fs);

        switch (*rpc) {
        case '\n':
            if (rpc >= fs->limit) {
                return eoi_token;
            } else {
                fs->need_line = true;
                fs->bol = true;
                newline_token->src = source;
                INCLINE(fs, 0);
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
            fs->cur = rpc;
            space_token->src = source;
            return space_token;

            // punctuators
        case '/':
            if (rpc[1] == '/') {
                line_comment(fs);
                continue;
            } else if (rpc[1] == '*') {
                block_comment(fs);
                continue;
            } else if (rpc[1] == '=') {
                fs->cur++;
                return make_token(fs, DIVEQ);
            } else {
                return make_token(fs, '/');
            }

        case '+':
            if (rpc[1] == '+') {
                fs->cur++;
                return make_token(fs, INCR);
            } else if (rpc[1] == '=') {
                fs->cur++;
                return make_token(fs, ADDEQ);
            } else {
                return make_token(fs, '+');
            }

        case '-':
            if (rpc[1] == '-') {
                fs->cur++;
                return make_token(fs, DECR);
            } else if (rpc[1] == '=') {
                fs->cur++;
                return make_token(fs, MINUSEQ);
            } else if (rpc[1] == '>') {
                fs->cur++;
                return make_token(fs, DEREF);
            } else {
                return make_token(fs, '-');
            }

        case '*':
            if (rpc[1] == '=') {
                fs->cur++;
                return make_token(fs, MULEQ);
            } else {
                return make_token(fs, '*');
            }

        case '=':
            if (rpc[1] == '=') {
                fs->cur++;
                return make_token(fs, EQ);
            } else {
                return make_token(fs, '=');
            }

        case '!':
            if (rpc[1] == '=') {
                fs->cur++;
                return make_token(fs, NEQ);
            } else {
                return make_token(fs, '!');
            }

        case '%':
            if (rpc[1] == '=') {
                fs->cur++;
                return make_token(fs, MODEQ);
            } else if (rpc[1] == '>') {
                fs->cur++;
                return make_token(fs, '}');
            } else if (rpc[1] == ':' && rpc[2] == '%' && rpc[3] == ':') {
                fs->cur += 3;
                return make_token(fs, SHARPSHARP);
            } else if (rpc[1] == ':') {
                fs->cur++;
                return make_token(fs, '#');
            } else {
                return make_token(fs, '%');
            }

        case '^':
            if (rpc[1] == '=') {
                fs->cur++;
                return make_token(fs, XOREQ);
            } else {
                return make_token(fs, '^');
            }

        case '&':
            if (rpc[1] == '=') {
                fs->cur++;
                return make_token(fs, BANDEQ);
            } else if (rpc[1] == '&') {
                fs->cur++;
                return make_token(fs, AND);
            } else {
                return make_token(fs, '&');
            }

        case '|':
            if (rpc[1] == '=') {
                fs->cur++;
                return make_token(fs, BOREQ);
            } else if (rpc[1] == '|') {
                fs->cur++;
                return make_token(fs, OR);
            } else {
                return make_token(fs, '|');
            }

        case '<':
            if (rpc[1] == '=') {
                fs->cur++;
                return make_token(fs, LEQ);
            } else if (rpc[1] == '<' && rpc[2] == '=') {
                fs->cur += 2;
                return make_token(fs, LSHIFTEQ);
            } else if (rpc[1] == '<') {
                fs->cur++;
                return make_token(fs, LSHIFT);
            } else if (rpc[1] == '%') {
                fs->cur++;
                return make_token(fs, '{');
            } else if (rpc[1] == ':') {
                fs->cur++;
                return make_token(fs, '[');
            } else {
                return make_token(fs, '<');
            }

        case '>':
            if (rpc[1] == '=') {
                fs->cur++;
                return make_token(fs, GEQ);
            } else if (rpc[1] == '>' && rpc[2] == '=') {
                fs->cur += 2;
                return make_token(fs, RSHIFTEQ);
            } else if (rpc[1] == '>') {
                fs->cur++;
                return make_token(fs, RSHIFT);
            } else {
                return make_token(fs, '>');
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
            return make_token(fs, *rpc);

        case ':':
            if (rpc[1] == '>') {
                fs->cur++;
                return make_token(fs, ']');
            } else {
                return make_token(fs, ':');
            }

        case '#':
            if (rpc[1] == '#') {
                fs->cur++;
                return make_token(fs, SHARPSHARP);
            } else {
                return make_token(fs, '#');
            }

            // constants
        case '\'':
            return sequence(fs, false, '\'');

        case '"':
            return sequence(fs, false, '"');

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
            return ppnumber(fs);

        case '.':
            if (rpc[1] == '.' && rpc[2] == '.') {
                fs->cur += 2;
                return make_token(fs, ELLIPSIS);
            } else if (isdigit(rpc[1])) {
                return ppnumber(fs);
            } else {
                return make_token(fs, '.');
            }

            // identifiers
        case 'L':
            if (rpc[1] == '\'')
                return sequence(fs, true, '\'');
            else if (rpc[1] == '"')
                return sequence(fs, true, '"');
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
            return identifier(fs);

        default:
            // illegal character
            if (isgraph(*rpc))
                error("illegal character '%c'", *rpc);
            else
                error("illegal character '\\0%o'", *rpc);
        }
    }
}

static void skipline(struct file *fs, bool over)
{
    while (*fs->cur != '\n')
        fs->cur++;
    if (over) {
        INCLINE(fs, 0);
        next_clean_line(fs);
    }
}

static const char *hq_char_sequence(struct file *fs, int sep)
{
    const unsigned char *rpc = fs->cur;
    int ch;
    const char *name;

    for (;;) {
        ch = *fs->cur++;
        if (ch == sep || isnewline(ch)) {
            fs->cur--;
            break;
        }
    }

    if (ch != sep)
        error("missing '%c' in header name", sep);

    name = xstrndup((const char *)rpc, fs->cur - rpc);
    skipline(fs, true);
    return name;
}

struct token *header_name(struct file *fs)
{
    while (iswhitespace(*fs->cur))
        fs->cur++;

    SET_COLUMN(fs, fs->cur - fs->line_base);
    char ch = *fs->cur++;

    // mark for 'error/warning etc.'
    markc(fs);
    if (ch == '<') {
        const char *name = hq_char_sequence(fs, '>');
        return new_token(&(struct token) {
                .name = name, .kind = ch});
    } else if (ch == '"') {
        const char *name = hq_char_sequence(fs, '"');
        return new_token(&(struct token) {
                .name = name, .kind = ch});
    } else {
        // pptokens
        fs->cur--;
        return NULL;
    }
}

static void skip_sequence(struct file *fs, int sep)
{
    int ch;
    for (;;) {
        ch = *fs->cur++;
        if (ch == sep || isnewline(ch))
            break;
        if (ch == '\\')
            fs->cur++;
    }
    if (ch != sep)
        fs->cur--;
}

static void skip_spaces(struct file *fs)
{
    // skip spaces, including comments
    int ch;

    for (;;) {
        ch = *fs->cur++;
        if (iswhitespace(ch))
            continue;
        if (ch == '/' && *fs->cur == '/') {
            line_comment(fs);
            continue;
        }
        if (ch == '/' && *fs->cur == '*') {
            block_comment(fs);
            continue;
        }
        break;
    }
    fs->cur--;
}

void unget(struct file *fs, struct token *t)
{
    vec_push(fs->buffer, t);
}

static struct source chsrc(struct file *fs)
{
    struct source src;
    src.file = fs->name;
    src.line = fs->line;
    src.column = fs->column;
    return src;
}

/* Skip part of conditional group.
 */
void skip_ifstub(struct file *fs)
{
    unsigned lines = 0;
    bool bol = true;
    int nest = 0;
    struct token *t0 = lex(fs);
    lines++;
    assert(IS_NEWLINE(t0) || t0->id == EOI);
    for (;;) {
        if (fs->need_line)
            next_clean_line(fs);
        // skip spaces
        skip_spaces(fs);
        int ch = *fs->cur++;
        if (fs->cur >= fs->limit)
            break;
        if (isnewline(ch)) {
            bol = true;
            lines++;
            fs->need_line = true;
            continue;
        }
        if (ch == '\'' || ch == '"') {
            skip_sequence(fs, ch);
            bol = false;
            continue;
        }
        if (ch != '#' || !bol) {
            bol = false;
            continue;
        }
        struct source src = chsrc(fs);
        struct token *t = lex(fs);
        while (IS_SPACE(t))
            t = lex(fs);
        if (t->id != ID) {
            if (IS_NEWLINE(t)) {
                bol = true;
                lines++;
                fs->need_line = true;
            } else {
                bol = false;
            }
            continue;
        }
        const char *name = t->name;
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
            unget(fs, t);
            struct token *t0 = new_token(&(struct token){
                    .id = '#', .src = src, .bol = true });
            unget(fs, t0);
            break;
        }
        if (nest && !strcmp(name, "endif")) {
            nest--;
            bol = false;
        }
        skipline(fs, false);
    }

    while (lines-- > 0)
        unget(fs, newline_token);
}

struct token *lex(struct file *fs)
{
    struct vector *v = fs->buffer;
    struct token *t;
    if (v && v->len)
        t = vec_pop(v);
    else
        t = dolex(fs);
    MARK(t);
    return t;
}
