#include <assert.h>
#include <stdio.h>
#include <ctype.h>
#include <limits.h>
#include <errno.h>
#include <stdlib.h>
#include <wchar.h>
#include "lex.h"
#include "libutils/utils.h"
#include "internal.h"

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

static struct token *eoi_token = &(struct token){.id = EOI};
static struct token *newline_token = &(struct token){.id = '\n'};
struct token *space_token = &(struct token){.id = ' '};

struct source source;

#define ISWHITESPACE(ch)  (map[ch] & BLANK)
#define ISNEWLINE(ch)     (map[ch] & NEWLINE)
#define ISDIGITLETTER(ch) (map[ch] & (DIGIT|LETTER))
#define ISXALPHA(ch)      (map[ch] & HEX)
#define ISDIGIT(ch)       (map[ch] & DIGIT)
#define ISXDIGIT(ch)      (map[ch] & (DIGIT|HEX))
#define ISGRAPH(ch)       isgraph(ch)

#define INCLINE(fs, col)  do {                  \
        fs->line++;                             \
        fs->column = col;                       \
    } while (0)

#define MARK(t)  source = t->src

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

const char *tok2s(struct token *t)
{
    if (t->id == ID)
        return TOK_ID_STR(t);
    else if (t->id == SCONSTANT ||
             t->id == ICONSTANT ||
             t->id == FCONSTANT)
        return TOK_LIT_STR(t);
    else if (t->u.lit.str)
        return t->u.lit.str;
    else
        return id2s(t->id);
}

int isletter(int c)
{
    return map[c] & LETTER;
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
            assert(0);
        }
    }
}

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
        // search '\n', '\\', '\r'
        while (*s != '\n' && *s != '\\' && *s != '\r')
            s++;

        c = *s;
        if (c == '\\')
            pbackslash = s++;
        else
            break;
    }

    // d must be '\n' or '\r'
    d = (unsigned char *)s;

    if (d == pb->limit)
        goto done;
    if (c == '\r' && s[1] == '\n') {
        s++;
        if (s == pb->limit)
            goto done;
    }
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

        if (c == '\n' || c == '\r') {
            if (c == '\r' && s != pb->limit && s[1] == '\n')
                s++;
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

struct tokenrun *next_tokenrun(struct tokenrun *prev, unsigned int count)
{
    struct tokenrun *run = xmalloc(sizeof(struct tokenrun));
    run->base = zmalloc(count * sizeof(struct token));
    run->limit = run->base + count;
    run->prev = prev;
    return run;
}

struct token *new_token(struct token *tok)
{
    struct token *t = xmalloc(sizeof(struct token));
    memcpy(t, tok, sizeof(struct token));
    return t;
}

static void line_comment(struct file *pfile)
{
    struct buffer *pb = pfile->buffer;
    while (*pb->cur != '\n')
        pb->cur++;
    process_line_notes(pb);
}

// fs->cur points to the initial asterisk of the comment.
static void block_comment(struct file *pfile)
{
    struct buffer *pb = pfile->buffer;
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
                cpp_error("unterminated /* comment");
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

static void float_constant(struct file *pfile, struct token *result)
{
    struct buffer *pb = pfile->buffer;
    const unsigned char *pc = pb->cur - 1;
    int suffix;
    const char *s;

    if (pc[0] == '.') {
        assert(ISDIGIT(pc[1]));
        goto dotted;
    } else if (pc[0] == '0' && (pc[1] == 'x' || pc[1] == 'X')) {
        // base 16
        pc += 2;
        if (*pc == '.') {
            if (!ISXDIGIT(pc[1]))
                cpp_error("hexadecimal floating constants require a significand");
            goto dotted_hex;
        } else {
            assert(ISXDIGIT(*pc));

            while (ISXDIGIT(*pc))
                pc++;
        dotted_hex:
            if (*pc == '.') {
                pc++;
                while (ISXDIGIT(*pc))
                    pc++;
            }
            if (*pc == 'p' || *pc == 'P') {
                pc++;
                if (*pc == '+' || *pc == '-')
                    pc++;
                if (ISDIGIT(*pc)) {
                    do
                        pc++;
                    while (ISDIGIT(*pc));
                } else {
                    cpp_error("exponent has no digits");
                }
            } else {
                cpp_error("hexadecimal floating constants require an exponent");
            }
        }
    } else {
        // base 10
        assert(ISDIGIT(*pc));

        while (ISDIGIT(*pc))
            pc++;
    dotted:
        if (*pc == '.') {
            pc++;
            while (ISDIGIT(*pc))
                pc++;
        }
        if (*pc == 'e' || *pc == 'E') {
            pc++;
            if (*pc == '+' || *pc == '-')
                pc++;
            if (ISDIGIT(*pc)) {
                do
                    pc++;
                while (ISDIGIT(*pc));
            } else {
                cpp_error("exponent used with no following digits");
            }
        }
    }

    // suffix
    if (*pc == 'f' || *pc == 'F') {
        pc++;
        suffix = FLOAT;
    } else if (*pc == 'l' || *pc == 'L') {
        pc++;
        suffix = LONG + DOUBLE;
    } else {
        suffix = 0;
    }

    s = strn((const char *)pb->cur - 1, pc - pb->cur + 1);
    
    errno = 0;
    switch (suffix) {
    case FLOAT:
        result->u.lit.v.d = strtof(s, NULL);
        break;
    case LONG + DOUBLE:
        result->u.lit.v.d = strtold(s, NULL);
        break;
    default:
        result->u.lit.v.d = strtod(s, NULL);
        break;
    }

    if (errno == ERANGE)
        cpp_error("float constant overflow: %s", s);

    result->id = FCONSTANT;
    result->u.lit.suffix = suffix;
    result->u.lit.str = s;
    pb->cur = pc;
}

static void integer_constant(struct file *pfile, struct token *result)
{
    struct buffer *pb = pfile->buffer;
    const unsigned char *pc = pb->cur - 1;
    bool overflow = 0;
    unsigned long long n = 0;
    int base;
    int suffix;
    const char *s;

    if (pc[0] == '0' && (pc[1] == 'x' || pc[1] == 'X')) {
        base = 16;
        pc += 2;
        for (; ISXDIGIT(*pc); pc++) {
            if (n & ~(~0ULL >> 4)) {
                overflow = 1;
            } else {
                int d;
                if (ISXALPHA(*pc))
                    d = (*pc & 0x5f) - 'A' + 10;
                else
                    d = *pc - '0';

                n = (n << 4) + d;
            }
        }
    } else if (pc[0] == '0') {
        base = 8;
        bool err = 0;
        for (; ISDIGIT(*pc); pc++) {
            if (*pc == '8' || *pc == '9')
                err = 1;

            if (n & ~(~0ULL >> 3))
                overflow = 1;
            else
                n = (n << 3) + (*pc - '0');
        }

        if (err)
            cpp_error("invalid octal constant");
    } else {
        base = 0;
        for (; ISDIGIT(*pc); pc++) {
            int d = *pc - '0';
            if (n > (~0ULL - d) / 10)
                overflow = 1;
            else
                n = n * 10 + d;
        }
    }

    // suffix
    if ((pc[0] == 'u' || pc[0] == 'U') &&
        ((pc[1] == 'l' && pc[2] == 'l') || (pc[1] == 'L' && pc[2] == 'L'))) {
        pc += 3;
        suffix = UNSIGNED + LONG + LONG;
    } else if (((pc[0] == 'l' && pc[1] == 'l') || (pc[0] == 'L' && pc[1] == 'L')) &&
               (pc[2] == 'u' || pc[2] == 'U')) {
        pc += 3;
        suffix = UNSIGNED + LONG + LONG;
    } else if ((pc[0] == 'l' && pc[1] == 'l') || (pc[0] == 'L' && pc[1] == 'L')) {
        pc += 2;
        suffix = LONG + LONG;
    } else if ((pc[0] == 'l' || pc[0] == 'L') && (pc[1] == 'u' || pc[1] == 'U')) {
        pc += 2;
        suffix = UNSIGNED + LONG;
    } else if ((pc[0] == 'u' || pc[0] == 'U') && (pc[1] == 'l' || pc[1] == 'L')) {
        pc += 2;
        suffix = UNSIGNED + LONG;
    } else if (pc[0] == 'l' || pc[0] == 'L') {
        pc += 1;
        suffix = LONG;
    } else if (pc[0] == 'u' || pc[0] == 'U') {
        pc += 1;
        suffix = UNSIGNED;
    } else {
        suffix = 0;
    }

    s = strn((const char *)pb->cur - 1, pc - pb->cur + 1);
    
    if (overflow)
        cpp_error("integer constant overflow: %s", s);

    result->id = ICONSTANT;
    result->u.lit.base = base;
    result->u.lit.suffix = suffix;
    result->u.lit.v.u = n;
    result->u.lit.str = s;
    pb->cur = pc;
}

// 0-9 or .[0-9]
static void number(struct file *pfile, struct token *result)
{
    struct buffer *pb = pfile->buffer;
    const unsigned char *pc = pb->cur - 1;

    if (pc[0] == '.') {
        float_constant(pfile, result);
    } else if (pc[0] == '0' && (pc[1] == 'x' || pc[1] == 'X')) {
        // Hex
        pc += 2;
        if (!ISXDIGIT(*pc) && pc[0] != '.') {
            cpp_error("incomplete hex constant");
            integer_constant(pfile, result);
            return;
        }
        if (*pc == '.') {
            float_constant(pfile, result);
        } else {
            while (ISXDIGIT(*pc))
                pc++;
            if (*pc == '.' || *pc == 'p' || *pc == 'P')
                float_constant(pfile, result);
            else
                integer_constant(pfile, result);
        }
    } else {
        // Oct/Dec
        assert(ISDIGIT(*pc));
        
        while (ISDIGIT(*pc))
            pc++;
        if (*pc == '.' || *pc == 'e' || *pc == 'E')
            float_constant(pfile, result);
        else
            integer_constant(pfile, result);
    }
}

static const char *string_constant(struct file *pfile, bool wide)
{
    struct buffer *pb = pfile->buffer;
    const unsigned char *rpc = pb->cur - 1;
    int sep = '"';
    const char *name;
    int ch;

    if (wide) pb->cur++;

    for (;;) {
        ch = *pb->cur++;
        if (ch == sep || ISNEWLINE(ch))
            break;
        if (ch == '\\')
            pb->cur++;
    }

    if (ch != sep) {
        char *str = xstrndup((const char *)rpc, pb->cur - rpc + 1);
        str[pb->cur - rpc] = sep;
        name = str;
        cpp_error("untermiated string constant: %s", name);
    } else {
        name = xstrndup((const char *)rpc, pb->cur - rpc);
    }

    return name;
}

static unsigned int escape(const unsigned char **pc)
{
    unsigned int c = 0;
    const unsigned char *s = *pc;
    assert(*s == '\\');
    s += 1;
    switch (*s++) {
    case 'a':
        c = 7;
        break;
    case 'b':
        c = '\b';
        break;
    case 'f':
        c = '\f';
        break;
    case 'n':
        c = '\n';
        break;
    case 'r':
        c = '\r';
        break;
    case 't':
        c = '\t';
        break;
    case 'v':
        c = '\v';
        break;
    case '\'':
    case '"':
    case '\\':
    case '\?':
        c = s[-1];
        break;
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
        c = s[-1] - '0';
        if (*s >= '0' && *s <= '7') {
            c = (c << 3) + (*s++) - '0';
            if (*s >= '0' && *s <= '7')
                c = (c << 3) + (*s++) - '0';
        }
        break;
    case 'x':
        {
            bool overflow = 0;
            for (; ISXDIGIT(*s);) {
                if (overflow) {
                    s++;
                    continue;
                }
                if (c & ~(WCHAR_MAX >> 4)) {
                    overflow = 1;
                    cpp_error("hex escape sequence out of range");
                } else {
                    if (ISDIGIT(*s))
                        c = (c << 4) + *s - '0';
                    else
                        c = (c << 4) + (*s & 0x5f) - 'A' + 10;
                }
                s++;
            }
        }
        break;
    case 'u':
    case 'U':
        {
            int x = 0;
            int n = s[-1] == 'u' ? 4 : 8;
            for (; ISXDIGIT(*s); x++, s++) {
                if (x == n)
                    break;
                if (ISDIGIT(*s))
                    c = (c << 4) + *s - '0';
                else
                    c = (c << 4) + (*s & 0x5f) - 'A' + 10;
            }
        }
        break;
    default:
        c = s[-1];
        break;
    }

    *pc = s;
    return c;
}

static void char_constant(struct file *pfile, struct token *result, bool wide)
{
    struct buffer *pb = pfile->buffer;
    const unsigned char *pc = pb->cur - 1;
    unsigned long long c = 0;
    char ws[MB_LEN_MAX];
    int len = 0;
    bool overflow = 0;
    bool char_rec = 0;
    const char *s;

    wide ? (pc += 2) : (pc += 1);

    for (; *pc != '\'' && !ISNEWLINE(*pc);) {
        if (char_rec)
            overflow = 1;
        if (*pc == '\\') {
            c = escape(&pc);
            char_rec = 1;
        } else {
            if (wide) {
                if (len >= MB_LEN_MAX)
                    cpp_error("multibyte character overflow");
                else
                    ws[len++] = (char)*pc++;
            } else {
                c = *pc++;
                char_rec = 1;
            }
        }
    }

    if (*pc == '\'') {
        pc++;
        s = strn((const char *)pb->cur - 1, pc - pb->cur + 1);
    } else {
        s = strn((const char *)pb->cur - 1, pc - pb->cur + 1);
        cpp_error("unterminated character constant: %s", s);
    }

    if (!char_rec && !len)
        cpp_error("incomplete character constant: %s", s);
    else if (overflow)
        cpp_error("extraneous characters in character constant: %s", s);
    else if ((!wide && c > UCHAR_MAX) || (wide && c > WCHAR_MAX))
        cpp_error("character constant overflow: %s", s);
    else if (len && mbtowc((wchar_t *)&c, ws, len) != len)
        cpp_error("illegal multi-character sequence");
    
    result->id = ICONSTANT;
    result->u.lit.v.u = wide ? (wchar_t)c : (unsigned char)c;
    result->u.lit.chr = wide ? 2 : 1;
    result->u.lit.str = s;
    pb->cur = pc;
}

static struct ident *identifier(struct file *pfile)
{
    struct buffer *pb = pfile->buffer;
    const unsigned char *rpc = pb->cur - 1;
    unsigned int hash = IMAP_HASHSTEP(0, *rpc);
    unsigned int len;
    
    while (ISDIGITLETTER(*pb->cur)) {
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
    struct token *result;
    struct buffer *pb = pfile->buffer;

    if (pb->need_line)
        next_clean_line(pb);
    // pb->buf maybe NULL
    if (pb->cur >= pb->limit)
        return eoi_token;

    if (pfile->cur_token == pfile->tokenrun->limit) {
        pfile->tokenrun = next_tokenrun(pfile->tokenrun, 1024);
        pfile->cur_token = pfile->tokenrun->base;
    }
    result = pfile->cur_token++;
    
 start:
    if (pb->cur >= pb->notes[pb->cur_note].pos)
        process_line_notes(pb);
    SET_COLUMN(pb, pb->cur - pb->line_base);
    rpc = pb->cur++;
    MARKC(pb);

    switch (*rpc) {
    case '\n':
        if (rpc >= pb->limit) {
            pfile->cur_token--;
            return eoi_token;
        } else {
            pb->need_line = true;
            pb->bol = true;
            newline_token->src = source;
            INCLINE(pb, 0);
            pfile->cur_token--;
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
        while (ISWHITESPACE(*rpc));
        pb->cur = rpc;
        space_token->src = source;
        pfile->cur_token--;
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
            result->id = DIVEQ;
        } else {
            result->id = '/';
        }
        break;

    case '+':
        if (rpc[1] == '+') {
            pb->cur++;
            result->id = INCR;
        } else if (rpc[1] == '=') {
            pb->cur++;
            result->id = ADDEQ;
        } else {
            result->id = '+';
        }
        break;

    case '-':
        if (rpc[1] == '-') {
            pb->cur++;
            result->id = DECR;
        } else if (rpc[1] == '=') {
            pb->cur++;
            result->id = MINUSEQ;
        } else if (rpc[1] == '>') {
            pb->cur++;
            result->id = DEREF;
        } else {
            result->id = '-';
        }
        break;

    case '*':
        if (rpc[1] == '=') {
            pb->cur++;
            result->id = MULEQ;
        } else {
            result->id = '*';
        }
        break;

    case '=':
        if (rpc[1] == '=') {
            pb->cur++;
            result->id = EQ;
        } else {
            result->id = '=';
        }
        break;

    case '!':
        if (rpc[1] == '=') {
            pb->cur++;
            result->id = NEQ;
        } else {
            result->id = '!';
        }
        break;

    case '%':
        if (rpc[1] == '=') {
            pb->cur++;
            result->id = MODEQ;
        } else if (rpc[1] == '>') {
            pb->cur++;
            result->id = '}';
        } else if (rpc[1] == ':' && rpc[2] == '%' && rpc[3] == ':') {
            pb->cur += 3;
            result->id = SHARPSHARP;
        } else if (rpc[1] == ':') {
            pb->cur++;
            result->id = '#';
        } else {
            result->id = '%';
        }
        break;

    case '^':
        if (rpc[1] == '=') {
            pb->cur++;
            result->id = XOREQ;
        } else {
            result->id = '^';
        }
        break;

    case '&':
        if (rpc[1] == '=') {
            pb->cur++;
            result->id = BANDEQ;
        } else if (rpc[1] == '&') {
            pb->cur++;
            result->id = ANDAND;
        } else {
            result->id = '&';
        }
        break;

    case '|':
        if (rpc[1] == '=') {
            pb->cur++;
            result->id = BOREQ;
        } else if (rpc[1] == '|') {
            pb->cur++;
            result->id = OROR;
        } else {
            result->id = '|';
        }
        break;

    case '<':
        if (rpc[1] == '=') {
            pb->cur++;
            result->id = LEQ;
        } else if (rpc[1] == '<' && rpc[2] == '=') {
            pb->cur += 2;
            result->id = LSHIFTEQ;
        } else if (rpc[1] == '<') {
            pb->cur++;
            result->id = LSHIFT;
        } else if (rpc[1] == '%') {
            pb->cur++;
            result->id = '{';
        } else if (rpc[1] == ':') {
            pb->cur++;
            result->id = '[';
        } else {
            result->id = '<';
        }
        break;

    case '>':
        if (rpc[1] == '=') {
            pb->cur++;
            result->id = GEQ;
        } else if (rpc[1] == '>' && rpc[2] == '=') {
            pb->cur += 2;
            result->id = RSHIFTEQ;
        } else if (rpc[1] == '>') {
            pb->cur++;
            result->id = RSHIFT;
        } else {
            result->id = '>';
        }
        break;

    case '(': case ')':
    case '{': case '}':
    case '[': case ']':
    case ',': case ';':
    case '~': case '?':
        result->id = *rpc;
        break;

    case ':':
        if (rpc[1] == '>') {
            pb->cur++;
            result->id = ']';
        } else {
            result->id = ':';
        }
        break;

    case '#':
        if (rpc[1] == '#') {
            pb->cur++;
            result->id = SHARPSHARP;
        } else {
            result->id = '#';
        }
        break;

        // constants
    case '\'':
        char_constant(pfile, result, false);
        break;

    case '"':
        result->id = SCONSTANT;
        result->u.lit.str = string_constant(pfile, false);
        break;

    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
        number(pfile, result);
        break;

    case '.':
        if (rpc[1] == '.' && rpc[2] == '.') {
            pb->cur += 2;
            result->id = ELLIPSIS;
        } else if (ISDIGIT(rpc[1])) {
            number(pfile, result);
        } else {
            result->id = '.';
        }
        break;

        // identifiers
    case 'L':
        if (rpc[1] == '\'') {
            char_constant(pfile, result, true);
            break;
        } else if (rpc[1] == '"') {
            result->id = SCONSTANT;
            result->u.lit.str = string_constant(pfile, true);
            break;
        }
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
        result->id = ID;
        result->u.ident = identifier(pfile);
        break;

    default:
        // illegal character
        if (ISGRAPH(*rpc))
            cpp_error("illegal character '%c'", *rpc);
        else
            cpp_error("illegal character '\\0%o'", *rpc);
        goto start;
    }

    // done
    result->src = source;
    result->bol = pb->bol;
    pb->bol = false;
    return result;
}

static void skipline(struct file *pfile, bool over)
{
    struct buffer *pb = pfile->buffer;
    while (*pb->cur != '\n')
        pb->cur++;
    if (over) {
        INCLINE(pb, 0);
        next_clean_line(pb);
    }
}

static const char *hq_char_sequence(struct file *pfile, int sep)
{
    struct buffer *pb = pfile->buffer;
    const unsigned char *rpc = pb->cur;
    int ch;
    const char *name;

    for (;;) {
        ch = *pb->cur++;
        if (ch == sep || ISNEWLINE(ch)) {
            pb->cur--;
            break;
        }
    }

    if (ch != sep)
        cpp_error("missing '%c' in header name", sep);

    name = xstrndup((const char *)rpc, pb->cur - rpc);
    skipline(pfile, true);
    return name;
}

struct token *header_name(struct file *pfile)
{
    struct buffer *pb = pfile->buffer;
    
    while (ISWHITESPACE(*pb->cur))
        pb->cur++;

    SET_COLUMN(pb, pb->cur - pb->line_base);
    int ch = *pb->cur++;

    // mark for 'error/warning etc.'
    MARKC(pb);
    if (ch == '<') {
        const char *name = hq_char_sequence(pfile, '>');
        return new_token(&(struct token){
                .u.lit.str = name, .kind = ch});
    } else if (ch == '"') {
        const char *name = hq_char_sequence(pfile, '"');
        return new_token(&(struct token){
                .u.lit.str = name, .kind = ch});
    } else {
        // pptokens
        pb->cur--;
        return NULL;
    }
}

/* Skip part of conditional group.
 */
void skip_ifstack(struct file *pfile)
{
    struct buffer *pb = pfile->buffer;
    int nest = 0;
    assert(vec_len(pb->ungets) == 0);
    for (;;) {
        struct token *t0 = dolex(pfile);
        if (t0->id == EOI)
            break;
        if (t0->id != '#' || !t0->bol)
            continue;
        struct token *t = dolex(pfile);
        while (IS_SPACE(t))
            t = dolex(pfile);
        if (t->id != ID)
            continue;
        const char *name = TOK_ID_STR(t);
        if (!strcmp(name, "if") ||
            !strcmp(name, "ifdef") ||
            !strcmp(name, "ifndef")) {
            nest++;
            continue;
        }
        if (!nest &&
            (!strcmp(name, "elif") ||
             !strcmp(name, "else") ||
             !strcmp(name, "endif"))) {
            // found
            unget(pfile, t);
            unget(pfile, t0);
            break;
        }
        if (nest && !strcmp(name, "endif"))
            nest--;
        skipline(pfile, false);
    }
}

struct token *lex(struct file *pfile)
{
    struct vector *v = pfile->buffer->ungets;
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
        const char *name = unwrap_scon(tok2s(ti));
        if (name)
            strbuf_cats(s, name);
    }
    strbuf_catc(s, '"');
    t->u.lit.str = strbuf_str(s);
    return t;
}

static struct token *do_cctoken(struct file *pfile)
{
    struct token *t = one_token(pfile);
    if (t->id == SCONSTANT) {
        struct vector *v = vec_new1(t);
        struct token *t1 = peek_token(pfile);
        const char *name0 = TOK_LIT_STR(t);
        bool wide = name0[0] == 'L';
        while (t1->id == SCONSTANT) {
            const char *name1 = TOK_LIT_STR(t1);
            if (name1[0] == 'L')
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
        const char *name = TOK_ID_STR(t);
        for (int i = 0; i < ARRAY_SIZE(kws); i++) {
            if (!strcmp(name, kws[i])) {
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
        cpp_error("expect token '%s'", id2s(t));
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

int skipto(int (*test) (struct token *))
{
    int i;
    struct token *t = token;
    for (i = 0; token->id != EOI; i++, gettok()) {
        if (test(token))
            break;
    }
    
    if (i > 1)
        cpp_error_at(t->src,
                     "invalid token '%s', %d tokens skipped",
                     tok2s(t), i);
    else if (i)
        cpp_error_at(t->src, "invalid token '%s'", tok2s(t));
    else
        die("nothing skipped, may be an internal error");
    return i;
}
