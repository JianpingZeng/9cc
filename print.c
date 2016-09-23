#include <stdio.h>
#include <assert.h>
#include <stdint.h>
#include "cc.h"
#include "libutils/color.h"

static FILE *outfd;

#define STR(str)  ((str) ? (str) : "<null>")
#define SET_OUTFD(fd)                           \
    FILE *_saved_fd = outfd;                    \
    outfd = fd

#define RESTORE_OUTFD()                         \
    outfd = _saved_fd


static void print_expr1(struct expr * node, int level);

static void putf(const char *fmt, ...)
{
    if (!outfd) outfd = stderr;
    va_list ap;
    va_start(ap, fmt);
    vfprintf(outfd, fmt, ap);
    va_end(ap);
}

static void putln(const char *fmt, ...)
{
    if (!outfd) outfd = stderr;
    va_list ap;
    va_start(ap, fmt);
    vfprintf(outfd, fmt, ap);
    fprintf(outfd, "\n");
    va_end(ap);
}

static void print_ty(struct type * ty)
{
    if (ty) {
        if (isfunc(ty) || isptr(ty) || isarray(ty))
            putf(RED_BOLD("'%s' "), TYPE_NAME(ty));

        putf(GREEN("'%s' "), type2s(ty));

        if (isarray(ty) || isstruct(ty) || isunion(ty)) {
            putf(YELLOW("<size=%ld> "), TYPE_SIZE(ty));
        } else if (isfunc(ty)) {
            putf("%s ", TYPE_OLDSTYLE(ty) ? "oldstyle" : "prototype");
            if (TYPE_INLINE(ty))
                putf("inline ");
        }
    }
}

static void print_type1(struct symbol *sym, int level)
{
    struct type *ty = sym->type;
    
    for (int i = 0; i < level; i++)
        putf("  ");
    print_ty(ty);
    putf("\n");
}

void print_type(struct symbol *sym)
{
    print_type1(sym, 0);
}

static void print_field1(struct field * node, int level)
{
    const char *name = node->name;
    struct type *ty = node->type;

    putf(GREEN("Field "));
    if (node->isbit)
        putf(RED("<offset=%d, bitoff=%d, bits=%d> "),
             node->offset, node->bitoff, node->bitsize);
    else
        putf(GREEN("<offset=%d> "), node->offset);

    print_ty(ty);
    putf(CYAN("%s"), STR(name));
    putf("\n");
}

static void print_symbol1(struct symbol *sym, int level)
{
    putf(CYAN("%s "), STR(sym->name));
    
    if (sym->defined)
        putf(YELLOW("<defined> "));

    struct type *ty = sym->type;
    print_ty(ty);
    putf("<scope: %d>", sym->scope);
    putf(YELLOW("<line:%u col:%u> "), sym->src.line, sym->src.column);
    putf("\n");

    if (isfuncdef(sym)) {
        // TODO: 
    } else {
        struct expr *init = sym->u.init;
        if (init)
            print_expr1(init, level + 1);
    }
}

void print_symbol(struct symbol *sym)
{
    print_symbol1(sym, 0);
}

static void print_expr1(struct expr * node, int level)
{
    for (int i = 0; i < level; i++)
        putf("  ");
    
    int op = EXPR_OP(node);
    bool prefix = EXPR_PREFIX(node);

    putf(PURPLE("%s ") YELLOW("%p "), nname(EXPR_ID(node)), node);
    print_ty(EXPR_TYPE(node));
    if (islvalue(node))
        putf("'" CYAN("lvalue") "' ");

    if (EXPR_SYM(node))
        putf(CYAN("%s "), STR(EXPR_SYM(node)->name));
    if (op == INCR || op == DECR)
        putf("%s ", (prefix ? "prefix" : "postfix"));
    if (op > 0)
        putf("'%s' ", id2s(op));
    if (EXPR_NAME(node))
        putf("<" RED("%s") "> ", EXPR_NAME(node));
    if (isiliteral(node)) {
        if (TYPE_OP(EXPR_TYPE(node)) == INT)
            putf(RED("%lld"), ILITERAL_VALUE(node).i);
        else
            putf(RED("%llu"), ILITERAL_VALUE(node).u);
    } else if (isfliteral(node)) {
        putf(RED("%Lf"), FLITERAL_VALUE(node).d);
    }

    putf("\n");

    if (EXPR_ID(node) == CALL_EXPR) {
        struct expr *func = EXPR_OPERAND(node, 0);
        if (func)
            print_expr1(func, level + 1);
        struct expr **args = EXPR_ARGS(node);
        if (args) {
            for (size_t i = 0; args[i]; i++) {
                struct expr *arg = args[i];
                print_expr1(arg, level + 1);
            }
        }
    } else if (EXPR_ID(node) == INITS_EXPR) {
        struct expr **inits = EXPR_INITS(node);
        if (inits) {
            for (size_t i = 0; inits[i]; i++) {
                struct expr *init = inits[i];
                print_expr1(init, level + 1);
            }
        }
    } else {
        if (EXPR_OPERAND(node, 0))
            print_expr1(EXPR_OPERAND(node, 0), level + 1);

        if (EXPR_OPERAND(node, 1))
            print_expr1(EXPR_OPERAND(node, 1), level + 1);

        if (EXPR_OPERAND(node, 2))
            print_expr1(EXPR_OPERAND(node, 2), level + 1);
    }
}

void print_field(struct field *field)
{
    print_field1(field, 0);
}

void print_expr(struct expr *expr)
{
    print_expr1(expr, 0);
}

void ast_dump_symbol(struct symbol *n)
{
    SET_OUTFD(stdout);
    print_symbol(n);
    RESTORE_OUTFD();
}

void ast_dump_type(struct symbol *n)
{
    SET_OUTFD(stdout);
    print_type(n);
    RESTORE_OUTFD();
}

/**
 * Convert type node to string.
 */

#define LPAREN  1
#define RPAREN  2
#define FCOMMA  3
#define FSPACE  4
struct type2s {
    int id;
    int qual;
    struct type *type;
};
static struct vector *type2s1(struct type * ty);

static struct type2s *paren(int id, struct type * ty)
{
    struct type2s *s = zmalloc(sizeof(struct type2s));
    s->id = id;
    s->type = ty;
    return s;
}

static void dotype2s(struct vector *l, struct vector *r)
{
    struct type2s *s;
    int k;

    if (vec_empty(l))
        return;

    s = vec_tail(l);
    k = TYPE_KIND(s->type);
    switch (k) {
    case POINTER:
        {
            struct vector *v = vec_new();
            for (int i = vec_len(l) - 1; i >= 0; i--) {
                struct type2s *s = vec_at(l, i);
                if (!isptr(s->type))
                    break;
                vec_push(v, s);
                vec_pop(l);
            }
            s = vec_tail(l);
            if (isfunc(s->type) || isarray(s->type)) {
                struct type2s *s2 = vec_head(r);
                bool rfunc = s2 && s2->type && isfunc(s2->type);
                if (rfunc)
                    vec_push_front(r,
                                   paren(LPAREN, s2->type));
                for (int i = 0; i < vec_len(v); i++)
                    vec_push_front(r, vec_at(v, i));
                vec_push_front(r, paren(LPAREN, s->type));
                vec_push_front(r, paren(FSPACE, NULL));
                if (rfunc)
                    vec_push(r, paren(RPAREN, s2->type));
                vec_push(r, paren(RPAREN, s->type));
            } else {
                for (int i = 0; i < vec_len(v); i++)
                    vec_push_front(r, vec_at(v, i));
                vec_push_front(r, paren(FSPACE, NULL));
            }
        }
        break;
    case FUNCTION:
        {
            struct type **params = TYPE_PROTO(s->type);
            size_t len = length(params);
            vec_push(r, paren(FSPACE, NULL));
            vec_push(r, paren(LPAREN, s->type));
            for (size_t i = 0; i < len; i++) {
                struct type *ty = params[i];
                struct vector *v = type2s1(ty);
                vec_add(r, v);
                if (i < len - 1) {
                    vec_push(r, paren(FCOMMA, NULL));
                    vec_push(r, paren(FSPACE, NULL));
                }
            }
            if (TYPE_VARG(s->type)) {
                vec_push(r, paren(FCOMMA, NULL));
                vec_push(r, paren(FSPACE, NULL));
                vec_push(r, paren(ELLIPSIS, NULL));
            }
            vec_push(r, paren(RPAREN, s->type));
            vec_pop(l);
        }
        break;
    case ARRAY:
        {
            vec_push(r, s);
            vec_pop(l);
        }
        break;
    default:
        {
            vec_push_front(r, s);
            vec_pop(l);
        }
        break;
    }

    dotype2s(l, r);
}

static struct vector *type2s1(struct type * ty)
{
    struct vector *l, *r, *v;

    v = vec_new();
    while (ty) {
        struct type2s *s = zmalloc(sizeof(struct type2s));
        if (isqual(ty)) {
            s->qual = _TYPE_KIND(ty);
            s->type = unqual(ty);
        } else {
            s->type = ty;
        }
        vec_push(v, s);
        if (isenum(s->type))
            ty = NULL;
        else
            ty = _TYPE_TYPE(s->type);
    }

    l = vec_reverse(v);
    r = vec_new();

    dotype2s(l, r);
    return r;
}

static void qualstr(struct strbuf *s, int q)
{
    if (isconst1(q))
        strbuf_cats(s, "const ");
    if (isvolatile1(q))
        strbuf_cats(s, "volatile ");
    if (isrestrict1(q))
        strbuf_cats(s, "restrict ");
}

const char *type2s(struct type * ty)
{
    struct strbuf *buf = strbuf_new();
    struct vector *v = type2s1(ty);
    for (int i = 0; i < vec_len(v); i++) {
        struct type2s *s = vec_at(v, i);
        if (s->id == LPAREN) {
            strbuf_cats(buf, "(");
        } else if (s->id == RPAREN) {
            strbuf_cats(buf, ")");
        } else if (s->id == FCOMMA) {
            strbuf_cats(buf, ",");
        } else if (s->id == FSPACE) {
            strbuf_cats(buf, " ");
        } else if (s->id == ELLIPSIS) {
            strbuf_cats(buf, "...");
        } else if (isptr(s->type)) {
            strbuf_cats(buf, "*");
            qualstr(buf, s->qual);
        } else if (isarray(s->type)) {
            if (TYPE_LEN(s->type) > 0) {
                strbuf_cats(buf, "[");
                strbuf_catd(buf, TYPE_LEN(s->type));
                strbuf_cats(buf, "]");
            } else {
                strbuf_cats(buf, "[]");
            }
        } else if (isenum(s->type) || isstruct(s->type)
                   || isunion(s->type)) {
            qualstr(buf, s->qual);
            strbuf_cats(buf, TYPE_NAME(s->type));
            if (TYPE_TAG(s->type)) {
                strbuf_cats(buf, " ");
                strbuf_cats(buf, TYPE_TAG(s->type));
            }
        } else {
            qualstr(buf, s->qual);
            strbuf_cats(buf, TYPE_NAME(s->type));
        }
    }

    return strbuf_str(strbuf_strip(buf));
}

/**
 * Convert expression node to string.
 */
const char *expr2s(struct expr * node)
{
    struct strbuf *s = strbuf_new();
    int id = EXPR_ID(node);
    struct expr *l = EXPR_OPERAND(node, 0);
    struct expr *r = EXPR_OPERAND(node, 1);

    switch (id) {
    case BINARY_OPERATOR:
        strbuf_cats(s, expr2s(l));
        strbuf_cats(s, format(" %s ", id2s(EXPR_OP(node))));
        strbuf_cats(s, expr2s(r));
        break;
    case UNARY_OPERATOR:
        switch (EXPR_OP(node)) {
        case INCR:
        case DECR:
            if (EXPR_PREFIX(node)) {
                strbuf_cats(s, id2s(EXPR_OP(node)));
                strbuf_cats(s, expr2s(l));
            } else {
                strbuf_cats(s, expr2s(l));
                strbuf_cats(s, id2s(EXPR_OP(node)));
            }
            break;
        case '*':
        case '&':
        case '+':
        case '-':
        case '~':
        case '!':
        case SIZEOF:
        default:
            assert(0);
        }
        break;
    case SUBSCRIPT_EXPR:
        strbuf_cats(s, expr2s(l));
        strbuf_cats(s, "[");
        strbuf_cats(s, expr2s(r));
        strbuf_cats(s, "]");
        break;
    case COND_EXPR:
        strbuf_cats(s, expr2s(EXPR_COND(node)));
        strbuf_cats(s, " ? ");
        strbuf_cats(s, expr2s(EXPR_THEN(node)));
        strbuf_cats(s, " : ");
        strbuf_cats(s, expr2s(EXPR_ELSE(node)));
        break;
    case MEMBER_EXPR:
        strbuf_cats(s, expr2s(l));
        strbuf_cats(s, id2s(EXPR_OP(node)));
        strbuf_cats(s, EXPR_NAME(node));
        break;
    case PAREN_EXPR:
        strbuf_cats(s, format("(%s)", expr2s(l)));
        break;
    case CALL_EXPR:
        {
            const char *func = expr2s(l);
            struct expr **args = EXPR_ARGS(node);
            size_t len = length(args);
            strbuf_cats(s, func);
            strbuf_cats(s, "(");
            for (size_t i = 0; i < len; i++) {
                struct expr *arg = args[i];
                const char *s1 = expr2s(arg);
                strbuf_cats(s, s1);
                if (i != len - 1)
                    strbuf_cats(s, ", ");
            }
            strbuf_cats(s, ")");
        }
        break;
    case CAST_EXPR:
        strbuf_cats(s, format("(%s)%s", type2s(EXPR_TYPE(node)), expr2s(l)));
        break;
    case CONV_EXPR:
        strbuf_cats(s, expr2s(l));
        break;
    case REF_EXPR:
        strbuf_cats(s, EXPR_SYM(node)->name);
        break;
    case INTEGER_LITERAL:
        if (TYPE_OP(EXPR_TYPE(node)) == INT)
            strbuf_cats(s, format("%lld", ILITERAL_VALUE(node).i));
        else
            strbuf_cats(s, format("%llu", ILITERAL_VALUE(node).u));
        break;
    case FLOAT_LITERAL:
        strbuf_cats(s, format("%Lf", FLITERAL_VALUE(node).d));
        break;
    case STRING_LITERAL:
        strbuf_cats(s, EXPR_SYM(node)->name);
        break;
    case COMPOUND_LITERAL:
        strbuf_cats(s, format("(%s){...}", type2s(EXPR_TYPE(node))));
        break;
    case INITS_EXPR:
    case VINIT_EXPR:
        strbuf_cats(s, "{initializer}");
        break;
    default:
        assert(0);
    }
    return STR(strbuf_str(s));
}

void print_source(struct source src)
{
    println("location: %s:%u:%u", src.file, src.line, src.column);
}

// TODO: print typedef names
