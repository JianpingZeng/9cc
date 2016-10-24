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

#define kVarDecl            "VarDecl"
#define kTypedefDecl        "TypedefDecl"
#define kFuncDecl           "FuncDecl"
#define kStructDecl         "StructDecl"
#define kUnionDecl          "UnionDecl"
#define kEnumDecl           "EnumDecl"
#define kFieldDecl          "FieldDecl"
#define kIndirectFieldDecl  "IndirectFieldDecl"
#define kField              "Field"
#define kEnumConstDecl      "EnumConstantDecl"

static void print_expr1(struct expr * node, int level);
static void print_stmt1(struct stmt *stmt, int level);
static void print_type(struct symbol *sym);
static void print_symbol1(struct symbol *sym, int level, const char *prefix);

static const char *nnames[] = {
    "null",
    /// constant
    "CNST",
    /// address
    "ADDRG",
    "ADDRP",
    "ADDRL",
    /// indirection
    "INDIR",
    // conversion
    "CVI",
    "CVU",
    "CVF",
    "CVP",
    /// binary
    "ASGN",
    "MUL",
    "DIV",
    "ADD",
    "SUB",
    "MOD",
    "SHL",
    "SHR",
    "BAND",
    "BOR",
    "XOR",
    "EQ",
    "NE",
    "GT",
    "GE",
    "LT",
    "LE",
    "AND",
    "OR"
    /// unary
    "NEG",
    "NOT",
    /// postfix
    "COMPOUND",
    "CALL",
    "BFIELD",
    /// others
    "COND",
    "RIGHT",
};

static const char *nname(int op)
{
    return nnames[OPINDEX(op)];
}

static char *opidname(int op)
{
    const char *index, *type;

    index = nname(op);
    switch (OPTYPE(op)) {
    case I: type = "I"; break;
    case U: type = "U"; break;
    case F: type = "F"; break;
    case P: type = "P"; break;
    case S: type = "S"; break;
    case 0: type = ""; break;
    default: assert(0 && "unknown op type");
    }
    
    return format("%s%s", index, type);
}

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

static void print_level(int level)
{
    for (int i = 0; i < level; i++)
        putf("  ");
}

static void print_ty(struct type * ty)
{
    if (ty) {
        if (isfunc(ty) || isptr(ty) || isarray(ty))
            putf(RED_BOLD("'%s' "), TYPE_NAME(ty));

        putf(GREEN("'%s' "), type2s(ty));

        if (isarray(ty) || isstruct(ty) || isunion(ty)) {
            putf("<" YELLOW("size=%ld") "> ", TYPE_SIZE(ty));
        } else if (isfunc(ty)) {
            putf("%s ", TYPE_OLDSTYLE(ty) ? "oldstyle" : "prototype");
            if (TYPE_INLINE(ty))
                putf("inline ");
        }
    }
}

static void print_field1(struct field * node, int level, const char *prefix)
{
    const char *name = node->name;
    struct type *ty = node->type;

    print_level(level);

    putf(GREEN("%s ") YELLOW("%p "), prefix, node);
    if (node->isbit)
        putf("<" RED("offset=%lu, bitoff=%d, bits=%d" "> "),
             node->offset, node->bitoff, node->bitsize);
    else
        putf("<" GREEN("offset=%lu") "> ", node->offset);

    print_ty(ty);
    if (name)
        putf(CYAN_BOLD("%s"), name);
    else
        putf("anonymous");
    putf("\n");
}

static void print_field(struct field * node, int level)
{
    if (isindirect(node)) {
        print_level(level);
        putf(GREEN("%s ") YELLOW("%p "), kIndirectFieldDecl, node);
        putf("<" GREEN("offset=%lu") "> ", node->offset);
        putf(CYAN_BOLD("%s"), node->indir->name);
        putf("\n");
        for (int i = 0; node->of[i]; i++)
            print_field1(node->of[i], level + 1, kField);
        print_field1(node->indir, level + 1, kField);
    } else {
        print_field1(node, level, kFieldDecl);
    }
}

static void print_type1(struct symbol *sym, int level)
{
    struct type *ty = sym->type;
    
    print_level(level);
    if (sym->sclass == TYPEDEF)
        putf(GREEN_BOLD("%s ") YELLOW("%p ") CYAN_BOLD("%s "),
             kTypedefDecl, sym, sym->name);
    else if (isstruct(ty))
        putf(GREEN_BOLD("%s ") YELLOW("%p "),
             kStructDecl, sym);
    else if (isunion(ty))
        putf(GREEN_BOLD("%s ") YELLOW("%p "),
             kUnionDecl, sym);
    else if (isenum(ty))
        putf(GREEN_BOLD("%s ") YELLOW("%p "),
             kEnumDecl, sym);
    else
        CC_UNAVAILABLE();
    print_ty(ty);
    putf("<" YELLOW("%s:line:%u col:%u") "> ",
         sym->src.file, sym->src.line, sym->src.column);
    putf("\n");
    if (isstruct(ty) || isunion(ty)) {
        struct field *first = sym->u.s.flist;
        for (struct field *p = first; p; p = p->link)
            print_field(p, level + 1);
    } else if (isenum(ty)) {
        struct symbol **ids = sym->u.s.ids;
        for (int i = 0; ids[i]; i++)
            print_symbol1(ids[i], level + 1, kEnumConstDecl);
    }
}

static void print_type(struct symbol *sym)
{
    print_type1(sym, 0);
}

static void print_symbol1(struct symbol *sym, int level, const char *prefix)
{
    print_level(level);

    putf(GREEN_BOLD("%s "), prefix);
    putf(YELLOW("%p ") CYAN_BOLD("%s "), sym, STR(sym->name));
    
    if (sym->defined)
        putf("<" YELLOW("defined") "> ");

    struct type *ty = sym->type;
    print_ty(ty);
    putf("<" YELLOW("%s:line:%u col:%u") "> ",
         sym->src.file, sym->src.line, sym->src.column);
    putf("\n");

    if (isfuncdef(sym)) {
        //NOTE: print in ast_dump_symbol
    } else {
        struct expr *init = sym->u.init;
        if (init)
            print_expr1(init, level + 1);
    }
}

static void print_symbol(struct symbol *sym, const char *prefix)
{
    print_symbol1(sym, 0, prefix);
}

static void print_init1(struct init *init, int level)
{
    for (struct init *p = init; p; p = p->link) {
        print_level(level);
        putln("<" GREEN("offset=%lu, boff=%lu, bsize=%lu, type='%s', body=%p") ">",
             p->offset, p->boff, p->bsize, type2s(p->type), p->body);
    }
}

static void print_args1(struct expr **args, int level)
{
    assert(args);
    for (int i = 0; args[i]; i++) {
        print_level(level);
        putf("ARG[%d]: \n", i);
        print_expr1(args[i], level + 1);
    }
}

static void print_expr1(struct expr *node, int level)
{
    const char *name = opidname(node->op);

    print_level(level);
    putf(PURPLE_BOLD("%s ") YELLOW("%p "), name, node);
    putf(GREEN("'%s' "), type2s(node->type));
    if (node->x.sym) {
        if (issliteral(node))
            putf(CYAN_BOLD("%s"), node->x.sym->u.cnst->name);
        else
            putf(CYAN_BOLD("%s"), node->x.sym->name);
    }

    putf("\n");
    if (node->kids[0])
        print_expr1(node->kids[0], level + 1);
    if (node->kids[1])
        print_expr1(node->kids[1], level + 1);

    switch (OPINDEX(node->op)) {
    case COMPOUND:
        print_init1(node->x.u.inits, level + 1);
        break;
    case CALL:
        print_args1(node->x.u.args, level + 1);
        break;
    }
}

static void print_stmt1(struct stmt *stmt, int level)
{
    if (stmt->id != GEN)
        print_level(level);

    switch (stmt->id) {
    case LABEL:
        putln(".L%d:", stmt->u.label);
        break;

    case GEN:
        assert(stmt->u.expr && "null expr in gen node");
        print_expr1(stmt->u.expr, level);
        break;

    case JMP:
        putln("goto .L%d", stmt->u.label);
        break;

    case CBR:
        if (stmt->u.cbr.tlab)
            putln("iftrue goto .L%d", stmt->u.cbr.tlab);
        else if (stmt->u.cbr.flab)
            putln("iffalse goto .L%d", stmt->u.cbr.flab);

        if (stmt->u.cbr.expr)
            print_expr1(stmt->u.cbr.expr, level + 1);
        break;

    case RET:
        putln("ret");
        if (stmt->u.expr)
            print_expr1(stmt->u.expr, level + 1);
        break;

    default:
        assert(0 && "unknown stmt type");
    }
}

static void ast_dump_symbol(struct symbol *n, const char *prefix, bool funcdef)
{
    SET_OUTFD(stdout);
    print_symbol(n, prefix);
    if (funcdef) {
        for (struct stmt *stmt = n->u.f.stmt; stmt; stmt = stmt->next)
            print_stmt1(stmt, 1);
    }
    RESTORE_OUTFD();
}

void ast_dump_vardecl(struct symbol *n)
{
    ast_dump_symbol(n, kVarDecl, false);
}

void ast_dump_funcdecl(struct symbol *n)
{
    ast_dump_symbol(n, kFuncDecl, false);
}

void ast_dump_funcdef(struct symbol *n)
{
    ast_dump_symbol(n, kFuncDecl, true);
}

void ast_dump_typedecl(struct symbol *n)
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
            s->qual = ty->kind;
            s->type = unqual(ty);
        } else {
            s->type = ty;
        }
        vec_push(v, s);
        if (isenum(s->type))
            ty = NULL;
        else
            ty = s->type->type;
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
                strbuf_cats(buf, " [");
                strbuf_catd(buf, TYPE_LEN(s->type));
                strbuf_cats(buf, "]");
            } else {
                strbuf_cats(buf, " []");
            }
        } else if (isenum(s->type) ||
                   isstruct(s->type) ||
                   isunion(s->type)) {
            qualstr(buf, s->qual);
            strbuf_cats(buf, TYPE_NAME(s->type));
            if (TYPE_TSYM(s->type)->name) {
                strbuf_cats(buf, " ");
                strbuf_cats(buf, TYPE_TSYM(s->type)->name);
            }
        } else {
            qualstr(buf, s->qual);
            strbuf_cats(buf, TYPE_NAME(s->type));
        }
    }

    return strbuf_str(strbuf_strip(buf));
}
// TODO: print typedef names

// for debug
const char *desig2s(struct desig *desig)
{
    const char *s = "";
    
    assert(desig);

    for (struct desig *d = desig; d;) {
        switch (d->id) {
        case DESIG_NONE:
            assert(d->prev == NULL);
            if (d->all) {
                d = d->all;
                continue;
            } else {
                s = format("<%s>%s", type2s(d->type), s);
            }
            break;

        case DESIG_FIELD:
            s = format(".%s%s", d->u.field->name, s);
            break;

        case DESIG_INDEX:
            s = format("[%ld]%s", d->u.index, s);
            break;

        default:
            assert(0 && "unknown designator type");
        }
        d = d->prev;
    }
    
    return s;
}
