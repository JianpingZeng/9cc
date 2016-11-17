#include <stdio.h>
#include <assert.h>
#include <stdint.h>
#include "cc.h"
#include "libutils/color.h"


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

static const char *opkindstr[] = {
    "null",
    /// comma
    "RIGHT",
    /// cond
    "COND",
    /// constant
    "CNST",
    /// address
    "ADDRG",
    "ADDRP",
    "ADDRL",
    /// indirection
    "INDIR",
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
    "OR",
    /// unary
    "NEG",
    "BNOT",
    /// postfix
    "INITS",
    "CALL",
    "BFIELD",
    /// conversion
    "CVI",
    "CVU",
    "CVF",
    "CVP",
};

static const char *optypestr[] = {
    "", "I", "U", "F", "P", "S"
};

static void print_symbol1(FILE *fp, struct symbol *sym,
                          int level, const char *prefix);
static void print_expr1(FILE *fp, struct tree *expr, int level);
static const char *type2s(struct type *ty);
static const char *desig2s(struct desig *desig);

void vfprint(FILE *fp, const char *fmt, va_list ap)
{
    for (; *fmt; fmt++) {
        if (*fmt == '%') {
            switch (*++fmt) {
            case 'c':
                fprintf(fp, "%c", (char)va_arg(ap, int));
                break;
            case 'd':
            case 'i':
                fprintf(fp, "%d", va_arg(ap, int));
                break;
            case 'u':
                fprintf(fp, "%u", va_arg(ap, unsigned int));
                break;
            case 'x':
                fprintf(fp, "%x", va_arg(ap, int));
                break;
            case 'X':
                fprintf(fp, "%X", va_arg(ap, int));
                break;
            case 'o':
                fprintf(fp, "%o", va_arg(ap, int));
                break;
            case 's':
                fputs(va_arg(ap, char *), fp);
                break;
            case 'p':
                fprintf(fp, "%p", va_arg(ap, void *));
                break;
            case 'f':
                fprintf(fp, "%f", va_arg(ap, double));
                break;
                // lu, ld, llu, lld
            case 'l':
                if (fmt[1] == 'd') {
                    fmt++;
                    fprintf(fp, "%ld", va_arg(ap, long));
                } else if (fmt[1] == 'u') {
                    fmt++;
                    fprintf(fp, "%lu", va_arg(ap, unsigned long));
                } else if (fmt[1] == 'l' && fmt[2] == 'd') {
                    fmt += 2;
                    fprintf(fp, "%lld", va_arg(ap, long long));
                } else if (fmt[1] == 'l' && fmt[2] == 'u') {
                    fmt += 2;
                    fprintf(fp, "%llu", va_arg(ap, unsigned long long));
                } else {
                    putc(*fmt, fp);
                }
                break;
                // Lf
            case 'L':
                if (fmt[1] == 'f') {
                    fmt++;
                    fprintf(fp, "%Lf", va_arg(ap, long double));
                } else {
                    putc(*fmt, fp);
                }
                break;
                /// customize
                // type
            case 'T':
                {
                    struct type *ty = va_arg(ap, struct type *);
                    fprintf(fp, "%s", type2s(ty));
                }
                break;
                // source
            case 'S':
                {
                    struct source src = va_arg(ap, struct source);
                    fprintf(fp, "%s:%u:%u", src.file, src.line, src.column);
                }
                break;
                // token
            case 't':
                {
                    struct token *t = va_arg(ap, struct token *);
                    fprintf(fp, "%s", tok2s(t));
                }
                break;
                // desig
            case 'D':
                {
                    struct desig *d = va_arg(ap, struct desig *);
                    fprintf(fp, "%s", desig2s(d));
                }
                break;
            default:
                putc(*fmt, fp);
                break;
            }
        } else {
            putc(*fmt, fp);
        }
    }
}

void fprint(FILE *fp, const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    vfprint(fp, fmt, ap);
    va_end(ap);
}

void print(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    vfprint(stdout, fmt, ap);
    va_end(ap);
}

static char *opidname(int op)
{
    const char *index, *type;

    index = opkindstr[opindex(op)];
    type = optypestr[OPTYPE(op)];
    return format("%s%s", index, type);
}

static void print_level(FILE *fp, int level)
{
    for (int i = 0; i < level; i++)
        fprint(fp, "  ");
}

static void print_ty(FILE *fp, struct type *ty)
{
    if (ty) {
        if (isfunc(ty) || isptr(ty) || isarray(ty))
            fprint(fp, RED_BOLD("'%s' "), TYPE_NAME(ty));

        fprint(fp, GREEN("'%T' "), ty);

        if (isarray(ty) || isstruct(ty) || isunion(ty)) {
            fprint(fp, "<" YELLOW("size=%ld") "> ", TYPE_SIZE(ty));
        } else if (isfunc(ty)) {
            fprint(fp, "%s ", TYPE_OLDSTYLE(ty) ? "oldstyle" : "prototype");
            if (TYPE_INLINE(ty))
                fprint(fp, "inline ");
        }
    }
}

static void print_field1(FILE *fp, struct field *field,
                         int level, const char *prefix)
{
    const char *name = field->name;
    struct type *ty = field->type;

    print_level(fp, level);
    fprint(fp, GREEN("%s ") YELLOW("%p "), prefix, field);

    if (field->isbit)
        fprint(fp, "<" RED("offset=%lu, bitoff=%d, bits=%d" "> "),
               field->offset, field->bitoff, field->bitsize);
    else
        fprint(fp, "<" GREEN("offset=%lu") "> ", field->offset);

    print_ty(fp, ty);

    if (name)
        fprint(fp, CYAN_BOLD("%s"), name);
    else
        fprint(fp, "anonymous");

    fprint(fp, "\n");
}

static void print_field(FILE *fp, struct field *field, int level)
{
    if (isindirect(field)) {
        print_level(fp, level);
        fprint(fp, GREEN("%s ") YELLOW("%p "), kIndirectFieldDecl, field);
        fprint(fp, "<" GREEN("offset=%lu") "> ", field->offset);
        fprint(fp, CYAN_BOLD("%s"), field->indir->name);
        fprint(fp, "\n");

        for (int i = 0; field->of[i]; i++)
            print_field1(fp, field->of[i], level + 1, kField);

        print_field1(fp, field->indir, level + 1, kField);
    } else {
        print_field1(fp, field, level, kFieldDecl);
    }
}

static void print_type1(FILE *fp, struct symbol *sym, int level)
{
    struct type *ty = sym->type;
    
    print_level(fp, level);
    if (sym->sclass == TYPEDEF)
        fprint(fp, GREEN_BOLD("%s ") YELLOW("%p ") CYAN_BOLD("%s "),
               kTypedefDecl, sym, sym->name);
    else if (isstruct(ty))
        fprint(fp, GREEN_BOLD("%s ") YELLOW("%p "),
               kStructDecl, sym);
    else if (isunion(ty))
        fprint(fp, GREEN_BOLD("%s ") YELLOW("%p "),
               kUnionDecl, sym);
    else if (isenum(ty))
        fprint(fp, GREEN_BOLD("%s ") YELLOW("%p "),
               kEnumDecl, sym);
    else
        CC_UNAVAILABLE();
    print_ty(fp, ty);
    fprint(fp, "<" YELLOW("%s:line:%u col:%u") "> ",
           sym->src.file, sym->src.line, sym->src.column);
    fprint(fp, "\n");
    if (sym->sclass != TYPEDEF) {
        if (isstruct(ty) || isunion(ty)) {
            struct field *first = sym->u.s.flist;
            for (struct field *p = first; p; p = p->link)
                print_field(fp, p, level + 1);
        } else if (isenum(ty)) {
            struct symbol **ids = sym->u.s.ids;
            for (int i = 0; ids[i]; i++)
                print_symbol1(fp, ids[i], level + 1, kEnumConstDecl);
        }
    }
}

static void print_type(FILE *fp, struct symbol *sym)
{
    print_type1(fp, sym, 0);
}

static void print_symbol1(FILE *fp, struct symbol *sym,
                          int level, const char *prefix)
{
    print_level(fp, level);

    fprint(fp, GREEN_BOLD("%s "), prefix);
    fprint(fp, YELLOW("%p ") CYAN_BOLD("%s "), sym, sym->name);
    
    if (sym->defined)
        fprint(fp, "<" YELLOW("defined") "> ");

    struct type *ty = sym->type;
    print_ty(fp, ty);
    // location
    fprint(fp, "<" YELLOW("%s:line:%u col:%u") "> ",
           sym->src.file, sym->src.line, sym->src.column);
    // scope
    if (sym->scope >= LOCAL)
        fprint(fp, "<" YELLOW("scope:%d") ">", sym->scope);
    fprint(fp, "\n");

    if (isfuncdef(sym)) {
        //NOTE: print in ast_dump_symbol
    } else if (sym->sclass != ENUM) {
        // skip enum id
        struct tree *init = sym->u.init;
        if (init)
            print_expr1(fp, init, level + 1);
    }
}

static void print_symbol(FILE *fp, struct symbol *sym, const char *prefix)
{
    print_symbol1(fp, sym, 0, prefix);
}

static void print_init1(FILE *fp, struct tree *init, int level)
{
    for (struct init *p = init->s.u.ilist; p; p = p->link) {
        print_level(fp, level);
        if (p->desig->kind == DESIG_FIELD &&
            p->desig->u.field->isbit)
            fprint(fp, "<"
                   GREEN("offset=%lu, boff=%lu, bsize=%lu, type='%T'")
                   ">\n",
                   p->desig->offset,
                   p->desig->u.field->bitoff,
                   p->desig->u.field->bitsize,
                   p->desig->type);
        else
            fprint(fp, "<" GREEN("offset=%lu, type='%T'") ">\n",
                   p->desig->offset, p->desig->type);;

        if (p->body)
            print_expr1(fp, p->body, level + 1);
    }
}

static void print_args1(FILE *fp, struct tree **args, int level)
{
    assert(args);
    for (int i = 0; args[i]; i++) {
        print_level(fp, level);
        fprint(fp, "ARG[%d]: \n", i);
        print_expr1(fp, args[i], level + 1);
    }
}

static void print_expr1(FILE *fp, struct tree *expr, int level)
{
    const char *name = opidname(expr->op);

    print_level(fp, level);
    fprint(fp, PURPLE_BOLD("%s ") YELLOW("%p "), name, expr);
    fprint(fp, GREEN("'%T' "), expr->type);

    if (issliteral(expr)) {
        fprint(fp, CYAN_BOLD("\"%s\""), expr->s.sym->name);
    } else if (isiliteral(expr)) {
        if (TYPE_OP(expr->type) == INT)
            fprint(fp, RED("%ld"), expr->s.value.i);
        else
            fprint(fp, RED("%lu"), expr->s.value.u);
    } else if (isfliteral(expr)) {
        if (TYPE_KIND(expr->type) == FLOAT)
            fprint(fp, RED("%f"), (float)expr->s.value.d);
        else if (TYPE_KIND(expr->type) == DOUBLE)
            fprint(fp, RED("%f"), (double)expr->s.value.d);
        else
            fprint(fp, RED("%Lf"), (long double)expr->s.value.d);
    } else if (ispliteral(expr)) {
        fprint(fp, RED("%p"), expr->s.value.p);
    } else if (expr->s.sym) {
        fprint(fp, CYAN_BOLD("%s"), expr->s.sym->name);
    }

    fprint(fp, "\n");
    if (expr->kids[0])
        print_expr1(fp, expr->kids[0], level + 1);
    if (expr->kids[1])
        print_expr1(fp, expr->kids[1], level + 1);

    if (OPKIND(expr->op) == CALL)
        // print arguments
        print_args1(fp, expr->s.u.args, level + 1);
    else if (iscpliteral(expr))
        // print compound literal
        print_init1(fp, COMPOUND_SYM(expr)->u.init, level + 1);
    else if (OPKIND(expr->op) == INITS)
        print_init1(fp, expr, level + 1);
}

static void print_stmt1(FILE *fp, struct stmt *stmt, int level)
{
    if (stmt->id != GEN)
        print_level(fp, level);

    switch (stmt->id) {
    case LABEL:
        fprint(fp, ".L%d:\n", stmt->u.label);
        break;

    case GEN:
        assert(stmt->u.expr && "null expr in gen node");
        print_expr1(fp, stmt->u.expr, level);
        break;

    case JMP:
        fprint(fp, "goto .L%d\n", stmt->u.label);
        break;

    case CBR:
        if (stmt->u.cbr.tlab)
            fprint(fp, "iftrue goto .L%d\n", stmt->u.cbr.tlab);
        else if (stmt->u.cbr.flab)
            fprint(fp, "iffalse goto .L%d\n", stmt->u.cbr.flab);

        if (stmt->u.cbr.expr)
            print_expr1(fp, stmt->u.cbr.expr, level + 1);
        break;

    case RET:
        fprint(fp, "ret\n");
        if (stmt->u.expr)
            print_expr1(fp, stmt->u.expr, level + 1);
        break;

    default:
        assert(0 && "unknown stmt type");
    }
}

void print_expr(FILE *fp, struct tree *expr)
{
    print_expr1(fp, expr, 0);
}

void ast_dump_vardecl(struct symbol *n)
{
    print_symbol(stdout, n, kVarDecl);
}

void ast_dump_funcdecl(struct symbol *n)
{
    print_symbol(stdout, n, kFuncDecl);
}

void ast_dump_funcdef(struct symbol *n)
{
    print_symbol(stdout, n, kFuncDecl);
    for (struct symbol *sym = n->u.f.lvars; sym; sym = sym->local) {
        if (!sym->temporary && !(sym->predefine && sym->refs == 0))
            print_symbol1(stdout, sym, 1, kVarDecl);
    }
    for (struct stmt *stmt = n->u.f.stmt; stmt; stmt = stmt->next)
        print_stmt1(stdout, stmt, 1);
}

void ast_dump_typedecl(struct symbol *n)
{
    print_type(stdout, n);
}

/// Convert type node to string.

#define LPAREN  1
#define RPAREN  2
#define FCOMMA  3
#define FSPACE  4
struct type2s {
    int id;
    int qual;
    struct type *type;
};
static struct vector *type2s1(struct type *ty);

static struct type2s *paren(int id, struct type *ty)
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

static struct vector *type2s1(struct type *ty)
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

static const char *qualstr(int q)
{
    switch (q) {
    case CONST:
        return "const ";
    case VOLATILE:
        return "volatile ";
    case RESTRICT:
        return "restrict ";
    case CONST+VOLATILE:
        return "const volatile ";
    case CONST+RESTRICT:
        return "const restrict ";
    case VOLATILE+RESTRICT:
        return "volatile restrict ";
    case CONST+VOLATILE+RESTRICT:
        return "const volatile restrict ";
    default:
        return "";
    }
}

static const char *type2s(struct type *ty)
{
    char buf[1024];
    char *bp = buf, *be = buf + ARRAY_SIZE(buf);
    int size = ARRAY_SIZE(buf);
    struct vector *v = type2s1(ty);
    for (int i = 0; i < vec_len(v); i++) {
        struct type2s *s = vec_at(v, i);
        if (s->id == LPAREN) {
            snprintf(bp, size, "(");
        } else if (s->id == RPAREN) {
            snprintf(bp, size, ")");
        } else if (s->id == FCOMMA) {
            snprintf(bp, size, ",");
        } else if (s->id == FSPACE) {
            snprintf(bp, size, " ");
        } else if (s->id == ELLIPSIS) {
            snprintf(bp, size, "...");
        } else if (isptr(s->type)) {
            snprintf(bp, size, "*%s", qualstr(s->qual));
        } else if (isarray(s->type)) {
            if (TYPE_LEN(s->type) > 0)
                snprintf(bp, size, "[%lu]", TYPE_LEN(s->type));
            else
                snprintf(bp, size, " []");
        } else if (isenum(s->type) ||
                   isstruct(s->type) ||
                   isunion(s->type)) {
            snprintf(bp, size, "%s%s", qualstr(s->qual), TYPE_NAME(s->type));
            if (!TYPE_TSYM(s->type)->anonymous) {
                bp += strlen(bp);
                size = be - bp;
                snprintf(bp, size, " %s", TYPE_TSYM(s->type)->name);
            }
        } else {
            snprintf(bp, size, "%s%s", qualstr(s->qual), TYPE_NAME(s->type));
        }
        bp += strlen(bp);
        size = be - bp;
    }

    return strip(buf);
}
// TODO: print typedef names

// for debug
static const char *desig2s(struct desig *desig)
{
    const char *s = "";
    
    assert(desig);

    for (struct desig *d = desig; d;) {
        switch (d->kind) {
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
