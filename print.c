#include <stdio.h>
#include <assert.h>
#include <stdint.h>
#include "cc.h"
#include "utils/color.h"

#define STR(str)  ((str) ? (str) : "<null>")
static FILE *outfd;
#define SET_OUTFD(fd) \
    FILE *_saved_fd = outfd; \
    outfd = fd

#define RESTORE_OUTFD() \
    outfd = _saved_fd


static void print_stmt1(struct stmt * node, int level);
static void print_expr1(struct expr * node, int level);

static void ensure_outfd(void)
{
    if (!outfd)
        outfd = stderr;
}

static void putf(const char *fmt, ...)
{
    ensure_outfd();
    va_list ap;
    va_start(ap, fmt);
    vfprintf(outfd, fmt, ap);
    va_end(ap);
}

static void putln(const char *fmt, ...)
{
    ensure_outfd();
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
    struct type *ty = SYM_TYPE(sym);
    
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
    const char *name = FIELD_NAME(node);
    struct type *ty = FIELD_TYPE(node);

    putf(GREEN("Field "));
    if (FIELD_ISBIT(node))
        putf(RED("<offset=%d, bitoff=%d, bits=%d> "),
             FIELD_OFFSET(node), FIELD_BITOFF(node), FIELD_BITSIZE(node));
    else
        putf(GREEN("<offset=%d> "), FIELD_OFFSET(node));

    print_ty(ty);
    putf(CYAN("%s"), STR(name));
    putf("\n");
}

static void print_label(const char *name, int level)
{
    for (int i = 0; i < level; i++)
        putf("  ");
    putf(RED("%s\n"), name);
}

static void print_symbol1(struct symbol *sym, int level)
{
    putf(CYAN("%s "), STR(SYM_NAME(sym)));
    
    if (SYM_DEFINED(sym))
        putf(YELLOW("<defined> "));

    struct type *ty = SYM_TYPE(sym);
    print_ty(ty);
    putf("<scope: %d>", SYM_SCOPE(sym));
    putf(YELLOW("<line:%u col:%u> "), SYM_SRC(sym).line, SYM_SRC(sym).column);

    if (isfuncdef(sym))
        putf("%llu localvars ", vec_len(SYM_X_LVARS(sym)));

    putf("\n");

    if (isfuncdef(sym)) {
        struct stmt *init = SYM_COMPOUND(sym);
        if (init)
            print_stmt1(init, level + 1);
    } else {
        struct expr *init = SYM_INIT(sym);
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
        putf(CYAN("%s "), STR(SYM_NAME(EXPR_SYM(node))));
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

static void print_stmt1(struct stmt * node, int level)
{
    for (int i = 0; i < level; i++)
        putf("  ");
    
    putf(PURPLE("%s ") YELLOW("%p "), nname(STMT_ID(node)), node);
    putf("\n");

    switch (STMT_ID(node)) {
    case COMPOUND_STMT:
        {
            struct stmt **blks = STMT_BLKS(node);
            if (blks) {
                for (size_t i = 0; blks[i]; i++) {
                    struct stmt *blk = blks[i];
                    print_stmt1(blk, level + 1);
                }
            }
        }
        break;
    case FOR_STMT:
        {
            struct expr *init = STMT_FOR_INIT(node);
            struct expr *cond = STMT_FOR_COND(node);
            struct expr *ctrl = STMT_FOR_CTRL(node);
            struct stmt *body = STMT_FOR_BODY(node);
            if (init) {
                print_expr1(init, level + 1);
            } else {
                for (int i = 0; i < level + 1; i++)
                    putf("  ");
                putf("init: <NULL>\n");
            }
        
            if (cond) {
                print_expr1(cond, level + 1);
            } else {
                for (int i = 0; i < level + 1; i++)
                    putf("  ");
                putf("cond: <NULL>\n");
            }
        
            if (ctrl) {
                print_expr1(ctrl, level + 1);
            } else {
                for (int i = 0; i < level + 1; i++)
                    putf("  ");
                putf("ctrl: <NULL>\n");
            }

            if (body) {
                print_stmt1(body, level + 1);
            } else {
                for (int i = 0; i < level + 1; i++)
                    putf("  ");
                putf("ctrl: <NULL>\n");
            }
        }
        break;
    case IF_STMT:
        {
            struct expr *cond = STMT_COND(node);
            struct stmt *then = STMT_THEN(node);
            struct stmt *els = STMT_ELSE(node);

            if (cond)
                print_expr1(cond, level + 1);
            if (then)
                print_stmt1(then, level + 1);
            if (els)
                print_stmt1(els, level + 1);
        }
        break;
    case DO_WHILE_STMT:
    case WHILE_STMT:
        {
            struct expr *cond = STMT_WHILE_COND(node);
            struct stmt *body = STMT_WHILE_BODY(node);

            if (cond)
                print_expr1(cond, level + 1);
            if (body)
                print_stmt1(body, level + 1);
        }
        break;
    case SWITCH_STMT:
        {
            struct expr *expr = STMT_SWITCH_EXPR(node);
            struct stmt *body = STMT_SWITCH_BODY(node);

            if (expr)
                print_expr1(expr, level + 1);
            if (body)
                print_stmt1(body, level + 1);
        }
        break;
    case CASE_STMT:
    case DEFAULT_STMT:
        {
            struct stmt *body = STMT_CASE_BODY(node);

            if (body)
                print_stmt1(body, level + 1);
        }
        break;
    case RETURN_STMT:
        {
            struct expr *expr = STMT_RETURN_EXPR(node);

            if (expr)
                print_expr1(expr, level + 1);
        }
    case GOTO_STMT:
        {
            const char *label = STMT_LABEL_NAME(node);

            if (label)
                print_label(label, level + 1);
        }
        break;
    case LABEL_STMT:
        {
            const char *label = STMT_LABEL_NAME(node);
            struct stmt *body = STMT_LABEL_BODY(node);

            if (label)
                print_label(label, level + 1);
            if (body)
                print_stmt1(body, level + 1);
        }
        break;
    case EXPR_STMT:
        {
            struct expr *expr = STMT_EXPR_BODY(node);
            if (expr)
                print_expr1(expr, level + 1);
        }
        break;
    case BREAK_STMT:
    case CONTINUE_STMT:
    case NULL_STMT:
        break;
    default:
        assert(0);
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

void print_stmt(struct stmt *stmt)
{
    print_stmt1(stmt, 0);
}

static const char * operand2s(struct operand *operand)
{
    switch (operand->op) {
    case IR_SUBSCRIPT:
        return format("%ld(%s,%s,%d)",
                      operand->disp,
                      SYM_X_LABEL(operand->sym),
                      operand->index ? SYM_X_LABEL(operand->index) : "",
                      operand->scale);
    case IR_INDIRECTION:
        return format("* %s", SYM_X_LABEL(operand->sym));
    case IR_NONE:
        return SYM_X_LABEL(operand->sym);
    default:
        assert(0);
    }
}

void print_tac(struct tac *tac)
{
    putf(GREEN_COLOR);
    switch (tac->opsize) {
    case Quad:
        putf("[Q] ");
        break;
    case Long:
        putf("[L] ");
        break;
    case Word:
        putf("[W] ");
        break;
    case Byte:
        putf("[B] ");
        break;
    default:
        putf("[Z] ");
        break;
    }
    switch (tac->op) {
    case IR_NONE:
        break;
    case IR_LABEL:
        putf(RED("%s:"), operand2s(tac->operands[0]));
        break;
    case IR_GOTO:
        putf("%s " YELLOW("%s"),
             rop2s(tac->op),
             operand2s(tac->operands[0]));
        break;
    case IR_RETURNI:
    case IR_RETURNF:
        putf("%s %s",
             rop2s(tac->op),
             tac->operands[1] ? operand2s(tac->operands[1]) : "");
        break;
    case IR_IF_I:
    case IR_IF_F:
    case IR_IF_FALSE_I:
    case IR_IF_FALSE_F:
        if (tac->relop) {
            // rel if
            putf("%s %s %s %s %s " YELLOW("%s"),
                 rop2s(tac->op),
                 operand2s(tac->operands[1]),
                 id2s(tac->relop),
                 operand2s(tac->operands[2]),
                 rop2s(IR_GOTO),
                 operand2s(tac->operands[0]));
        } else {
            // simple if
            putf("%s %s %s " YELLOW("%s"),
                 rop2s(tac->op),
                 operand2s(tac->operands[1]),
                 rop2s(IR_GOTO),
                 operand2s(tac->operands[0]));
        }
        if (tac->op == IR_IF_I || tac->op == IR_IF_FALSE_I)
            putf(" \t(Integer)");
        else
            putf(" \t(Float)");
        putf(" (%s)", tac->sign ? "SIGNED" : "UNSIGNED");
        break;
    case IR_ASSIGNI:
    case IR_ASSIGNF:
        putf("%s = %s \t(%s)",
             operand2s(tac->operands[0]),
             operand2s(tac->operands[1]),
             tac->op == IR_ASSIGNF ? "ASSIGNF" : "ASSIGNI");
        break;
    case IR_ADDI:
    case IR_ADDF:
    case IR_SUBI:
    case IR_SUBF:
    case IR_DIVI:
    case IR_IDIVI:
    case IR_DIVF:
    case IR_MULF:
    case IR_MULI:
    case IR_IMULI:
    case IR_MOD:
    case IR_IMOD:
    case IR_OR:
    case IR_AND:
    case IR_XOR:
    case IR_LSHIFT:
    case IR_RSHIFT:
    case IR_ILSHIFT:
    case IR_IRSHIFT:
        putf("%s = %s %s %s",
             operand2s(tac->operands[0]),
             operand2s(tac->operands[1]),
             rop2s(tac->op),
             operand2s(tac->operands[2]));
        break;
    case IR_NOT:
    case IR_MINUSI:
    case IR_MINUSF:
    case IR_ADDRESS:
        putf("%s = %s %s",
             operand2s(tac->operands[0]),
             rop2s(tac->op),
             operand2s(tac->operands[1]));
        break;
    case IR_PARAM:
        putf("%s %s", rop2s(tac->op), operand2s(tac->operands[1]));
        break;
    case IR_CALL:
        if (tac->operands[0]) {
            putf("%s = %s %s",
                 operand2s(tac->operands[0]),
                 rop2s(tac->op),
                 operand2s(tac->operands[1]));
        } else {
            putf("%s %s",
                 rop2s(tac->op),
                 operand2s(tac->operands[1]));
        }
        break;
    case IR_CONV_UI_UI:
    case IR_CONV_SI_SI:
    case IR_CONV_UI_SI:
    case IR_CONV_SI_UI:
    case IR_CONV_SI_F:
    case IR_CONV_UI_F:
    case IR_CONV_F_UI:
    case IR_CONV_F_SI:
    case IR_CONV_FF:
    case IR_CONV_P_B:
    case IR_CONV_I_B:
    case IR_CONV_F_B:
        putf("%s = (%s) %s \t(%d => %d)",
             operand2s(tac->operands[0]),
             rop2s(tac->op),
             operand2s(tac->operands[1]),
             tac->from_opsize,
             tac->to_opsize);
        break;
    case IR_SUBSCRIPT:
    case IR_INDIRECTION:
        // operand
        break;
    default:
        die("unexpected rop %s", rop2s(tac->op));
    }
    putf(RESET "\n");
}

static void print_use(struct tac *tac)
{
    for (int i = 0; i < ARRAY_SIZE(tac->operands); i++) {
        struct operand *operand = tac->operands[i];
        if (operand) {
            if (operand->sym && REF_SYM(operand->sym)) {
                struct uses use = tac->uses[i*2];
                putf("%s: ", SYM_X_LABEL(operand->sym));
                if (use.live)
                    putln("live at %p", use.next);
                else
                    putln("die");
            }
            if (operand->index && REF_SYM(operand->index)) {
                struct uses use = tac->uses[i*2+1];
                putf("%s: ", SYM_X_LABEL(operand->index));
                if (use.live)
                    putln("live at %p", use.next);
                else
                    putln("die");
            }
        }
    }
}

static void print_sym_set(struct set *set)
{
    struct vector *objs = set_objects(set);
    for (size_t i = 0; i < vec_len(objs); i++) {
        struct symbol *sym = vec_at(objs, i);
        putf("%s", SYM_X_LABEL(sym));
        if (i < vec_len(objs) - 1)
            putf(", ");
    }
}

static void print_basic_block(struct basic_block *block)
{
    if (block->label)
        putln(RED("%s:"), block->label);
    for (struct tac *tac = block->head; tac; tac = tac->next) {
        print_tac(tac);
        if (opts.ir_dump_level >= 1)
            print_use(tac);
    }

    if (opts.ir_dump_level >= 1) {
        putf("def: ");
        print_sym_set(block->def);
        putf("\n");

        putf("use: ");
        print_sym_set(block->use);
        putf("\n");

        putf("in: ");
        print_sym_set(block->in);
        putf("\n");

        putf("out: ");
        print_sym_set(block->out);
        putf("\n");
    }
}

static void print_ir_data1(const char *name, struct vector *xvalues)
{
    putln("%s:", name);
    for (int i = 0; i < vec_len(xvalues); i++) {
        struct xvalue *value = vec_at(xvalues, i);
        switch (value->size) {
        case Zero:
            putln(".zero %s", value->name);
            break;
        case Byte:
            putln(".byte %s", value->name);
            break;
        case Word:
            putln(".short %s", value->name);
            break;
        case Long:
            putln(".long %s", value->name);
            break;
        case Quad:
            putln(".quad %s", value->name);
            break;
        case ASCIZ:
            putln(".asciz %s", value->name);
            break;
        default:
            assert(0);
        }
    }
}

void print_ir_data(struct symbol *sym)
{
    print_ir_data1(SYM_X_LABEL(sym), SYM_X_XVALUES(sym));
}

void print_ir_bss(struct symbol *sym)
{
    putln("%s,%llu,%d",
          SYM_X_LABEL(sym), TYPE_SIZE(SYM_TYPE(sym)), TYPE_ALIGN(SYM_TYPE(sym)));
}

void print_ir_text(struct symbol *sym)
{
    putln("%s:", SYM_X_LABEL(sym));
    struct basic_block *block = SYM_X_BASIC_BLOCK(sym);
    for (; block; block = block->successors[0])
        print_basic_block(block);
    putln("");
}

void print_ir_compounds(struct map *compounds)
{
    struct vector *keys = map_keys(compounds);
    if (vec_len(keys)) {
        for (int i = 0; i < vec_len(keys); i++) {
            const char *label = vec_at(keys, i);
            struct expr *init = map_get(compounds, label);
            print_ir_data1(label, EXPR_X_XVALUES(init));
        }
    }
}

void print_ir_strings(struct map *strings)
{
    struct vector *keys = map_keys(strings);
    if (vec_len(keys)) {
        for (int i = 0; i < vec_len(keys); i++) {
            const char *name = vec_at(keys, i);
            const char *label = map_get(strings, name);
            putln("%s:", label);
            putln(".string %s", name);
        }
    }
}

void print_ir_floats(struct map *floats)
{
    struct vector *keys = map_keys(floats);
    if (vec_len(keys)) {
        for (int i = 0; i < vec_len(keys); i++) {
            const char *name = vec_at(keys, i);
            const char *label = map_get(floats, name);
            struct symbol *sym = lookup(name, constants);
            assert(sym);
            struct type *ty = SYM_TYPE(sym);
            putln("%s:", label);
            switch (TYPE_KIND(ty)) {
            case FLOAT:
                {
                    float f = SYM_VALUE(sym).d;
                    putln(".long %u", *(uint32_t *)&f);
                }
                break;
            case DOUBLE:
            case LONG + DOUBLE:
                {
                    double d = SYM_VALUE(sym).d;
                    putln(".quad %llu", *(uint64_t *)&d);
                }
                break;
            default:
                assert(0);
            }
        }
    }
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
        strbuf_cats(s, SYM_NAME(EXPR_SYM(node)));
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
        strbuf_cats(s, SYM_NAME(EXPR_SYM(node)));
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

// TODO: typedef names

// debug
void dump_operand(struct operand *operand)
{
    SET_OUTFD(stderr);
    println("%p: op:%s,%ld(%s,%s,%d), kind: %d, reg: %s",
            operand, rop2s(operand->op),operand->disp,
            SYM_X_LABEL(operand->sym),
            operand->index ? SYM_X_LABEL(operand->index) : "",
            operand->scale,
            SYM_X_KIND(operand->sym),
            SYM_X_REG(operand->sym) ? SYM_X_REG(operand->sym)->r[Q] : "");
    RESTORE_OUTFD();
}

void dump_reg(struct reg *reg)
{
    SET_OUTFD(stderr);
    println("dump %s:", reg->r[Q]);
    struct vector *vars = set_objects(reg->vars);
    for (int i = 0; i < vec_len(vars); i++) {
        struct rvar *v = vec_at(vars, i);
        println("[%d] %s, %d", i, SYM_X_LABEL(v->sym), v->size);
    }
    RESTORE_OUTFD();
}

void dump_tacs(struct tac *head)
{
    SET_OUTFD(stderr);
    for (struct tac *tac = head; tac; tac = tac->next)
        print_tac(tac);
    RESTORE_OUTFD();
}

void print_source(struct source src)
{
    println("location: %s:%u:%u", src.file, src.line, src.column);
}
