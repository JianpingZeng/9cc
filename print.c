#include "cc.h"

#define STR(str)  ((str) ? (str) : "<null>")
static FILE *outfd;
#define SET_OUTFD(fd) \
    FILE *_saved_fd = outfd; \
    outfd = fd

#define RESTORE_OUTFD() \
    outfd = _saved_fd

struct print_context {
    int level;
    node_t *node;
};

static void print_tree1(struct print_context context);

static void ensure_outfd(void)
{
    if (!outfd)
        outfd = stdout;
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

static void print_ty(node_t * ty)
{
    if (ty) {
        if (isfunc(ty) || isptr(ty) || isarray(ty))
            putf(RED_BOLD("'%s' "), TYPE_NAME(ty));
        putf(GREEN("'%s' "), type2s(ty));
        if (isarray(ty) || isstruct(ty) || isunion(ty)) {
            putf(YELLOW("<size=%ld> "), TYPE_SIZE(ty));
        } else if (isfunc(ty)) {
            putf("%s ",
                 TYPE_OLDSTYLE(ty) ? "oldstyle" : "prototype");
            if (TYPE_INLINE(ty))
                putf("inline ");
        }
    }
}

static void print_type(node_t * node, struct print_context context)
{
    putf(PURPLE("%s ") YELLOW("%p "), nname(node), node);
    print_ty(node);
    putf("\n");
}

static void print_field(node_t * node, struct print_context context)
{
    const char *name = FIELD_NAME(node);
    node_t *ty = FIELD_TYPE(node);

    putf(GREEN("%s "), nname(node));
    if (FIELD_ISBIT(node))
        putf(RED("<offset=%d, bitoff=%d, bits=%d> "),
             FIELD_OFFSET(node), FIELD_BITOFF(node), FIELD_BITSIZE(node));
    else
        putf(GREEN("<offset=%d> "), FIELD_OFFSET(node));

    print_ty(ty);
    putf(CYAN("%s"), STR(name));
    putf("\n");
}

static void print_label(int level, const char *name)
{
    for (int i = 0; i < level; i++)
        putf("  ");
    putf(RED("%s\n"), name);
}

static void print_decl(node_t * node, struct print_context context)
{
    int level;

    putf(GREEN("%s ") YELLOW("%p "), nname(node), node);

    node_t *sym = DECL_SYM(node);
    if (sym) {
        if (SYM_DEFINED(sym))
            putf(YELLOW("<defined> "));

        node_t *ty = SYM_TYPE(sym);
        print_ty(ty);
        putf(CYAN("%s "), STR(SYM_NAME(sym)));
        putf("<scope: %d>", SYM_SCOPE(sym));
        putf(YELLOW("<line:%u col:%u> "), AST_SRC(sym).line,
             AST_SRC(sym).column);
    }
    if (isfuncdef(node))
        putf("%llu localvars ", vec_len(DECL_X_LVARS(node)));
    putf("\n");

    level = context.level + 1;

    switch (AST_ID(node)) {
    case TU_DECL:
        {
            struct vector *exts = DECL_EXTS(node);
            if (exts) {
                for (int i = 0; i < vec_len(exts); i++) {
                    node_t *ext = vec_at(exts, i);
                    struct print_context con = {level, ext};
                    print_tree1(con);
                }
            }
        }
        break;
    case STRUCT_DECL:
    case UNION_DECL:
        {
            node_t *ty = SYM_TYPE(sym);
            struct vector *fields = TYPE_FIELDS(ty);
            for (int i = 0; i < vec_len(fields); i++) {
                node_t *field = vec_at(fields, i);
                struct print_context con = {level, field};
                print_tree1(con);
            }
        }
        break;
    case ENUM_DECL:
        break;
    default:
        break;
    }

    node_t *init = DECL_BODY(node);
    if (init) {
        struct print_context con = { level, init };
        print_tree1(con);
    }
}

static void print_expr(node_t * node, struct print_context context)
{
    int level;
    int op = EXPR_OP(node);
    bool prefix = EXPR_PREFIX(node);

    putf(PURPLE("%s ") YELLOW("%p "), nname(node), node);
    print_ty(AST_TYPE(node));
    if (islvalue(node))
        putf("'" CYAN("lvalue") "' ");

    if (EXPR_SYM(node))
        putf(CYAN("%s "), STR(SYM_NAME(EXPR_SYM(node))));
    if (op == INCR || op == DECR)
        putf("%s ", (prefix ? "prefix" : "postfix"));
    if (op > 0)
        putf("'%s' ", id2s(op));
    if (AST_NAME(node))
        putf("<" RED("%s") "> ", AST_NAME(node));
    if (isiliteral(node)) {
        if (TYPE_OP(AST_TYPE(node)) == INT)
            putf(RED("%lld"), ILITERAL_VALUE(node));
        else
            putf(RED("%llu"), ILITERAL_VALUE(node));
    } else if (isfliteral(node)) {
        putf(RED("%Lf"), FLITERAL_VALUE(node));
    }

    putf("\n");

    level = context.level + 1;

    if (AST_ID(node) == CALL_EXPR) {
        node_t *func = EXPR_OPERAND(node, 0);
        if (func) {
            struct print_context con = { level, func };
            print_tree1(con);
        }
        struct vector *args = EXPR_ARGS(node);
        if (args) {
            for (int i = 0; i < vec_len(args); i++) {
                node_t *arg = vec_at(args, i);
                struct print_context con = { level, arg };
                print_tree1(con);
            }
        }
    } else if (AST_ID(node) == INITS_EXPR) {
        struct vector *inits = EXPR_INITS(node);
        for (int i = 0; i < vec_len(inits); i++) {
            node_t *init = vec_at(inits, i);
            struct print_context con = { level, init };
            print_tree1(con);
        }
    } else {
        if (EXPR_OPERAND(node, 0)) {
            struct print_context lcontext;
            lcontext.level = level;
            lcontext.node = EXPR_OPERAND(node, 0);
            print_tree1(lcontext);
        }

        if (EXPR_OPERAND(node, 1)) {
            struct print_context rcontext;
            rcontext.level = level;
            rcontext.node = EXPR_OPERAND(node, 1);
            print_tree1(rcontext);
        }

        if (EXPR_OPERAND(node, 2)) {
            struct print_context rcontext;
            rcontext.level = level;
            rcontext.node = EXPR_OPERAND(node, 2);
            print_tree1(rcontext);
        }
    }
}

static void print_stmt(node_t * node, struct print_context context)
{
    int level = context.level + 1;

    putf(PURPLE("%s ") YELLOW("%p "), nname(node), node);
    putf("\n");

    switch (AST_ID(node)) {
    case COMPOUND_STMT:
        {
            struct vector *blks = STMT_BLKS(node);
            for (int i = 0; i < vec_len(blks); i++) {
                node_t *blk = vec_at(blks, i);
                struct print_context con = {level, blk};
                print_tree1(con);
            }
        }
        break;
    case FOR_STMT:
        {
            struct vector *decl = STMT_FOR_DECL(node);
            node_t *init = STMT_FOR_INIT(node);
            node_t *cond = STMT_FOR_COND(node);
            node_t *ctrl = STMT_FOR_CTRL(node);
            node_t *body = STMT_FOR_BODY(node);
            if (decl) {
                for (int i=0; i < vec_len(decl); i++) {
                    node_t *dcl = vec_at(decl, i);
                    struct print_context con = {level, dcl};
                    print_tree1(con);
                }
            } else if (init) {
                struct print_context con = {level, init};
                print_tree1(con);
            } else {
                for (int i=0; i < level; i++)
                    putf("  ");
                putf("init: <NULL>\n");
            }
        
            if (cond) {
                struct print_context con = {level, cond};
                print_tree1(con);
            } else {
                for (int i=0; i < level; i++)
                    putf("  ");
                putf("cond: <NULL>\n");
            }
        
            if (ctrl) {
                struct print_context con = {level, ctrl};
                print_tree1(con);
            } else {
                for (int i=0; i < level; i++)
                    putf("  ");
                putf("ctrl: <NULL>\n");
            }

            if (body) {
                struct print_context con = {level, body};
                print_tree1(con);
            } else {
                for (int i=0; i < level; i++)
                    putf("  ");
                putf("ctrl: <NULL>\n");
            }
        }
        break;
    case IF_STMT:
        {
            node_t *cond = STMT_COND(node);
            node_t *then = STMT_THEN(node);
            node_t *els = STMT_ELSE(node);

            if (cond) {
                struct print_context con = {level, cond};
                print_tree1(con);
            }
            if (then) {
                struct print_context con = {level, then};
                print_tree1(con);
            }
            if (els) {
                struct print_context con = {level, els};
                print_tree1(con);
            }
        }
        break;
    case DO_WHILE_STMT:
    case WHILE_STMT:
        {
            node_t *cond = STMT_WHILE_COND(node);
            node_t *body = STMT_WHILE_BODY(node);

            if (cond) {
                struct print_context con = {level, cond};
                print_tree1(con);
            }
            if (body) {
                struct print_context con = {level, body};
                print_tree1(con);
            }
        }
        break;
    case SWITCH_STMT:
        {
            node_t *expr = STMT_SWITCH_EXPR(node);
            node_t *body = STMT_SWITCH_BODY(node);

            if (expr) {
                struct print_context con = {level, expr};
                print_tree1(con);
            }
            if (body) {
                struct print_context con = {level, body};
                print_tree1(con);
            }
        }
        break;
    case CASE_STMT:
    case DEFAULT_STMT:
        {
            node_t *body = STMT_CASE_BODY(node);

            if (body) {
                struct print_context con = {level, body};
                print_tree1(con);
            }
        }
        break;
    case RETURN_STMT:
        {
            node_t *expr = STMT_RETURN_EXPR(node);

            if (expr) {
                struct print_context con = {level, expr};
                print_tree1(con);
            }
        }
    case GOTO_STMT:
        {
            const char *label = STMT_LABEL_NAME(node);

            if (label)
                print_label(level, label);
        }
        break;
    case LABEL_STMT:
        {
            const char *label = STMT_LABEL_NAME(node);
            node_t *body = STMT_LABEL_BODY(node);

            if (label)
                print_label(level, label);
            if (body) {
                struct print_context con = {level, body};
                print_tree1(con);
            }
        }
        break;
    case EXPR_STMT:
        {
            node_t *expr = STMT_EXPR_BODY(node);
            if (expr) {
                struct print_context con = {level, expr};
                print_tree1(con);
            }
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

static void print_tree1(struct print_context context)
{
    node_t *node = context.node;

    for (int i = 0; i < context.level; i++)
        putf("  ");

    if (isdecl(node))
        print_decl(node, context);
    else if (isexpr(node))
        print_expr(node, context);
    else if (istype(node))
        print_type(node, context);
    else if (isfield(node))
        print_field(node, context);
    else if (isstmt(node))
        print_stmt(node, context);
    else
        assert(0);
}

void print_tree(node_t * tree)
{
    struct print_context context = { 0, tree };
    print_tree1(context);
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

static void print_data(struct section *section)
{
    putln("%s:", section->label);
    for (int i = 0; i < vec_len(section->u.xvalues); i++) {
        struct xvalue *value = vec_at(section->u.xvalues, i);
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

static void print_sym_set(struct set *set)
{
    struct vector *objs = set_objects(set);
    for (size_t i = 0; i < vec_len(objs); i++) {
        node_t *sym = vec_at(objs, i);
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

static void print_bss(struct section *section)
{
    putln("%s,%llu,%d",
          section->label, section->size, section->align);
}

static void print_text(struct section *section)
{
    node_t *decl = section->u.decl;
    putln("%s:", SYM_X_LABEL(DECL_SYM(decl)));
    struct basic_block *block = DECL_X_BASIC_BLOCK(decl);
    for (; block; block = block->successors[0])
        print_basic_block(block);
    putln("");
}

static void print_compounds(struct map *compounds)
{
    struct vector *keys = map_keys(compounds);
    if (vec_len(keys)) {
        for (int i = 0; i < vec_len(keys); i++) {
            const char *label = vec_at(keys, i);
            struct section *section = map_get(compounds, label);
            print_data(section);
        }
    }
}

static void print_strings(struct map *strings)
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

static void print_floats(struct map *floats)
{
    struct vector *keys = map_keys(floats);
    if (vec_len(keys)) {
        for (int i = 0; i < vec_len(keys); i++) {
            const char *name = vec_at(keys, i);
            const char *label = map_get(floats, name);
            node_t *sym = lookup(name, constants);
            assert(sym);
            node_t *ty = SYM_TYPE(sym);
            putln("%s:", label);
            switch (TYPE_KIND(ty)) {
            case FLOAT:
                {
                    float f = SYM_VALUE_D(sym);
                    putln(".long %u", *(uint32_t *)&f);
                }
                break;
            case DOUBLE:
            case LONG + DOUBLE:
                {
                    double d = SYM_VALUE_D(sym);
                    putln(".quad %llu", *(uint64_t *)&d);
                }
                break;
            default:
                assert(0);
            }
        }
    }
}

void print_ir(struct externals *exts)
{
    for (int i = 0; i < vec_len(exts->sections); i++) {
        struct section *section = vec_at(exts->sections, i);
        switch (section->id) {
        case SECTION_TEXT:
            print_text(section);
            break;
        case SECTION_DATA:
            print_data(section);
            break;
        case SECTION_BSS:
            print_bss(section);
            break;
        default:
            assert(0);
        }
    }
    print_compounds(exts->compounds);
    print_strings(exts->strings);
    print_floats(exts->floats);
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
    node_t *type;
};
static struct vector *type2s1(node_t * ty);

static struct type2s *paren(int id, node_t * ty)
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
            struct vector *params = TYPE_PARAMS(s->type);
            int len = vec_len(params);
            vec_push(r, paren(FSPACE, NULL));
            vec_push(r, paren(LPAREN, s->type));
            for (int i = 0; i < len; i++) {
                node_t *param = vec_at(params, i);
                node_t *ty = SYM_TYPE(param);
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

static struct vector *type2s1(node_t * ty)
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

const char *type2s(node_t * ty)
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
static const char *expr2s(node_t * node)
{
    assert(isexpr(node));

    struct strbuf *s = strbuf_new();
    int id = AST_ID(node);
    node_t *l = EXPR_OPERAND(node, 0);
    node_t *r = EXPR_OPERAND(node, 1);

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
            strbuf_cats(s, id2s(EXPR_OP(node)));
            if (istype(l))
                strbuf_cats(s, type2s(l));
            else
                strbuf_cats(s, expr2s(l));
            break;
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
        strbuf_cats(s, AST_NAME(node));
        break;
    case PAREN_EXPR:
        strbuf_cats(s, format("(%s)", expr2s(l)));
        break;
    case CALL_EXPR:
        {
            const char *func = expr2s(l);
            struct vector *args = EXPR_ARGS(node);
            strbuf_cats(s, func);
            strbuf_cats(s, "(");
            for (int i = 0; i < vec_len(args); i++) {
                node_t *arg = vec_at(args, i);
                const char *s1 = expr2s(arg);
                strbuf_cats(s, s1);
                if (i != vec_len(args) - 1)
                    strbuf_cats(s, ", ");
            }
            strbuf_cats(s, ")");
        }
        break;
    case CAST_EXPR:
        strbuf_cats(s,
                    format("(%s)%s", type2s(AST_TYPE(node)),
                           expr2s(l)));
        break;
    case CONV_EXPR:
        strbuf_cats(s, expr2s(l));
        break;
    case REF_EXPR:
        strbuf_cats(s, SYM_NAME(EXPR_SYM(node)));
        break;
    case INTEGER_LITERAL:
        if (TYPE_OP(AST_TYPE(node)) == INT)
            strbuf_cats(s, format("%lld", ILITERAL_VALUE(node)));
        else
            strbuf_cats(s, format("%llu", ILITERAL_VALUE(node)));
        break;
    case FLOAT_LITERAL:
        strbuf_cats(s, format("%Lf", FLITERAL_VALUE(node)));
        break;
    case STRING_LITERAL:
        strbuf_cats(s, SYM_NAME(EXPR_SYM(node)));
        break;
    case COMPOUND_LITERAL:
        strbuf_cats(s, format("(%s){...}", type2s(AST_TYPE(node))));
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

const char *node2s(node_t * node)
{
    if (istype(node))
        return type2s(node);
    else if (issymbol(node))
        return SYM_NAME(node);
    else if (isexpr(node))
        return expr2s(node);
    else
        return AST_NAME(node);
}

void print_node_size(void)
{
    println("ast_decl: %llu\n"
            "ast_expr: %llu\n"
            "ast_stmt: %llu\n"
            "ast_type: %llu\n"
            "ast_type.u: %llu\n"
            "ast_type.limits: %llu\n"
            "ast_symbol: %llu\n"
            "ast_field: %llu\n"
            "ast_node: %llu (node_t: %llu)\n"
            "ast_common: %llu\n",
            sizeof(struct ast_decl),
            sizeof(struct ast_expr),
            sizeof(struct ast_stmt),
            sizeof(struct ast_type),
            FIELD_SIZEOF(struct ast_type, u),
            FIELD_SIZEOF(struct ast_type, limits),
            sizeof(struct ast_symbol),
            sizeof(struct ast_field),
            sizeof(union ast_node), sizeof(node_t),
            sizeof(struct ast_common)
            );
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
