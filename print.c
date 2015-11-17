#include "cc.h"

#define STR(str)  ((str) ? (str) : "<null>")

struct print_context {
    int level;
    node_t * node;
};

static void print_tree1(struct print_context context);

static void putf(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stdout, fmt, ap);
    va_end(ap);
}

static void print_ty(node_t *ty)
{
    if (ty) {
	if (isfunc(ty) || isptr(ty) || isarray(ty))
	    putf(RED_BOLD("'%s' "), TYPE_NAME(ty));
	putf(GREEN("'%s' "), type2s(ty));
	if (isarray(ty) || isstruct(ty) || isunion(ty))
	    putf(YELLOW("<size=%ld> "), TYPE_SIZE(ty));
	else if (isfunc(ty))
	    putf("%s ", TYPE_OLDSTYLE(ty) ? "oldstyle" : "prototype");
    }
}

static void print_type(node_t *node, struct print_context context)
{
    putf(PURPLE("%s ") YELLOW("%p "), nname(node), node);
    print_ty(node);
    putf("\n");
}

static void print_field(node_t *node, struct print_context context)
{
    const char *name = FIELD_NAME(node);
    node_t *ty = FIELD_TYPE(node);
    
    putf(GREEN("%s "), nname(node));
    if (isbitfield(node))
	putf(RED("<offset=%d, bits=%d> "), FIELD_OFFSET(node), FIELD_BITSIZE(node));
    else
	putf(GREEN("<offset=%d> "), FIELD_OFFSET(node));
    
    print_ty(ty);
    putf(CYAN("%s"), STR(name));
    putf("\n");
}

static void print_decl(node_t *node, struct print_context context)
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
	putf(YELLOW("<line:%u col:%u> "), AST_SRC(sym).line, AST_SRC(sym).column);
    }
    if (isfuncdef(node))
	putf("%llu localvars ", LIST_LEN(DECL_LVARS(node)));
    putf("\n");
    
    level = context.level + 1;

    switch (AST_ID(node)) {
    case TU_DECL:
	{
	    node_t **exts = DECL_DCLS(node);
	    if (exts) {
		for (int i=0; exts[i]; i++) {
		    struct print_context con = {level, exts[i]};
		    print_tree1(con);
		}
	    }
	}
	break;
    default:
	break;
    }
    
    node_t *init = DECL_BODY(node);
    if (init) {
        struct print_context con = {level, init};
        print_tree1(con);
    }
}

static void print_expr(node_t *node, struct print_context context)
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
	putf("<" RED("%s")  "> ", AST_NAME(node));
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
	    struct print_context con = {level, func};
	    print_tree1(con);
	}
        node_t **args = EXPR_ARGS(node);
        if (args) {
            for (int i=0; args[i]; i++) {
                struct print_context con = {level, args[i]};
                print_tree1(con);
            }
        }
    } else if (AST_ID(node) == INITS_EXPR) {
        node_t **inits = EXPR_INITS(node);
        for (int i=0; inits && inits[i]; i++) {
            struct print_context con = {level, inits[i]};
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

static void print_stmt(node_t *node, struct print_context context)
{
    int level = context.level + 1;
    node_t **list = STMT_LIST(node);

    switch (AST_ID(node)) {
    case AST_COMPOUND:
	{
	    putf(PURPLE("%s ") YELLOW("%p ") "\n", nname(node), node);
	    for (int i = 0; i < LIST_LEN(list); i++) {
		node_t *n = list[i];
		struct print_context con = { level, n };
		print_tree1(con);
	    }
	}
	break;
    case AST_IF:
	{
	    node_t *cond = STMT_COND(node);
	    node_t *then = STMT_THEN(node);
	    node_t *els = STMT_ELSE(node);

	    putf(WHITE_BOLD("IF %p") "\n", node);
	    if (cond) {
		struct print_context con = { level, cond };
		print_tree1(con);
	    }

	    if (then) {
		for (int i=0; i < context.level; i++)
		    putf("  ");
		putf(WHITE_BOLD("THEN %p") "\n", node);
		struct print_context con = { level, then };
		print_tree1(con);
	    }

	    if (els) {
		for (int i=0; i < context.level; i++)
		    putf("  ");
		putf(WHITE_BOLD("ELSE %p") "\n", node);
		struct print_context con = { level, els };
		print_tree1(con);
	    }
	}
	break;
    case AST_JUMP:
	{
	    putf(RED_BOLD("JMP to ") CYAN("%s ") "\n", STMT_LABEL(node));
	}
	break;
    case AST_LABEL:
	{
	    putf(CYAN_BOLD("%s: ") "\n", STMT_LABEL(node));
	}
	break;
    case AST_RETURN:
	{
	    putf(PURPLE("%s ") YELLOW("%p ") "\n", nname(node), node);
	    node_t *expr = STMT_OPERAND(node);
	    if (expr) {
		struct print_context con = { level, expr };
		print_tree1(con);
	    }
	}
	break;
    default:
	putf(PURPLE("%s ") YELLOW("%p ") "\n", nname(node), node);
	break;
    }
}

static void print_tree1(struct print_context context)
{
    node_t *node = context.node;
    
    for (int i=0; i < context.level; i++)
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
        cc_assert(0);
}

void print_tree(node_t *tree)
{
    struct print_context context = {0, tree};
    print_tree1(context);
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
static struct vector *type2s1(node_t *ty);

static struct type2s * paren(int id, node_t *ty)
{
    struct type2s *s = zmalloc(sizeof (struct type2s));
    s->id = id;
    s->type = ty;
    return s;
}

static void dotype2s(struct vector *l, struct vector *r)
{
    struct type2s *s;
    int k;

    if (vec_len(l) == 0)
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
		    vec_push_front(r, paren(LPAREN, s2->type));
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
            node_t **params = TYPE_PARAMS(s->type);
	    int len = LIST_LEN(params);
	    vec_push(r, paren(FSPACE, NULL));
	    vec_push(r, paren(LPAREN, s->type));
	    for (int i=0; params && params[i]; i++) {
		node_t *ty = SYM_TYPE(params[i]);
		struct vector *v = type2s1(ty);
		vec_add(r, v);
		vec_free(v);
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

static struct vector *type2s1(node_t *ty)
{
    struct vector *l, *r, *v;

    v = vec_new();
    while (ty) {
	struct type2s *s = zmalloc(sizeof (struct type2s));
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
    vec_free(v);

    dotype2s(l, r);
    vec_free(l);
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

const char *type2s(node_t *ty)
{
    const char *ret;
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
	} else if (isenum(s->type) || isstruct(s->type) || isunion(s->type)) {
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

    ret = strs(strbuf_strip(buf)->str);
    strbuf_free(buf);
    vec_purge(v);
    return ret;
}

/**
 * Convert expression node to string.
 */
static const char * expr2s(node_t *node)
{
    cc_assert(isexpr(node));
    
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
	    cc_assert(0);
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
	    node_t ** args = EXPR_ARGS(node);
	    strbuf_cats(s, func);
	    strbuf_cats(s, "(");
	    for (int i = 0; i < LIST_LEN(args); i++) {
		const char *s1 = expr2s(args[i]);
		strbuf_cats(s, s1);
		if (i != LIST_LEN(args) - 1)
		    strbuf_cats(s, ", ");
	    }
	    strbuf_cats(s, ")");
	}
	break;
    case CAST_EXPR:
	strbuf_cats(s, format("(%s)%s", type2s(AST_TYPE(node)), expr2s(l)));
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
	cc_assert(0);
    }
    return STR(strbuf_str(s));
}

const char * node2s(node_t *node)
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
	    "ast_common: %llu\n"
	    ,
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
