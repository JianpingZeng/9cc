#include "cc.h"

#define STR(str)  ((str) ? (str) : "<null>")

struct print_context {
    int level;
    node_t * node;
};

static void print_tree1(struct print_context context);

static void print_ty(node_t *ty)
{
    if (ty) {
	if (isfunc(ty) || isptr(ty) || isarray(ty))
	    fprintf(stderr, RED_BOLD("'%s' "), TYPE_NAME(unqual(ty)));
	fprintf(stderr, GREEN("'%s' "), type2s(ty));
	if (isarray(ty) || isstruct(ty) || isunion(ty))
	    fprintf(stderr, YELLOW("<size=%ld> "), TYPE_SIZE(ty));
    }
}

static void print_type(node_t *node, struct print_context context)
{
    fprintf(stderr, PURPLE("%s ") YELLOW("%p "), nname(node), node);
    print_ty(node);
    fprintf(stderr, "\n");
}

static void print_field(node_t *node, struct print_context context)
{
    const char *name = FIELD_NAME(node);
    node_t *ty = FIELD_TYPE(node);
    
    fprintf(stderr, GREEN("%s "), nname(node));
    if (isbitfield(node)) {
	fprintf(stderr, PURPLE("bit "));
	if (name)
	    fprintf(stderr, YELLOW("<offset=%d, size=%d> "), FIELD_OFFSET(node), FIELD_BITSIZE(node));
	else
	    fprintf(stderr, RED("<offset=%d, size=%d> "), FIELD_OFFSET(node), FIELD_BITSIZE(node));
    }
    print_ty(ty);
    fprintf(stderr, CYAN("%s"), STR(name));
    fprintf(stderr, "\n");
}

static void print_decl(node_t *node, struct print_context context)
{
    int level;

    fprintf(stderr, GREEN("%s ") YELLOW("%p "), nname(node), node);
    if (DECL_SYM(node)) {
	if (SYM_DEFINED(DECL_SYM(node)))
	    fprintf(stderr, YELLOW("<defined> "));
	
	node_t *ty = SYM_TYPE(DECL_SYM(node));
        print_ty(ty);
	fprintf(stderr, CYAN("%s "), STR(SYM_NAME(DECL_SYM(node))));
    }
    fprintf(stderr, "\n");
    
    level = context.level + 1;
    
    node_t **exts = DECL_EXTS(node);
    if (exts) {
        for (int i=0; exts[i]; i++) {
            struct print_context con = {level, exts[i]};
            print_tree1(con);
        }
    }
    
    node_t *init = DECL_BODY(node);
    if (init) {
        struct print_context con = {level, init};
        print_tree1(con);
    }

    if (AST_ID(node) == STRUCT_DECL || AST_ID(node) == UNION_DECL) {
	node_t *sym = DECL_SYM(node);
	if (sym && SYM_DEFINED(sym)) {
	    node_t *ty = SYM_TYPE(sym);
	    for (int i = 0; TYPE_FIELDS(ty)[i]; i++) {
		node_t *field = TYPE_FIELDS(ty)[i];
		struct print_context con = { level, field };
	        print_tree1(con);
	    }

	}
    }
}

static void print_expr(node_t *node, struct print_context context)
{
    int level;
    int oper = EXPR_OP(node);
    bool prefix = EXPR_PREFIX(node);

    fprintf(stderr, PURPLE("%s ") YELLOW("%p "), nname(node), node);
    print_ty(AST_TYPE(node));
    if (islvalue(node))
	fprintf(stderr, "'" CYAN("lvalue") "' ");
    
    if (EXPR_SYM(node))
	fprintf(stderr, CYAN("%s "), STR(SYM_NAME(EXPR_SYM(node))));
    if (oper == INCR || oper == DECR)
	fprintf(stderr, "%s ", (prefix ? "prefix" : "postfix"));
    if (oper > 0)
	fprintf(stderr, "'%s' ", tname(oper));
    if (AST_NAME(node))
	fprintf(stderr, "<" RED("%s")  "> ", AST_NAME(node));
    if (isiliteral(node)) {
	if (op(AST_TYPE(node)) == INT)
	    fprintf(stderr, RED("%lld"), SYM_VALUE(EXPR_SYM(node)).u);
	else
	    fprintf(stderr, RED("%llu"), SYM_VALUE(EXPR_SYM(node)).u);
    } else if (isfliteral(node)) {
	fprintf(stderr, RED("%Lf"), SYM_VALUE(EXPR_SYM(node)).d);
    }
    
    fprintf(stderr, "\n");

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
    }
}

static void print_stmt(node_t *node, struct print_context context)
{
    int level;
    node_t *up = STMT_UP(node);

    fprintf(stderr, PURPLE("%s ") YELLOW("%p "), nname(node), node);
    if (up)
        fprintf(stderr, "-> %s %p\n", nname(up), up);
    fprintf(stderr, "\n");
    
    level = context.level + 1;
    
    if (AST_ID(node) == COMPOUND_STMT) {
        node_t **blks = STMT_BLKS(node);
        if (blks) {
            for (int i=0; blks[i]; i++) {
                struct print_context con = {level, blks[i]};
                print_tree1(con);
            }
        }
    } else if (AST_ID(node) == FOR_STMT) {
        node_t **decl = STMT_DECL(node);
        node_t *init = STMT_INIT(node);
        node_t *cond = STMT_COND(node);
        node_t *ctrl = STMT_CTRL(node);
        if (decl) {
            for (int i=0; decl[i]; i++) {
                struct print_context con = {level, decl[i]};
                print_tree1(con);
            }
        } else if (init) {
            struct print_context con = {level, init};
            print_tree1(con);
        } else {
            for (int i=0; i < level; i++)
                fprintf(stderr, "  ");
            fprintf(stderr, "init: <NULL>\n");
        }
        
        if (cond) {
            struct print_context con = {level, cond};
            print_tree1(con);
        } else {
            for (int i=0; i < level; i++)
                fprintf(stderr, "  ");
            fprintf(stderr, "cond: <NULL>\n");
        }
        
        if (ctrl) {
            struct print_context con = {level, ctrl};
            print_tree1(con);
        } else {
            for (int i=0; i < level; i++)
                fprintf(stderr, "  ");
            fprintf(stderr, "ctrl: <NULL>\n");
        }
    }
}

static void print_tree1(struct print_context context)
{
    node_t *node = context.node;
    int level = context.level + 1;
    
    for (int i=0; i < context.level; i++)
        fprintf(stderr, "  ");
    
    if (isdecl(node))
        print_decl(node, context);
    else if (isexpr(node))
        print_expr(node, context);
    else if (isstmt(node))
        print_stmt(node, context);
    else if (istype(node))
	print_type(node, context);
    else if (isfield(node))
	print_field(node, context);
    else
        CCAssert(0);

    if (!(isexpr(node) && AST_ID(node) == CALL_EXPR)) {
	if (AST_KID(context.node, 0)) {
	    struct print_context lcontext;
	    lcontext.level = level;
	    lcontext.node = AST_KID(context.node, 0);
	    print_tree1(lcontext);
	}
    }
    
    if (AST_KID(context.node, 1)) {
	struct print_context rcontext;
	rcontext.level = level;
	rcontext.node = AST_KID(context.node, 1);
	print_tree1(rcontext);
    }

    if (isexpr(node) && EXPR_OPERAND(node, 2)) {
	struct print_context rcontext;
        rcontext.level = level;
        rcontext.node = EXPR_OPERAND(context.node, 2);
        print_tree1(rcontext);
    }
}

void print_tree(node_t *tree)
{
    struct print_context context = {0, tree};
    print_tree1(context);
}

// TODO: typedef names
