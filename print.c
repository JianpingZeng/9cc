#include "cc.h"

#define STR(str)  ((str) ? (str) : "<null>")

struct print_context {
    int level;
    union node * node;
};

static void print_tree1(struct print_context context);

static void print_ty(struct type *ty)
{
    if (ty) {
	if (isfunc(ty) || isptr(ty) || isarray(ty))
	    fprintf(stderr, RED("'%s' ") GREEN("'%s' "), unqual(ty)->name, type2s(ty));
	else
	    fprintf(stderr, GREEN("'%s' "), type2s(ty));
    }
}

static void print_decl(union node *node, struct print_context context)
{
    int level;

    fprintf(stderr, GREEN("%s ") YELLOW("%p "), nname(node), node);
    if (DECL_SYM(node)) {
	if (DECL_SYM(node)->defined)
	    fprintf(stderr, YELLOW("<defined> "));
	
	struct type *ty = DECL_SYM(node)->type;
        print_ty(ty);
	fprintf(stderr, CYAN("%s "), STR(DECL_SYM(node)->name));
    }
    fprintf(stderr, "\n");
    
    level = context.level + 1;
    
    union node **exts = DECL_EXTS(node);
    if (exts) {
        for (int i=0; exts[i]; i++) {
            struct print_context con = {level, exts[i]};
            print_tree1(con);
        }
    }
    
    union node *init = DECL_BODY(node);
    if (init) {
        struct print_context con = {level, init};
        print_tree1(con);
    }
}

static void print_expr(union node *node, struct print_context context)
{
    int level;
    int op = EXPR_OP(node);
    bool prefix = EXPR_PREFIX(node);

    fprintf(stderr, PURPLE("%s ") YELLOW("%p "), nname(node), node);
    print_ty(AST_TYPE(node));
    if (islvalue(node))
	fprintf(stderr, "'" CYAN("lvalue") "' ");
    
    if (EXPR_SYM(node))
	fprintf(stderr, CYAN("%s "), STR(EXPR_SYM(node)->name));
    if (op == INCR || op == DECR)
	fprintf(stderr, "%s ", (prefix ? "prefix" : "postfix"));
    if (op > 0)
	fprintf(stderr, "'%s' ", tname(op));
    if (AST_NAME(node))
	fprintf(stderr, "<" RED("%s")  "> ", AST_NAME(node));
    
    fprintf(stderr, "\n");

    level = context.level + 1;

    if (AST_ID(node) == CALL_EXPR) {
	union node *func = EXPR_OPERAND(node, 0);
	if (func) {
	    struct print_context con = {level, func};
	    print_tree1(con);
	}
        union node **args = EXPR_ARGS(node);
        if (args) {
            for (int i=0; args[i]; i++) {
                struct print_context con = {level, args[i]};
                print_tree1(con);
            }
        }
    } else if (AST_ID(node) == INITS_EXPR) {
        union node **inits = EXPR_INITS(node);
        for (int i=0; inits && inits[i]; i++) {
            struct print_context con = {level, inits[i]};
            print_tree1(con);
        }
    }
}

static void print_stmt(union node *node, struct print_context context)
{
    int level;
    union node *up = STMT_UP(node);

    fprintf(stderr, PURPLE("%s ") YELLOW("%p "), nname(node), node);
    if (up)
        fprintf(stderr, "-> %s %p\n", nname(up), up);
    fprintf(stderr, "\n");
    
    level = context.level + 1;
    
    if (AST_ID(node) == COMPOUND_STMT) {
        union node **blks = STMT_BLKS(node);
        if (blks) {
            for (int i=0; blks[i]; i++) {
                struct print_context con = {level, blks[i]};
                print_tree1(con);
            }
        }
    } else if (AST_ID(node) == FOR_STMT) {
        union node **decl = STMT_DECL(node);
        union node *init = STMT_INIT(node);
        union node *cond = STMT_COND(node);
        union node *ctrl = STMT_CTRL(node);
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
    union node *node = context.node;
    int level = context.level + 1;
    
    for (int i=0; i < context.level; i++)
        fprintf(stderr, "  ");
    
    if (isdecl(node))
        print_decl(node, context);
    else if (isexpr(node))
        print_expr(node, context);
    else if (isstmt(node))
        print_stmt(node, context);
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

void print_tree(union node *tree)
{
    struct print_context context = {0, tree};
    print_tree1(context);
}
