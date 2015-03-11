#include "cc.h"

#define STR(str)  ((str) ? (str) : "null")

struct print_context {
    int level;
    struct node * node;
};

struct type_context {
    int level;
    struct type * type;
};

static void print_tree1(struct print_context context);
static void print_type1(struct type_context context);

static void print_spec(struct type *type)
{
    if (type->sclass)
	fprintf(stderr, "%s ", tname(type->sclass));
    
    if (isconst(type)) {
        fprintf(stderr, "const ");
    }
    if (isvolatile(type)) {
        fprintf(stderr, "volatile ");
    }
    if (isrestrict(type)) {
        fprintf(stderr, "restrict ");
    }
    if (isinline(type)) {
        fprintf(stderr, "inline ");
    }
}

static void print_params(struct type_context context)
{
    struct node **proto = context.type->f.proto;
    if (proto) {
	for (int i=0; proto[i]; i++) {
	    struct print_context pcontext = {context.level+1, proto[i]};
	    print_tree1(pcontext);
	}
    }
}

static void print_return(struct type_context context)
{
    struct type_context rcontext = {context.level+1, context.type};
    for (int i=0; i < rcontext.level; i++)
	fprintf(stderr, "  ");
    fprintf(stderr, "Return ");
    print_type1(rcontext);
}

static void print_type1(struct type_context context)
{
    struct type *type = context.type;
    if (type) {
	struct type_context tcontext = {context.level, type->type};
	print_spec(type);
        if (isfunction(type)) {
            fprintf(stderr, "%s", tname(type->op));
	    fprintf(stderr, "\n");
	    print_return(tcontext);
            print_params(context);
        } else if (ispointer(type)) {
            fprintf(stderr, "%s to ", tname(type->op));
	    print_type1(tcontext);
        } else if (isarray(type)) {
            fprintf(stderr, "%s %d of ", tname(type->op), type->size);
	    print_type1(tcontext);
        } else if (istypedef(type)) {
	    fprintf(stderr, "%s aka ", type->name);
	    while (type->type && istypedef(type->type))
		type = type->type;
	    tcontext.type = type->type;
	    print_type1(tcontext);
	} else if (type->op == ENUM || type->op == STRUCT || type->op == UNION) {
	    fprintf(stderr, "%s %s ", tname(type->op), type->name);
	    print_type1(tcontext);
	} else {
            fprintf(stderr, "%s ", type->name);
	    print_type1(tcontext);
        }
    } else {
	fprintf(stderr, "\n");
    }
}

void print_type(struct type *type)
{
    struct type_context context = {0, type};
    print_type1(context);
}

static void print_tree1(struct print_context context)
{
    struct node *node = context.node;
    int level;

    for (int i=0; i < context.level; i++)
	fprintf(stderr, "  ");

    if (isdecl(node)) {
	if (node->symbol) {
	    fprintf(stderr, "%s '%s' ", nname(node), STR(node->symbol->name));
	    if (node->symbol->type) {
		struct type_context tcontext = {context.level, node->symbol->type};
		print_type1(tcontext);
	    } else {
		fprintf(stderr, "\n");
	    }
	} else {
	    fprintf(stderr, "%s\n", nname(node));
	}
    } else if (isexpr(node)) {
	struct expr *e = (struct expr *)node;
	fprintf(stderr, "%s '%s'\n", nname(node), tname(e->op));
    } else if (isstmt(node)){
	struct stmt *s = (struct stmt *)node;
	if (s->up)
	    fprintf(stderr, "%s %p -> %s %p\n",
		    nname(node), node, nname(NODE(s->up)), s->up);
	else
	    fprintf(stderr, "%s %p\n", nname(node), node);
    } else if (node->symbol) {
	fprintf(stderr, "%s '%s' ", nname(node), STR(node->symbol->name));
    } else {
	fprintf(stderr, "%s\n", nname(node));
    }

    level = context.level + 1;

    if (isdecl(node)) {
	struct decl *decl = (struct decl *) node;
	if (decl->exts) {
	    for (int i=0; decl->exts[i]; i++) {
		struct print_context con = {level, decl->exts[i]};
		print_tree1(con);
	    }
	}
    } else if (isstmt(node) && node->id == COMPOUND_STMT) {
	struct stmt *stmt = (struct stmt *) node;
	if (stmt->compoundstmt.blks) {
	    for (int i=0; stmt->compoundstmt.blks[i]; i++) {
		struct print_context con = {level, stmt->compoundstmt.blks[i]};
		print_tree1(con);
	    }
	}
    } else if (isstmt(node) && node->id == FOR_STMT) {
	struct stmt *stmt = (struct stmt *) node;
	if (stmt->forstmt.decl) {
	    for (int i=0; stmt->forstmt.decl[i]; i++) {
		struct print_context con = {level, (struct node*)stmt->forstmt.decl[i]};
		print_tree1(con);
	    }
	} else if (stmt->forstmt.init) {
	    struct print_context con = {level, (struct node*)stmt->forstmt.init};
	    print_tree1(con);
	} else {
	    for (int i=0; i < level; i++)
		fprintf(stderr, "  ");
	    fprintf(stderr, "init: <NULL>\n");
	}

	if (stmt->forstmt.cond) {
	    struct print_context con = {level, (struct node*)stmt->forstmt.cond};
	    print_tree1(con);
	} else {
	    for (int i=0; i < level; i++)
		fprintf(stderr, "  ");
	    fprintf(stderr, "cond: <NULL>\n");
	}

	if (stmt->forstmt.ctrl) {
	    struct print_context con = {level, (struct node*)stmt->forstmt.ctrl};
	    print_tree1(con);
	} else {
	    for (int i=0; i < level; i++)
		fprintf(stderr, "  ");
	    fprintf(stderr, "ctrl: <NULL>\n");
	}
    } else if (isexpr(node) && node->id == ARGS_EXPR) {
	struct expr *expr = (struct expr *) node;
	if (expr->args) {
	    for (int i=0; expr->args[i]; i++) {
		struct print_context con = {level, (struct node*)expr->args[i]};
		print_tree1(con);
	    }
	}
    }

    if (context.node->kids[0]) {
	struct print_context lcontext;
	lcontext.level = level;
	lcontext.node = context.node->kids[0];
	print_tree1(lcontext);
    }
	
    if (context.node->kids[1]) {
	struct print_context rcontext;
	rcontext.level = level;
	rcontext.node = context.node->kids[1];
	print_tree1(rcontext);
    }
}

void print_tree(struct node *tree)
{
    struct print_context context = {0, tree};
    print_tree1(context);
}
