#include "cc.h"

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
	fprint(stderr, "%s ", tname(type->sclass));
    
    if (isconst(type)) {
        fprint(stderr, "const ");
    }
    if (isvolatile(type)) {
        fprint(stderr, "volatile ");
    }
    if (isrestrict(type)) {
        fprint(stderr, "restrict ");
    }
    if (isinline(type)) {
        fprint(stderr, "inline ");
    }
}

static void print_params(struct type_context context)
{
    struct node *node = NODE(context.type->u.f.proto);
    if (node) {
	struct print_context pcontext = {context.level+1, node};
	print_tree1(pcontext);
    }
}

static void print_return(struct type_context context)
{
    struct type_context rcontext = {context.level+1, context.type};
    for (int i=0; i < rcontext.level; i++)
	fprint(stderr, "  ");
    fprint(stderr, "Return ");
    print_type1(rcontext);
}

static void print_type1(struct type_context context)
{
    struct type *type = context.type;
    if (type) {
	struct type_context tcontext = {context.level, type->type};
        if (isfunction(type)) {
            print_spec(type);
            fprint(stderr, "%s", tname(type->op));
	    fprint(stderr, "\n");
	    print_return(tcontext);
            print_params(context);
        }
        else if (ispointer(type)) {
            print_spec(type);
            fprint(stderr, "%s to ", tname(type->op));
	    print_type1(tcontext);
        }
        else if (isarray(type)) {
            print_spec(type);
            fprint(stderr, "%s %d of ", tname(type->op), type->size);
	    print_type1(tcontext);
        }
        else {
            print_spec(type);
            fprint(stderr, "%s ", type->name);
	    print_type1(tcontext);
        }
    } else {
	fprint(stderr, "\n");
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

    if (context.node->id != CONCAT_NODE) {
	for (int i=0; i < context.level; i++)
	    fprint(stderr, "  ");

	if (node->symbol) {	
	    fprint(stderr, "%s '%s' ", nname(node), node->symbol->name);
	    if (node->symbol->type) {
		struct type_context tcontext = {context.level, node->symbol->type};
		print_type1(tcontext);
	    } else {
		fprint(stderr, "\n");
	    }
	} else if (isexpr(node)) {
	    struct expr *e = (struct expr *)node;
	    fprint(stderr, "%s '%s'\n", nname(node), tname(e->op));
	} else if (isstmt(node)){
	    struct stmt *s = (struct stmt *)node;
	    if (s->up)
		fprint(stderr, "%s %p -> %s %p\n",
		       nname(node), node, nname(NODE(s->up)), s->up);
	    else
		fprint(stderr, "%s %p\n", nname(node), node);
	} else {
	    fprint(stderr, "%s\n", nname(node));
	}
    }

    if (context.node->id == CONCAT_NODE)
	level = context.level;
    else
	level = context.level + 1;
    
    if (context.node->kids[0]) {
	struct print_context lcontext;
	lcontext.level = level;
	lcontext.node = context.node->kids[0];
	print_tree1(lcontext);
    }
    else if (context.node->id == CONCAT_NODE) {
	for (int i=0; i < context.level; i++)
	    fprint(stderr, "  ");
	
	fprint(stderr, "<<<NULL>>>\n");
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
