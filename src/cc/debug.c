#include "cc.h"

static void print_spec(struct type *type)
{
    if (type->sclass)
	printf("%s ", tname(type->sclass));
    
    if (isconst(type)) {
        printf("const ");
    }
    if (isvolatile(type)) {
        printf("volatile ");
    }
    if (isrestrict(type)) {
        printf("restrict ");
    }
    if (isinline(type)) {
        printf("inline ");
    }
}

static void print_type1(struct type *type);

static void print_fparams(struct type *ftype)
{
	printf(" with params:\n");
        if (ftype->u.f.proto) {
	    print_tree(NODE(ftype->u.f.proto));
	}
}

static void print_type1(struct type *type)
{
    if (type) {
        if (isfunction(type)) {
            print_spec(type);
            printf("%s returning ", tname(type->op));
	    print_type1(type->type);
            print_fparams(type);
        }
        else if (ispointer(type)) {
            print_spec(type);
            printf("%s to ", tname(type->op));
	    print_type1(type->type);
        }
        else if (isarray(type)) {
            print_spec(type);
            printf("%s %d of ", tname(type->op), type->size);
	    print_type1(type->type);
        }
        else {
            print_spec(type);
            printf("%s ", type->name);
	    print_type1(type->type);
        }
    }
}

void print_type(struct type *type)
{
    print_type1(type);
    printf("\n");
}

struct print_context {
    int level;
    struct node * node;
};

static void print_tree1(struct print_context context)
{
    struct node *node = context.node;
    int level;

    if (context.node->id != CONCAT_NODE) {
	for (int i=0; i < context.level; i++)
	    fprint(stderr, "  ");

	if (node->symbol) {	
	    fprint(stderr, "%s '%s' ", nname(node), node->symbol->name);
	    if (node->symbol->type)
		print_type(node->symbol->type);
	    else
		fprint(stderr, "\n");
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
