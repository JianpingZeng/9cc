#include "cc.h"

static const char * node_names[] = {
#define _ns(a)   "",
#define _n(a, b) b,
#include "node.h"
};

const char *nname(struct node * node)
{
    if (node == NULL)
	return "<NULL>";

    assert(node->id > BEGIN_NODE_ID && node->id < END_NODE_ID);
    
    return node_names[node->id];
}

struct print_context {
    int level;
    struct node * node;
};

static void print_tree1(struct print_context context)
{
    for (int i=0; i < context.level; i++) {
	fprint(stderr, "  ");
    }

    if (context.node->symbol) {	
	fprint(stderr, "%s '%s'\n", nname(context.node), context.node->symbol->name);
    } else if (isexpr(context.node)) {
	struct expr *e = (struct expr *)context.node;
	fprint(stderr, "%s '%s'\n", nname(context.node), tname(e->op));
    } else {
	fprint(stderr, "%s\n", nname(context.node));
    }
    
    if (context.node->kids[0]) {
	struct print_context lcontext;
	lcontext.level = context.level+1;
	lcontext.node = context.node->kids[0];
	print_tree1(lcontext);
    }
    if (context.node->kids[1]) {
	struct print_context rcontext;
	rcontext.level = context.level+1;
	rcontext.node = context.node->kids[1];
	print_tree1(rcontext);
    }
}

void print_tree(struct node *tree)
{
    struct print_context context = {0, tree};
    print_tree1(context);
}

const char * node_print_function(void *data)
{
    struct node *p = data;
    return nname(p);
}

struct expr * expr_node(int id, int op, struct expr *l, struct expr *r)
{
    struct expr * expr = alloc_expr_node();
    expr->node.id = id;
    expr->op = op;
    expr->node.kids[0] = NODE(l);
    expr->node.kids[1] = NODE(r);
    return expr;
}

