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

    if (node->id == 0) {
	struct expr * e = node;
	return tname(e->op);
    }
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
    fprint(stderr, "%s\n", nname(context.node));
    
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

void print_tree(struct node * root)
{
    struct print_context context = {0, root};
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
    expr->node.kids[0] = l;
    expr->node.kids[1] = r;
    return expr;
}

