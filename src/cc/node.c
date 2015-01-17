#include "c.h"

static const char * node_names[] = {
#define _ns(a)      "",
#define _n(a, b)    b,
#include "nodename.h"
};

const char *nname(Node node)
{
    if (node == NULL) return "<<<NULL>>>";
    assert(node->id > NODE_ID_BEGIN && node->id < NODE_ID_END);
    
    return node_names[node->id];
}

// print context
struct print_context {
    int level;
    int last;
    Node node;
    struct print_context *prev;
};

static void printnode1(struct print_context context)
{
    // print leading chars
    Vector v = new_vector(sizeof(char *));
    for (struct print_context *prev=context.prev; prev; prev=prev->prev) {
        if (prev->level < 1) {
            break;
        }
        if (prev->last) {
            vector_insert(v, 0, "  ");
        }
        else {
            vector_insert(v, 0, "| ");
        }
    }
    for (int i=0; i < vector_length(v); i++) {
        char *str = (char *) vector_at(v, i);
        printf("%s", str);
    }
    free_vector(v);
    if (context.level > 0) {
        if (context.last) {
            putchar('`');
        }
        else {
            putchar('|');
        }
        putchar('-');
    }
    if (context.node == NULL) {
        printf("%s\n", nname(context.node));
        return;
    }
    printf("%s %p <%s %p '%s'>\n", nname(context.node), context.node,
           context.node->s ? context.node->s->lex.name : "",
           context.node->s ? context.node->s : 0,
           (context.node->s && context.node->s->type) ? tname(context.node->s->type->op) : "");
    switch (context.node->id) {
        case TRANSLATION_UNIT_DECL:
        {
            TranslationUnitDecl tudecl = (TranslationUnitDecl) context.node;
            for (int i=0; tudecl->exts[i]; i++) {
                Node n = tudecl->exts[i];
                struct print_context next_context = { context.level+1, tudecl->exts[i+1] == NULL ? 1 : 0, n, &context };
                printnode1(next_context);
            }
        }
            break;
            
        case FUNC_DECL:
        {
            FuncDecl funcdecl = (FuncDecl) context.node;
            if (funcdecl->params) {
                for (int i=0; funcdecl->params[i]; i++) {
                    Node n = funcdecl->params[i];
                    struct print_context next_context = { context.level+1, (funcdecl->params[i+1] == NULL && funcdecl->cs == NULL) ? 1 : 0, n, &context };
                    printnode1(next_context);
                }
            }
            if (funcdecl->cs) {
                struct print_context next_context = { context.level+1, 1, NODE(funcdecl->cs), &context };
                printnode1(next_context);
            }
        }
            break;
            
        case COMPOUND_STMT:
        {
            CompoundStmt csstmt = (CompoundStmt) context.node;
            for (int i=0; csstmt->stmts[i]; i++) {
                Node n = csstmt->stmts[i];
                struct print_context next_context = { context.level+1, csstmt->stmts[i+1] == NULL ? 1 : 0, n, &context };
                printnode1(next_context);
            }
        }
            break;
            
            
        default:
            break;
    }
}

void printnode(Node node)
{
    struct print_context context = {0, 0, node, NULL};
    printnode1(context);
}

Decl decl_node(int id, int scope)
{
    Decl decl;
    NEW(decl);
    decl->n.id = id;
    decl->scope = scope;
    
    return decl;
}

FuncDecl funcdecl_node(int scope)
{
    FuncDecl fdecl;
    NEW(fdecl);
    fdecl->d.n.id = FUNC_DECL;
    fdecl->d.scope = scope;
    
    return fdecl;
}

TranslationUnitDecl tudecl_node()
{
    TranslationUnitDecl tudecl;
    NEW(tudecl);
    tudecl->d.n.id = TRANSLATION_UNIT_DECL;
    tudecl->d.scope = GLOBAL;
    
    return tudecl;
}

CompoundStmt compound_stmt_node()
{
    CompoundStmt cstmt;
    NEW(cstmt);
    cstmt->s.n.id = COMPOUND_STMT;
    
    return cstmt;
}

int is_funcdef_node(Node node)
{
    if (!node || node->id != FUNC_DECL) {
        return 0;
    }
    
    FuncDecl fdecl = (FuncDecl) node;
    return fdecl->cs != NULL;
}


LiteralExpr literal_expr_node(int id)
{
    assert(id > LITERAL_ID_BEGIN && id < LITERAL_ID_END);
    LiteralExpr expr;
    NEW(expr);
    expr->e.n.id = id;
    
    return expr;
}

AddrExpr addr_expr_node(const char *id)
{
    AddrExpr aexpr;
    NEW(aexpr);
    aexpr->e.n.id = ADDR_OPERATOR;
    aexpr->id = id;
    return aexpr;
}

const char * node_print_function(void *data)
{
    Node *p = data;
    
}

