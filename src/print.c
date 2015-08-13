#include "cc.h"

#define STR(str)  ((str) ? (str) : "<null>")

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

static void print_qual(struct type *type)
{
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
    struct symbol **params = context.type->u.f.params;
    if (params) {
        for (int i=0; params[i]; i++) {
            struct symbol *sym = params[i];
            struct type_context con = {context.level+1, sym->type};
            for (int i=0; i < context.level+1; i++)
                fprintf(stderr, "  ");
            fprintf(stderr, "'%s' %s ", STR(sym->name), sym->defined ? "<defined>" : "");
            print_type1(con);
        }
    }
}

static void print_return(struct type_context context)
{
    struct type_context rcontext = {context.level+1, context.type};
    for (int i=0; i < rcontext.level; i++)
        fprintf(stderr, "  ");
    fprintf(stderr, "return ");
    print_type1(rcontext);
}

static void print_type1(struct type_context context)
{
    struct type *type = context.type;
    if (type) {
        struct type_context tcontext = {context.level, type->type};
        print_qual(type);
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
            fprintf(stderr, "%s '%s' %s ", nname(node), STR(node->symbol->name), node->symbol->defined ? "<defined>" : "");
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
        if (node->symbol)
            fprintf(stderr, "%s '%s' %s %s\n", nname(node), tname(e->op), STR(node->symbol->name), (e->op == INCR || e->op == DECR) ? (e->u.prefix ? "prefix" : "postfix") : "");
        else
            fprintf(stderr, "%s '%s' %s\n", nname(node), tname(e->op), (e->op == INCR || e->op == DECR) ? (e->u.prefix ? "prefix" : "postfix") : "");
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
        if (stmt->u.compoundstmt.blks) {
            for (int i=0; stmt->u.compoundstmt.blks[i]; i++) {
                struct print_context con = {level, stmt->u.compoundstmt.blks[i]};
                print_tree1(con);
            }
        }
    } else if (isstmt(node) && node->id == FOR_STMT) {
        struct stmt *stmt = (struct stmt *) node;
        if (stmt->u.forstmt.decl) {
            for (int i=0; stmt->u.forstmt.decl[i]; i++) {
                struct print_context con = {level, (struct node*)stmt->u.forstmt.decl[i]};
                print_tree1(con);
            }
        } else if (stmt->u.forstmt.init) {
            struct print_context con = {level, (struct node*)stmt->u.forstmt.init};
            print_tree1(con);
        } else {
            for (int i=0; i < level; i++)
                fprintf(stderr, "  ");
            fprintf(stderr, "init: <NULL>\n");
        }
        
        if (stmt->u.forstmt.cond) {
            struct print_context con = {level, (struct node*)stmt->u.forstmt.cond};
            print_tree1(con);
        } else {
            for (int i=0; i < level; i++)
                fprintf(stderr, "  ");
            fprintf(stderr, "cond: <NULL>\n");
        }
        
        if (stmt->u.forstmt.ctrl) {
            struct print_context con = {level, (struct node*)stmt->u.forstmt.ctrl};
            print_tree1(con);
        } else {
            for (int i=0; i < level; i++)
                fprintf(stderr, "  ");
            fprintf(stderr, "ctrl: <NULL>\n");
        }
    } else if (isexpr(node) && node->id == CALL_EXPR) {
        struct expr *expr = (struct expr *) node;
        if (expr->u.args) {
            for (int i=0; expr->u.args[i]; i++) {
                struct print_context con = {level, (struct node*)expr->u.args[i]};
                print_tree1(con);
            }
        }
    }
    
    if (KID0(context.node)) {
        struct print_context lcontext;
        lcontext.level = level;
        lcontext.node = KID0(context.node);
        print_tree1(lcontext);
    }
    
    if (KID1(context.node)) {
        struct print_context rcontext;
        rcontext.level = level;
        rcontext.node = KID1(context.node);
        print_tree1(rcontext);
    }
}

void print_tree(struct node *tree)
{
    struct print_context context = {0, tree};
    print_tree1(context);
}
