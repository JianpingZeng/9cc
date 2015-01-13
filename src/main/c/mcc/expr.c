#include "c.h"

static Expr primary_expr();
static Expr postfix_expr();
static Expr unary_expr();
static Expr cast_expr();

Expr expr()
{
    BEGIN_CALL(expr);
    
    postfix_expr();
    
    END_CALL(expr);
    
    return NULL;
}

Expr assignment_expr()
{
    return NULL;
}

Expr constant_expr()
{
    return NULL;
}

static Expr primary_expr()
{
    BEGIN_CALL(primary_expr);
    
    Expr e = NULL;
    switch (tok) {
        case ID:
        {
            AddrExpr aexpr = addr_expr_node(toklex.name);
            e = (Expr) aexpr;
            match(ID);
        }
            break;
            
        case ICONSTANT:
        case FCONSTANT:
        case SCONSTANT:
        {
            LiteralExpr lexpr = literal_expr_node(tok);
            lexpr->u = toklex.u;
            e = (Expr) lexpr;
            match(tok);
        }
            break;
            
        case '(':
        {
            match('(');
            e = expr();
            match(')');
        }
            break;
            
        default:
            ERROR("invalid primary expression");
            break;
    }
    
    END_CALL(primary_expr);
        
    return e;
}

static Vector argument_expr_list()
{
    return NULL;
}

static Expr postfix_expr()
{
    BEGIN_CALL(postfix_expr);
    
    if (tok == '(') {
        int t = lookahead();
        if (kind(t) & (TYPE_QUAL|TYPE_SPEC)) {
            //type-name
            match('(');
            typename();
            match(')');
            match('{');
            initializer_list();
            if (tok == ',') {
                match(',');
                match('}');
            }
            else if (tok == '}') {
                match('}');
            }
            else {
                ERROR("expect ',' or '}'");
            }
        }
        else {
            //expression
            match('(');
            expr();
            match(')');
        }
    }
    else {
        primary_expr();
    }
    
    for (; tok == '[' || tok == '(' || tok == '.' || tok == DEREF || tok == INCR || tok == DECR; ) {
        switch (tok) {
            case '[':
                match('[');
                expr();
                match(']');
                break;
                
            case '(':
                match('(');
                if (tok != ')') {
                    argument_expr_list();
                }
                match(')');
                break;
                
            case '.':
                match('.');
                match(ID);
                break;
                
            case DEREF:
                match(DEREF);
                match(ID);
                break;
                
            case INCR:
                match(INCR);
                break;
                
            case DECR:
                match(DECR);
                break;
                
            default:
                ERROR("invalid token %s", tname(tok));
                break;
        }
    }
    
    END_CALL(postfix_expr);
    
    return NULL;
}

static Expr unary_expr()
{
    BEGIN_CALL(unary_expr);
    
    switch (tok) {
        case INCR:
        case DECR:
            match(tok);
            unary_expr();
            break;
            
        case '&': case '*': case '+':
        case '-': case '~': case '!':
            match(tok);
            cast_expr();
            break;
            
        case SIZEOF:
            match(SIZEOF);
            
            break;
            
        default:
            postfix_expr();
            break;
    }
    
    END_CALL(unary_expr);
    
    return NULL;
}

static Expr cast_expr()
{
    BEGIN_CALL(cast_expr);
    
    END_CALL(cast_expr);
    
    return NULL;
}
