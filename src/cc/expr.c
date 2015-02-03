#include "cc.h"

// static struct expr * primary_expr();
// static struct expr * postfix_expr();
// static struct expr * unary_expr();
// static struct expr * cast_expr();

// struct expr * expr()
// {
//     BEGIN_CALL(expr);
    
//     postfix_expr();
    
//     END_CALL(expr);
    
//     return NULL;
// }

// struct expr * assignment_expr()
// {
//     return NULL;
// }

// struct expr * constant_expr()
// {
//     return NULL;
// }

// static struct expr * primary_expr()
// {
//     BEGIN_CALL(primary_expr);
    
//     struct expr * e = NULL;
//     switch (token->id) {
//         case ID:
//         {
//             Addrstruct expr * aexpr = addr_expr_node(token->name);
//             e = (struct expr *) aexpr;
//             match(ID);
//         }
//             break;
            
//         case ICONSTANT:
//         case FCONSTANT:
//         case SCONSTANT:
//         {
//             Literalstruct expr * lexpr = literal_expr_node(token->id);
//             //lexpr->u = token->v.u.u;
//             e = (struct expr *) lexpr;
//             match(token->id);
//         }
//             break;
            
//         case '(':
//         {
//             match('(');
//             e = expr();
//             match(')');
//         }
//             break;
            
//         default:
//             error("invalid primary expression");
//             break;
//     }
    
//     END_CALL(primary_expr);
        
//     return e;
// }

// static Vector * argument_expr_list()
// {
//     return NULL;
// }

// static struct expr * postfix_expr()
// {
//     BEGIN_CALL(postfix_expr);
    
//     if (token->id == '(') {
//         int t = lookahead();
//         if (kind(t) & (TYPE_QUAL|TYPE_SPEC)) {
//             //type-name
//             match('(');
//             typename();
//             match(')');
//             match('{');
//             initializer_list();
//             if (token->id == ',') {
//                 match(',');
//                 match('}');
//             }
//             else if (token->id == '}') {
//                 match('}');
//             }
//             else {
//                 error("expect ',' or '}'");
//             }
//         }
//         else {
//             //expression
//             match('(');
//             expr();
//             match(')');
//         }
//     }
//     else {
//         primary_expr();
//     }
    
//     for (; token->id == '[' || token->id == '(' || token->id == '.' || token->id == DEREF || token->id == INCR || token->id == DECR; ) {
//         switch (token->id) {
//             case '[':
//                 match('[');
//                 expr();
//                 match(']');
//                 break;
                
//             case '(':
//                 match('(');
//                 if (token->id != ')') {
//                     argument_expr_list();
//                 }
//                 match(')');
//                 break;
                
//             case '.':
//                 match('.');
//                 match(ID);
//                 break;
                
//             case DEREF:
//                 match(DEREF);
//                 match(ID);
//                 break;
                
//             case INCR:
//                 match(INCR);
//                 break;
                
//             case DECR:
//                 match(DECR);
//                 break;
                
//             default:
//                 error("invalid token '%k'", token);
//                 break;
//         }
//     }
    
//     END_CALL(postfix_expr);
    
//     return NULL;
// }

// static struct expr * unary_expr()
// {
//     BEGIN_CALL(unary_expr);
    
//     switch (token->id) {
//         case INCR:
//         case DECR:
//             match(token->id);
//             unary_expr();
//             break;
            
//         case '&': case '*': case '+':
//         case '-': case '~': case '!':
//             match(token->id);
//             cast_expr();
//             break;
            
//         case SIZEOF:
//             match(SIZEOF);
            
//             break;
            
//         default:
//             postfix_expr();
//             break;
//     }
    
//     END_CALL(unary_expr);
    
//     return NULL;
// }

// static struct expr * cast_expr()
// {
//     BEGIN_CALL(cast_expr);
    
//     END_CALL(cast_expr);
    
//     return NULL;
// }
