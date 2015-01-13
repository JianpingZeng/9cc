#include "c.h"

CompoundStmt compound_stmt()
{
    Vector v = new_vector();
    CompoundStmt cs = compound_stmt_node();
    
    match('{');
    enterscope();
    match('}');
    exitscope();
    
    cs->stmts = (Node *) vector_to_array(v);
    
    return cs;
}


