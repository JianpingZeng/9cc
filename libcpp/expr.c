#include "lex.h"

struct cpp_num {
    union value v;
};

/// assignment-expression:
///   conditional-expression
///   unary-expression assignment-operator assignment-expression
///
/// assignment-operator:
///   '=' '*=' '/=' '%=' '+=' '-=' '<<=' '>>=' '&=' '^=' '|='
///
static struct cpp_num assign_expr(void)
{
    
}

/// expression:
///   assignment-expression
///   expression ',' assignment-expression
///
static struct cpp_num expr(void)
{
    
}

/// primary-expression:
///   identifier
///   constant
///   string-literal
///   '(' expression ')'
///
static struct cpp_num primary_expr(void)
{
    
}

/// postfix-expression:
///   primary-expression
///   postfix-expression '[' expression ']'
///   postfix-expression '(' argument-expression-list[opt] ')'
///   postfix-expression '.' identifier
///   postfix-expression '->' identifier
///   postfix-expression '++'
///   postfix-expression '--'
///   '(' type-name ')' '{' initializer-list '}'
///   '(' type-name ')' '{' initializer-list ',' '}'
///
static struct cpp_num postfix_expr(void)
{
    
}

/// unary-expression:
///   postfix-expression
///   '++' unary-expression
///   '--' unary-expression
///   unary-operator cast-expression
///   'sizeof' unary-expression
///   'sizeof' '(' type-name ')'
///
static struct cpp_num unary_expr(void)
{
    
}

/// cast-expression:
///   unary-expression
///   '(' type-name ')' cast-expression
///
static struct cpp_num cast_expr(void)
{
    
}

/// multiplicative-expression:
///   cast-expression
///   multiplicative-expression '*' cast-expression
///   multiplicative-expression '/' cast-expression
///   multiplicative-expression '%' cast-expression
///
static struct cpp_num multiple_expr(void)
{
    
}

/// additive-expression:
///   multiplicative-expression
///   additive-expression '+' multiplicative-expression
///   additive-expression '-' multiplicative-expression
///
static struct cpp_num additive_expr(void)
{
    
}

/// shift-expression:
///   additive-expression
///   shift-expression '<<' additive-expression
///   shift-expression '>>' additive-expression
///
static struct cpp_num shift_expr(void)
{
    
}

/// relational-expression:
///   shift-expression
///   relational-expression '<' shift-expression
///   relational-expression '>' shift-expression
///   relational-expression '<=' shift-expression
///   relational-expression '>=' shift-expression
///
static struct cpp_num relational_expr(void)
{
    
}

/// equality-expression:
///   relational-expression
///   equality-expression '==' relational-expression
///   euqality-expression '!=' relational-expression
///
static struct cpp_num equality_expr(void)
{
    
}

/// AND-expression:
///   equality-expression
///   AND-expression '&' equality-expression
///
static struct cpp_num and_expr(void)
{
    
}

/// exclusive-OR-expression:
///   AND-expression
///   exclusive-OR-expression '^' AND-expression
///
static struct cpp_num exclusive_or_expr(void)
{
    
}

/// inclusive-OR-expression:
///   exclusive-OR-expression
///   inclusive-OR-expression '|' exclusive-OR-expression
///
static struct cpp_num inclusive_or_expr(void)
{
    
}

/// logical-AND-expression:
///   inclusive-OR-expression
///   logical-AND-expression '&&' inclusive-OR-expression
///
static struct cpp_num logical_and_expr(void)
{
    
}

/// logical-OR-expression:
///   logical-AND-expression
///   logical-OR-expression '||' logical-AND-expression
///
static struct cpp_num logical_or_expr(void)
{
    
}

/// conditional-expression:
///   logical-OR-expression
///   logical-OR-expression '?' expression ':' conditional-expression
///
static struct cpp_num cond_expr(void)
{
    
}

static int intexpr(void)
{
    
}

/// constant-expression:
///   conditional-expression
///
bool eval_cpp_const_expr(void)
{
    gettok();
    return intexpr();
}
