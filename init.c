#include <assert.h>
#include "cc.h"

///
/// Initialization        C99 [6.7.8]
///

static void parse_initializer(struct type *ty, long offset);
static void parse_initializer_list(struct type *ty, long offset);

static void parse_initializer(struct type *ty, long offset)
{
    if (token->id == '{') {
        parse_initializer_list(ty, offset);
    } else {
        assign_expr();
    }
}

static void parse_initializer_list(struct type *ty, long offset)
{
    expect('{');
    
    for (;;) {        
        if (token->id == '.' || token->id == '[') {
            do {
                if (token->id == '.') {
                    expect('.');
                    expect(ID);
                } else {
                    expect('[');
                    intexpr();
                    match(']', skip_to_squarebracket);
                }
            } while (token->id == '.' || token->id == '[');

            expect('=');
        }

        parse_initializer(ty, offset);

        if (token->id != ',')
            break;

        expect(',');

        if (token->id == '}')
            break;
    }

    match('}', skip_to_brace);
}

/// initializer:
///   assignment-expression
///   '{' initializer-list '}'
///   '{' initializer-list ',' '}'
///
struct expr *initializer(struct type * ty)
{
    if (token->id == '{')
        return initializer_list(ty);
    else
        return assign_expr();
}

/// initializer-list:
///   designation[opt] initializer
///   initializer-list ',' designation[opt] initializer
///
/// designation:
///   designator-list '='
///
/// designator-list:
///   designator
///   designator-list designator
///
/// designator:
///   '[' constant-expression ']'
///   '.' identifier
///
struct expr *initializer_list(struct type * ty)
{
    struct expr *ret = ast_expr(COMPOUND, ty, NULL, NULL);
    
    parse_initializer_list(ty, 0);
    
    return ret;
}
