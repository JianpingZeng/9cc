#include <assert.h>
#include "cc.h"

///
/// Initialization        C99 [6.7.8]
///
static void parse_initializer(void);
static void parse_initializer_list(void);

// designation
struct desig {
    int id;                     // '.' or '['
    union {
        const char *name;
        long index;
    };
};


static void parse_designated_initializer(void)
{
    assert(token->id == '.' || token->id == '[');
    
    do {
        if (token->id == '.') {
            expect('.');
            expect(ID);
        } else {
            expect('[');
            intexpr();
            expect(']');
        }
    } while (token->id == '.' || token->id == '[');
    expect('=');
    parse_initializer();
}

static void parse_initializer(void)
{
    if (token->id == '{') {
        parse_initializer_list();
    } else {
        assign_expr();
    }
}

static void parse_initializer_list(void)
{
    expect('{');
    
    for (;;) {
        if (token->id == '.' || token->id == '[')
            parse_designated_initializer();
        else
            parse_initializer();

        if (token->id != ',')
            break;

        expect(',');

        if (token->id == '}')
            break;
    }

    match('}', skip_to_rbrace);
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
    parse_initializer_list();
    
    return actions.initlist(ty);
}
