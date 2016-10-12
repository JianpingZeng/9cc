#include <assert.h>
#include "cc.h"

///
/// Initialization        C99 [6.7.8]
///

// designator
struct desig {
    struct type *type;
    long offset;
    struct field *field;
};

static void parse_initializer(struct desig *);
static void parse_initializer_list(struct desig *);

static void scalar_init(struct desig *desig, struct expr *expr)
{
    // TODO: 
}

struct desig *next_designator(struct desig *desig)
{
    // TODO:
    return NULL;
}

struct desig *parse_designator(struct desig *desig)
{
    assert(token->id == '.' || token->id == '[');
    
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

    // TODO:
    return NULL;
}

static void parse_initializer(struct desig *desig)
{
    if (token->id == '{')
        parse_initializer_list(desig);
    else
        scalar_init(desig, assign_expr());
}

static void parse_initializer_list(struct desig *desig)
{
    struct desig *d = desig;
    
    expect('{');
    
    for (;;) {
        if (token->id == '.' || token->id == '[')
            d = parse_designator(desig);
        else
            d = next_designator(d);

        parse_initializer(d);

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
    struct desig desig = {ty, 0, NULL};
    
    parse_initializer_list(&desig);
    
    return ret;
}
