#include "cc.h"

///
/// Initialization        C99 [6.7.8]
///

static bool first_init(struct token *t)
{
    return t->id == '{' || first_expr(t) || t->id == '.' || t->id == '[';
}

/// initializer:
///   assignment-expression
///   '{' initializer-list '}'
///   '{' initializer-list ',' '}'
///
struct expr *initializer(struct type * ty)
{
    if (token->id == '{') {
        return initializer_list(ty);
    } else if (first_expr(token)) {
        return assign_expr();
    } else {
        error("expect '{' or assignment expression");
        return NULL;
    }
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
    int follow[] = { ',', IF, '[', ID, '.', DEREF, 0 };
    struct expr *ret = ast_expr(COMPOUND, ty, NULL, NULL);

    expect('{');
    
    for (; first_init(token);) {
        if (token->id == '.' || token->id == '[') {
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
        }

        initializer(ty);

        if (token->id != ',')
            break;

        expect(',');
    }

    match('}', follow);
    
    return ret;
}
