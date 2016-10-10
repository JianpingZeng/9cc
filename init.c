#include "cc.h"

///
/// Initializer
///

static bool first_init(struct token *t)
{
    return first_expr(t) || t->id == '.' || t->id == '{' || t->id == '[';
}

static void eat_initializer(void)
{
    if (token->id == '[' || token->id == '.') {
        do {
            if (token->id == '.') {
                expect('.');
                expect(ID);
            } else {
                expect('[');
                intexpr();
                expect(']');
            }
        } while (token->id == '[' || token->id == '.');
        expect('=');
    }

    initializer(NULL);
}

static void eat_initlist(void)
{
    do {
        eat_initializer();
        if (token->id != ',')
            break;

        expect(',');
    } while (first_init(token));
}

static void struct_init(struct type *ty, long offset, struct init **init, bool brace)
{
    
}

static void array_init(struct type *ty, long offset, struct init **init, bool brace)
{
    
}

static void scalar_init(struct type *ty, long offset, struct init **init)
{
    
}

static void initializer_list1(struct type *ty, long offset, struct init **init)
{
    int follow[] = { ',', IF, '[', ID, '.', DEREF, 0 };

    expect('{');

    if (ty) {
        if (first_init(token)) {
            if (isstruct(ty) || isunion(ty))
                struct_init(ty, offset, init, true);
            else if (isarray(ty))
                array_init(ty, offset, init, true);
            else
                scalar_init(ty, offset, init);

            if (token->id == ',')
                expect(',');

            if (first_init(token)) {
                warning("excess element in %s initializer at '%s'",
                        TYPE_NAME(ty), tok2s(token));
                eat_initlist();
            }
        } else {
            error("expect initializer at '%s'", tok2s(token));
        }
    } else {
        eat_initlist();
    }

    match('}', follow);
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
    struct expr *ret = ast_expr(COMPOUND, ty, NULL, NULL);
    struct init *init = NULL;

    initializer_list1(ty, 0, &init);
    ret->u.inits = init;
    
    return ret;
}
