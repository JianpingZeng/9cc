#include <assert.h>
#include "cc.h"

///
/// Initialization        C99 [6.7.8]
///

// designator id
enum { DESIG_NONE, DESIG_FIELD, DESIG_INDEX };

// designator
struct desig {
    int id;                     // designator id
    struct source src;          // source location
    struct type *type;          // destination type
    long offset;                // destination offset (absolute)
    union {
        struct field *field;    // struct/union field
        long index;             // array index
        const char *name;       // designator identifier
    } u;
    struct desig *prev;
};

static void parse_initializer(struct desig *);
static void parse_initializer_list(struct desig *);

// for debug
static const char *desig2s(struct desig *desig)
{
    const char *s = "";
    
    assert(desig);

    for (struct desig *d = desig; d; d = d->prev) {
        switch (d->id) {
        case DESIG_NONE:
            s = format("<%s>%s", type2s(d->type), s);
            break;

        case DESIG_FIELD:
            s = format(".%s%s", d->u.field->name, s);
            break;

        case DESIG_INDEX:
            s = format("[%ld]%s", d->u.index, s);
            break;

        default:
            assert(0 && "unknown designator type");
        }
    }
    
    return s;
}

static struct desig *new_desig_name(const char *name, struct source src)
{
    struct desig *d = NEWS0(struct desig, FUNC);
    d->id = DESIG_FIELD;
    d->u.name = name;
    d->src = src;
    return d;
}

static struct desig *new_desig_index(long index, struct source src)
{
    struct desig *d = NEWS0(struct desig, FUNC);
    d->id = DESIG_INDEX;
    d->u.index = index;
    d->src = src;
    return d;
}

static struct desig *copy_desig(struct desig *desig)
{
    struct desig *ret = NULL;
    struct desig **pp = &ret;
    
    if (!desig)
        return NULL;
    
    // others
    for (struct desig *s = desig; s; s = s->prev) {
        *pp = NEWS(struct desig, FUNC);
        *(*pp) = *s;
        pp = &(*pp)->prev;
    }

    return ret;
}

static void element_init(struct desig *desig, struct expr *expr)
{
    // TODO: 
    if (!desig || !expr)
        return;

    dlog("%s: (offset=%ld) <expr %p>", desig2s(desig), desig->offset, expr);
}

// TODO: incomplete array type
static struct desig *sema_desig(struct desig *desig, struct desig **ds)
{
    assert(desig && ds);

    desig = copy_desig(desig);

    for (int i = 0; ds[i]; i++) {
        struct desig *d = ds[i];
        switch (d->id) {
        case DESIG_FIELD:
            {
                const char *name = d->u.name;
                if (!name)
                    return NULL;
                if (!isrecord(desig->type)) {
                    error_at(d->src,
                             "expect struct or union type, not '%s'",
                             type2s(desig->type));
                    return NULL;
                }
                struct field *field = find_field(desig->type, name);
                if (!field) {
                    field_not_found_error(d->src, desig->type, name);
                    return NULL;
                }
                d->offset = desig->offset + field->offset;
                d->type = field->type;
                d->u.field = field;
                d->prev = desig;
                desig = d;
            }
            break;

        case DESIG_INDEX:
            {
                if (!isarray(desig->type)) {
                    error_at(d->src,
                             "expect array type, not '%s'",
                             type2s(desig->type));
                    return NULL;
                }
                size_t len = TYPE_LEN(desig->type);
                if (len && d->u.index >= len) {
                    error_at(d->src,
                             "array designator index [%ld] exceeds array bounds (%lu)",
                             d->u.index, len);
                    return NULL;
                }
                struct type *rty = rtype(desig->type);
                d->offset = desig->offset + d->u.index * TYPE_SIZE(rty);
                d->type = rty;
                d->prev = desig;
                desig = d;
            }
            break;

        default:
            assert(0 && "unexpected designator id");
        }
    }
    
    return desig;
}

static struct desig *next_designator1(struct desig *desig)
{
    // TODO:
    return NULL;
}

static struct desig *next_designator(struct desig *desig)
{
    if (!desig)
        return NULL;

    return next_designator1(desig);
}

static struct desig *parse_designator(struct desig *desig)
{
    struct list *list = NULL;
    
    assert(token->id == '.' || token->id == '[');
    
    do {
        if (token->id == '.') {
            expect('.');
            struct source src = source;
            // only create list item when desig != NULL
            if (desig) {
                if (token->id == ID)
                    list = list_append(list, new_desig_name(TOK_ID_STR(token), src));
                else
                    // create a desig with name of NULL
                    list = list_append(list, new_desig_name(NULL, src));
            }
            expect(ID);
        } else {
            expect('[');
            struct source src = source;
            long index = intexpr();
            match(']', skip_to_squarebracket);
            // only create list item when desig != NULL
            if (desig)
                list = list_append(list, new_desig_index(index, src));
        }
    } while (token->id == '.' || token->id == '[');

    expect('=');

    return desig ? sema_desig(desig, ltoa(&list, FUNC)) : NULL;
}

static void parse_initializer(struct desig *desig)
{
    if (token->id == '{') {
        // begin a new root designator
        struct desig d = {.id = DESIG_NONE, .type = desig->type, .offset = desig->offset};
        parse_initializer_list(&d);
    } else {
        element_init(desig, assign_expr());
    }
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
    if (ty) {
        struct expr *ret = ast_expr(COMPOUND, ty, NULL, NULL);
        struct desig desig = {.id = DESIG_NONE, .type = ty, .offset = 0};
        parse_initializer_list(&desig);
        // TODO: sort inits
        // TODO: merge bitfields
        return ret;
    } else {
        parse_initializer_list(NULL);
        return NULL;
    }
}
