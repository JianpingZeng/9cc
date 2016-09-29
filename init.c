#include "cc.h"

///
/// Initializer
///
static void struct_init(struct type * ty, bool brace, struct vector *v);
static void array_init(struct type * ty, bool brace, struct vector *v);
static void scalar_init(struct type * ty, struct vector *v);
static void elem_init(struct type * sty, struct type * ty, bool designated, struct vector *v, int i);

#define INIT_OVERRIDE    "initializer overrides prior initialization"

static bool first_init(struct token *t)
{
    return t->id == '[' || t->id == '.' || t->id == '{' || first_expr(t);
}

static void eat_initializer(void)
{
    if (token->id == '[' || token->id == '.') {
        do {
            if (token->id == '[') {
                expect('[');
                intexpr();
                expect(']');
            } else {
                expect('.');
                expect(ID);
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

static bool is_string(struct type * ty)
{
    if (!isarray(ty))
        return false;

    struct type *rty = rtype(ty);
    return TYPE_KIND(rty) == CHAR || unqual(rty) == wchartype;
}

static struct expr *find_elem(struct vector *v, int i)
{
    for (int j = vec_len(v); j <= i; j++)
        vec_push(v, ast_vinit());
    return vec_at(v, i);
}

static struct expr *init_elem_conv(struct type *ty, struct expr *node)
{
    // VINIT_EXPR means failure.
    // cannot pass VINIT_EXPR to assignconv
    if (node->id == VINIT_EXPR)
        return NULL;

    struct expr *ret = assignconv(ty, node);
    if (ret == NULL)
        error_at(node->src, INCOMPATIBLE_TYPES,
                 type2s(node->type), type2s(ty));

    return ret;
}

static void aggregate_set(struct type *ty, struct vector *v, int i, struct expr *node)
{
    if (!node)
        return;

    struct expr *n = find_elem(v, i);
    if (n->id != VINIT_EXPR)
        warning_at(node->src, INIT_OVERRIDE);

    if (node->id == INITS_EXPR) {
        vec_set(v, i, node);
    } else if (is_string(ty) && issliteral(node)) {
        init_string(ty, node);
        vec_set(v, i, node);
    } else if (isrecord(ty) && isrecord(node->type)
               && eqtype(unqual(ty), unqual(node->type))) {
        vec_set(v, i, node);
    } else {
        struct type *rty = NULL;
        if (isarray(ty)) {
            rty = rtype(ty);
        } else {
            if (length(TYPE_FIELDS(ty))) {
                struct field *field = TYPE_FIELDS(ty)[0];
                rty = field->type;
            }
        }

        if (rty) {
            struct expr *n1 = ast_inits(ty, source);
            struct vector *v1 = vec_new();
            vec_set(v, i, n1);

            if (isarray(rty) || isstruct(rty) || isunion(rty))
                aggregate_set(rty, v1, 0, node);
            else
                vec_push_safe(v1, init_elem_conv(rty, node));

            EXPR_INITS(n1) = vtoa(v1, PERM);
        }
    }
}

static void scalar_set(struct type *ty, struct vector *v, int i, struct expr *node)
{
    if (!node)
        return;

    struct expr *n = find_elem(v, i);
    if (n->id != VINIT_EXPR)
        warning_at(node->src, INIT_OVERRIDE);

    if (node->id == INITS_EXPR) {
        struct expr **inits;
    loop:
        inits = EXPR_INITS(node);
        if (length(inits)) {
            node = inits[0];
            if (node->id == INITS_EXPR)
                goto loop;
            vec_set_safe(v, i, init_elem_conv(ty, node));
        }
    } else {
        vec_set_safe(v, i, init_elem_conv(ty, node));
    }
}

static void struct_init(struct type * ty, bool brace, struct vector *v)
{
    bool designated = false;
    int len = length(TYPE_FIELDS(ty));

    for (int i = 0;; i++) {
        struct type *fieldty = NULL;

        if (token->id == '.') {
            const char *name = NULL;
            expect('.');
            if (token->id == ID)
                name = TOK_ID_STR(token);
            expect(ID);
            struct field *field = find_field(ty, name);
            if (field) {
                i = indexof_field(ty, field);
                fieldty = field->type;
            } else {
                i--;
                if (name) {
                    if (isincomplete(ty))
                        error(INCOMPLETE_DEFINITION_OF_TYPE, type2s(ty));
                    else
                        error(FIELD_NOT_FOUND_ERROR, type2s(ty), name);
                }
            }
            designated = true;
        }

        if (i >= len)
            break;

        if (!designated) {
            struct field *field = TYPE_FIELDS(ty)[i];
            fieldty = field->type;
        }
        elem_init(ty, fieldty, designated, v, i);
        designated = false;

        struct token *ahead = lookahead();
        if (token->id == '}' || (token->id == ',' && ahead->id == '}'))
            break;
        if ((ahead->id == '.' || ahead->id == '[') && !brace)
            break;
        if (brace || i < len - 1)
            expect(',');
    }
}

static void array_init(struct type *ty, bool brace, struct vector *v)
{
    bool designated = false;
    int c = 0;
    int len = TYPE_LEN(ty);

    if (is_string(ty) && token->id == SCONSTANT) {
        struct expr *expr = assign_expr();
        if (vec_len(v)) {
            warning_at(expr->src, INIT_OVERRIDE);
            vec_clear(v);
        }
        aggregate_set(ty, v, 0, expr);
        return;
    }

    for (int i = 0;; i++) {
        struct type *rty = NULL;

        if (token->id == '[') {
            expect('[');
            i = intexpr();
            expect(']');
            designated = true;
        }

        if (len > 0 && i >= len && !designated)
            break;

        c = MAX(c, i);
        if (len > 0 && i >= len)
            error("array designator index [%d] exceeds array bounds (%d)",
                  i, len);
        else
            rty = rtype(ty);
        elem_init(ty, rty, designated, v, i);
        designated = false;

        struct token *ahead = lookahead();
        if (token->id == '}' || (token->id == ',' && ahead->id == '}'))
            break;
        if ((ahead->id == '.' || ahead->id == '[') && !brace)
            break;
        if (brace || (len > 0 && i < len - 1) || len == 0)
            expect(',');
    }

    if (isincomplete(ty)) {
        TYPE_LEN(ty) = c + 1;
        set_typesize(ty);
    }
}

static void scalar_init(struct type * ty, struct vector *v)
{
    if (token->id == '.' || token->id == '[') {
        error("designator in initializer for scalar type '%s'",
              type2s(ty));
        eat_initializer();
    } else if (token->id == '{') {
        static int braces;
        if (braces++ == 0)
            warning("too many braces around scalar initializer");
        scalar_set(ty, v, 0, initializer_list(ty));
        braces--;
    } else {
        scalar_set(ty, v, 0, initializer(ty));
    }
}

static bool is_string_vec(struct type *ty, struct vector *v)
{
    return is_string(ty) && vec_len(v) == 1 && issliteral((struct expr *)vec_head(v));
}

static void elem_init(struct type * sty, struct type * ty, bool designated, struct vector *v, int i)
{
    if (isunion(sty))
        i = 0;                // always set the first elem

    if (ty == NULL) {
        if (token->id == '.' || token->id == '[') {
            eat_initializer();
        } else {
            if (token->id == '=')
                expect('=');
            initializer(ty);
        }
    } else if (isstruct(ty) || isunion(ty) || isarray(ty)) {
        if (token->id == '=') {
            if (!designated)
                error("expect designator before '='");
            expect('=');
            aggregate_set(ty, v, i, initializer(ty));
        } else if (token->id == '{') {
            if (designated)
                error("expect '=' or another designator at '%s'",
                      tok2s(token));
            aggregate_set(ty, v, i, initializer_list(ty));
        } else if ((token->id == '.' && isarray(ty)) ||
                   (token->id == '[' && !isarray(ty))) {
            SAVE_ERRORS;
            eat_initializer();
            // inhibit redundant errors
            if (NO_ERROR)
                error("%s designator cannot initialize non-%s type '%s'",
                      TYPE_NAME(ty), TYPE_NAME(ty), type2s(ty));
        } else {
            struct expr *n = find_elem(v, i);
            struct vector *v1 = vec_new();
            if (n->id == INITS_EXPR) {
                vec_add_array(v1, EXPR_INITS(n));
            } else if (n->id == STRING_LITERAL) {
                vec_push(v1, n);
            }

            if (isarray(ty))
                array_init(ty, false, v1);
            else
                struct_init(ty, false, v1);

            if (is_string_vec(ty, v1)) {
                // string literal
                vec_set(v, i, (struct expr *) vec_head(v1));
            } else {
                if (n->id != INITS_EXPR) {
                    n = ast_inits(ty, source);
                    vec_set(v, i, n);
                }
                EXPR_INITS(n) = vtoa(v1, PERM);
            }
        }
    } else {
        if (designated)
            expect('=');
        if (is_string_vec(sty, v)) {
            warning(INIT_OVERRIDE);
            vec_clear(v);
        }
        scalar_set(ty, v, i, initializer(ty));
    }
}

void init_string(struct type *ty, struct expr *node)
{
    int len1 = TYPE_LEN(ty);
    int len2 = TYPE_LEN(node->type);
    if (len1 > 0) {
        if (len1 < len2 - 1)
            warning("initializer-string for char array is too long");
    } else if (isincomplete(ty)) {
        TYPE_LEN(ty) = len2;
        set_typesize(ty);
    }
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
    struct vector *v = vec_new();

    expect('{');

    if (ty) {
        if (first_init(token)) {
            if (isstruct(ty) || isunion(ty))
                struct_init(ty, true, v);
            else if (isarray(ty))
                array_init(ty, true, v);
            else
                scalar_init(ty, v);

            if (token->id == ',')
                expect(',');

            if (first_init(token)) {
                warning("excess elements in %s initializer at '%s'",
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
    EXPR_INITS(ret) = vtoa(v, PERM);
    return ret;
}

struct expr *ensure_init(struct expr *init, struct type *ty, struct symbol *sym)
{
    struct source src = init->src;
    
    SAVE_ERRORS;
    if (init->id != INITS_EXPR) {
        if (isarray(ty)) {
            if (is_string(ty) && issliteral(init))
                init_string(ty, init);
            else
                error("array initializer must be an initializer list or string literal");
        } else if (isstruct(ty) || isunion(ty)) {
            if (!eqtype(ty, init->type))
                error("initialzing '%s' with an expression of imcompatible type '%s'",
                      type2s(ty), type2s(init->type));
        } else {
            init = init_elem_conv(ty, init);
        }
    }

    if (NO_ERROR && init && has_static_extent(sym)) {
        init = eval(init, ty);
        if (init == NULL)
            error_at(src, "initializer element is not a compile-time constant");
    }

    return init;
}
