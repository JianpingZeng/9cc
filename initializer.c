#include "cc.h"

#define FIRST_INIT(t) (t->id == '[' || t->id == '.' || t->id == '{' || firstexpr(t))

static void struct_init(node_t *ty, bool brace, struct vector *v);
static void array_init(node_t *ty, bool brace, struct vector *v);
static void scalar_init(node_t *ty, struct vector *v);
static void elem_init(node_t *sty, node_t *ty, bool designated, struct vector *v, int i);
static node_t * initializer(node_t *ty);

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
    } while (FIRST_INIT(token));
}

static bool is_string(node_t *ty)
{
    if (!isarray(ty))
	return false;
    
    node_t *rty = rtype(ty);
    return TYPE_KIND(rty) == CHAR || unqual(rty) == wchartype;
}

static node_t * find_elem(struct vector *v, int i)
{
    for (int j = vec_len(v); j <= i; j++)
	vec_push(v, ast_vinit());
    return vec_at(v, i);
}

static node_t * init_elem_conv(node_t *ty, node_t *node)
{
    // VINIT_EXPR means failure.
    // cannot pass VINIT_EXPR to init_conv
    if (AST_ID(node) == VINIT_EXPR)
	return NULL;

    node_t *ret = init_conv(ty, node);
    if (ret == NULL)
	error(INCOMPATIBLE_TYPES, type2s(AST_TYPE(node)), type2s(ty));

    return ret;
}

void init_string(node_t *ty, node_t *node)
{
    int len1 = TYPE_LEN(ty);
    int len2 = TYPE_LEN(AST_TYPE(node));
    if (len1 > 0) {
	if (len1 < len2-1)
	    warning("initializer-string for char array is too long");
    } else if (isincomplete(ty)) {
	TYPE_LEN(ty) = len2;
	set_typesize(ty);
    }
}

static void aggregate_set(node_t *ty, struct vector *v, int i, node_t *node)
{
    if (!node)
	return;
    
    node_t *n = find_elem(v, i);
    if (AST_ID(n) != VINIT_EXPR)
	warning("initializer overrides prior initialization");
    
    if (AST_ID(node) == INITS_EXPR) {
	vec_set(v, i, node);
    } else if (is_string(ty) && issliteral(node)) {
        init_string(ty, node);
	vec_set(v, i, node);
    } else {
	node_t *rty = NULL;
	if (isarray(ty)) {
	    rty = rtype(ty);
	} else {
	    if (TYPE_FIELDS(ty))
		rty = FIELD_TYPE(TYPE_FIELDS(ty)[0]);
	}

	if (rty) {
	    node_t *n1 = ast_inits();
	    struct vector *v1 = vec_new();
	    vec_set(v, i, n1);

	    if (isarray(rty) || isstruct(rty) || isunion(rty))
		aggregate_set(rty, v1, 0, node);
	    else
		vec_push_safe(v1, init_elem_conv(rty, node));
	
	    EXPR_INITS(n1) = (node_t **)vtoa(v1);
	}
    }
}

static void scalar_set(node_t *ty, struct vector *v, int i, node_t *node)
{
    if (!node)
	return;
    
    node_t *n = find_elem(v, i);
    if (AST_ID(n) != VINIT_EXPR)
	warning("initializer overrides prior initialization");

    if (AST_ID(node) == INITS_EXPR) {	
	node_t **inits;
    loop:
	inits = EXPR_INITS(node);
	if (inits) {
	    node = inits[0];
	    if (AST_ID(node) == INITS_EXPR)
		goto loop;
	    vec_set_safe(v, i, init_elem_conv(ty, node));
	}
    } else {
	vec_set_safe(v, i, init_elem_conv(ty, node));
    }
}

static void struct_init(node_t *ty, bool brace, struct vector *v)
{
    bool designated = false;
    int len = LIST_LEN(TYPE_FIELDS(ty));

    for (int i = 0; ; i++) {
	node_t *fieldty = NULL;

	if (token->id == '.') {
	    const char *name = NULL;
	    expect('.');
	    if (token->id == ID)
		name = token->name;
	    expect(ID);
	    node_t *field = find_field(ty, name);
	    if (field) {
		i = indexof_field(ty, field);
		fieldty = FIELD_TYPE(field);
	    } else {
		i--;
		if (name)
		    field_not_found_error(ty, name);
	    }
	    designated = true;
	}

	if (i >= len)
	    break;

	if (!designated)
	    fieldty = FIELD_TYPE(TYPE_FIELDS(ty)[i]);
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

static void array_init(node_t *ty, bool brace, struct vector *v)
{
    bool designated = false;
    int c = 0;
    int len = TYPE_LEN(ty);

    if (is_string(ty) && token->id == SCONSTANT) {
	node_t *expr = assign_expr();
	if (vec_len(v)) {
	    warning("initializer overrides prior initialization");
	    vec_clear(v);
	}
	aggregate_set(ty, v, 0, expr);
	return;
    }
    
    for (int i = 0; ; i++) {
	node_t *rty = NULL;
	
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
	    error("array designator index [%d] exceeds array bounds (%d)", i, len);
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

static void scalar_init(node_t *ty, struct vector *v)
{
    if (token->id == '.' || token->id == '[') {
	error("designator in initializer for scalar type '%s'", type2s(ty));
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

static void elem_init(node_t *sty, node_t *ty, bool designated, struct vector *v, int i)
{
#define IS_STRING_VEC(ty, v)			\
    (is_string(ty) &&				\
     vec_len(v) == 1 &&				\
     issliteral((node_t *)vec_head(v)))
    
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
		error("expect '=' or another designator at '%s'", token->name);
	    aggregate_set(ty, v, i, initializer_list(ty));
	} else if ((token->id == '.' && isarray(ty)) ||
		   (token->id == '[' && !isarray(ty))) {
	    SAVE_ERRORS;
	    eat_initializer();
	    // inhibit redundant errors
	    if (NO_ERROR)
		error("%s designator cannot initialize non-%s type '%s'", TYPE_NAME(ty), TYPE_NAME(ty), type2s(ty));
	} else {
	    node_t *n = find_elem(v, i);
	    struct vector *v1 = vec_new();
	    if (AST_ID(n) == INITS_EXPR) {
		vec_add_array(v1, (void **)EXPR_INITS(n));
	    } else if (AST_ID(n) == STRING_LITERAL) {
		vec_push(v1, n);
	    }
	    
	    if (isarray(ty))
		array_init(ty, false, v1);
	    else
		struct_init(ty, false, v1);

	    if (IS_STRING_VEC(ty, v1)) {
		// string literal
		vec_set(v, i, (node_t *)vec_head(v1));
	    } else {
		if (AST_ID(n) != INITS_EXPR) {
		    n = ast_inits();
		    vec_set(v, i, n);
		}
		EXPR_INITS(n) = (node_t **)vtoa(v1);
	    }
	}
    } else {
	if (designated)
	    expect('=');
	if (IS_STRING_VEC(sty, v)) {
	    warning("initializer overrides prior initialization");
	    vec_clear(v);
	}
	scalar_set(ty, v, i, initializer(ty));
    }
}

static node_t * initializer(node_t *ty)
{
    if (token->id == '{') {
        return initializer_list(ty);
    } else if (firstexpr(token)) {
        return assign_expr();
    } else {
        error("expect '{' or assignment expression");
        return NULL;
    }
}

node_t * initializer_list(node_t *ty)
{
    int follow[] = {',', IF, '[', ID, '.', DEREF, 0};
    node_t *ret = ast_inits();
    struct vector *v = vec_new();
    
    expect('{');
    if (FIRST_INIT(token)) {
	if (ty) {
	    if (isstruct(ty) || isunion(ty))
		struct_init(ty, true, v);
	    else if (isarray(ty))
		array_init(ty, true, v);
	    else
		scalar_init(ty, v);

	    if (token->id == ',')
		expect(',');

	    if (FIRST_INIT(token)) {
		warning("excess elements in %s initializer at '%s'", TYPE_NAME(ty), token->name);
		eat_initlist();
	    }
	} else {
	    eat_initlist();
	}
    } else {
	// inhibit redundant errors
	if (ty)
	    error("expect initializer at '%s'", token->name);
    }
    
    match('}', follow);
    EXPR_INITS(ret) = (node_t **)vtoa(v);
    AST_TYPE(ret) = ty;
    return ret;
}

static bool has_static_extent(node_t *sym)
{
    return SYM_SCLASS(sym) == EXTERN ||
	SYM_SCLASS(sym) == STATIC ||
	SYM_SCOPE(sym) == GLOBAL;
}

void decl_initializer(node_t *decl, node_t *sym, int sclass, int kind)
{
    node_t *ty = SYM_TYPE(sym);
    struct source src = AST_SRC(sym);
    node_t *init;

    expect('=');

    if (kind == PARAM) {
	error("C does not support default arguments");
	initializer(NULL);
	return;
    } else if (!(isscalar(ty) || isarray(ty) || isrecord(ty))) {
	error("'%s' cannot have an initializer", TYPE_NAME(ty));
	initializer(NULL);
	return;
    }

    init = initializer(ty);
    if (init == NULL)
	return;
    
    if (sclass == EXTERN) {
	if (kind == GLOBAL) {
	    warningf(src, "'extern' variable has an initializer");
	} else {
	    errorf(src, "'extern' variable cannot have an initializer");
	    return;
	}
    } else if (sclass == TYPEDEF) {
	errorf(src, "illegal initializer (only variable can be initialized)");
	return;
    }

    if (kind == GLOBAL) {
	if (SYM_DEFINED(sym))
	    redefinition_error(src, sym);
	SYM_DEFINED(sym) = true;
    }

    if (istag(ty) && isincomplete(ty)) {
	error("variable has incomplete type '%s'", type2s(ty));
	return;
    } else if (isvarray(ty)) {
	error("variable-sized object may not be initialized");
	return;
    }

    if (AST_ID(init) != INITS_EXPR) {
	if (isarray(ty)) {
	    if (is_string(ty) && issliteral(init))
		init_string(ty, init);
	    else
		error("array initializer must be an initializer list or string literal");
	} else if (isstruct(ty) || isunion(ty)) {
	    if (!eqtype(ty, AST_TYPE(init)))
		error("initialzing '%s' with an expression of imcompatible type '%s'",
		      type2s(ty), type2s(AST_TYPE(init)));
	} else {
	    init = init_elem_conv(ty, init);
	}
    }

    if (init && has_static_extent(sym)) {
        init = eval(init, ty);
	if (init == NULL)
	    error("initializer element is not a compile-time constant");
    }

    DECL_BODY(decl) = init;
}
