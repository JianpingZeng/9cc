#include "cc.h"

static void define_builtin_func(const char *name, node_t *rtype, struct vector *ptypes)
{
    node_t *ftype = func_type();
    _TYPE_TYPE(ftype) = rtype;
    struct vector *params = vec_new();
    cc_assert(SCOPE == GLOBAL);
    enter_scope();
    for (int i = 0; i < vec_len(ptypes); i++) {
        node_t *ptype = vec_at(ptypes, i);
        node_t *param = anonymous(&identifiers, SCOPE);
        SYM_TYPE(param) = ptype;
        vec_push(params, param);
    }
    exit_scope();
    _TYPE_PARAMS(ftype) = (node_t **)vtoa(params);
    node_t *sym = install(name, &identifiers, GLOBAL);
    SYM_TYPE(sym) = ftype;
    SYM_DEFINED(sym) = true;
}

void builtin_init(void)
{
    struct vector *voidptr = vec_new1(ptr_type(chartype));
    struct vector *voidptr2 = vec_new();
    vec_push(voidptr2, ptr_type(voidtype));
    vec_push(voidptr2, ptr_type(voidtype));
    // __builtin_va_list
    node_t *sym = lookup(BUILTIN_VA_LIST, identifiers);
    cc_assert(sym);
    node_t *type = SYM_TYPE(sym);
    println("%s", type2s(type));
    define_builtin_func(BUILTIN_VA_START, voidtype, voidptr2);
    define_builtin_func(BUILTIN_VA_END, voidtype, voidptr);
    define_builtin_func(BUILTIN_VA_COPY, voidtype, voidptr2);
    define_builtin_func(BUILTIN_VA_ARG, voidtype, voidptr2);
}
