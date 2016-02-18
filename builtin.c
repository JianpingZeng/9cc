#include "cc.h"

static void define_builtin_func(const char *name, node_t *rtype, bool varg, ...)
{
    node_t *ftype = func_type();
    _TYPE_TYPE(ftype) = rtype;
    if (varg)
        _TYPE_VARG(ftype) = true;
    struct vector *params = vec_new();
    enter_scope();
    va_list ap;
    va_start(ap, varg);
    for (node_t *ptype; (ptype = va_arg(ap, node_t *)) != NULL; ) {
        node_t *param = anonymous(&identifiers, SCOPE);
        if (isfunc(ptype))
            SYM_TYPE(param) = ptr_type(ptype);
        else if (isarray(ptype))
            SYM_TYPE(param) = ptr_type(rtype(ptype));
        else
            SYM_TYPE(param) = ptype;
        vec_push(params, param);
    }
    va_end(ap);
    exit_scope();
    _TYPE_PARAMS(ftype) = (node_t **)vtoa(params);
    node_t *sym = install(name, &identifiers, GLOBAL);
    SYM_TYPE(sym) = ftype;
    SYM_DEFINED(sym) = true;
}

/**
   typedef struct __builtin_va_list_tag {
       unsigned int gp_offset;
       unsigned int fp_offset;
       void *overflow_arg_area;
       void *reg_save_area;
   } __builtin_va_list[1];
 */

static node_t * define_builtin_va_list(void)
{
    struct source src = {.file = "<builtin>", .line = 1, .column = 0};
    node_t *record = tag_type(STRUCT, BUILTIN_VA_LIST "_tag", src);
    SYM_DEFINED(record) = true;
    node_t *type = SYM_TYPE(record);
    TYPE_SIZE(type) = 24;
    
    node_t *array = array_type(type);
    _TYPE_LEN(array) = 1;
    _TYPE_SIZE(array) = _TYPE_LEN(array) * TYPE_SIZE(type);
    
    // typedef
    node_t *sym = install(BUILTIN_VA_LIST, &identifiers, GLOBAL);
    SYM_SCLASS(sym) = TYPEDEF;
    SYM_TYPE(sym) = array;
    
    return SYM_TYPE(sym);
}

void builtin_init(void)
{
    cc_assert(SCOPE == GLOBAL);

    node_t *va_list_type = define_builtin_va_list();
    node_t *voidptr = ptr_type(voidtype);
    // __builtin_va_list
    define_builtin_func(BUILTIN_VA_START, voidtype, true, va_list_type, NULL);
    define_builtin_func(BUILTIN_VA_END, voidtype, false, va_list_type, NULL);
    define_builtin_func(BUILTIN_VA_COPY, voidtype, false, voidptr, voidptr, NULL);
    define_builtin_func(BUILTIN_VA_ARG_CLASS, inttype, false, va_list_type, voidptr, NULL);
}
