#include "cc.h"

#define isdeclspec(t) (kind(t) & (SCLASS_SPEC|TYPE_QUAL|TYPE_SPEC|FUNC_SPEC))

// int scopelevel;

// static struct type * declaration_specifier(int *sclass);
// static void declarator(struct type **, const char **, int abstract);
// static struct type * pointer();

// static struct symbol * declparam(const char *id, struct type *ty);

// // abstract
// enum {
//     NON_ABSTRACT,
//     ABSTRACT,
//     COMPAT, // both
// };

static int kinds[] = {
#define _a(x, y, z)
#define _x(a, b, c, d)  c,
#define _t(a, b, c)  c,
#include "token.h"
};

int kind(int t)
{
    if (t >= ID && t < TOKEND) {
        return kinds[t-ID];
    }
    else {
        return 0;
    }
}

int is_typename(struct token *t)
{
    return kind(t->id) & (TYPE_SPEC|TYPE_QUAL) || is_typedef_name(t->name);
}

// static struct symbol ** parameter_type_list(struct type *ftype)
// {
//     BEGIN_CALL(parameter_type_list);

//     Vector *v = new_vector();
//     enterscope();
//     if (isdeclspec(token->id) || (token->id == ID && istypedefname(token->name))) {
//         int n = 0;
//         for (;;) {
//             struct type *basety = NULL;
//             struct type *type = NULL;
//             const char *id = NULL;
//             int sclass;

//             n++;
//             basety = declaration_specifier(&sclass);
//             declarator(&type, &id, COMPAT);
//             attach_type(&type, basety);
//             type = scls(sclass, type);
//             if (isvoid(type)) {
//                 if (n > 1 || id || type->sclass || isqual(type)) {
//                     error("invalid void type as parameter");
//                 }
//             }
//             if (id == NULL) {
//                 id = stringd(n);
//             }
//             vector_push(v, declparam(id, type));
//             if (token->id == ')') {
//                 break;
//             }
//             match(',');
//             if (token->id == ELLIPSIS) {
//                 match(ELLIPSIS);
//                 break;
//             }

//             if (!(isdeclspec(token->id) || (token->id == ID && istypedefname(token->name)))) {
//                 error("invalid token '%k', missing type name", token);
//                 break;
//             }
//         }

//         ftype->u.f.oldstyle = 0;
//     }
//     exitscope();

//     END_CALL(parameter_type_list);

//     return (struct symbol **)vector_to_array(v);
// }

// static struct symbol ** funcparams(struct type *ftype)
// {
//     BEGIN_CALL(funcparams);

//     struct symbol ** syms;
    
//     if (isdeclspec(token->id) || (token->id == ID && istypedefname(token->name))) {
//         // prototype
//         syms = parameter_type_list(ftype);
//         ftype->u.f.proto = syms;
//     }
//     else {
//         Vector *v = new_vector();
//         if (token->id == ID) {
//             // old style
//             enterscope();
//             for ( ; ; ) {
//                 struct symbol *s;
//                 if (token->id != ID) {
//                     error("expect identifier at '%k'", token);
//                     break;
//                 }
//                 s = declparam(token->name, inttype);
//                 vector_push(v, s);
//                 match(ID);
//                 if (token->id != ',') {
//                     break;
//                 }
//                 match(',');
//             }
//             ftype->u.f.proto = NULL;
//             ftype->u.f.oldstyle = 1;
//             exitscope();
//         }
//         syms = vector_to_array(v);
//     }

//     END_CALL(funcparams);
    
//     return syms;
// }

// static struct type * func_or_array()
// {
//     struct type *ty = NULL;
//     BEGIN_CALL(func_or_array);
    
//     for (; token->id == '(' || token->id == '['; ) {
//         if (token->id == '[') {
//             struct type *atype = new_type();
//             match('[');
//             if (token->id == ICONSTANT) {
//                 match(ICONSTANT);
//             }
//             match(']');
//             atype->op = ARRAY;
//             atype->size = token->v.u.i;
//             attach_type(&ty, atype);
//         }
//         else {
//             match('(');
//             struct type *ftype = new_type();
//             ftype->op = FUNCTION;
//             ftype->u.f.proto = funcparams(ftype);
//             attach_type(&ty, ftype);
//             match(')');
//         }
//     }
    
//     END_CALL(func_or_array);
    
//     return ty;
// }

// static struct type * abstract_func_or_array()
// {
//     struct type *ty = NULL;
//     BEGIN_CALL(abstract_func_or_array);
    
//     for (; token->id == '(' || token->id == '['; ) {
//         if (token->id == '[') {
//             match('[');
//             if (token->id == ']') {
//                 match(']');
//             }
//             else if (token->id == '*') {
//                 match('*');
//                 match(']');
//             }
//             else {
//                 assignment_expr();
//                 match(']');
//             }
//             struct type *atype = new_type();
//             atype->op = ARRAY;
//             attach_type(&ty, atype);
//         }
//         else {
//             match('(');
//             struct type *ftype = new_type();
//             ftype->op = FUNCTION;
//             ftype->u.f.proto = parameter_type_list(ftype);
//             attach_type(&ty, ftype);
//             match(')');
//         }
//     }
    
//     END_CALL(abstract_func_or_array);
    
//     return ty;
// }

// static void direct_abstract_declarator(struct type **type, const char **id)
// {
//     BEGIN_CALL(direct_abstract_declarator);
    
//     assert(token->id == '(' || token->id == '[');
    
//     if (token->id == '[') {
//         struct type *type2 = abstract_func_or_array();
//         prepend_type(type, type2);
//     }
//     else if (token->id == '(') {
//         int t = lookahead();
//         if (t == '*' || t == '(' || t == '[') {
//             struct type *type1 = *type;
//             match('(');
//             declarator(type, id, ABSTRACT);
//             match(')');
//             if (token->id == '(' || token->id == '[') {
//                 struct type *type2;
//                 struct type *rtype = *type;
//                 type2 = abstract_func_or_array();
//                 attach_type(&type2, type1);
//                 // deref list
//                 while (rtype && rtype->type != type1) {
//                     rtype = rtype->type;
//                 }
//                 assert(rtype);
//                 if (type1 == rtype->type) {
//                     rtype->type = NULL;
//                 }
//                 attach_type(type, type2);
//             }
//         }
//         else {
//             struct type *type2 = abstract_func_or_array();
//             prepend_type(type, type2);
//         }
//     }
    
//     END_CALL(direct_abstract_declarator);
// }

// static void direct_declarator(struct type **type, const char **id)
// {
//     BEGIN_CALL(direct_declarator);
    
//     assert(token->id == ID || token->id == '(');
    
//     if (token->id == ID) {
//         if (id) {
//             *id = token->name;
//         }
//         match(ID);
//         if (token->id == '(' || token->id == '[') {
//             struct type *type2 = func_or_array();
//             prepend_type(type, type2);
//         }
//     }
//     else if (token->id == '(') {
//         struct type *type1 = *type;
//         match('(');
//         declarator(type, id, NON_ABSTRACT);
//         match(')');
//         if (token->id == '(' || token->id == '[') {
//             struct type *type2 = NULL;
//             struct type *rtype = *type;
//             type2 = func_or_array();
//             attach_type(&type2, type1);
//             // deref list
//             while (rtype && rtype->type != type1) {
//                 rtype = rtype->type;
//             }
//             assert(rtype);
//             if (type1 == rtype->type) {
//                 rtype->type = NULL;
//             }
//             attach_type(type, type2);
//         }
//     }
    
//     END_CALL(direct_declarator);
// }

// static struct type * pointer()
// {
//     struct type *ty = NULL;
//     int con, vol, res, type;
//     assert(token->id == '*');
    
//     BEGIN_CALL(pointer);
    
//     con = vol = res = type = 0;
    
//     for (; ; ) {
//         int *p, t = token->id;
//         switch (token->id) {
//             case CONST:
//                 p = &con;
//                 gettok();
//                 break;
                
//             case VOLATILE:
//                 p = &vol;
//                 gettok();
//                 break;
                
//             case RESTRICT:
//                 p = &res;
//                 gettok();
//                 break;
                
//             case '*':
//             {
//                 struct type *ptype = new_type();
//                 ptype->op = POINTER;
//                 con = vol = res = type = 0;
//                 p = &type;
//                 prepend_type(&ty, ptype);
//                 gettok();
//             }
//                 break;
                
//             default:
//                 p = NULL;
//                 break;
//         }
        
//         if (p == NULL) {
//             break;
//         }
        
//         if (*p != 0) {
//             error("duplicate type qulifier '%k'", token);
//         }
        
//         *p = t;
        
//         if (t == CONST || t == VOLATILE || t == RESTRICT) {
//             ty = qual(t, ty);
//         }
//     }
    
//     END_CALL(pointer);
    
//     return ty;
// }

// // abstract = NON_ABSTRACT  : only direct_declarator
// //          = ABSTRACT      : only direct_abstract_declarator
// //          = COMPAT        : both
// static void declarator(struct type **type, const char **id, int abstract)
// {
//     BEGIN_CALL(declarator);
    
//     if (token->id == '*') {
//         struct type *ptype = pointer();
//         prepend_type(type, ptype);
//     }
    
//     if (abstract == NON_ABSTRACT) {
//         // direct declarator
//         if (token->id == ID || token->id == '(') {
//             direct_declarator(type, id);
//         }
//         else {
//             error("expect identifier or '('");
//         }
//     }
//     else if (abstract == ABSTRACT) {
//         // direct abstract declarators
//         if (token->id == '(' || token->id == '[') {
//             direct_abstract_declarator(type, id);
//         }
//         // can be pointer only
//     }
//     else if (abstract == COMPAT) {
//         if (token->id == ID) {
//             direct_declarator(type, id);
//         }
//         else if (token->id == '[') {
//             direct_abstract_declarator(type, id);
//         }
//         else if (token->id == '(') {
// 	    int t = lookahead();
// 	    if (t == '*' || t == ID || t == '(' || t == '[') {
// 		struct type *type1 = *type;
// 		match('(');
// 		declarator(type, id, abstract);
// 		match(')');
// 		if (token->id == '(' || token->id == '[') {
// 		    struct type *type2;
// 		    assert(id);
// 		    if (*id) {
// 			type2 =  func_or_array();
// 		    }
// 		    else {
// 			type2 = abstract_func_or_array();
// 		    }
// 		    struct type *rtype = *type;
// 		    attach_type(&type2, type1);
// 		    // deref list
// 		    while (rtype && rtype->type != type1) {
// 			rtype = rtype->type;
// 		    }
// 		    assert(rtype);
// 		    if (type1 == rtype->type) {
// 			rtype->type = NULL;
// 		    }
// 		    attach_type(type, type2);
// 		}
// 	    }
// 	    else {
//                 match('(');
// 		struct type *ftype = new_type();
//                 ftype->op = FUNCTION;
// 		ftype->u.f.proto = parameter_type_list(ftype);
//                 attach_type(type, ftype);
//                 match(')');
// 		if (token->id == '(' || token->id == '[') {
// 		    abstract_func_or_array();
// 		}
// 	    }
//         }
//     }
    
//     END_CALL(declarator);
// }

// static struct type * enum_decl()
// {
//     return NULL;
// }

// static struct type * record_decl()
// {
//     return NULL;
// }

// static struct type * declaration_specifier(int *sclass)
// {
//     int cls, sign, size, type;
//     int cons, vol, res, inl;
//     struct type *basety = NULL;
    
//     BEGIN_CALL(decl_spec);
    
//     cls = sign = size = type = 0;
//     cons = vol = res = inl = 0;

//     for (;;) {
//         int *p, t = token->id;
//         switch (token->id) {
//             case AUTO: case EXTERN: case REGISTER:
//             case STATIC: case TYPEDEF:
//                 p = &cls;
//                 gettok();
//                 break;
                
//             case CONST:
//                 p = &cons;
//                 gettok();
//                 break;
                
//             case VOLATILE:
//                 p = &vol;
//                 gettok();
//                 break;
                
//             case RESTRICT:
//                 p = &res;
//                 gettok();
//                 break;
                
//             case INLINE:
//                 p = &inl;
//                 gettok();
//                 break;
                
//             case ENUM:
//                 p = &type;
//                 basety = enum_decl();
//                 gettok();
//                 break;
                
//             case STRUCT:
//             case UNION:
//                 p = &type;
//                 basety = record_decl();
//                 gettok();
//                 break;
             
//             case LONG:
//                 if (size == LONG) {
//                     t = LONG + LONG;
//                     size = 0;   // clear
//                 }
//                 // go through
//             case SHORT:
//                 p = &size;
//                 gettok();
//                 break;
            
//             case FLOAT:
//             case DOUBLE:
//             case VOID:
//             case CHAR:
//             case INT:
//                 p = &type;
//                 basety = pretype(token->id);
//                 gettok();
//                 break;
                
//             case SIGNED:
//             case UNSIGNED:
//                 p = &sign;
//                 gettok();
//                 break;
                
//             case ID:
//                 // typedef name
//             default:
//                 p = NULL;
//                 break;
//         }
        
//         if (p == NULL) {
//             break;
//         }
        
//         if (*p != 0) {
//             error("invalid syntax at '%k'", token);
//         }
        
//         *p = t;
//     }
    
//     // default is int
//     if (type == 0) {
//         if (sign == 0 && size == 0) {
//             if (stdc99) {
//                 error("missing type specifier");
//             }
//             else {
//                 error("missing type specifier, default is int");
//             }
//         }
//         type = INT;
//         basety = inttype;
//     }
    
//     // type check
//     if ((size == SHORT && type != INT) ||
//         (size == LONG + LONG && type != INT) ||
//         (size == LONG && type != INT && type != DOUBLE)) {
//         if (size == LONG + LONG) {
//             error("%s %s %s is invalid", tname(size/2), tname(size/2), tname(type));
//         }
//         else {
//             error("%s %s is invalid", tname(size), tname(type));
//         }
//     }
//     else if ((sign && type != INT && type != CHAR)) {
//         error("'%s' cannot be signed or unsigned", tname(type));
//     }
    
//     if (type == CHAR && sign) {
//         basety = sign == UNSIGNED ? unsignedchartype : signedchartype;
//     }
//     else if (size == SHORT) {
//         basety = sign == UNSIGNED ? unsignedshorttype : shorttype;
//     }
//     else if (type == INT && size == LONG) {
//         basety = sign == UNSIGNED ? unsignedlongtype : longtype;
//     }
//     else if (size == LONG + LONG) {
//         basety = sign == UNSIGNED ? unsignedlonglongtype : longlongtype;
//     }
//     else if (type == DOUBLE && size == LONG) {
//         basety = longdoubletype;
//     }
//     else if (sign == UNSIGNED) {
//         basety = unsignedinttype;
//     }
    
//     // qulifier
//     if (cons) {
//         basety = qual(CONST, basety);
//     }
//     if (vol) {
//         basety = qual(VOLATILE, basety);
//     }
//     if (res) {
//         basety = qual(RESTRICT, basety);
//     }
//     if (inl) {
//         basety = qual(INLINE, basety);
//     }
    
//     *sclass = cls;
    
//     END_CALL(decl_spec);
    
//     return basety;
// }

// static struct node * declare(const char *id, struct type *ty)
// {
//     struct node *n = NULL;
//     struct symbol *sym = lookupsym(id, identifiers);
//     if (sym == NULL || equal_type(sym->type, ty)) {
//         if (sym == NULL) {
//             sym = installsym(id, &identifiers, scopelevel);
//             sym->type = ty;
//         }
//         // new node for symbol
//         if (isfunction(ty)) {
//             struct decl *fdecl = funcdecl_node(scopelevel);
//             fdecl->d.n.s = sym;
//             n = NODE(fdecl);
//         }
//         else {
//             struct decl *vdecl = decl_node(VAR_DECL, scopelevel);
//             vdecl->n.s = sym;
//             n = NODE(vdecl);
//         }
//     }
//     else {
//         error("Confliting types for %s", id);
//     }
    
//     return n;
// }

// static struct node * declglobal(const char *id, struct type *ty)
// {
//     struct symbol *s;
//     assert(scopelevel == GLOBAL);
    
//     if (ty->sclass == 0) {
//         ty = scls(EXTERN, ty);
//     }
//     else if (ty->sclass == AUTO || ty->sclass == REGISTER) {
//         error("invalid storage class specifier '%s' for '%s'", tname(ty->sclass), id);
//         ty = scls(EXTERN, ty);
//     }
//     s = lookupsym(id, identifiers);
//     if (s && s->scope == GLOBAL) {
//         error("redeclaratoin of '%s'", id);
//     }
//     if (s == NULL || s->scope != GLOBAL) {
//         s = installsym(id, &identifiers, scopelevel);
//     }
//     s->type = ty;
    
//     return NULL;
// }

// static struct symbol * declparam(const char *id, struct type *ty)
// {
//     struct symbol *sym;
//     assert(scopelevel > GLOBAL);
    
//     if (ty->sclass != 0 && ty->sclass != REGISTER) {
//         error("invalid storage class specifier '%s' for '%s' in parameter list", tname(ty->sclass), id);
//     }
    
//     sym = lookupsym(id, identifiers);
//     if (sym && sym->scope == scopelevel) {
//         error("redeclaration '%s' in parameter list", id);
//     }
//     else {
//         sym = installsym(id, &identifiers, scopelevel);
//     }
//     sym->type = ty;
    
//     return sym;
// }

// static struct node * declocal(const char *id, struct type *ty)
// {
//     struct node *n = NULL;
//     assert(scopelevel > PARAM);
    
//     return n;
// }

// static void initializer()
// {
    
// }

struct decl * initializer_list()
{
    return NULL;
}

// struct node ** declaration()
// {
//     Vector *v = new_vector();
//     const char *id = NULL;
//     struct type *basety, *type = NULL;
//     int sclass;
    
//     BEGIN_CALL(declaration);
    
//     basety = declaration_specifier(&sclass);
//     if (token->id == ID || token->id == '*' || token->id == '(') {
//         declarator(&type, &id, NON_ABSTRACT);
//         attach_type(&type, basety);
//         type = scls(sclass, type);
//         if (token->id == ';' || token->id == '=' || token->id == ',') {
//             struct node *n;
//             // initializer
//             if (token->id == '=') {
//                 match('=');
//                 initializer();
//             }
//             // next declarator
//             n = declare(id, type);
//             if (n) {
//                 vector_push(v, n);
//             }
//             while (token->id == ',') {
//                 match(',');
//                 id = NULL;
//                 type = NULL;
//                 if (token->id == ID || token->id == '*' || token->id == '(') {
//                     declarator(&type, &id, NON_ABSTRACT);
//                     attach_type(&type, basety);
//                     // initializer
//                     if (token->id == '=') {
//                         match('=');
//                         initializer();
//                     }
//                     n = declare(id, type);
//                     if (n) {
//                         vector_push(v, n);
//                     }
//                 }
//                 else {
//                     error("invalid token '%k' in declaration", token);
//                 }
//             }
//             match(';');
//         }
//         else if (token->id == '{' && isfunction(type)) {
//             // function definiction
//             if (scopelevel == GLOBAL) {
//                 struct stmt *cs = compound_stmt();
//                 struct decl *fdecl = declglobal(id, type);
//                 if (fdecl) {
//                     Vector *pv = new_vector();
//                     for (int i=0; type->u.f.proto[i]; i++) {
//                         struct decl *param_decl = decl_node(PARAMVAL_DECL, PARAM);
//                         param_decl->n.s = type->u.f.proto[i];
//                         vector_push(pv, param_decl);
//                     }
//                     fdecl->params = vector_to_array(pv);
//                     fdecl->cs = cs;
//                     vector_push(v, fdecl);
//                 }
//             }
//             else {
//                 error("function definition can only in global scope");
//             }
//         }
//         else if ((kind(token->id) & (SCLASS_SPEC|TYPE_QUAL|TYPE_SPEC) || token->id == '(') &&
//                  isfunction(type) && type->u.f.oldstyle) {
//             // old style function
            
//         }
//         else {
            
//         }
//     }
//     else if (token->id == ';') {
//         // struct/union/enum
//         match(';');
//     }
//     else {
//         error("invalid token '%k' in declaration", token);
//     }
    
//     END_CALL(declaration);
    
//     return (struct node **) vector_to_array(v);
// }

// struct decl ** translation_unit()
// {
//     Vector *v = new_vector();
//     identifiers = table(NULL, GLOBAL);
//     gettok();
//     for (; token->id != EOI; ) {
//         if (isdeclspec(token->id) || token->id == ID || token->id == '(') {
//             vector_add_from_array(v, (void *) declaration());
//         }
//         else if (token->id == ';') {
//             warning("empty declaration");
//             match(';');
//         }
//         else {
//             error("invalid token '%k'", token);
//             gettok();
//         }
//     }
//     return vector_to_array(v);
// }

