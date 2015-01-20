#ifndef cc_cc_h
#define cc_cc_h

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <memory.h>
#include <stdarg.h>
#include "lib.h"

// cc modules
#include "config.h"
#include "lex.h"
#include "type.h"
#include "sym.h"
#include "node.h"

// error
extern unsigned errors;
extern unsigned warnings;
extern void warning(const char *fmt, ...);
extern void error(const char *fmt, ...);
extern void do_log(const char *file, unsigned line, const char *fmt, ...);
#define log(fmt, ...)  do_log(__FILE__, __LINE__, fmt, __VA_ARGS__)
extern void begin_call(const char *funcname);
extern void end_call(const char *funcname);
#ifdef SHOW_CALL_TREE
#define BEGIN_CALL(funcname)    begin_call(#funcname)
#define END_CALL(funcname)      end_call(#funcname)
#else
#define BEGIN_CALL(funcname)
#define END_CALL(funcname)
#endif

//kind
enum {
    SCLASS_SPEC = 01, TYPE_QUAL = 02, TYPE_SPEC = 04,
    FUNC_SPEC = 010,
};

// decl
extern int scopelevel;
extern TranslationUnitDecl *translation_unit();
extern Node ** declaration();
extern unsigned char kind(int t);
extern void initializer_list();

// stmt
extern CompoundStmt * compound_stmt();

// expr
extern Expr * expr();
extern Expr * assignment_expr();


#endif
