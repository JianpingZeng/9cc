#include "cc.h"

unsigned errors;
unsigned warnings;

#define MAX_ERRORS 32

static void cc_print_lead(int tag, struct source src, const char *fmt, va_list ap)
{
    const char *lead;
    switch (tag) {
    case WRN:
	lead = PURPLE("warning:");
	break;
    case ERR:
	lead = RED("error:");
	break;
    case FTL:
	lead = RED("fatal:");
	break;
    default:
	CCAssert(0);
    }

    fprintf(stderr, CLEAR "%s:%u:%u:" RESET " %s ", src.file, src.line, src.column, lead);
    fprintf(stderr, CLEAR);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, RESET);
    fprintf(stderr, "\n");
}

void warningf(struct source src, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    cc_print_lead(WRN, src, fmt, ap);
    va_end(ap);
    ++warnings;
}

void errorf(struct source src, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    cc_print_lead(ERR, src, fmt, ap);
    va_end(ap);
    ++errors;
    if (errors >= MAX_ERRORS) {
        fprintf(stderr, "Too many errors.\n");
        exit(EXIT_FAILURE);
    }
}

void fatalf(struct source src, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    cc_print_lead(FTL, src, fmt, ap);
    va_end(ap);
    exit(EXIT_FAILURE);
}

void redefinition_error(struct source src, node_t *sym)
{
    errorf(src, "redefinition of '%s', previous definition at %s:%u:%u",
           SYM_NAME(sym), AST_SRC(sym).file, AST_SRC(sym).line, AST_SRC(sym).column);
}

void conflicting_types_error(struct source src, node_t *sym)
{
    errorf(src, "conflicting types for '%s', previous at %s:%u:%u",
           SYM_NAME(sym), AST_SRC(sym).file, AST_SRC(sym).line, AST_SRC(sym).column);
}

void field_not_found_error(node_t *ty, const char *name)
{
    if (isincomplete(ty))
	error("incomplete definition of type '%s'", type2s(ty));
    else
	error("'%s' has no field named '%s'", type2s(ty), name);
}
