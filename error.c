#include "cc.h"

unsigned errors;
unsigned warnings;

#define MAX_ERRORS 32

#define WRN  1
#define ERR  2
#define FTL  3

static void cc_print_lead(int tag, const char *file, unsigned line, const char *fmt, va_list ap)
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

    fprintf(stderr, CLEAR "%s:%u:" RESET " %s ", file, line, lead);
    fprintf(stderr, CLEAR);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, RESET);
    fprintf(stderr, "\n");
}

void warningf(struct source src, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    cc_print_lead(WRN, src.file, src.line, fmt, ap);
    va_end(ap);
    ++warnings;
}

void warning(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    cc_print_lead(WRN, source.file, source.line, fmt, ap);
    va_end(ap);
    ++warnings;
}

void errorf(struct source src, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    cc_print_lead(ERR, src.file, src.line, fmt, ap);
    va_end(ap);
    ++errors;
    if (errors >= MAX_ERRORS) {
        fprintf(stderr, "Too many errors.\n");
        exit(EXIT_FAILURE);
    }
}

void error(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    cc_print_lead(ERR, source.file, source.line, fmt, ap);
    va_end(ap);
    ++errors;
    if (errors >= MAX_ERRORS) {
        fprintf(stderr, "Too many errors.\n");
        exit(EXIT_FAILURE);
    }
}

void fatal(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    cc_print_lead(FTL, source.file, source.line, fmt, ap);
    va_end(ap);
    exit(EXIT_FAILURE);
}

static long call_depth = -1;

void begin_call(const char *funcname)
{
    call_depth++;
    for (int i=0; i < call_depth; i++)
        fprintf(stderr, " ");
    fprintf(stderr, "begin %s\n", funcname);
}

void end_call(const char *funcname)
{
    for (int i=0; i < call_depth; i++)
        fprintf(stderr, " ");
    fprintf(stderr, "end %s\n", funcname);
    call_depth--;
}

void redefinition_error(struct source src, node_t *sym)
{
    errorf(src, "redefinition of '%s', previous definition at %s line %u",
           SYM_NAME(sym), SYM_SRC(sym).file, SYM_SRC(sym).line);
}

void conflicting_types_error(struct source src, node_t *sym)
{
    errorf(src, "conflicting types for '%s', previous at %s line %u",
           SYM_NAME(sym), SYM_SRC(sym).file, SYM_SRC(sym).line);
}

void field_not_found_error(node_t *ty, const char *name)
{
    if (isincomplete(ty))
	error("incomplete definition of type '%s'", type2s(ty));
    else
	error("'%s' has no field named '%s'", type2s(ty), name);
}
