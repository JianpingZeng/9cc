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
            if (ENV.is_color_term)
                lead = PURPLE "warning:" RESET;
            else
                lead = "warning:";
            break;
            
        case ERR:
            if (ENV.is_color_term)
                lead = RED "error:" RESET;
            else
                lead = "error:";
            break;
            
        case FTL:
            if (ENV.is_color_term)
                lead = RED "fatal:" RESET;
            else
                lead = "fatal:";
            break;
            
        default:
            assert(0);
    }

    if (ENV.is_color_term) {
        fprintf(stderr, CLEAR "%s:%u:" RESET " %s ", file, line, lead);
        fprintf(stderr, CLEAR);
        vfprintf(stderr, fmt, ap);
        fprintf(stderr, RESET);
    } else {
        fprintf(stderr, "%s:%u: %s ", file, line, lead);
        vfprintf(stderr, fmt, ap);
    }
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

void redefinition_error(struct source src, struct symbol *sym)
{
    errorf(src, "redefinition of '%s', previous definition at %s line %u",
           sym->name, sym->src.file, sym->src.line);
}

void conflicting_types_error(struct source src, struct symbol *sym)
{
    errorf(src, "conflicting types for '%s', previous at %s line %u",
           sym->name, sym->src.file, sym->src.line);
}

void incompatible_types_error(struct type *ty1, struct type *ty2)
{
    error("imcompatible type: '%s' and '%s'", type2s(ty1), type2s(ty2));
}
