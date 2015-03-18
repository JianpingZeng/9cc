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
#ifdef SHOW_COLOR_TERM
            lead = "\e[1;35mwarning:\e[0m";
#else
            lead = "warning:";
#endif
            break;
            
        case ERR:
#ifdef SHOW_COLOR_TERM
            lead = "\e[1;31merror:\e[0m";
#else
            lead = "error:";
#endif
            break;
            
        case FTL:
#ifdef SHOW_COLOR_TERM
            lead = "\e[1;31mfatal:\e[0m";
#else
            lead = "fatal:";
#endif
            break;
            
        default:
            assert(0);
    }
#ifdef SHOW_COLOR_TERM
    fprintf(stderr, "\e[1;38m%s:%u:\e[0m %s ", file, line, lead);
    fprintf(stderr, "\e[1;38m");
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\e[0m");
#else
    fprintf(stderr, "%s:%u: %s ", file, line, lead);
    vfprintf(stderr, fmt, ap);
#endif
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
