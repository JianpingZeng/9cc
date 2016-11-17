#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include "libutils/color.h"
#include "cc.h"

enum { WRN = 1, ERR, FTL, ITL };
static unsigned int cc_errors, cc_warnings;

#define MAX_ERRORS 10

unsigned int errors(void)
{
    return cc_errors + cpp_file->errors;
}

unsigned int warnings(void)
{
    return cc_warnings + cpp_file->warnings;
}

static void exit_if_too_many_errors(void)
{
    if (errors() >= MAX_ERRORS) {
        fprint(stderr, "Too many errors.\n");
        exit(EXIT_FAILURE);
    }
}

static void
cc_print_lead(int level, struct source src, const char *fmt, va_list ap)
{
    const char *lead;
    switch (level) {
    case WRN:
        lead = PURPLE_BOLD("warning:");
        break;
    case ERR:
        lead = RED_BOLD("error:");
        break;
    case FTL:
        lead = RED_BOLD("fatal:");
        break;
    case ITL:
        lead = RED_BOLD("internal:");
        break;
    default:
        assert(0);
    }

    fprint(stderr, CLEAR "%S:" RESET " %s ", src, lead);
    fprint(stderr, CLEAR);
    vfprint(stderr, fmt, ap);
    fprint(stderr, RESET);
    fprint(stderr, "\n");
}

void warning_at(struct source src, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    cc_print_lead(WRN, src, fmt, ap);
    va_end(ap);
    cc_warnings++;
    if (opts.Werror) {
        cc_errors++;
        exit_if_too_many_errors();
    }
}

void error_at(struct source src, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    cc_print_lead(ERR, src, fmt, ap);
    va_end(ap);
    cc_errors++;
    exit_if_too_many_errors();
}

void fatal_at(struct source src, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    cc_print_lead(FTL, src, fmt, ap);
    va_end(ap);
    exit(EXIT_FAILURE);
}

void intal_at(struct source src, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    cc_print_lead(ITL, src, fmt, ap);
    va_end(ap);
    cc_errors++;
}
