#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include "config.h"
#include "libutils/color.h"
#include "cc.h"

enum { WRN = 1, ERR, FTL };
static unsigned int cc_errors, cc_warnings;

#define MAX_ERRORS 32

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
        fprintf(stderr, "Too many errors.\n");
        exit(EXIT_FAILURE);
    }
}

static void cc_print_lead(int level, struct source src, const char *fmt, va_list ap)
{
    const char *lead;
    switch (level) {
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
        assert(0);
    }

    fprintf(stderr, CLEAR "%s:%u:%u:" RESET " %s ",
            src.file, src.line, src.column, lead);
    fprintf(stderr, CLEAR);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, RESET);
    fprintf(stderr, "\n");
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
