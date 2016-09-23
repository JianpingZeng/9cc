#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include "config.h"
#include "libutils/color.h"
#include "cc.h"

unsigned int errors, warnings;

#define MAX_ERRORS 32

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

void warningf(struct source src, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    cc_print_lead(WRN, src, fmt, ap);
    va_end(ap);
    warnings++;
}

void errorf(struct source src, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    cc_print_lead(ERR, src, fmt, ap);
    va_end(ap);
    if (errors++ >= MAX_ERRORS) {
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
