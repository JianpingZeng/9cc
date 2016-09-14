#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include "config.h"
#include "utils/color.h"
#include "cc.h"

unsigned int errors;
unsigned int warnings;

#define MAX_ERRORS 32

static void cc_print_lead(int tag,
                          const char *file, unsigned int line, unsigned int column,
                          const char *fmt, va_list ap)
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
        assert(0);
    }

    fprintf(stderr, CLEAR "%s:%u:%u:" RESET " %s ", file, line,
            column, lead);
    fprintf(stderr, CLEAR);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, RESET);
    fprintf(stderr, "\n");
}

void warningf(const char *file, unsigned int line, unsigned int column,
              const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    cc_print_lead(WRN, file, line, column, fmt, ap);
    va_end(ap);
    ++warnings;
}

void errorf(const char *file, unsigned int line, unsigned int column,
            const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    cc_print_lead(ERR, file, line, column, fmt, ap);
    va_end(ap);
    ++errors;
    if (errors >= MAX_ERRORS) {
        fprintf(stderr, "Too many errors.\n");
        exit(EXIT_FAILURE);
    }
}

void fatalf(const char *file, unsigned int line, unsigned int column,
            const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    cc_print_lead(FTL, file, line, column, fmt, ap);
    va_end(ap);
    exit(EXIT_FAILURE);
}
