#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include "../config.h"
#include "../color.h"
#include "error.h"

unsigned int cpp_errors;

#define MAX_ERRORS 8

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

void cpp_warningf(const char *file, unsigned int line, unsigned int column,
                  const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    cc_print_lead(WRN, file, line, column, fmt, ap);
    va_end(ap);
}

void cpp_errorf(const char *file, unsigned int line, unsigned int column,
                const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    cc_print_lead(ERR, file, line, column, fmt, ap);
    va_end(ap);
    ++cpp_errors;
    if (cpp_errors >= MAX_ERRORS) {
        fprintf(stderr, "Too many errors.\n");
        exit(EXIT_FAILURE);
    }
}

void cpp_fatalf(const char *file, unsigned int line, unsigned int column,
                const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    cc_print_lead(FTL, file, line, column, fmt, ap);
    va_end(ap);
    exit(EXIT_FAILURE);
}
