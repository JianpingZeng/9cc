#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include "color.h"
#include "lex.h"
#include "internal.h"

static void
cc_print_lead(int tag, struct source src, const char *fmt, va_list ap)
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

    fprintf(stderr, CLEAR "%s:%u:%u:" RESET " %s ",
            src.file, src.line, src.column, lead);
    fprintf(stderr, CLEAR);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, RESET);
    fprintf(stderr, "\n");
}

void cpp_warning_at(struct source src, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    cc_print_lead(WRN, src, fmt, ap);
    va_end(ap);
    cpp_file->warnings++;
}

void cpp_error_at(struct source src, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    cc_print_lead(ERR, src, fmt, ap);
    va_end(ap);
    cpp_file->errors++;
}

void cpp_fatal_at(struct source src, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    cc_print_lead(FTL, src, fmt, ap);
    va_end(ap);
    exit(EXIT_FAILURE);
}
