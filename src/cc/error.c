#include "cc.h"

unsigned errors;
unsigned warnings;

#define MAX_ERRORS 32

static void cc_print_lead(const char *lead, const char *file, unsigned line, const char *fmt, va_list ap)
{
    fprint(stderr, "%s:%u: %s: ", file, line, lead);
    vfprint(stderr, fmt, ap);
    fprint(stderr, "\n");
}

void warningf(struct source src, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    cc_print_lead("warning", src.file, src.line, fmt, ap);
    va_end(ap);
    ++warnings;
}

void warning(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    cc_print_lead("warning", source.file, source.line, fmt, ap);
    va_end(ap);
    ++warnings;
}

void errorf(struct source src, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    cc_print_lead("error", src.file, src.line, fmt, ap);
    va_end(ap);
    ++errors;
    if (errors >= MAX_ERRORS) {
	fprint(stderr, "Too many errors.\n");
	exit(EXIT_FAILURE);
    }
}

void error(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    cc_print_lead("error", source.file, source.line, fmt, ap);
    va_end(ap);
    ++errors;
    if (errors >= MAX_ERRORS) {
	fprint(stderr, "Too many errors.\n");
	exit(EXIT_FAILURE);
    }
}

void fatal(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    cc_print_lead("fatal", source.file, source.line, fmt, ap);
    va_end(ap);
    exit(EXIT_FAILURE);
}

static void logv(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vfprint(stderr, fmt, ap);
    fprint(stderr, "\n");
    va_end(ap);
}

static void logi(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vfprint(stdout, fmt, ap);
    fprint(stdout, "\n");
    va_end(ap);
}

struct log Log = { logv, logv, logi };

static long call_depth = -1;

void begin_call(const char *funcname)
{
    call_depth++;
    for (int i=0; i < call_depth; i++)
	fprint(stderr, " ");
    fprint(stderr, "begin %s\n", funcname);
}

void end_call(const char *funcname)
{
    for (int i=0; i < call_depth; i++)
	fprint(stderr, " ");
    fprint(stderr, "end %s\n", funcname);
    call_depth--;
}

