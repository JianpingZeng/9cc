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

void warning(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    cc_print_lead("warning", src.file, src.line, fmt, ap);
    va_end(ap);
    ++warnings;
}

void error(const char *fmt, ...)
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

void fatal(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    cc_print_lead("fatal", src.file, src.line, fmt, ap);
    va_end(ap);
    exit(EXIT_FAILURE);
}

void cclog(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vfprint(stderr, fmt, ap);
    fprint(stderr, "\n");
    va_end(ap);
}

static long call_depth = -1;
static const char *depth_str()
{
    char *str = (char *) allocate(call_depth+1, 0);
    str[call_depth] = 0;
    memset(str, '|', call_depth);
    return str;
}

void begin_call(const char *funcname)
{
    call_depth++;
    print("%4d%sbegin %s\n", call_depth, depth_str(), funcname);
}

void end_call(const char *funcname)
{
    print("%4d%send %s\n", call_depth, depth_str(), funcname);
    call_depth--;
}

