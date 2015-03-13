#include "cc.h"

unsigned errors;
unsigned warnings;

#define MAX_ERRORS 32

static const char * basename(const char *file)
{
    int i;
    for (i=strlen(file)-1; i >= 0; i--) {
	if (file[i] == '/') 
	    break;
    }
    return file+(i+1);
}

static void cc_print_lead(const char *lead, const char *file, unsigned line, const char *fmt, va_list ap)
{
    fprintf(stderr, "%s:%u: %s: ", basename(file), line, lead);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
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
	fprintf(stderr, "Too many errors.\n");
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
	fprintf(stderr, "Too many errors.\n");
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

void cclog(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    va_end(ap);
}

void die(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
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
