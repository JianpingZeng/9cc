#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include "lib.h"

int errcnt;
int warncnt;

#define MAX_ERRORS	1

void printreport()
{
    fprintf(stderr, "=== %d errors, %d warnings ===\n", errcnt, warncnt);
}

void fatal(const char *fmt, ...)
{
    fprintf(stderr, "fatal error: ");
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fprintf(stderr, "\n");
    exit(EXIT_FAILURE);
}

void errorl(const char *file, int line, int lineno, const char *fmt, ...)
{
	if (file) {
		fprintf(stderr, "%s:%d:", file, line); 
	}
    fprintf(stderr, "[Line %d] error: ", lineno);
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fprintf(stderr, "\n");
	if (++errcnt >= MAX_ERRORS) {
        printreport();
		exit(EXIT_FAILURE);
	}
}

void warningl(const char *file, int line, int lineno, const char *fmt, ...)
{
	if (file) {
		fprintf(stderr, "%s:%d:", file, line);
	}
    fprintf(stderr, "[Line %d] warning: ", lineno);
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fprintf(stderr, "\n");
	++warncnt;
}

static int call_depth = -1;
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
    printf("%4d%sbegin %s\n", call_depth, depth_str(), funcname);
}

void end_call(const char *funcname)
{
    printf("%4d%send %s\n", call_depth, depth_str(), funcname);
    call_depth--;
}

