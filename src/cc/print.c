#include <stdio.h>
#include <stdarg.h>

static void cc_fputs(FILE *f, const char *s)
{
    fputs(s, f);
}

static void cc_fputc(FILE *f, int c)
{
    fputc(c, f);
}

void vfprint(FILE *f, const char *fmt, va_list ap)
{
    for (; *fmt; fmt++) {
	if (*fmt == '%') {

	}
	else {
	    cc_fputc(f, *fmt);
	}
    }
}

void print(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vfprint(stdout, fmt, ap);
    va_end(ap);
}

void fprint(FILE *f, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vfprint(f, fmt, ap);
    va_end(ap);
}


