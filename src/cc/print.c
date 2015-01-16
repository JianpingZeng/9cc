#include <stdio.h>
#include <stdarg.h>

typedef const char * (*PrintFunc) (void *data);

static void cc_fputs(FILE *f, const char *s)
{
    fputs(s, f);
}

static void cc_fputc(FILE *f, int c)
{
    fputc(c, f);
}

static void cc_int(FILE *f, long l)
{
    char buf[32];
    char *s = &buf[sizeof buf];
    long m = l >= 0 ? l : -l;

    *--s = 0;
    do {
	*--s = m%10 + '0';
    } while ((m/=10));

    if (l < 0) {
	*--s = '-';
    }

    cc_fputs(f, s);
}

static void cc_uint(FILE *f, unsigned long l, int base, int uppercase)
{
    char buf[32];
    char *s = &buf[sizeof buf];
    
    *--s = 0;
    do {
	if (uppercase) {
	    *--s = "0123456789ABCDEF"[l%base];
	}
	else {
	    *--s = "0123456789abcdef"[l%base];
	}
    } while ((l/=base));

    cc_fputs(f, s);
}

int register_print_function(char c, PrintFunc p)
{
    static char reserved[] = {'d', 'i', 'u', 'l', 'f', 'e',
			      'E', 'g', 'G', 'o', 'x', 'X',
			      's', 'p', 'c', '%'};
    return -1;
}

void unregister_print_function(char c)
{

}

int is_format_registered(char c)
{
    return 0;
}

void vfprint(FILE *f, const char *fmt, va_list ap)
{
    for (; *fmt; fmt++) {
	if (*fmt == '%') {
	    switch(*++fmt) {
	    case 'd':
	    case 'i':
		cc_int(f, va_arg(ap, int));
		break;
	    case 'u':
		cc_uint(f, va_arg(ap, unsigned), 10, 0);
		break;
	    case 'l':
		++fmt;
		if (*fmt == 'd') {
		    cc_int(f, va_arg(ap, long));
		}
		else if (*fmt == 'u') {
		    cc_uint(f, va_arg(ap, unsigned long), 10, 0);
		}
		else {
		    cc_fputc(f, 'l');
		    cc_fputc(f, *fmt);
		}
		break;
	    case 'f':
	    case 'g':case 'G':
	    case 'e':case 'E':
		{
		    char buf[128];
		    char format[] = "%?";
		    format[1] = *fmt;
		    sprintf(buf, format, va_arg(ap, double));
		    cc_fputs(f, buf);
		}
		break;
	    case 'o':
		cc_uint(f, va_arg(ap, unsigned), 8, 0);
		break;
	    case 'x':
		cc_uint(f, va_arg(ap, unsigned), 16, 0);
		break;
	    case 'X':
		cc_uint(f, va_arg(ap, unsigned), 16, 1);
		break;
	    case 's':
		cc_fputs(f, va_arg(ap, char *));
		break;
	    case 'p':
		{
		    void *p = va_arg(ap, void *);
		    if (p) {
			cc_fputs(f, "0x");
			cc_uint(f, (unsigned long)p, 16, 0);
		    }
		    else {
			cc_fputs(f, "(null)");
		    }
		}
		break;
	    case 'c':
		cc_fputc(f, va_arg(ap, int));
		break;
	    default:
		// search for register formats
		if (is_format_registered(*fmt)) {

		}
		else {
		    cc_fputc(f, *fmt);
		}
		break;
	    }
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


