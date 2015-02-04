#include "cc.h"

static const char * (*print_funcs[128]) (void *);

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

    if (l < 0)
	*--s = '-';

    cc_fputs(f, s);
}

static void cc_uint(FILE *f, unsigned long l, int base, int uppercase)
{
    char buf[32];
    char *s = &buf[sizeof buf];
    
    *--s = 0;
    do {
	if (uppercase)
	    *--s = "0123456789ABCDEF"[l%base];
	else
	    *--s = "0123456789abcdef"[l%base];
    } while ((l/=base));

    cc_fputs(f, s);
}

static void cc_uint_ex(FILE *f, unsigned long l, int base, int uppercase, char lead, unsigned width)
{
    if (width == 0) {
	return cc_uint(f, l, base, uppercase);
    }
    else {
	char buf[64];
	char *s = &buf[sizeof buf];
	char *e = s-1;
    
	*--s = 0;
	do {
	    if (uppercase)
		*--s = "0123456789ABCDEF"[l%base];
	    else
		*--s = "0123456789abcdef"[l%base];
	} while ((l/=base));
	
	if (e-s >= width) {
	    cc_fputs(f, s);
	}
	else if (width <= (sizeof buf - 1)){
	    unsigned left = width - (e-s);
	    while(left--)
		*--s = lead;
	    cc_fputs(f, s);
	}
	else {
	    char *p = malloc(width+1);
	    char *pe = p+width;
	    char *ps = pe - (e-s);
	    unsigned left = width - (e-s);
	    assert(p);
	    memcpy(ps, s, e-s+1);
	    while (left--)
		*--ps = lead;
	    cc_fputs(f, ps);
	    free(p);
	}
    }
}

void register_print_function(char c, const char * (*g) (void *data))
{
    static char reserved[] = {'d', 'i', 'u', 'l', 'f', 'e',
			      'E', 'g', 'G', 'o', 'x', 'X',
			      's', 'p', 'c', '%', 'S'};
    
    if (c >= '0' && c <= '9')
	die("Can't register print function for '%c'", c);
    
    for (int i=0; i < ARRAY_SIZE(reserved); i++) {
	if (c == reserved[i])
	    die("Can't register print function for '%c'", c);
    }

    print_funcs[(int)c] = g;
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
		if (fmt[0] == 'l' && fmt[1] == 'd') {
		    cc_int(f, va_arg(ap, long long));
		    fmt++;
		}
		else if (fmt[0] == 'l' && fmt[1] == 'u') {
		    cc_uint(f, va_arg(ap, unsigned long long), 10, 0);
		    fmt++;
		}
		else if (fmt[0] == 'l' && (fmt[1] == 'x' || fmt[1] == 'X')) {
		    cc_uint(f, va_arg(ap, unsigned long long), 16, fmt[1] == 'x' ? 0 : 1);
		    fmt++;
		}
		else if (fmt[0] == 'l' && fmt[1] == 'o') {
		    cc_uint(f, va_arg(ap, unsigned long long), 8, 0);
		    fmt++;
		}
		else if (*fmt == 'd') {
		    cc_int(f, va_arg(ap, long));
		}
		else if (*fmt == 'u') {
		    cc_uint(f, va_arg(ap, unsigned long), 10, 0);
		}
		else if (*fmt == 'x' || *fmt == 'X') {
		    cc_uint(f, va_arg(ap, unsigned long), 16, *fmt == 'x' ? 0 : 1);
		}
		else if (*fmt == 'o') {
		    cc_uint(f, va_arg(ap, unsigned long), 8, 0);
		}
		else {
		    cc_fputc(f, 'l');
		    cc_fputc(f, *fmt);
		}
		break;
	    case 'L':
		++fmt;
		if (*fmt == 'f') {
		    fprintf(f, "%Lf", va_arg(ap, long double));
		}
		else {
		    cc_fputc(f, 'L');
		    cc_fputc(f, *fmt);
		}
		break;
	    case 'f':
	    case 'g':case 'G':
	    case 'e':case 'E':
		{
		    char format[3] = "%?";
		    format[1] = *fmt;
		    fprintf(f, format, va_arg(ap, double));
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
	    case 'S':
		{
		    char *str = va_arg(ap, char *);
		    int len = va_arg(ap, int);
		    while (len-- > 0)
			cc_fputc(f, *str++);
		}
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
	    case '0':case '1':case'2':case '3':case '4':
	    case '5':case '6':case '7':case '8':case '9':
		{
		    char lead = 32; // white space
		    const char *saved = fmt;
		    if (*fmt == '0') {
			lead = '0';
			++fmt;
		    }
		    unsigned len = 0;
		    while (*fmt >= '0' && *fmt <= '9') {
			len = len * 10 + (*fmt - '0');
			++fmt;
		    }
		    if (*fmt == 'x') {
			cc_uint_ex(f, va_arg(ap, unsigned), 16, 0, lead, len);
		    }
		    else if (*fmt == 'X') {
			cc_uint_ex(f, va_arg(ap, unsigned), 16, 1, lead, len);
		    }
		    else if (*fmt == 'o') {
			cc_uint_ex(f, va_arg(ap, unsigned), 8, 0, lead, len);
		    }
		    else {
			// undefined
			while (saved <= fmt) {
			    cc_fputc(f, *saved);
			    saved++;
			}
		    }
		}
		break;
	    default:
		// search for register formats
		if (print_funcs[(int)*fmt]) {
		    const char * (*p) (void *) = print_funcs[(int)*fmt];
		    const char *str = p(va_arg(ap, void*));
		    cc_fputs(f, str);
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

void die(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vfprint(stderr, fmt, ap);
    fprint(stderr, "\n");
    va_end(ap);
    exit(EXIT_FAILURE);
}
