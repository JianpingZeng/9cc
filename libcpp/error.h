#ifndef _CPP_ERROR_H
#define _CPP_ERROR_H

enum { WRN = 1, ERR, FTL };

extern unsigned int cpp_errors;
extern void cpp_warningf(const char *file, unsigned int line, unsigned int column,
                         const char *fmt, ...);
extern void cpp_errorf(const char *file, unsigned int line, unsigned int column,
                       const char *fmt, ...);
extern void cpp_fatalf(const char *file, unsigned int line, unsigned int column,
                       const char *fmt, ...);

#define SAVE_ERRORS    unsigned int __err = cpp_errors
#define NO_ERROR       (__err == cpp_errors)
#define HAS_ERROR      (__err != cpp_errors)

#define cpp_warning_at(s, ...)   cpp_warningf(s.file, s.line, s.column, __VA_ARGS__)
#define cpp_error_at(s, ...)     cpp_errorf(s.file, s.line, s.column, __VA_ARGS__)
#define cpp_fatal_at(s, ...)     cpp_fatalf(s.file, s.line, s.column, __VA_ARGS__)
#define cpp_warning(...)         cpp_warning_at(source, __VA_ARGS__)
#define cpp_error(...)           cpp_error_at(source, __VA_ARGS__)
#define cpp_fatal(...)           cpp_fatal_at(source, __VA_ARGS__)

#endif
