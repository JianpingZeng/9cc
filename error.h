#ifndef _ERROR_H
#define _ERROR_H

enum { WRN = 1, ERR, FTL };

extern unsigned int errors;
extern unsigned int warnings;
extern void warningf(const char *file, unsigned int line, unsigned int column,
                     const char *fmt, ...);
extern void errorf(const char *file, unsigned int line, unsigned int column,
                   const char *fmt, ...);
extern void fatalf(const char *file, unsigned int line, unsigned int column,
                   const char *fmt, ...);

#define SAVE_ERRORS    unsigned int __err = errors
#define NO_ERROR       (__err == errors)
#define HAS_ERROR      (__err != errors)

#define warning_at(s, ...)   warningf(s.file, s.line, s.column, __VA_ARGS__)
#define error_at(s, ...)     errorf(s.file, s.line, s.column, __VA_ARGS__)
#define fatal_at(s, ...)     fatalf(s.file, s.line, s.column, __VA_ARGS__)
#define warning(...)         warning_at(source, __VA_ARGS__)
#define error(...)           error_at(source, __VA_ARGS__)
#define fatal(...)           fatal_at(source, __VA_ARGS__)

#endif
