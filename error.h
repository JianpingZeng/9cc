#ifndef _ERROR_H
#define _ERROR_H

enum { WRN = 1, ERR, FTL };

extern unsigned int errors;
extern unsigned int warnings;
extern void err_warning(const char *file, unsigned int line, unsigned int column,
                        const char *fmt, ...);
extern void err_error(const char *file, unsigned int line, unsigned int column,
                      const char *fmt, ...);
extern void err_fatal(const char *file, unsigned int line, unsigned int column,
                      const char *fmt, ...);

#define SAVE_ERRORS    unsigned int __err = errors
#define NO_ERROR       (__err == errors)
#define HAS_ERROR      (__err != errors)

#define warningf(s, ...)   err_warning(s.file, s.line, s.column, __VA_ARGS__)
#define errorf(s, ...)     err_error(s.file, s.line, s.column, __VA_ARGS__)
#define fatalf(s, ...)     err_fatal(s.file, s.line, s.column, __VA_ARGS__)
#define warning(...)       warningf(source, __VA_ARGS__)
#define error(...)         errorf(source, __VA_ARGS__)
#define fatal(...)         fatalf(source, __VA_ARGS__)

#endif
