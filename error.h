#ifndef _ERROR_H
#define _ERROR_H

enum { WRN = 1, ERR, FTL };

extern unsigned int errors;
extern unsigned int warnings;
extern void warning(const char *file, unsigned int line, unsigned int column,
                     const char *fmt, ...);
extern void error(const char *file, unsigned int line, unsigned int column,
                  const char *fmt, ...);
extern void fatal(const char *file, unsigned int line, unsigned int column,
                  const char *fmt, ...);

#define SAVE_ERRORS    unsigned int __err = errors
#define NO_ERROR       (__err == errors)
#define HAS_ERROR      (__err != errors)

#endif
