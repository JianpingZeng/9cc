// Copyright 2012 Rui Ueyama. Released under the MIT license.
// Copyright 2015 Guiyang Huang. Released under the MIT license.

#include <stdbool.h>

extern void ffail(char *file, int line, char *msg, ...);
extern void fexpectb(char *file, int line, bool a);
extern void fexpecti(char *file, int line, int a, int b);
extern void fexpectl(char *file, int line, long a, long b);
extern void fexpects(char *file, int line, const char *a, const char *b);
extern void fexpectf(char *file, int line, float a, float b);
extern void fexpectd(char *file, int line, double a, double b);
extern void fexpectp(char *file, int line, void *a, void *b);

#define fail(...)       ffail(__FILE__, __LINE__, __VA_ARGS__)
#define expectb(expr)   fexpectb(__FILE__, __LINE__, expr)
#define expecti(a, b)   fexpecti(__FILE__, __LINE__, a, b)
#define expectl(a, b)   fexpectl(__FILE__, __LINE__, a, b)
#define expects(a, b)   fexpects(__FILE__, __LINE__, a, b)
#define expectf(a, b)   fexpectf(__FILE__, __LINE__, a, b)
#define expectd(a, b)   fexpectd(__FILE__, __LINE__, a, b)
#define expectp(a, b)   fexpectp(__FILE__, __LINE__, a, b)
