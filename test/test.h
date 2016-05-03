// Copyright 2015 Guiyang Huang. Released under the MIT license.

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

extern void ffail(char *file, int line, char *msg, ...);
extern void fexpectb(char *file, int line, char *msg, bool b);
extern void fexpecti(char *file, int line, char *msg, long a, long b);
extern void fexpectu(char *file, int line, char *msg, unsigned long a, unsigned long b);
extern void fexpects(char *file, int line, char *msg, const char *a, const char *b);
extern void fexpectf(char *file, int line, char *msg, float a, float b);
extern void fexpectd(char *file, int line, char *msg, double a, double b);
extern void fexpectp(char *file, int line, char *msg, void *a, void *b);

#define fail(...)          ffail(__FILE__, __LINE__, __VA_ARGS__)
#define assert_true(a)     fexpectb(__FILE__, __LINE__, #a, a)      
#define assert_false(a)    fexpectb(__FILE__, __LINE__, #a, !(a))
#define assert_eqi(a, b)   fexpecti(__FILE__, __LINE__, #a, a, b)
#define assert_equ(a, b)   fexpectu(__FILE__, __LINE__, #a, a, b)
#define assert_eqs(a, b)   fexpects(__FILE__, __LINE__, #a, a, b)
#define assert_eqf(a, b)   fexpectf(__FILE__, __LINE__, #a, a, b)
#define assert_eqd(a, b)   fexpectd(__FILE__, __LINE__, #a, a, b)
#define assert_eqp(a, b)   fexpectp(__FILE__, __LINE__, #a, a, b)
