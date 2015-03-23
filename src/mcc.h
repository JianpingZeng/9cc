#ifndef _MCC_H
#define _MCC_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <locale.h>
#include <stdarg.h>

extern int cpp_main(int argc, char **argv);
extern int cc_main(int argc, char **argv);

// path
extern char *mk_temp_dir();
extern int file_exists(const char *path);
extern int is_directory(const char *path);
extern int rmdir(const char *dir);
extern char *expanduser(const char *path);

extern int callsys(const char *path, char **argv);
extern char *replace_suffix(const char *path, const char *suffix);
extern void cc_sleep(int seconds);

#include "lib.h"

#endif
