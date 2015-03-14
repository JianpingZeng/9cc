#ifndef MCC_ETC_H
#define MCC_ETC_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

extern const char *cpp[];
extern int cpp_main(int argc, char **argv);

extern char *mk_temp_dir();
extern int file_exists(const char *path);
extern int callsys(const char *path, char **argv);
extern char *replace_suffix(const char *path, const char *suffix);
extern int rmdir(const char *dir);
extern char *expanduser(char *path);

#include "lib.h"

#endif
