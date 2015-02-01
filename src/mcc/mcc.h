#ifndef driver_mcc_h
#define driver_mcc_h

extern char *mk_temp_dir();
extern int file_exists(const char *path);
extern int callsys(const char *path, char *argv[]);
extern const char *replace_suffix(const char *path, const char *suffix);

extern int rmdir(const char *dir);
extern const char *abspath(const char *path);

#include "lib.h"

#endif
