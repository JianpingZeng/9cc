#ifndef _SYS_H
#define _SYS_H

// setup
extern void setup_sys();

// path
extern const char *mktmpdir();
extern int file_exists(const char *path);
extern int file_size(const char *path);
extern int isdir(const char *path);
extern int rmdir(const char *dir);
extern const char *abspath(const char *path);
extern const char *replace_suffix(const char *path, const char *suffix);
extern const char *join(const char *dir, const char *name);
extern char *dirname(const char *path);
extern char *basename(const char *path);
extern const char *file_suffix(const char *path);

// process
extern int callsys(const char *file, char **argv);

// time
extern void set_localtime(const time_t * timep, struct tm *result);

// include
extern struct vector *sys_include_dirs(void);

extern char *ld[];
extern char *as[];
extern char *cc[];

#define BUILTIN_HEADER "7cc.h"

#endif
