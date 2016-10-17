#ifndef SYS_H
#define SYS_H

// setup
extern void sys_setup(void);

// path
extern char *sys_dirname(const char *path);
extern char *sys_basename(const char *path);
extern const char *sys_mktmpdir();
extern int sys_rmdir(const char *dir);
extern const char *sys_abspath(const char *path);
extern const char *sys_join(const char *dir, const char *name);

// file
extern int file_exists(const char *path);
extern long file_size(const char *path);
extern const char *file_suffix(const char *path);
extern const char *replace_suffix(const char *path, const char *suffix);

// process
extern int sys_call(const char *file, char **argv);

// include
extern struct vector *sys_include_dirs(void);

extern char *ld[];
extern char *as[];
extern char *cc[];

#define BUILTIN_HEADER "7cc.h"

#endif
