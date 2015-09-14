#ifndef _SYS_H
#define _SYS_H

extern void setup_sys();

// path
extern const char *mktmpdir();
extern int file_exists(const char *path);
extern int isdir(const char *path);
extern int rmdir(const char *dir);
extern const char *abspath(const char *path);
extern const char *replace_suffix(const char *path, const char *suffix);
extern const char *join(const char *dir, const char *name);

// process
extern int callsys(const char *path, char **argv);
extern int runproc(int (*proc) (void *), void *context);

// terminal
extern int is_color_term();

#endif
