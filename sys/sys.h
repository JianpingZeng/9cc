#ifndef _SYS_H
#define _SYS_H

// path
extern char *mktmpdir();
extern int file_exists(const char *path);
extern int isdir(const char *path);
extern int rmdir(const char *dir);
extern char *expanduser(const char *path);

extern int callsys(const char *path, char **argv);
extern int runproc(int (*proc) (void *), void *context);
extern char *replace_suffix(const char *path, const char *suffix);
extern void cc_sleep(int seconds);

extern int is_color_term();

#endif
