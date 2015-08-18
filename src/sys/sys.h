#ifndef _SYS_H
#define _SYS_H

// path
extern char *mk_temp_dir();
extern int file_exists(const char *path);
extern int is_directory(const char *path);
extern int rmdir(const char *dir);
extern char *expanduser(const char *path);

extern int callsys(const char *path, char **argv);
extern int runproc(int (*proc) (void *), void *context);
extern char *replace_suffix(const char *path, const char *suffix);
extern void cc_sleep(int seconds);
extern int get_pid(void);
extern int get_ppid(void);

#endif
