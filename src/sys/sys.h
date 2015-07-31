#ifndef _etc_h
#define _etc_h

// path
extern char *mk_temp_dir();
extern int file_exists(const char *path);
extern int is_directory(const char *path);
extern int rmdir(const char *dir);
extern char *expanduser(const char *path);

extern int callsys(const char *path, char **argv);
extern char *replace_suffix(const char *path, const char *suffix);
extern void cc_sleep(int seconds);

#endif
