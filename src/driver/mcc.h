#ifndef driver_mcc_h
#define driver_mcc_h

extern char *mk_temp_dir();
extern int file_exists(const char *path);
extern int callsys(const char *path, char *argv[]);

#endif
