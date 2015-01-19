#ifndef cpp_cpp_h
#define cpp_cpp_h

extern const char *cpp[];
extern int callsys(const char *path, const char *argv[]);
extern const char *mk_tmp_dir();
extern int file_exists(const char *path);

#endif
