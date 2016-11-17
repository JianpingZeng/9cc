/*
  This file imports the system dependent routines.
 */

#ifndef _BSD_SOURCE
#define _BSD_SOURCE
#endif

// strdup, strndup
#ifndef _DEFAULT_SOURCE
#define _DEFAULT_SOURCE
#endif

#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
/**
 * NOTE!!!
 * The 'dirname()' manual page says:
 * Both dirname() and basename() may modify
 * the contents of path, so it may be desirable
 * to pass a copy when calling one of these functions.
 */
// dirname, basename
#include <libgen.h>
// uname
#include <sys/utsname.h>
