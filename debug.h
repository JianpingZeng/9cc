#ifndef DEBUG_H
#define DEBUG_H

#ifdef NDEBUG
#define D(x)  ((void)0)
#else
#define D(x)  x
#endif

extern void debug_init(int, char *[]);
extern void debug_exit(void);

extern char debug[128];

#endif /* DEBUG_H */
