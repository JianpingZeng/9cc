#ifndef cc_error_h
#define cc_error_h

extern unsigned errors;
extern unsigned warnings;
extern void warning(const char *fmt, ...);
extern void error(const char *fmt, ...);
extern void fatal(const char *fmt, ...);
extern void warningf(unsigned line, const char *fmt, ...);
extern void errorf(unsigned line, const char *fmt, ...);

struct {
    void (*v) (const char *fmt, ...);
    void (*d) (const char *fmt, ...);
    void (*i) (const char *fmt, ...);
} Log;
extern void init_logger();

extern void begin_call(const char *funcname);
extern void end_call(const char *funcname);

#ifdef SHOW_CALL_TREE
#define BEGIN_CALL(funcname)    begin_call(#funcname)
#define END_CALL(funcname)      end_call(#funcname)
#else
#define BEGIN_CALL(funcname)
#define END_CALL(funcname)
#endif

#endif
