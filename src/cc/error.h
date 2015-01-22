#ifndef cc_error_h
#define cc_error_h

extern unsigned errors;
extern unsigned warnings;
extern void warning(const char *fmt, ...);
extern void error(const char *fmt, ...);

#ifdef GNU_EXTENSION

extern void dolog(const char *file, unsigned line,  const char *fmt, ...);
#define log(fmt, ...)  dolog(__FILE__, __LINE__, fmt, ##__VA_ARGS__)

#else

extern void log(const char *fmt, ...);

#endif
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
