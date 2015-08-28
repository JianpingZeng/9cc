#ifndef _STRING_H
#define _STRING_H

struct string {
    char    *str;
    size_t  len;
    size_t  alloc;
};

extern struct string * new_string();

extern void free_string(struct string *s);

extern size_t str_len(struct string *s);

extern void str_add(struct string *s, struct string *s2);

extern void str_cats(struct string *s, const char *src);

extern void str_catn(struct string *s, const char *src, size_t len);

extern void str_catd(struct string *s, long d);

extern void str_lstrip(struct string *s);

extern void str_rstrip(struct string *s);

extern void str_strip(struct string *s);

// key-value storage

extern char *strs(const char *str);

extern char *strn(const char *src, size_t len);

extern char *strd(long n);

extern char * stoa(struct string *s);

extern char *format(const char *fmt, ...);

#endif