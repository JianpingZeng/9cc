#ifndef _STRBUF_H
#define _STRBUF_H

struct strbuf {
    char *str;
    size_t len;
    size_t alloc;
};

extern struct strbuf *strbuf_new(void);

extern void strbuf_free(struct strbuf *s);

extern size_t strbuf_len(struct strbuf *s);

extern const char *strbuf_str(struct strbuf *s);

extern void strbuf_add(struct strbuf *s, struct strbuf *s2);

extern void strbuf_cats(struct strbuf *s, const char *src);

extern void strbuf_catn(struct strbuf *s, const char *src, size_t len);

extern void strbuf_catd(struct strbuf *s, long d);

extern void strbuf_catc(struct strbuf *s, char c);

extern struct strbuf *strbuf_lstrip(struct strbuf *s);

extern struct strbuf *strbuf_rstrip(struct strbuf *s);

extern struct strbuf *strbuf_strip(struct strbuf *s);

#endif
