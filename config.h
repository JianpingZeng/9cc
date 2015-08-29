#ifndef _CONFIG_H
#define _CONFIG_H

#define SHOW_CALL_TREE
#ifdef SHOW_CALL_TREE
#define BEGIN_CALL    begin_call(__func__);
#define END_CALL      end_call(__func__);
#else
#define BEGIN_CALL
#define END_CALL
#endif

struct {
    unsigned is_color_term : 1;
} ENV;

#endif