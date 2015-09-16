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

#ifdef COLOR_TERM

#define RESET    "\e[0m"
#define CLEAR    "\e[1;38m"

#define BLACK    "\e[0;30m"
#define RED      "\e[0;31m"
#define GREEN    "\e[0;32m"
#define YELLOW   "\e[0;33m"
#define BLUE     "\e[0;34m"
#define PURPLE   "\e[0;35m"
#define CYAN     "\e[0;36m"
#define WHITE    "\e[0;37m"

#else

#define RESET    ""
#define CLEAR    ""

#define BLACK    ""
#define RED      ""
#define GREEN    ""
#define YELLOW   ""
#define BLUE     ""
#define PURPLE   ""
#define CYAN     ""
#define WHITE    ""

#endif

#endif
