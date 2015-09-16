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

#define BLACK_COLOR    "\e[0;30m"
#define RED_COLOR      "\e[0;31m"
#define GREEN_COLOR    "\e[0;32m"
#define YELLOW_COLOR   "\e[0;33m"
#define BLUE_COLOR     "\e[0;34m"
#define PURPLE_COLOR   "\e[0;35m"
#define CYAN_COLOR     "\e[0;36m"
#define WHITE_COLOR    "\e[0;37m"

#else

#define RESET    ""
#define CLEAR    ""

#define BLACK_COLOR    ""
#define RED_COLOR      ""
#define GREEN_COLOR    ""
#define YELLOW_COLOR   ""
#define BLUE_COLOR     ""
#define PURPLE_COLOR   ""
#define CYAN_COLOR     ""
#define WHITE_COLOR    ""

#endif

#define BLACK(str)    BLACK_COLOR str RESET
#define RED(str)      RED_COLOR str RESET
#define GREEN(str)    GREEN_COLOR str RESET
#define YELLOW(str)   YELLOW_COLOR str RESET
#define BLUE(str)     BLUE_COLOR str RESET
#define PURPLE(str)   PURPLE_COLOR str RESET
#define CYAN(str)     CYAN_COLOR str RESET
#define WHITE(str)    WHITE_COLOR str RESET

#endif
