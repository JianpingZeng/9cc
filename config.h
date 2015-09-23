#ifndef _CONFIG_H
#define _CONFIG_H

#define SHOW_CALL_TREE

#ifdef SHOW_CALL_TREE

#define BEGIN_CALL    begin_call(__func__);
#define END_CALL      end_call(__func__);

#else

#define BEGIN_CALL
#define END_CALL

#endif	// SHOW_CALL_TREE

#ifdef CONFIG_COLOR_TERM

#define RESET    "\e[0m"
#define CLEAR    "\e[1;38m"

// regular
#define BLACK_COLOR    "\e[0;30m"
#define RED_COLOR      "\e[0;31m"
#define GREEN_COLOR    "\e[0;32m"
#define YELLOW_COLOR   "\e[0;33m"
#define BLUE_COLOR     "\e[0;34m"
#define PURPLE_COLOR   "\e[0;35m"
#define CYAN_COLOR     "\e[0;36m"
#define WHITE_COLOR    "\e[0;37m"

// bold
#define BLACK_BOLD_COLOR    "\e[1;30m"
#define RED_BOLD_COLOR      "\e[1;31m"
#define GREEN__BOLDCOLOR    "\e[1;32m"
#define YELLOW_BOLD_COLOR   "\e[1;33m"
#define BLUE_BOLD_COLOR     "\e[1;34m"
#define PURPLE_BOLD_COLOR   "\e[1;35m"
#define CYAN_BOLD_COLOR     "\e[1;36m"
#define WHITE_BOLD_COLOR    "\e[1;37m"

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

#define BLACK_BOLD_COLOR    ""
#define RED_BOLD_COLOR      ""
#define GREEN__BOLDCOLOR    ""
#define YELLOW_BOLD_COLOR   ""
#define BLUE_BOLD_COLOR     ""
#define PURPLE_BOLD_COLOR   ""
#define CYAN_BOLD_COLOR     ""
#define WHITE_BOLD_COLOR    ""

#endif	// CONFIG_COLOR_TERM

#define BLACK(str)    BLACK_COLOR str RESET
#define RED(str)      RED_COLOR str RESET
#define GREEN(str)    GREEN_COLOR str RESET
#define YELLOW(str)   YELLOW_COLOR str RESET
#define BLUE(str)     BLUE_COLOR str RESET
#define PURPLE(str)   PURPLE_COLOR str RESET
#define CYAN(str)     CYAN_COLOR str RESET
#define WHITE(str)    WHITE_COLOR str RESET

#define BLACK_BOLD(str)    BLACK__BOLD_COLOR str RESET
#define RED_BOLD(str)      RED_BOLD_COLOR str RESET
#define GREEN_BOLD(str)    GREEN_BOLD_COLOR str RESET
#define YELLOW_BOLD(str)   YELLOW_BOLD_COLOR str RESET
#define BLUE_BOLD(str)     BLUE_BOLD_COLOR str RESET
#define PURPLE_BOLD(str)   PURPLE_BOLD_COLOR str RESET
#define CYAN_BOLD(str)     CYAN_BOLD_COLOR str RESET
#define WHITE_BOLD(str)    WHITE_BOLD_COLOR str RESET

#endif
