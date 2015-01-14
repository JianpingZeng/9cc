#ifndef mcc_config_h
#define mcc_config_h

enum {
    CC_OPTION_STD_DEFAULT,
    CC_OPTION_STD_C99,
    CC_OPTION_STD_C89,
};

enum {
    CC_OPTION_W_DEFAULT,
    CC_OPTION_W_ALL,
};

extern int cc_option_std;
extern int cc_option_w;

#define stdc99     (cc_option_std == CC_OPTION_STD_C99)
#define stdc89     (cc_option_std == CC_OPTION_STD_C89)

#define SHOW_CALL_TREE

#undef DEBUG
#define DEBUG   1

#endif
