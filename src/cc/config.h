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

typedef struct {
    int std;
    unsigned Wall : 1;
} CCOptions;

extern CCOptions cc_options;

#define stdc99  (cc_options.std == CC_OPTION_STD_C99)
#define stdc89  (cc_options.std == CC_OPTION_STD_C89)
#define Wall  cc_options.Wall    

#define SHOW_CALL_TREE

#undef DEBUG
#define DEBUG   1

#endif
