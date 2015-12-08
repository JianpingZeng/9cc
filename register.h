#ifndef _REGISTER_H
#define _REGISTER_H

#define NUM_IARG_REGS  6
#define NUM_FARG_REGS  8

struct reg {
    const char *r64;
    const char *r32;
    const char *r16;
    const char *r8;
    unsigned uses;
};

enum {
    OPERAND_REGISTER,
    OPERAND_MEMORY,
    OPERAND_LITERAL
};

struct operand {
    int kind;
    union {
        const char *name;
        struct reg *reg;
    }u;
};

#endif
