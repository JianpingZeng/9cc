#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <stdint.h>
#include "cc.h"

static void init(int argc, char *argv[])
{
}

static void finalize(void)
{
}

static void defvar(struct symbol *sym, int seg)
{
}

static void defun(struct symbol *sym)
{
}

struct machine *IM = &(struct machine) {
    // os/arch
    .os = "linux",
    .arch = "x86_64",
    // size  align  rank
    .boolmetrics = {1, 1, 10},
    .charmetrics = {1, 1, 20},
    .shortmetrics = {2, 2, 30},
    .wcharmetrics = {4, 4, 40},
    .intmetrics = {4, 4, 40},
    .longmetrics = {8, 8, 50},
    .longlongmetrics = {8, 8, 60},
    .floatmetrics = {4, 4, 70},
    .doublemetrics = {8, 8, 80},
    .longdoublemetrics = {8, 8, 90},
    .ptrmetrics = {8, 8, 0},
    .zerometrics = {0, 1, 0},
    // func
    .init = init,
    .finalize = finalize,
    .defvar = defvar,
    .defun = defun,
};
