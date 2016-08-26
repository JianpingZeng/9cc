#include "cc.h"

static void progbeg(int argc, char *argv[]);
static void progend(void);
static void defvar(node_t *node);
static void defun(node_t *node);

struct imachine *IM = &(struct imachine) {
    .progbeg = progbeg,
    .progend = progend,
    .defvar = defvar,
    .defun = defun
};

static void metrics_init(void)
{
#define METRICS(m, size, align, rank)  IM->m = (struct metrics) { size, align, rank }

    // size  align  rank
    METRICS(boolmetrics, 1, 1, 10);
    METRICS(charmetrics, 1, 1, 20);
    METRICS(shortmetrics, 2, 2, 30);
    METRICS(wcharmetrics, 4, 4, 40);
    METRICS(intmetrics, 4, 4, 40);
    METRICS(longmetrics, 8, 8, 50);
    METRICS(longlongmetrics, 8, 8, 60);
    METRICS(floatmetrics, 4, 4, 70);
    METRICS(doublemetrics, 8, 8, 80);
    METRICS(longdoublemetrics, 8, 8, 90);
    METRICS(ptrmetrics, 8, 8, 0);
    METRICS(zerometrics, 0, 1, 0);

#undef METRICS
}

static void progbeg(int argc, char *argv[])
{
}

static void progend(void)
{
}

static void defvar(node_t *node)
{
}

static void defun(node_t *node)
{
}

void arch_init(void)
{
    metrics_init();
}

