# Makefile for mcc

CFLAGS=-Wall -std=c99 -Isys -I. -g
LDFLAGS=
MCC=mcc

UTILS_OBJ=alloc.o \
        strbuf.o \
        vector.o \
        map.o \
        wrapper.o \
        string.o

UTILS_INC= strbuf.h \
	vector.h \
	map.h \
	utils.h

CC1_OBJ=ast.o \
        cc.o \
        cpp.o \
        print.o \
        decl.o \
        error.o \
	eval.o \
        expr.o \
        gen.o \
        lex.o \
        stmt.o \
        sym.o \
        type.o \
        $(UTILS_OBJ)

CC1_INC=cc.h \
	ast.h \
        config.h \
        node.def \
        token.def \
        $(UTILS_INC)

MCC_OBJ=mcc.o
SYS_INC=sys/sys.h
SYSDIR=include/linux
SYS_OBJ:=sys/linux.o
SYS_INC+=$(wildcard $(SYSDIR)/*.h)
ARCH:=$(shell uname -m)
KERNEL:=$(shell uname)
CFLAGS+=-D_BSD_SOURCE -DCONFIG_COLOR_TERM

ifneq (, $(findstring CYGWIN, $(KERNEL)))
CFLAGS+=-DCONFIG_CYGWIN
else ifeq (Darwin, $(KERNEL))
CFLAGS+=-DCONFIG_DARWIN
else
CFLAGS+=-DCONFIG_LINUX
endif

ifeq ($(ARCH), i386)
CFLAGS+=-DCONFIG_X32
else ifeq ($(ARCH), i686)
CFLAGS+=-DCONFIG_X32
else ifeq ($(ARCH), x86_64)
CFLAGS+=-DCONFIG_X64
endif

all: $(MCC)

$(MCC): $(MCC_OBJ) $(CC1_OBJ) $(SYS_OBJ)
	$(CC) $(LDFALGS) $(MCC_OBJ) $(SYS_OBJ) $(CC1_OBJ) -o $@

$(CC1_OBJ): $(CC1_INC)

$(SYS_OBJ): $(SYS_INC)

#
# Bootstrap
#
stage1:
	$(MAKE) clean
	$(MAKE) CC=cc $(MCC)
	mv mcc stage1

stage2: stage1
	$(MAKE) clean
	$(MAKE) CC=./stage1 $(MCC)
	mv mcc stage2

stage3: stage2
	$(MAKE) clean
	$(MAKE) CC=./stage2 $(MCC)
	mv mcc stage3

bootstrap: stage3
	cmp stage2 stage3

#
# Test suite
#
TESTDIR=test
CFLAGS_TEST=-Wall -std=c99 -Os -I. -Isys -I$(TESTDIR)
TEST_MAIN_C=$(TESTDIR)/main.c
TEST_DEP=$(TEST_MAIN_C) $(TESTDIR)/test.h
TEST_INTERNAL := $(patsubst %.c, %, $(wildcard $(TESTDIR)/internal/*.c))
TESTS=$(TEST_INTERNAL)

$(TESTDIR)/internal/%: $(TESTDIR)/internal/%.c $(TEST_DEP) $(CC1_OBJ) $(SYS_OBJ)
	$(CC) $(CFLAGS_TEST) $(TEST_MAIN_C) $< $(CC1_OBJ) $(SYS_OBJ) -o $@

test: $(TESTS)
	@for test in $(TESTS); do \
	./$$test; \
	done

clean:
	@rm -f *.o *~ $(MCC) $(TESTS) sys/*.o

distclean: clean
	@rm -f stage1 stage2 stage3

.PHONY: all clean test distclean
