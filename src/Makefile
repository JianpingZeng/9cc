# Makefile for mcc

AR=ar
CC=cc
CFLAGS=-Wall -std=c99 -Os -Isys -Iutils
LDFLAGS=
MCC=mcc
CC1=libcc1.a
TARGETS=$(MCC) $(CC1)

CC1_SRC=ast.c \
        cc.c \
        cpp.c \
        print.c \
        decl.c \
        error.c \
        expr.c \
        gen.c \
        lex.c \
        stmt.c \
        sym.c \
        type.c \
        utils/alloc.c \
        utils/string.c \
        utils/vector.c \
        utils/misc.c

CC1_INC=cc.h \
        config.h \
        node.def \
        token.def \
        utils/utils.h \
        utils/macros.h

MCC_SRC=sys/mcc.c
MCC_INC=sys/sys.h

CC1_OBJ=$(notdir $(addsuffix .o, $(basename $(CC1_SRC))))

OS:=$(shell uname -s)

ifeq ($(OS), Darwin)
SYSDIR=sys/linux
CFLAGS+=-DDARWIN
endif

ifeq ($(OS), Linux)
SYSDIR=sys/linux
CFLAGS+=-DLINUX -D_BSD_SOURCE
endif

ifneq (, $(findstring CYGWIN, $(OS)))
SYSDIR=sys/linux
CFLAGS+=-DLINUX -D_BSD_SOURCE
endif

ifndef SYSDIR
all:
	@echo "Error: unsupported platform '$(OS)'"
	@exit
else
all: $(MCC)
MCC_INC+=$(wildcard $(SYSDIR)/include/*.h)
MCC_SRC+=$(SYSDIR)/sys.c
endif

$(MCC): $(CC1) $(MCC_INC) $(MCC_SRC)
	$(CC) $(CFLAGS) $(LDFALGS) $(MCC_SRC) $(CC1) -o $@

$(CC1): $(CC1_INC) $(CC1_SRC)
	$(CC) $(CFLAGS) -c $(CC1_SRC)
	$(AR) -rcs $@ $(CC1_OBJ)
	@rm $(CC1_OBJ)

#
# Test suite
#
TESTDIR=../test
TESTS=test-internal
CFLAGS_TEST=-Wall -std=c99 -Os -Iutils -I. -I$(TESTDIR)
TEST_MAIN_C=$(TESTDIR)/main.c
TEST_MAIN_H=$(TESTDIR)/test.h
TEST_DEP=$(TEST_MAIN_C) $(TEST_MAIN_H)
TEST_IN_H=$(TESTDIR)/internal/internal.h
TEST_IN_SRC=$(wildcard $(TESTDIR)/internal/*.c)
TEST_SRC=$(filter-out $(TESTDIR)/main.c, $(wildcard $(TESTDIR)/*.c))

test-internal: $(CC1) $(TEST_DEP) $(TEST_IN_H) $(TEST_IN_SRC)
	$(CC) $(CFLAGS_TEST) -I$(TESTDIR)/internal  $(TEST_MAIN_C) $(TEST_IN_SRC) $(CC1) -o $@

test: $(TESTS)
	@for test in $(TESTS); do \
	./$$test || exit; \
	done

clean:
	@rm -f *.o *.~ $(TARGETS) $(TESTS)


.PHONY: all clean test
