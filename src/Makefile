# Makefile for mcc

AR=ar
CC=cc
CFLAGS=-Wall -std=c99 -Os -Isys -Iutils
LDFLAGS=
MCC=mcc
CC1=libcc1.a
TARGETS=$(MCC) $(CC1)

UTILS_SRC=utils/alloc.c \
        utils/string.c \
        utils/vector.c \
        utils/misc.c

UTILS_INC=utils/utils.h \
        utils/macros.h

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
        $(UTILS_SRC)

CC1_INC=cc.h \
        config.h \
        node.def \
        token.def \
        $(UTILS_INC)

MCC_SRC=sys/mcc.c
SYS_INC=sys/sys.h

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
SYS_INC+=$(wildcard $(SYSDIR)/include/*.h)
SYS_SRC:=$(SYSDIR)/sys.c
MCC_SRC+=$(SYS_SRC)
MCC_INC=$(SYS_INC)
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
CFLAGS_TEST=-Wall -std=c99 -Os -Iutils -I. -I$(TESTDIR)
TEST_MAIN_C=$(TESTDIR)/main.c
TEST_DEP=$(TEST_MAIN_C) $(TESTDIR)/test.h
TEST_INTERNAL := $(patsubst %.c, %.bin, $(wildcard $(TESTDIR)/internal/*.c))
TEST_AST := $(TESTDIR)/ast/ast.bin
TESTS=$(TEST_INTERNAL) $(TEST_AST)

$(TESTDIR)/internal/%.bin: $(TESTDIR)/internal/%.c $(CC1) $(TEST_DEP)
	$(CC) $(CFLAGS_TEST) $(TEST_MAIN_C) $< $(CC1) -o $@

$(TEST_AST): $(TESTDIR)/ast/ast.c $(MCC) $(TEST_DEP) $(UTILS_INC) $(UTILS_SRC)
	$(CC) $(CFLAGS_TEST) $(TEST_MAIN_C) $(UTILS_SRC) $< $(CC1) -o $@

test: $(TESTS)
	@for test in $(TESTS); do \
	./$$test || exit; \
	done

clean:
	@rm -f *.o *.~ $(TARGETS) $(TESTS)


.PHONY: all clean test
