# Makefile for mcc

CC=cc
CFLAGS=-Wall -std=c99 -Os -Isys -I.
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
CFLAGS+=-DLINUX -D_BSD_SOURCE
SYS_INC+=$(wildcard $(SYSDIR)/*.h)

all: $(MCC)

$(MCC): $(MCC_OBJ) $(CC1_OBJ) $(SYS_OBJ)
	$(CC) $(CFLAGS) $(LDFALGS) $(MCC_OBJ) $(SYS_OBJ) $(CC1_OBJ) -o $@

$(CC1_OBJ): $(CC1_INC)

$(SYS_OBJ): $(SYS_INC)

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
	./$$test || exit; \
	done

clean:
	@rm -f *.o *~ $(MCC) $(TESTS) sys/*.o


.PHONY: all clean test
