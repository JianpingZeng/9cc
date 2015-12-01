# Makefile for mcc

CFLAGS_COMMON=-Wall -std=c99 -Isys -Iutils -I. -DBUILD_DIR='"$(shell pwd)"'
CFLAGS=$(CFLAGS_COMMON)
LDFLAGS=
MCC=mcc
UTILS=utils/
SYS=sys/

UTILS_OBJ=$(UTILS)wrapper.o \
        $(UTILS)strbuf.o \
        $(UTILS)vector.o \
        $(UTILS)map.o \
        $(UTILS)string.o \
	$(UTILS)hideset.o \
	$(UTILS)dict.o

UTILS_INC= $(UTILS)strbuf.h \
	$(UTILS)vector.h \
	$(UTILS)map.h \
	$(UTILS)hideset.h \
	$(UTILS)dict.h \
	$(UTILS)utils.h

CC1_OBJ=alloc.o \
	ast.o \
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
	input.o \
	initializer.o \
        $(UTILS_OBJ)

CC1_INC=cc.h \
	ast.h \
        mcc.h \
        node.def \
        token.def \
        $(UTILS_INC)

SYS_INC=$(SYS)sys.h
SYS_OBJ:=$(SYS)linux.o

MCC_OBJ=mcc.o

KERNEL:=$(shell uname)

CONFIG_FLAGS:=-DCONFIG_COLOR_TERM

ifneq (, $(findstring CYGWIN, $(KERNEL)))
CONFIG_FLAGS+=-DCONFIG_CYGWIN
else ifeq (Darwin, $(KERNEL))
CONFIG_FLAGS+=-DCONFIG_DARWIN
XCODE_SDK_DIR:=$(shell xcrun --show-sdk-path)
OSX_SDK_VERSION:=$(shell xcrun --show-sdk-version)
CONFIG_FLAGS+=-DXCODE_DIR='"$(XCODE_SDK_DIR)"'
CONFIG_FLAGS+=-DOSX_SDK_VERSION='"$(OSX_SDK_VERSION)"'
else
CONFIG_FLAGS+=-DCONFIG_LINUX
endif

CFLAGS+=$(CONFIG_FLAGS)

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
TEST=test/
INTERNAL=$(TEST)internal/
CPP=$(TEST)cpp/
CFLAGS_TEST=$(CFLAGS_COMMON) $(CONFIG_FLAGS) -I$(TEST)
TEST_MAIN_C=$(TEST)main.c
TEST_DEP=$(TEST_MAIN_C) $(TEST)test.h
TEST_INTERNAL := $(patsubst %.c, %.bin, $(filter-out $(INTERNAL)internal.c,$(wildcard $(INTERNAL)*.c)))
TEST_CPP := $(CPP)cpp.bin
TESTS=$(TEST_INTERNAL) $(TEST_CPP)

$(INTERNAL)%.bin: $(INTERNAL)%.c $(TEST_DEP) $(INTERNAL)internal.h $(INTERNAL)internal.c $(CC1_OBJ) $(SYS_OBJ)
	$(CC) $(CFLAGS_TEST) $(TEST_MAIN_C) $(INTERNAL)internal.c $< $(CC1_OBJ) $(SYS_OBJ) -o $@

$(TEST_CPP): $(CPP)cpp.c $(TEST_DEP) $(SYS_OBJ)
	$(CC) $(CFLAGS_TEST) $(TEST_MAIN_C) $< $(SYS_OBJ) -o $@

test: $(TESTS)
	@for test in $(TESTS); do \
	./$$test; \
	done

clean:
	@rm -f *.o *~ $(MCC) $(TESTS) $(SYS)*.o $(SYS)*~ $(UTILS)*.o $(UTILS)*~ $(INTERNAL)*~ include/*~ $(TEST)scripts/*~ mcc.exe*

distclean: clean
	@rm -f stage1 stage2 stage3

check-syntax:
	gcc $(CFLAGS) -o nul -S ${CHK_SOURCES}

.PHONY: all clean test distclean
