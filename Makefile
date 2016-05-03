# Makefile for mcc

CFLAGS_COMMON=-Wall -std=c99 -DBUILD_DIR='"$(shell pwd)"'
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
	$(UTILS)set.o

UTILS_INC= $(UTILS)strbuf.h \
	$(UTILS)vector.h \
	$(UTILS)map.h \
	$(UTILS)hideset.h \
	$(UTILS)set.h \
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
	ir.o \
	block.o \
        $(UTILS_OBJ)

CC1_INC=cc.h \
	ast.h \
        mcc.h \
        node.def \
        token.def \
	gen.h \
	rop.def \
	lex.h \
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
LDFLAGS+=-lunwind

endif

CFLAGS+=$(CONFIG_FLAGS)

all: $(MCC)

$(MCC): $(MCC_OBJ) $(CC1_OBJ) $(SYS_OBJ)
	$(CC) $(MCC_OBJ) $(SYS_OBJ) $(CC1_OBJ) $(LDFLAGS) -o $@

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

TESTS := $(patsubst %.c, %.bin, $(wildcard test/test_*.c))

test/%.o: test/%.c
	$(CC) -Wall -std=c99 -o $@ -c $<

test/%.bin: test/%.o test/main.o $(UTILS_OBJ)
	$(CC) $(LDFLAGS) -o $@ $< test/main.o $(UTILS_OBJ)

test: $(TESTS)
	@for test in $(TESTS); do \
		./$$test && exit; \
	done

clean:
	@rm -f *.o *~ $(MCC) $(SYS)*.o $(SYS)*~ $(UTILS)*.o $(UTILS)*~ include/*~ mcc.exe* $(TESTS) test/*.o test/*~

distclean: clean
	@rm -f stage1 stage2 stage3

.PHONY: all clean distclean test
