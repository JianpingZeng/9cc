# Makefile for mcc

CFLAGS_COMMON = -Wall -std=c99 -DBUILD_DIR='"$(shell pwd)"' -g
CFLAGS = $(CFLAGS_COMMON)
LDFLAGS =
MCC = mcc
CC1 = cc1
utils_dir = utils/
sys_dir = sys/
UTILS_OBJ =
UTILS_INC =
CC1_OBJ =
CC1_INC =
SYS_OBJ =
SYS_INC =
CONFIG_FLAGS =
KERNEL := $(shell uname)
RM = @rm -f

UTILS_OBJ += $(utils_dir)wrapper.o
UTILS_OBJ += $(utils_dir)strbuf.o
UTILS_OBJ += $(utils_dir)vector.o
UTILS_OBJ += $(utils_dir)map.o
UTILS_OBJ += $(utils_dir)string.o
UTILS_OBJ += $(utils_dir)hideset.o
UTILS_OBJ += $(utils_dir)set.o

UTILS_INC += $(utils_dir)strbuf.h
UTILS_INC += $(utils_dir)vector.h
UTILS_INC += $(utils_dir)map.h
UTILS_INC += $(utils_dir)hideset.h
UTILS_INC += $(utils_dir)set.h
UTILS_INC += $(utils_dir)utils.h

CC1_OBJ += alloc.o
CC1_OBJ += ast.o
CC1_OBJ += cc.o
CC1_OBJ += cpp.o
CC1_OBJ += print.o
CC1_OBJ += decl.o
CC1_OBJ += error.o
CC1_OBJ += eval.o
CC1_OBJ += expr.o
CC1_OBJ += gen.o
CC1_OBJ += lex.o
CC1_OBJ += stmt.o
CC1_OBJ += sym.o
CC1_OBJ += type.o
CC1_OBJ += input.o
CC1_OBJ += ir.o
CC1_OBJ += block.o

CC1_INC += cc.h
CC1_INC += ast.h
CC1_INC += mcc.h
CC1_INC += lex.h
CC1_INC += gen.h
CC1_INC += node.def
CC1_INC += token.def
CC1_INC += rop.def

SYS_OBJ += $(sys_dir)unix.o
SYS_INC += $(sys_dir)sys.h

MCC_OBJ = mcc.o

CONFIG_FLAGS += -DCONFIG_COLOR_TERM

ifeq (Linux, $(KERNEL))

SYS_INC += $(sys_dir)linux.h
CONFIG_FLAGS += -DCONFIG_LINUX
LDFLAGS += -lunwind

else ifeq (Darwin, $(KERNEL))

SYS_INC += $(sys_dir)darwin.h
CONFIG_FLAGS += -DCONFIG_DARWIN
XCODE_SDK_DIR := $(shell xcrun --show-sdk-path)
OSX_SDK_VERSION := $(shell xcrun --show-sdk-version)
CONFIG_FLAGS += -DXCODE_DIR='"$(XCODE_SDK_DIR)"'
CONFIG_FLAGS += -DOSX_SDK_VERSION='"$(OSX_SDK_VERSION)"'

else

$(error unsupported platform '$(KERNEL)')

endif

CFLAGS += $(CONFIG_FLAGS)

all: $(MCC) $(CC1)

$(MCC): $(MCC_OBJ) $(UTILS_OBJ) $(SYS_OBJ)
	$(CC) $(MCC_OBJ) $(SYS_OBJ) $(UTILS_OBJ) $(LDFLAGS) -o $@

$(CC1): $(CC1_OBJ) $(UTILS_OBJ) $(SYS_OBJ)
	$(CC) $(CC1_OBJ) $(UTILS_OBJ) $(SYS_OBJ) $(LDFLAGS) -o $@

$(CC1_OBJ): $(CC1_INC)

$(SYS_OBJ): $(SYS_INC)

$(UTILS_OBJ): $(UTILS_INC)

#
# Bootstrap
#
stage1:
	$(MAKE) objclean
	$(MAKE) CC=cc
	mv mcc stage1
	mv cc1 cc1_stage1
	ln -s cc1_stage1 cc1

stage2: stage1
	$(MAKE) objclean
	$(MAKE) CC=./stage1
	mv mcc stage2
	mv cc1 cc1_stage2
	ln -s cc1_stage2 cc1

stage3: stage2
	$(MAKE) objclean
	$(MAKE) CC=./stage2
	mv mcc stage3
	mv cc1 cc1_stage3
	ln -s cc1_stage3 cc1

bootstrap: stage3
	cmp stage2 stage3
	cmp cc1_stage2 cc1_stage3

TESTS := $(patsubst %.c, %.bin, $(wildcard test/test_*.c))

test/%.o: test/%.c
	$(CC) -Wall -std=c99 -o $@ -c $<

test/%.bin: test/%.o test/main.o $(UTILS_OBJ)
	$(CC) $(LDFLAGS) -o $@ $< test/main.o $(UTILS_OBJ)

test: $(TESTS)
	@for test in $(TESTS); do \
		./$$test && exit; \
	done

objclean:
	$(RM) *.o *~
	$(RM) $(sys_dir)*.o $(sys_dir)*~
	$(RM) $(utils_dir)*.o $(utils_dir)*~
	$(RM) $(TESTS) test/*.o test/*~
	$(RM) include/*~ mcc.exe*

clean: objclean
	$(RM) $(MCC) $(CC1)

distclean: clean
	$(RM) stage1 stage2 stage3 cc1_stage1 cc1_stage2 cc1_stage3

.PHONY: all clean distclean objclean test
