# Makefile for 7cc

# version
MAJOR = 0
MINOR = 4
FIXES = 0
EXTRAVERSION = -dev

CFLAGS = -Wall -std=c99 -I. -Ilibutils -Ilibcpp
LDFLAGS =
CONFIG_FLAGS =
KERNEL := $(shell uname)
AR = ar
ARFLAGS = cru
# install dir
INSTALL_BIN_DIR = /usr/local/bin/
INSTALL_LIB_DIR = /usr/local/lib/7cc
INSTALL_MAN_DIR = /usr/local/share/man/man1/
# 7CC_LIB_DIR = "$(shell pwd)"
7CC_LIB_DIR ?= $(INSTALL_LIB_DIR)

# build dir
BUILD_DIR = build/

# targets
7BURG = $(BUILD_DIR)7burg/7burg
7CC = $(BUILD_DIR)7cc
CC1 = $(BUILD_DIR)cc1
LIBUTILS = $(BUILD_DIR)libutils/libutils.a
LIBCPP = $(BUILD_DIR)libcpp/libcpp.a

7CC_OBJ =
7CC_INC =
CC1_OBJ =
CC1_INC =
LIBUTILS_OBJ =
LIBUTILS_INC =
LIBCPP_OBJ =
LIBCPP_INC =
7BURG_OBJ =
7BURG_INC =

# obects
7CC_OBJ += $(BUILD_DIR)7cc.o

ARCH_SPEC = x86_64-linux.7brg
ARCH_SRC = $(BUILD_DIR)x86_64-linux.c
ARCH_OBJ = $(BUILD_DIR)x86_64-linux.o

CC1_INC += cc.h
CC1_INC += gen.h
CC1_INC += debug.h

CC1_OBJ += $(BUILD_DIR)error.o
CC1_OBJ += $(BUILD_DIR)ast.o
CC1_OBJ += $(BUILD_DIR)cc.o
CC1_OBJ += $(BUILD_DIR)symtab.o
CC1_OBJ += $(BUILD_DIR)type.o
CC1_OBJ += $(BUILD_DIR)parser.o
CC1_OBJ += $(BUILD_DIR)sema.o
CC1_OBJ += $(BUILD_DIR)tree.o
CC1_OBJ += $(BUILD_DIR)eval.o
CC1_OBJ += $(BUILD_DIR)gen.o
CC1_OBJ += $(BUILD_DIR)print.o
CC1_OBJ += $(BUILD_DIR)debug.o

LIBUTILS_INC += libutils/libutils.h
LIBUTILS_INC += libutils/strbuf.h
LIBUTILS_INC += libutils/vector.h
LIBUTILS_INC += libutils/list.h
LIBUTILS_INC += libutils/color.h
LIBUTILS_INC += libutils/compat.h

LIBUTILS_OBJ += $(BUILD_DIR)libutils/alloc.o
LIBUTILS_OBJ += $(BUILD_DIR)libutils/wrapper.o
LIBUTILS_OBJ += $(BUILD_DIR)libutils/strbuf.o
LIBUTILS_OBJ += $(BUILD_DIR)libutils/vector.o
LIBUTILS_OBJ += $(BUILD_DIR)libutils/string.o
LIBUTILS_OBJ += $(BUILD_DIR)libutils/list.o
LIBUTILS_OBJ += $(BUILD_DIR)libutils/file.o

LIBCPP_INC += libcpp/token.def
LIBCPP_INC += libcpp/internal.h
LIBCPP_INC += libcpp/libcpp.h

LIBCPP_OBJ += $(BUILD_DIR)libcpp/lex.o
LIBCPP_OBJ += $(BUILD_DIR)libcpp/cpp.o
LIBCPP_OBJ += $(BUILD_DIR)libcpp/hideset.o
LIBCPP_OBJ += $(BUILD_DIR)libcpp/idtab.o
LIBCPP_OBJ += $(BUILD_DIR)libcpp/input.o
LIBCPP_OBJ += $(BUILD_DIR)libcpp/error.o
LIBCPP_OBJ += $(BUILD_DIR)libcpp/expr.o
LIBCPP_OBJ += $(BUILD_DIR)libcpp/strtab.o
LIBCPP_OBJ += $(BUILD_DIR)libcpp/sys.o

7BURG_INC += 7burg/7burg.h

7BURG_OBJ += $(BUILD_DIR)7burg/7burg.o
7BURG_OBJ += $(BUILD_DIR)7burg/grammar.o

ifneq (, ${STAGE})
CONFIG_FLAGS += -DSTAGE=${STAGE}
else
CFLAGS += -g
# CFLAGS += -pg
# LDFLAGS += -pg
endif

ifeq (Linux, $(KERNEL))
CONFIG_FLAGS += -DCONFIG_LINUX -DCONFIG_COLOR_TERM
else ifeq (Darwin, $(KERNEL))
CONFIG_FLAGS += -DCONFIG_DARWIN -DCONFIG_COLOR_TERM
XCODE_SDK_DIR := $(shell xcrun --show-sdk-path)
OSX_SDK_VERSION := $(shell xcrun --show-sdk-version)
else
$(error unsupported platform '$(KERNEL)')
endif

CFLAGS += $(CONFIG_FLAGS)

all:: config.h $(7CC) $(CC1)

$(7CC): $(7CC_OBJ) $(LIBUTILS)
	$(CC) $^ $(LDFLAGS) -o $@

$(CC1): $(CC1_OBJ) $(ARCH_OBJ) $(LIBCPP) $(LIBUTILS)
	$(CC) $^ $(LDFLAGS) -o $@

$(LIBUTILS): $(LIBUTILS_OBJ)
	$(AR) $(ARFLAGS) $@ $^

$(LIBCPP): $(LIBCPP_OBJ)
	$(AR) $(ARFLAGS) $@ $^

$(7BURG): $(7BURG_OBJ)
	$(CC) $(CFLAGS) $^ -o $@

$(ARCH_OBJ): $(ARCH_SRC)
	$(CC) $(CFLAGS) -c $(ARCH_SRC) -o $@

$(ARCH_SRC): $(ARCH_SPEC) $(CC1_INC) $(7BURG)
	$(7BURG) $(ARCH_SPEC) -o $@

$(BUILD_DIR)libutils/%.o: libutils/%.c
	$(CC) $(CFLAGS) -o $@ -c $<

$(BUILD_DIR)libcpp/%.o: libcpp/%.c
	$(CC) $(CFLAGS) -o $@ -c $<

$(BUILD_DIR)7burg/%.o: 7burg/%.c
	$(CC) $(CFLAGS) -o $@ -c $<

$(BUILD_DIR)%.o: %.c
	$(CC) $(CFLAGS) -o $@ -c $<

$(CC1_OBJ): $(CC1_INC)

$(LIBUTILS_OBJ): $(LIBUTILS_INC)

$(LIBCPP_OBJ): $(LIBCPP_INC)

$(7BURG_OBJ): $(7BURG_INC)

config.h:
	mkdir -p $(BUILD_DIR)libcpp
	mkdir -p $(BUILD_DIR)libutils
	mkdir -p $(BUILD_DIR)7burg
	@echo "/* Auto-generated by makefile. */" > $@
	@echo "#ifndef CONFIG_H" >> $@
	@echo "#define CONFIG_H" >> $@
	@echo >> $@
	@echo "#define VERSION \"$(MAJOR).$(MINOR).$(FIXES)$(EXTRAVERSION)\"" >> $@
	@echo "#define _7CC_LIB_DIR \"$(7CC_LIB_DIR)\"" >> $@
ifeq (Darwin, $(KERNEL))
	@echo "#define XCODE_DIR \"$(XCODE_SDK_DIR)\"" >> $@
	@echo "#define OSX_SDK_VERSION \"$(OSX_SDK_VERSION)\"" >> $@
endif
	@echo >> $@
	@echo "#endif" >> $@

#
# Bootstrap
#
stage1:
	$(MAKE) objclean
	$(MAKE) CC=cc STAGE=1
	mv 7cc stage1
	mv cc1 cc1_stage1
	ln -s cc1_stage1 cc1

stage2: stage1
	$(MAKE) objclean
	$(MAKE) CC=./stage1 STAGE=2
	mv 7cc stage2
	mv cc1 cc1_stage2
	ln -s cc1_stage2 cc1

stage3: stage2
	$(MAKE) objclean
	$(MAKE) CC=./stage2 STAGE=3
	mv 7cc stage3
	mv cc1 cc1_stage3
	ln -s cc1_stage3 cc1

bootstrap: stage3
	cmp stage2 stage3
	cmp cc1_stage2 cc1_stage3

install:: config.h  $(7CC) $(CC1)
	cp $(7CC) $(INSTALL_BIN_DIR)
	mkdir -p $(INSTALL_MAN_DIR)
	cp doc/7cc.1 $(INSTALL_MAN_DIR)
	mkdir -p $(INSTALL_LIB_DIR)
	cp $(CC1) $(INSTALL_LIB_DIR)
	cp -R include $(INSTALL_LIB_DIR)

uninstall::
	rm -f $(INSTALL_BIN_DIR)7cc
	rm -rf $(INSTALL_LIB_DIR)
	rm -f $(INSTALL_MAN_DIR)7cc.1

objclean::
	@rm -f $(CC1_OBJ)
	@rm -f $(LIBUTILS_OBJ) $(LIBUTILS)
	@rm -f $(LIBCPP_OBJ) $(LIBCPP)
	@rm -f $(7BURG_OBJ)

clean:: objclean
	@rm -rf $(BUILD_DIR)
	@rm -f config.h

