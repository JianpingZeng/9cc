
CC=cc
CFLAGS=-Wall -std=c99 -Os -Isys -Iutils
LDFLAGS=
TARGETS=mcc
SOURCES=ast.c \
	cc.c \
	cpp.c \
	print.c \
	decl.c \
	error.c \
	expr.c \
	gen.c \
	lex.c \
	mcc.c \
	stmt.c \
	sym.c \
	type.c \
    utils/alloc.c \
    utils/string.c \
	utils/vector.c \
    utils/misc.c

INCLUDES=cc.h \
        node.def \
        token.def \
        sys/sys.h \
        utils/utils.h


.PHONY: all clean

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
all: $(TARGETS)
INCLUDES+=$(wildcard $(SYSDIR)/include/*.h)
SOURCES+=$(SYSDIR)/sys.c
endif

mcc: $(INCLUDES) $(SOURCES)
	$(CC) $(CFLAGS) $(LDFALGS) $(SOURCES) -o $@

clean:
	@rm -f *.o *~ $(SYSDIR)/*~ $(SYSDIR)/include/*~  $(TARGETS)
