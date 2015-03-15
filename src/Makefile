
CC=cc
CFLAGS=-Wall -std=c99 -O2
LDFLAGS=
TARGETS=mcc
SOURCES=alloc.c \
	ast.c \
	cc.c \
	cpp.c \
	debug.c \
	decl.c \
	error.c \
	expr.c \
	gen.c \
	lex.c \
	mcc.c \
	stmt.c \
	string.c \
	sym.c \
	type.c \
	util.c \
	vector.c


INCLUDES=cc.h node.h token.h lib.h \
	include/stdarg.h include/float.h \
	mcc.h etc/linux.c etc/mac.c

.PHONY: all clean

ifndef platform
platform=mac
endif

ifndef BUILDDIR
BUILDDIR=.
endif

ifeq ($(platform), mac)
SPEC=etc/mac.c
endif

ifeq ($(platform), linux)
SPEC=etc/linux.c
endif

ifeq ($(platform), windows)
SPEC=etc/windows.c
endif

ifdef SPEC
SOURCES+=$(SPEC)
endif

all: $(TARGETS)

mcc: $(INCLUDES) $(SOURCES_MCC)
	$(CC) $(CFLAGS) $(LDFALGS) $(SOURCES) -o $@
	@strip $@

clean:
	@rm -f *.o *~ etc/*~ $(TARGETS)
