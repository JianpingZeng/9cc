
CC=cc
CFLAGS=-Wall -std=c99 -O2
LDFLAGS=
TARGETS=mcc cc
SOURCES_CC=alloc.c \
	ast.c \
	cc.c \
	debug.c \
	decl.c \
	error.c \
	expr.c \
	gen.c \
	lex.c \
	stmt.c \
	string.c \
	sym.c \
	type.c \
	vector.c \
	util.c

INCLUDES_CC=cc.h node.h token.h lib.h \
	include/stdarg.h include/float.h

INCLUDES_MCC=etc/mcc.h lib.h etc/linux.c etc/osx.c
SOURCES_MCC=vector.c util.c etc/cpp.c etc/mcc.c

.PHONY: all clean

ifndef platform
platform=linux
endif

ifndef BUILDDIR
BUILDDIR=.
endif

ifeq ($(platform), mac)
SPEC=etc/osx.c
endif

ifeq ($(platform), linux)
SPEC=etc/linux.c
endif

ifeq ($(platform), windows)
SPEC=etc/windows.c
endif

ifdef SPEC
SOURCES_MCC+=$(SPEC)
endif

all: $(TARGETS)

mcc: $(INCLUDES_MCC)
	$(CC) $(CFLAGS) -I. $(LDFALGS) $(SOURCES_MCC) -o $@
	@strip $@

cc: $(INCLUDES_CC) mcc
	$(CC) $(CFLAGS) $(LDFLAGS) $(SOURCES_CC) -o $@
	@strip $@

clean:
	@rm -f *.o *~ etc/*~ $(TARGETS)
