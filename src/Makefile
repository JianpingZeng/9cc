
CC=cc
CFLAGS=-Wall -std=c99 -o2
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
	vector.c 

INCLUDES=mcc.h cc.h node.h token.h

.PHONY: all clean

ifndef platform
platform=mac
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
SOURCES+=$(SPEC)
endif

all: $(TARGETS)

mcc: $(INCLUDES) $(SOURCES)
	$(CC) $(CFLAGS) $(LDFLAGS) $(SOURCES) -o $@
	@strip $@

clean:
	@rm -f *.o *~ $(TARGETS)
