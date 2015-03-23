
CC=cc
CFLAGS=-Wall -std=c99 -Os
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

INCLUDES=cc.h node.h token.h lib.h mcc.h

.PHONY: all clean

OS:=$(shell uname -s)

ifeq ($(OS), Darwin)
SYSDIR=etc/linux
CFLAGS+=-DDARWIN
endif

ifeq ($(OS), Linux)
SYSDIR=etc/linux
CFLAGS+=-DLINUX
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
