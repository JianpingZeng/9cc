
CC=cc
CFLAGS=-Wall -std=c99 -Os
LDFLAGS=
TARGETS=mcc cpp
SOURCES_MCC=alloc.c \
	ast.c \
	cc.c \
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

INCLUDES_MCC=cc.h node.h token.h lib.h mcc.h

SOURCES_CPP=alloc.c \
    string.c \
    vector.c \
    cpp.c

INCLUDES_CPP=mcc.h

.PHONY: all clean

OS:=$(shell uname -s)

ifeq ($(OS), Darwin)
SYSDIR=etc/darwin
endif

ifeq ($(OS), Linux)
SYSDIR=etc/linux
endif

ifndef SYSDIR
all:
	@echo "Error: unsupported platform '$(OS)'"
	@exit
else
all: $(TARGETS)
INCLUDES_MCC+=$(wildcard $(SYSDIR)/include/*.h)
SOURCES_MCC+=$(SYSDIR)/sys.c
INCLUDES_CPP+=$(wildcard $(SYSDIR)/include/*.h)
SOURCES_CPP+=$(SYSDIR)/sys.c
endif

cpp: $(INCLUDES_CPP) $(SOURCES_CPP)
	$(CC) $(CFLAGS) $(LDFLAGS) $(SOURCES_CPP) -o $@

mcc: $(INCLUDES_MCC) $(SOURCES_MCC) cpp
	$(CC) $(CFLAGS) $(LDFALGS) $(SOURCES_MCC) -o $@

clean:
	@rm -f *.o *~ $(SYSDIR)/*~ $(SYSDIR)/include/*~  $(TARGETS)
