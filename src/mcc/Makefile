ifndef BUILDDIR
BUILDDIR=../build
endif

BUILD=$(BUILDDIR)/objects/mcc
CC=gcc
LIBCC=$(BUILDDIR)/libcc.a
CFLAGS=-c -Wall -std=c99 -I../lib
LDFLAGS=-L$(BUILDDIR) -lcc
INCLUDES=$(wildcard *.h)
SOURCES=mcc.c

ifeq ($(platform), mac)
SPEC=mac.c posix.c
endif

ifeq ($(platform), linux)
SPEC=linux.c posix.c
endif

ifeq ($(platform), windows)
SPEC=windows.c
endif

EXECUTABLE=$(BUILDDIR)/mcc

ifdef SPEC
SOURCES+=$(SPEC)
OBJECTS=$(addprefix $(BUILD)/, $(SOURCES:.c=.o))
all: $(SOURCES) $(EXECUTABLE)
else
all:
	@echo "Usage: make platform=<mac, linux or windows>"
	@exit 1
endif

$(EXECUTABLE): $(OBJECTS) $(LIBCC)
	$(CC) $(OBJECTS) $(LDFLAGS) -o $@

$(BUILD)/%.o: %.c $(INCLUDES)
	@mkdir -p $(BUILD)
	$(CC) $(CFLAGS) $< -o $@

.PHONY: clean

clean:
	@rm -f $(OBJECTS) $(EXECUTABLE)
