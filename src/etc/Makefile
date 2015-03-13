ifndef BUILDDIR
BUILDDIR=../build
endif

BUILD=$(BUILDDIR)/objects/etc
CC=cc
CFLAGS=-c -Wall -std=c99 -O2
INCLUDES=$(wildcard *.h)
SOURCES=cpp.c mcc.c vector.c

ifeq ($(platform), mac)
SPEC=osx.c
endif

ifeq ($(platform), linux)
SPEC=linux.c
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

$(EXECUTABLE): $(OBJECTS)
	$(CC) $(OBJECTS) $(LDFLAGS) -o $@
	strip $@

$(BUILD)/%.o: %.c $(INCLUDES)
	@mkdir -p $(BUILD)
	$(CC) $(CFLAGS) $< -o $@

.PHONY: clean

clean:
	@rm -f $(OBJECTS) $(EXECUTABLE)
