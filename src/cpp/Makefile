ifndef BUILDDIR
BUILDDIR=../build
endif

BUILD=$(BUILDDIR)/objects/cpp
CC=cc
CFLAGS=-c -Wall -std=c99 -O2
LDFLAGS=
INCLUDES=$(wildcard *.h)
SOURCES=cpp.c vector.c

ifeq ($(platform), mac)
SPEC=mac.c mac.c
endif

ifeq ($(platform), linux)
SPEC=linux.c linux.c
endif

ifeq ($(platform), windows)
SPEC=windows.c
endif

EXECUTABLE=$(BUILDDIR)/cpp

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
