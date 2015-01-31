ifndef BUILDDIR
BUILDDIR=../build
endif

BUILD=$(BUILDDIR)/objects/cc
CC=gcc
LIBCC=$(BUILDDIR)/libcc.a
CFLAGS=-c -Wall -Wno-pointer-sign -std=c99 -I../lib
LDFLAGS=-L$(BUILDDIR) -lcc
SOURCES=$(wildcard *.c)
OBJECTS=$(addprefix $(BUILD)/, $(SOURCES:.c=.o))
EXECUTABLE=$(BUILDDIR)/cc
INCLUDES=$(wildcard *.h)

all: $(SOURCES) $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS) $(LIBCC)
	$(CC) $(OBJECTS) $(LDFLAGS) -o $@

$(BUILD)/%.o: %.c $(INCLUDES)
	@mkdir -p $(BUILD)
	$(CC) $(CFLAGS) $< -o $@

.PHONY: clean

clean:
	@rm -f $(OBJECTS) $(EXECUTABLE)
